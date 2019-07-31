#' create a svy object
#'
#' @description
#' `svy()` is a [tibble()] with one element for each question containing the
#' answers and all metadata in the attributes.
#'
#' @param dat a tibble containing the raw data as returned by `as_tibble()`
#' on odk data as stored in ODK's native json format
#' @param form a list created using [jsonlite::fromJSON()] on ODK's native json
#' format.
#' @param group this is only used when called on a roster within a group.
#' @export
#' @examples
#' use the default functions to retrieve the data from a kobo server
#' \dontrun{
#' s <- svy()
#' }
#' load a svy from local data:
#' \dontrun{
#' s <- svy(node = jsonlite::fromJSON("data/form.json"),
#' dat = jsonlite::fromJSON("data/data.json"))
#' }
#' @import magrittr tidyverse
svy <- function(dat = kobo_data(),
                form = kobo_form(),
                group = NULL) {
  if(length(dat)==1 && is.na(dat)) return(NULL)
  if(is.character(dat) && length(dat) == 1) 
    dat <- read_csv(dat, na = "n/a", col_types = cols(.default = "c"))
  dat %<>%
    tibble::as_tibble() %>%
    svq.group(node = form, group = group) %>%
    tibble::as_tibble(validate = FALSE) %>%
    structure(., class = c("svy", class(.)), 
              node=form, 
              languages=languages(.)
    )
}

#' a pseudo generic for creating \code{svq} objects.
#' @export
svq <- function(dat, node, group){
  fun <- get0(
    str_glue("svq.{make.names(node$type)}"),
    mode = "function",
    ifnotfound = svq.default
  )
  fun(dat, node, group) %>%
  structure(., 
            node = node,
            group = group,
            class = c("svq",class(.)))
}

svq.group <- function(dat, node, group){
  map(node$children, function(n){
    
    # recurse on groups
    if(n$type == "group") return(svq.group(dat, n, c(group, n$name)))
    col_name <- str_flatten(c(group, n$name), "/")
    
    # the csv export format places select multiples in multiple
    # columns so we have to allow that pattern extension
    pattern <- str_glue("^{col_name}(/[A-z0-9_]+)?$")
    datum <- dat[str_which(names(dat), pattern)]
    
    if(length(datum) == 1) datum <- datum[[1]]
    if(length(datum) == 0) datum <- rep(NA_character_, nrow(dat))
    
    svq(datum, n, group) %>%
      # (function(x){ browser(expr = n$name == "uuid"); x }) %>% 
      tibble() %>% 
      structure(
        names = col_name, 
        class = c("svy", class(.)), 
        node = node, 
        group = group
      )
  }) %>% 
    # debug_pipe() %>% 
    bind_cols.svy()
}


# svq.group <- function(dat, node, group){
#   col_name <- function(qn) c(group, qn$name) %>% str_flatten("/")
#   map(
#     node$children %>% set_names(map_chr(., col_name)), 
#     function(qn){
#       # for each child in node
#       cn <- col_name(qn)
#       
#       # column name pattern expected in dat
#       # allow for choice names in select_multiple
#       pattern <- str_glue(
#         "^{cn}(/[A-z0-9_]+)?$"
#       )
#       
#       # browser(expr = qn$type == "group")
#       # if it is a group, recurse
#       if(qn$type == "group"){
#         return(
#           svq.group(
#             dat = dat, 
#             node = qn, 
#             group = c(group, qn$name)
#           )
#         )
#       }
#       
#       # if there are multiple columns that start with that name, it may
#       # be a select multiple loaded from a csv export of the data
#       if(any(str_detect(names(dat), pattern))){
#         dat <- dat[str_which(names(dat), pattern)]
#       } else {
#         # if there is no data column with a matching name 
#         # warn (if not a note) and fill with na's
#         if(qn$type != "note")
#           warning(
#             "question \"", cn, "\" of type \"", qn$type, 
#             "\" not found in data, filling with NA"
#           )
#         dat <- tibble(a = rep(NA, NROW(dat))) %>% set_names(qn$name)
#       }
#       
#       stopifnot(is_tibble(dat))
#       dat %<>%
#         # svq.<type> expects one-column data to be a vector and 
#         # returns a vector
#         # FIXME
#         { if(NCOL(.) == 1) .[[1]] else . } %>% 
#         # process as a question
#         svq(qn, group) %>%
#         # rewrap if necessary
#         { if(is_tibble(.)) . else tibble(.) } %>%
#         # hide matrices in the attributes
#         { 
#           if(length(.) == 1 && is.matrix(.[[1]])) 
#             .[[1]] <- structure(1:nrow(.[[1]]), matrix = .[[1]])
#           .
#         } %>%
#         structure(names = cn)
#     }) %>%
#       (function(x){
#         browser(expr = is(tryCatch(bind_cols(x), error = identity),"error"))
#         x
#       }) %>% 
#     # debug_pipe %>%
#     bind_cols %>%
#     # restore the matrices
#     { 
#       for(colname in colnames(.)) 
#         if(!is.null(attr(.[[colname]], "matrix"))){
#           .[[colname]] <- attr(.[[colname]], "matrix")
#           attr(.[[colname]], "matrix") <- NULL
#         }
#       .
#     }
# }

svq.survey <- svq.group

# repeat is a reserved word and make.names adds the terminal dot
svq.repeat. <- function(dat, node, group){
  dat[!(map_lgl(dat,is.null) | is.na(dat))] %<>%
    map(structure, class = "odk_data") %>% 
    map(
      ~ if(length(.) == 0) list() else 
        svy(., form = node, group = c(group, node$name))
    )
  dat
}

svq.select.all.that.apply <- function(dat, node, group){
  choice_names <- map_chr(node$children, getElement, "name") # choice names
  
  # multicolumn dat means we have one column for each choice
  if( NCOL(dat) > 1 ){
    dat %<>% 
      map_dfc(as.logical) %>% 
      as.matrix(dat) %>%
      structure(dimnames = list(NULL, choice_names)) 
  # otherwise, we have a vector of strings
  } else {
    dat <- 
      dat %>% #debug_pipe %>% 
      str_split(" ") %>% # the ith element is a vector of the ith element of dat
      map_dfc(
        function(r)
          if(length(r) == 1 && is.na(r)) rep(NA, length(choice_names)) else
            choice_names %in% r
      ) %>%
      as.matrix %>% 
      t
    dimnames(dat) <- list(NULL, choice_names)
  }
  dat
}

svq.select.one <- function(dat, node, group){
  dat %>%
    factor(levels = sapply(node$children, getElement, "name"))
}

svq.integer <- function(dat,...) as.integer(dat)
svq.decimal <- function(dat, ...) as.numeric(dat)
svq.range <- function(x, ...)
  if(x %>% as.numeric %% 1 == 0 %>% all(na.rm = T))
    as.integer(x) else as.numeric(x)
svq.note <- function(dat, ...)rep('', NROW(dat))
svq.geopoint <- function(dat, node, group){
  dat %>%
    as.character %>% 
    strsplit(" ") %>%
    do.call(rbind, .) %>%
    as.numeric %>% 
    matrix(ncol = 4) %>% 
    structure(dimnames = list(NULL,c(
      "latitude",
      "longitude",
      "altitude",
      "precision"
    )))
}

svq.start <- 
  svq.end <-
  svq.dateTime <- 
  function(dat,...)kobo_time_parser_UTC(dat)

svq.today <- svq.date <- function(dat,...) as.Date(dat)

svq.default <- function(dat,...) dat


# these two functions borrowed from:
# https://github.com/mrdwab/koboloadeR

kobo_time_parser <- function(instring, timezone = Sys.timezone()) {
  format(kobo_time_parser_UTC(instring), tz = timezone, usetz = TRUE)
}

kobo_time_parser_UTC <- function(instring) {
  tmp <- gsub("\\.\\d{3}|:", "", instring)
  tmp <- chartr(" ", "0", format(tmp, justify = "left", width = 22))
  as.POSIXct(strptime(tmp, format = "%Y-%m-%dT%H%M%S%z", tz = "UTC"))
}



