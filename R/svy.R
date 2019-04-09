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
svy <- function(dat = kobo_data(),
                form = kobo_form(),
                group = NULL) {
  if(length(dat)==1 && is.na(dat)) return(NULL)
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
svq <- function(dat, node, group){
  # if(node$type == "integer") browser()
  get0(paste("svq", make.names(node$type),sep = "."),
       mode = "function",
       ifnotfound = svq.default
  )(dat, node, group) %>%
  structure(., 
            node = node,
            group = group,
            class = c("svq",class(.)))
}

svq.group <- function(dat, node, group){
  # if node$type is "survey", this is the survey head, and the node name is
  # not the group name.  otherwise push the node name onto the group list
  if(node$type != "survey") group <- paste(c(group, node$name), collapse = "/")

  node$children %>%
    # for each child in node
    lapply(function(qn) 
      # if it's not a group, process it as a svq
      if (qn$type != "group"){ 
        # column name expected in dat
        cn <- paste(c(group, qn$name), collapse = "/")
        # there is no data column with that name, warn and fill with na's
        if(! cn %in% colnames(dat)){
          warning("question \"", cn, "\" of type \"", qn$type, 
                  "\" not found in data, filling with NA")
          dat <- rep(NA, NROW(dat))
        # otherwise pull the right column
        } else dat %<>% getElement(cn) 
        dat %<>%
          # process as a question
          svq(qn, group) %>%  
          # add svg attributes
          structure(node = qn, group = group) %>% 
          list %>% # protect in a list
          structure(names = cn) # name the element in the list by the column
      } else svq(dat, qn, group)) %>% #%>% # sub-groups get passed in again. 
    do.call(c, .)
}

svq.survey <- svq.group

# repeat is a reserved word and make.names adds the terminal dot
svq.repeat. <- function(dat, node, group){
  # stopifnot(is.list(dat))
  # if(!is.list(dat)) browser()
  dat[!(sapply(dat,is.null) | is.na(dat))] %<>%
    lapply(structure, class="odk_data") %>% 
    lapply(svy, form = node, group = group) %>%
    structure(node = node, group = group)
  dat
}

svq.select.all.that.apply <- function(dat, node, group){
  ch <- sapply(node$children, getElement, "name") # choice names
  dat[is.na(dat)] <- ""
  dat %>% #debug_pipe(expr = ! is.character(.)) %>% 
    strsplit(" ") %>% # the ith element is a vector of the ith element of dat
    ldply(function(r)ch %in% r) %>%
    as.matrix %>% 
    structure(dimnames=list(NULL,ch))
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



