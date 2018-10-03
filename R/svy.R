#' create a svy object from odk's native form and data objects
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
#' \dontrun{
#' use the default functions to retrieve the data from a kobo server
#' s <- svy()
#' load a svy from local data:
#' s <- svy(node = jsonlite::fromJSON("data/form.json"),
#' dat = jsonlite::fromJSON("data/data.json"))
svy <- function(dat = kobo_data(),
                form = kobo_form(),
                group = NULL) {
  dat %<>%
    as_tibble %>%
    svq.group(node = form, group = group) %>%
    # (function(x){browser();x}) %>%
    as_tibble(validate = FALSE) %>%
    structure(class = c("svy", class(tibble())))
}

#' S3 generic for converting odk data in it's native json format to a tibble
#as_tibble <- function(x,...)UseMethod("as_tibble", x)

as_tibble.odk_data <- function(d){
  Reduce(union, lapply(d,names)) %>% # all names that appear in any instance
    sapply(function(n){
      qd <- sapply(d,function(i)
        if (n %in% names(i)) i[[n]] else NA_character_, simplify = FALSE
      )
      if(all(sapply(qd,is.character))) qd <- unlist(qd)
      qd
    }, simplify = F
    ) %>%

    as_tibble(validate = FALSE)
}

#' a pseudo generic for doing dispatch on survey questions.
svq <- function(dat, node, group){
  get0(paste("svq", make.names(node$type),sep = "."),
       mode = "function",
       ifnotfound = svq.default
  )(dat, node, group) %>%
  structure(node = node,
            group = group,
            class = class(c("svq",class(dat))))
}

svq.group <- function(dat, node, group){
  # if node$type is "survey", this is the survey head, and the node name is
  # not the group name.  otherwise push the node name onto the group list
  if(node$type!="survey") group <- paste(c(group, node$name), collapse = "/")

  node$children %>%
    lapply(function(qn)
      if (qn$type != "group"){
        # data becomes a single character vector for individual questions
        cn <- paste(c(group, qn$name), collapse = "/")
        if(! cn %in% colnames(dat)){
          warning("question \"", cn, "\" not found in data, filling with NA")
          dat <- rep(NA, NROW(dat))
        } else {
          dat %<>%
            getElement(cn) %>%
            as.character %>%
            svq(qn, group)
        }
        dat %<>%
          structure(node = qn, group = group) %>%
          list %>%
          structure(names = cn)
      } else svq(dat, qn, group)) %>%
    do.call(c, .)
}

svq.survey <- svq.group

# repeat is a reserved word and make.names adds the terminal dot
svq.repeat. <- function(dat, node, group){
  dat %>%
    lapply(svy, form = node, group = group) %>%
    structure(node = node, group = group)
}

svq.select.all.that.apply <- function(dat, node, group){
  dat %>%
    strsplit(" ") %>%
    lapply(function(r)1:length(node$children) %in%
             match(r,sapply(node$children,getElement,"name"))) %>%
    do.call(rbind, .)
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
svq.note <- function(dat, ...)rep('',nrow(dat))
svq.geopoint <- function(dat, node, group){
  dat %>%
    strsplit(" ") %>%
    do.call(rbind, .) %>%
    structure(dimnames = list(NULL,c(
      "latitude",
      "longitude",
      "altitude",
      "precision"
    )))
}

svq.start <- svq.end <-
  svq.dateTime <- function(dat,...)kobo_time_parser_UTC(dat)

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



