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
  if(length(dat)==1 && is.na(dat)) return(NULL)
  dat %<>%
    as_tibble %>%
    svq.group(node = form, group = group) %>%
    as_tibble(validate = FALSE) %>%
    structure(class = c("svy", class(tibble())), 
              node=form, 
              languages=languages(s)
    )
}

#' a pseudo generic for doing dispatch on survey questions.
svq <- function(dat, node, group){
  get0(paste("svq", make.names(node$type),sep = "."),
       mode = "function",
       ifnotfound = svq.default
  )(dat, node, group) %>%
  structure(node = node,
            group = group,
            class = c("svq",class(dat)))
}

svq.group <- function(dat, node, group){
  # if node$type is "survey", this is the survey head, and the node name is
  # not the group name.  otherwise push the node name onto the group list
  if(node$type!="survey") group <- paste(c(group, node$name), collapse = "/")

  node$children %>%
    lapply(function(qn) # for each child in node
      if (qn$type != "group"){ # if it's not a group, process it as a svq
        cn <- paste(c(group, qn$name), collapse = "/") # column name
        if(! cn %in% colnames(dat)){ # there is no data with that name
          warning("question \"", cn, "\" not found in data, filling with NA")
          dat <- rep(NA, NROW(dat)) # in this scope, dat become a single col
        } else {
          dat %<>% 
            getElement(cn) %>% # dat becomes the one column in dat
            svq(qn, group) # process as a question
        }
        dat %<>%
          structure(node = qn, group = group) %>% # add svg attributes
          list %>% # protect in a list
          structure(names = cn) # name the element in the list by the column
      } else svq(dat, qn, group)) ->.#%>% # sub-groups get passed in again. 
    do.call(c, .)
}

svq.survey <- svq.group

# repeat is a reserved word and make.names adds the terminal dot
svq.repeat. <- function(dat, node, group){
  stopifnot(is.list(dat))
  dat[!sapply(dat,is.null)] %<>%
    lapply(structure, class="odk_data") %>% 
    lapply(svy, form = node, group = group) %>%
    structure(node = node, group = group)
  dat
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



