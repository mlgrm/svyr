#' # prepare a \code{sv(q|y)} for uploading
#' db_format <- function(x, ...)UseMethod("db_format", x)
#' 
#' #' prepare \code{svy} a data column for insertion in a database
#' db_format.svq <- function(q)
#'   switch( # convert the columns for some types
#'     make.names(type(q)),
#'     
#'     # repeats just have the number of records in the roster
#'     repeat. = structure(laply(q,length), db_type = "INTEGER"),
#'                         
#'     # select multiples become an array of the selected choices                    
#'     select.all.that.apply = structure(
#'       aaply(q, 1, function(r) 
#'         sprintf("{%s}", paste(names(r)[r], collapse = ","))),
#'       db_type = "TEXT[]"
#'     ),
#'       
#'     # geopoints become an array of latitude, longitude, altitude and precision
#'     # TODO: upgrade to PostGIS
#'     geopoint = {
#'       structure(
#'         sprintf("{%s}", aaply(q, 1, function(r)
#'           if(any(is.na(r))) "" else paste(r, collapse = ","))),
#'         db_type = "REAL[]")
#'     },
#'     # by default, just return the svq
#'     q
#'   )
#' 
#' db_format.svy <- function(s){
#'   browser()
#'   t <- s %>% 
#'     llply(db_format) %>% 
#'     as_tibble %>%
#'     structure(
#'       # confusingly, names() is overloaded for svys -- fixed!
#'       names = ifelse(
#'         is.na(all_names(s)), 
#'         # internal names
#'         all_names(s),
#'         # svqs that don't have an internal name get their column name, 
#'         # minus the groups
#'         names(s) %>% 
#'           sub("^.*/", "", .)
#'       ) %>% make.sql.names
#'     )
#' }
#' 
#' db_format.data.frame <- function(df){
#'   # just make the column names sql-compliant and remove group-like prefixes
#'   structure(df, names = sub("^.*/", "", names(df)) %>% make.sql.names) 
#' }
#' 
#' 
