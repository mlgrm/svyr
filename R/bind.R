#' row bind two or more `svy`s together
#' 
#' @importFrom dplyr bind_cols
#' @export
bind_rows <- function(x, ...)UseMethod("bind_rows")

bind_rows.svy <- function(...){
  args <- list(...)
  suppressWarnings(dplyr::bind_rows(...))  
}

bind_rows.default <- dplyr::bind_rows
