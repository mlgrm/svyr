library(xtable)

#' show a dataframe as html in the viewer so you can copy and paste into a
#' spreadsheet
view <- function(df){
  f <- tempfile(fileext = ".html")
  print(xtable(df),file=f,type="html")
  getOption("viewer")(f)
  invisible(f)
}
