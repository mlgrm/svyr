#' bind `svy`s column-wise, preserving matrices and the attributes of the first `svy`
#' 
#' @param l list of `svy`s to bind
#' @param ... other arguments to bind_cols
#' @param .names names of attributes to preserve from `l[[1]]`
#' 
#' @export
bind_cols.svy <- function(l, 
                          ..., 
                          .names = getOption(
                            "preserve_atts", 
                            c("node", "class", "group", "languages", "groups"))){
  # for each tibble in l, save matrix columns in attributes and
  # replace with an index
  l <- map(l, function(s){
    matcols <- which(map_lgl(s, is.matrix))
    s[matcols] <- map_dfc(s[matcols], function(c){
      mat <- c
      c <- 1:nrow(c)
      attr(c, "svq_mat") <- mat
      c
    })
    s
  })
  
  # bind the columns
  browser(expr = is(tryCatch(bind_cols(l, ...), error = identity),"error"))
  tb <- bind_cols(l, ...)
  
  # pull the matrices back out
  matcols <- which(map_lgl(tb, ~ ! is.null(attr(., "svq_mat"))))
  tb[matcols] <- map(tb[matcols], attr, "svq_mat")
  
  # copy attributes both in .names and in attributes(l[[1]]) to tb
  atts <- .names[.names %in% names(attributes(l[[1]]))]
  attributes(tb)[atts] <- attributes(l[[1]])[atts]
  
  tb
}