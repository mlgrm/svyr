#' @export
print.svy <- function(x, ...){
  print(tibble::as_tibble(x, validate = FALSE),...)
}

#' @export
print.svq <- function(x,...){
  switch(
    type(x),
    "select one"={
      print(factor(levels(x)[x],levels=levels(x), labels=labels(x)))
    },
    "select all that apply"={
      print(tibble::as_tibble(x))
    },
    print(structure(x, class = class(x)[-1]))
  )
}

#' @export
str.svq <- function(x, ...){
  cat("survey question of type", type(x), "\n")
}

#' @export
str.svy <- function(x, ...){
  cat("survey with", nrow(x), "instances of", ncol(x), "questions\n")
}

#' @export
summary.svq <- function(x, ...){
  str(x)
  c(
    name = name(x),
    group = group(x),
    type = type(x),
    nobs = NROW(x)
  ) %>% rjust
  switch(
    type(x),
    "select all that apply"={
      cat("values:\n")
      rjust(structure(colSums(s[[96]]), names = colnames(s[[96]])))
    },
    "select one"={
      cat("values:\n")
      rjust(table(x))
    },
    summary(structure(x,class = class(x)[-1]))
  )
  cat("\n")
}

rjust <- function(s, width = max(nchar(names(s)))+2, sort = FALSE){
  if(isTRUE(sort) || sort %in% c("decreasing", "increasing")) 
    s <- sort(s, decreasing = sort != "decreasing")
  paste0(
    stringr::str_pad(names(s), width),
    ": ",
    stringr::str_pad(s, width = max(nchar(s)), 
                     side = if(is.numeric(s)) "left" else "right")
  ) %>% 
  cat(sep = "\n")
}

