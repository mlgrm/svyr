#' replace choice codes with labels
#' 
#' @export
labelize <- function(x) UseMethod("labelize",x)

#' s3 method for class svy
#' 
#' @export
labelize.svy <-  
  function(s, lang = getOption("svyLang", "English")){
    s <- preserve(s,function(x)tibble::as_tibble(llply(x,labelize), 
                                                 validate = F))
    s
  }

#' s3 method for class svq
#' 
#' @export
labelize.svq <- function(q, lang = getOption("svyLang", "English")){
  switch(type(q, empty_as_na = FALSE),
         "select one"={
           levels(q) <- labels(q, lang = lang)
           q
         },
         "select all that apply"={
           colnames(q) <- labels(q, lang = lang)
           q
         },
         q
  )
}

#' @export
labelize.default <- identity

