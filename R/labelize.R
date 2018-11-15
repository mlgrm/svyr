#' replace choice codes with labels
#' 
#' @export
labelize <- function(x) UseMethod("labelize",x)

#' s3 method for class svy
#' 
#' @export
labelize.svy <-  
  function(s){
    s <- preserve(s,function(x)lapply(x,labelize))
    s
  }

#' s3 method for class svq
#' 
#' @export
labelize.svq <- function(q){
  switch(type(q),
         "select one"={
           # message(paste(group(q),collapse = "."),".",name(q))
           # if(name(q)=="cooks_gender") browser()
           q <- preserve(q,function(x)
             factor(x,levels=levels(x),labels=labels(x)))
           q
         },
         "select all that apply"={
           colnames(q) <- labels(q)
           q
         },
         q
  )
}
