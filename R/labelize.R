labelize <- function(x) UseMethod("labelize",x)

labelize.list <- function(s){
  s <- preserve(s,function(x)lapply(x,labelize))
  s
}

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
