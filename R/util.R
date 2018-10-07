library(plyr)
library(dplyr)

#' @export
node <- function(x)attr(x,"node")

#' @export
name <- function(x, use.node = TRUE, empty.string = FALSE){
  if(use.node) x <- node(x)
  if(empty.string && is.null(x$name)) "" else x$name
}

#' @export
names.svy <- function(s)laply(s, name, empty.string = TRUE)

#' @export
type <- function(x, use.node=TRUE, empty.string = FALSE){
  if(use.node) x <- node(x)
  if(empty.string && is.null(x$type)) "" else x$type
}
#' @export
types <- function(s)laply(s, type, empty.string = TRUE)

#' @export
label <- function(x,use.node=TRUE){
  if(use.node) node <- node(x) else node <- x
  lbl <- if(is.list(node$label))
    node$label[[getOption("svyLang","English")]] else
      if(is.null(node$label)) "" else node$label
  # we can only get the question and answer if we got the whole datum
  if(use.node && !is.null(type(x)) && type(x)=="select all that apply")
    lbl <- paste(lbl,labels(x)[selected(x)],sep=":")
  lbl
}

#' @export
labels.svq <- function(x,use.node=TRUE){
  if(use.node) node <- node(x) else node <- x
  lbls <- sapply(node$children,label,use.node=FALSE)
  names(lbls) <- sapply(node$children,name,use.node=FALSE)
  lbls
}

#' @export
labels.svy <- function(s)laply(s,label)


#' @export
class1 <- function(x)class(x)[1]

#' @export
preserve <- function(x,fun,...) UseMethod("preserve", x)

# preserve.list <- function(
#   l, fun,...){
#   incl=getOption("svyAttrIncl",names(attributes(x)))
#   incl.list=getOption("svyAttrInclList", c(
#     "node",
#     "group",
#     "data"
#   ))
#   l0 <- fun(l,...)
#   for(i in 1:length(l)) attributes(l0[[i]])[incl] <- attributes(l[[i]])[incl]
#   attributes(l0)[incl.list] <- attributes(l)[incl.list]
#   if(class(l)[1]!=class(l0)[1]) class(l0) <- c(class(l)[1],class(l0))
#   l0
# }
# 
# preserve.data.frame <- preserve.list
# 
preserve.default <- function(
  x,fun,incl=getOption("svyAttrIncl",c("class","group","node","data")),...){
  # browser()
  x0 <- fun(x,...)
  incl <- incl[incl %in% names(attributes(x))]
  attributes(x0)[incl] <- attributes(x)[incl]
  if(class(x0)[1]!=class(x)[1]) class(x0) <- c(class(x)[1],class(x0))
  x0
}

selected <- function(x)attr(x,"selected")
group <- function(x)attr(x,"group")
data <- function(x)attr(x,"data")

#' s3 generic to list the choices in a survey or question
#'
#' @export
choices <- function(x,...) UseMethod("choices",x)

#' @export
choices.svq <- function(x,...)sapply(node(x)$children, getElement, "name")

#' @export
choices.svy <- function(x,...){
  x <- x[sapply(x, type) %in% c("select one","select all that apply")]
  ldply(x,function(q){
    tibble(
      choice=choices(q),
      labels=labels(q)
    )  
  },.id="name")
}

#' @export
counts <- list(
  select.one=function(x)as.integer(table(x)),
  select.all.that.apply=function(x)colSums(x,na.rm=TRUE)
)

data <- function(x) UseMethod("data", x)
data.svr <- function(x,...){
  l <- lapply(names(x),function(n){
    df <- as.data.frame(x[[n]])
    # if(is(tryCatch(rep(n,nrow(df)),error=identity),"error"))browser()
    df <- cbind(id=rep(n,nrow(df)),df)
  })
  do.call(rbind,l)
}
data.svq <- identity
data.svg <- function(x)attr(x,"data")
data.svy <- function(x)attr(x,"data")

apply <- function(x,f,...)UseMethod("apply", x)

apply.svy <- function(s,f,...)preserve(s,function(x)
  lapply(x,function(x1){
    browser()
    apply(x1,f,...)
  })
)
apply.svg <- apply.svy
apply.svr <- apply.svy
apply.svq <- function(x,f,...)preserve(x,f,...)

#' turn a data.frame into the instance list for kobo
df2dat <- function(df)
  df %>% 
  as.matrix %>%
  t %>% 
  as.data.frame(check.names=F,stringsAsFactors=F) %>% 
  lapply(function(r){names(r) <- names(df); r})

