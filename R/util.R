library(plyr)
library(dplyr)

#' get or set the node of a \code{svq|svy}
#' @export
node <- function(x)attr(x,"node")

#' @rdname node
#' @export
'node<-' <- function(x,value) structure(x, node = value)


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
label <- function(x, lang=getOption("svyLang", "English"), use.node = TRUE){
  if(use.node) node <- node(x) else node <- x
  if(is.list(node$label)){ # if there is more than one label
    i <- match(tolower(lang), tolower(names(node$label))) # ignore caps
    
    if(is.na(i)) stop("language ", lang, "not available. ", 
                      "available languages:", 
                      paste(names(node$label), collapse = ", "),
                      "."
    )
    lbl <- node$label[[i]] # choose the matching language
  } else {
    # if there's no label, empty string, else the string
    if(is.null(node$label))
      lbl <- "" else 
        lbl <- node$label
  }
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

#' get or set the languages of a survey
#' @export
languages <- function(s){
  if(!is.null(attr(s, "languages"))) return(attr(s,"languages"))
  llply(s, function(x)names(node(x)$label)) %>% 
    unlist %>% 
    unique
}

#' @rdname languages
#' @export
'languages<-' <- function(x,value) structure(x, languages = value)

#' @export
labels.svy <- function(s)laply(s,label)


#' @export
class1 <- function(x)class(x)[1]

#' perform an arbitrary function on an object, preserving some attributes 
#' 
#' @export
preserve <- function(x,
                             fun,
                             atts=getOption("svyAttrIncl",
                                            c(
                                              "group",
                                              "node", 
                                              "languages"
                                            )
                             ),
                             ...){
  x %>% 
    fun(...) %>% #debug_pipe %>%  
    copy_atts(x, atts = atts)
}

#' copy some attributes from one object to another, recursively
#' 
#' @export
#' @rdname copy_atts
copy_atts <- function(to, from, atts=getOption("svyAttrIncl", 
                                                    c(
                                                      "group",
                                                      "node", 
                                                      "languages"
                                                      )
                                                    ),
                      recursive=TRUE){
  # first copy childrens' attriibutes, then our own
  if(recursive && is.list(to) && length(to)==length(from)){
    for(n in names(to)) to[[n]] <- copy_atts(to[[n]],from[[n]])
    # to <- llply(purrr::transpose(list(to = to, from = from)), function(l){
    #   copy_atts(to = l$to, from = l$from, atts = atts)
    # })
  }
  atts <- atts[atts %in% attributes(from)]
  attributes(to)[atts] <- attributes(from)[atts]
  if(class(to)[1]!=class(from)[1]) class(to) <- c(class(from)[1], class(to))
  to
}

# selected <- function(x)attr(x,"selected")

#' get or set the group of an \code{svq}
#' @export
group <- function(x)attr(x,"group")

#' @rdname group
#' @export
'group<-' <- function(x,value) structure(x, group = value)

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

