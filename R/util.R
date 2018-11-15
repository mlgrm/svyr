#' get or set the node of a \code{svq|svy}
#' @export
node <- function(x)attr(x,"node")

#' @rdname node
#' @export
'node<-' <- function(x,value) structure(x, node = value)


#' @export
name <- function(x, use.node = TRUE, empty_as_na = TRUE){
  if(use.node) x <- node(x)
  if(empty_as_na && is.null(x$name)) NA else x$name
}

#' @export
all_names <- function(s)laply(s, name, empty_as_na = TRUE)

#' @export
type <- function(x, use.node=TRUE, empty_as_na = TRUE){
  if(use.node) x <- node(x)
  if(empty_as_na && is.null(x$type)) NA else x$type
}
#' @export
types <- function(s)laply(s, type, empty_as_na = TRUE)

#' S3 method for extracting a \code{svq}'s question label
#'
#' @importFrom xtable label
#' @export
label.svq <- function(x, 
                  lang = if(is.null(languages(x))) NULL else 
                    getOption("svyLang", "English"), 
                  use.node = TRUE){
  if(use.node) node <- node(x) else node <- x
  if(is.list(node$label)){ # if it's a list, specify language
    i <- match(tolower(lang), tolower(names(node$label))) # ignore caps
    if(is.na(i)){
      warning("language ", lang, " not available. ", 
                      "available languages: ", 
                      paste(names(node$label), collapse = ", "),
                      ".")
      return(NA)
    }
    lbl <- node$label[[i]] # choose the matching language
  } else {
    # if there's no label, empty string, else the string
    if(is.null(node$label))
      lbl <- NA else 
        lbl <- node$label
  }
  lbl
}

#' @export
has_label <- function(q, use.node = TRUE){
  if(use.node) node <- node(q) else node <- q
  ! is.null(node$label)
}
#' extract the choice labels of a multiple choice question in a language
#' @export
labels.svq <- function(x, 
                       lang = if(is.null(languages(x))) NULL else 
                         getOption("svyLang", "English"),
                       use.node = TRUE){
  if(use.node) node <- node(x) else node <- x
  lbls <- sapply(node$children, label.svq, lang = lang, use.node=FALSE)
  names(lbls) <- sapply(node$children,name,use.node=FALSE)
  lbls
}

has_labels <- function(q) 
  any(laply(node(q)$children, has_label, use.node = FALSE))

#' get the languages of a survey
#' 
#' @details if the language attribute is unset, languages will determine
#' the languages from the svqs, set the attribute, and return the value
#' 
#' @return a character vector of the languages of the survey, or "default"
#' if there are none
#' 
#' @export
languages <- function(s){
  if(! is.null(attr(s, "languages"))) return(attr(s,"languages"))
  attr(s, "languages") <- 
    llply(s, function(x)names(node(x)$label)) %>% 
    unlist %>% 
    unique %>%
    { if(is.null(.)) "default" else . }
  attr(s, "languages")
}

#' extract all the question labels of a survey
#' @export
labels.svy <- function(s)laply(s,label)


#' @export
class1 <- function(x)class(x)[1]

#' perform an arbitrary function on an object, preserving some attributes 
#' 
#' @export
preserve <- function(x, ...)UseMethod("preserve")

preserve.svq <- function(x, 
                         fun, 
                         atts = c("group","node"),
                         ...){
  x %>%
    structure(., class = class(.)[-1]) %>% 
    preserve(fun, atts = atts, ...) %>% 
    structure(., class = c("svq", class(.)))
}

preserve.svy <- function(x,
                         fun,
                         atts = c("group", "node", "languages"),
                         recursive = FALSE,
                         ...){
  x %>% 
    structure(., class = class(.)[-1]) %>% 
    preserve(fun, atts = atts, recursive = TRUE) %>% 
    structure(., class = c("svy", class(.)))
}

preserve.default <- function(x,
                             fun,
                             atts,
                             recursive = FALSE,
                             ...){
  x %>% 
    fun(...) %>% #debug_pipe %>%  
    copy_atts(x, atts = atts)
}

#' copy some attributes from one object to another, recursively
#' 
#' @export
#' @rdname copy_atts
copy_atts <- function(to, from, atts, recursive = FALSE){
  # first copy childrens' attributes, then our own
  if(recursive && is.list(to)){
    for(n in intersect(names(to), names(from))) 
      to[[n]] <- copy_atts(to[[n]], from[[n]])
  }
  atts <- atts[atts %in% names(attributes(from))]
  attributes(to)[atts] <- attributes(from)[atts]
  # if(class(to)[1] != class(from)[1]) class(to) <- c(class(from)[1], class(to))
  to
}

# selected <- function(x)attr(x,"selected")

#' get or set the group of an \code{svq}
#' @export
group <- function(x, empty_as_na = TRUE)
  if(empty_as_na && is.null(attr(x,"group"))) NA else attr(x,"group")

#' @rdname group
#' @export
'group<-' <- function(x,value) structure(x, group = value)

groups <- function(x)laply(x,group, empty_as_na = TRUE)

# print the db_type of a svq
db_type <- function(q)if(is.null(attr(q,"db_type"))) NA else attr(q, "db_type")

#' set the \code{db_type} of an array
#'
#' warning: \code{db_type} does no checking 
'db_type<-' <- function(x, value) structure(x, db_type = value)

data <- function(x)attr(x,"data")

#' s3 generic to list the choices in a survey or question
#'
#' @export
choices <- function(x,...) UseMethod("choices",x)

#' @export
choices.svq <- function(x,...)sapply(node(x)$children, getElement, "name")

#' @export
choices.svy <- function(x,...){
  x <- x[,laply(x, type) %in% c("select one","select all that apply")]
  as_tibble(
    ldply(x,function(q){
      tibble(
        choice=choices(q),
        labels=labels(q)
      )  
    },.id="name")
  )
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

