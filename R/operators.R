
#' compose two or more functions (mainly for use in *apply and plyr functions)
'%*%' <- function(f,g) UseMethod("compose",f)
compose.function <- function(f,g)function(...)f(g(...))
compose.default <- base::'%*%'

#' extract or replace parts of a \code{svy} object
#' 
#' @export
#' @rdname Extract.svy
'[.svy' <- function(x, i, j, ...){
  nargs_ <- nargs()
  missingi <- missing(i)
  missingj <- missing(j)
  if(nargs_ == 2){ # only one parameter; this is a column edit
    if(length(i)==1){ # if i is just one column, return a one question svy
      y <- tibble(x[,i])
      names(y) <- names(x)[i]
      y <- copy_atts(y,x)
      return(y)
    } else {
      y <- x[ ,i]
      return(y) # otherwise treat it like a two param call
    }
  }
  if(missingi){ # not editing rows
    y <- x
  } else { # editing rows
  if(missingj) j <- 1:ncol(x)
  y <- preserve(x,function(a){
    a <- as.data.frame(a)
    browser()
    a <- a[i,j]
    a <- as_tibble(a, validate = FALSE) 
  })
  }
  if(missingj){ # not editing columns
  } else {
    if(length(j)==1) y <- getElement(y,j) else {
      z <- preserve(y,function(a){
        a <- as.data.frame(a)
        a <- a[,j]
        a <- as_tibble(a, validate = FALSE)
      }) # do the subset, preserving atts
      if(!all(names(z) %in% names(y))){ # if new names appear
        z_ <- z[names(z) %in% names(y)] 
        z_ <- copy_atts(z_,y[names(z_)]) # copy atts for unchanged names
        extra <- z[! names(z) %in% names(y)]
        basenames <- sub("\\.[0-9]+$", names()) # find the origninal names
        stopifnot(all(basenames %in% names(z_))) # copy atts for new names
        extra <- copy_atts(extra, y[basenames])
        y <- cbind(z_,extra)[match(names(y),c(names(z_),names(extra)))]
        y <- copy_atts(y,z, recursive = FALSE)
      } else {
        z <- copy_atts(z, y)
        y <- z
      }
    }
  }
  y
}


