
#' compose two or more functions (mainly for use in *apply functions)
'%*%' <- function(f,g) UseMethod("compose",f)
compose.function <- function(f,g)function(...)f(g(...))
compose.default <- base::'%*%'

#' subset a survey
#' 
#' @export
'[.svy' <- function(x, i, j, ...){
  missingi <- missing(i)
  missingj <- missing(j)
  if(missingi){ # not editing rows
    y <- x
  } else { # editing rows
  if(missingj) j <- 1:ncol
  y <- preserve(x,function(y)
    y %>% 
      as.data.frame %>% 
      (function(a)a[i,j,...]) %>% 
      as_tibble
  )
  }
  if(missingj){ # not editing columns
  } else {
    if(length(j)==1) y <- preserve(y,function(a)a[[j]]) else {
      z <- preserve(y,function(a)a[j]) # do the subset, preserving atts
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
        z <- copy_atts(z, y[names(z)])
        y <- z
      }
    }
  }
  y
}


