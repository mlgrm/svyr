print <- function(x,...)UseMethod("print",x)

print.svq <- function(x,...){
  x <- switch(type(x),
              "select one"={
                factor(levels(x)[x],levels=levels(x), labels=labels(x))
              })
}
