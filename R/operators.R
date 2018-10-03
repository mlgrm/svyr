
#' compose two or more functions (mainly for use in *apply functions)
'%*%' <- function(f,g) UseMethod("compose",f)
compose <- function(x,...)

compose.function <- function(f,g,f.args=list(),g.args=list())
  function(x)do.call(
    f,c(list(
      do.call(g,c(list(x),g.args))),
      f.args))

compose.default <- base::'%*%'

# compose.list <- function(fl,args){
#   if(length(fl)!=length(args))
#     stop("function list and args list-of-lists must be the same length")
#   if(length(fl)==2) compose(fl[[1]],fl[[2]],args[[1]],args[[2]]) else
#     compose(fl[[1]],compose(fl[-1],args[-1]),f.args=args[[1]])
# }


