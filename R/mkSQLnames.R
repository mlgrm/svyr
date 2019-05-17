# raw applies to plain data.frames, so the attributes are not checked
mkSQLnames <- function(ns, 
                       max.len=32,
                       raw=FALSE,
                       remove.group=TRUE,
                       group.delimiter='/'){
  if(remove.group)
    ns <- sub(sprintf("^.*%s",group.delimiter),"",ns)
  ns <- tolower(ns)
  ns <- ifelse(ns %in% tolower(DBI::.SQL92Keywords), paste0(ns,"_"),ns)
  ns <- gsub("[^A-z0-9_]+","_",ns)
  i <- 0
  repeat{
    ns <- substr(ns,1,max.len-i)
    ns <- make.unique(ns,sep = "_")
    browser(expr=!is.logical(max(sapply(ns, nchar)) <= max.len))
    if(max(sapply(ns,nchar))<=max.len) break
    i <- i+1
  }
  # cat(i,"\n")
  ns
}

make.sql.names <- mkSQLnames