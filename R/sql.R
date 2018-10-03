library(dplyr)
library(dbplyr)
library(DBI)
library(RPostgres)

connection <- function(host=getOption("svyDBHost"),
                    user=getOption("svyDBUser"),
                    password=getOption("svyDBPassword"),
                    dbname=getOption("svyDBName"),
                    schema=getOption("svyDBSchema")){
  # if the default connection has not been set up or has expired,
  # set it up (again)
  if(!("svyDBConnection" %in% names(options()) &&
       dbIsValid(getOption("svyDBConnection")))){
    options(svyDBConnection=dbConnect(Postgres(),
                                      host=host,
                                      user=user,
                                      password=password,
                                      dbname=dbname
    ))
    setSchema(schema)
  }
  getOption("svyDBConnection")
}

doSQL <- function(statement,...)
  dbClearResult(
    dbSendQuery(connection(),statement, ...)
  )

getSQL <- function(statement,...)
  dbGetQuery(connection(),statement,...)

setSchema <- function(name=getOption("svyDBSchema"),con=connection()){
  message("checking if schema exists, creating otherwise...")
  suppressMessages(doSQL(paste("create schema if not exists",
                               getOption("svyDBSchema"))))
  message("setting default schema.")
  doSQL(paste("set search_path to",name))
}

getSchema <- function()getSQL("show search_path")$search_path

push <- function(s,name=deparse(substitute(df),...),
                 schema=getOption("svyDBSchema",NULL),
                 indexes=NULL,
                 overwrite=FALSE,
                 ...){
  # if(is.svy){
  # s <- flatten(s)
  # }
  db.names <- make.sql.names(sapply(names(s),function(n)
    if(!is.null(name(s[[n]])))name(s[[n]]) else n))

    # sapply(s,function(e){
    #   if(!is.null(group(e))) paste(paste(group(e),collapse="/"),name(e)) else
    #     name(e)
    # }))
  # if(make.map)
  #   map <- data.frame(name=names(s),
  #                   db.name=db.names,
  #                   type=sapply(s,type),
  #                   label=sapply(s,label))
  names(s) <- db.names

  # postgres does not appear to like factors
  s <- as.data.frame(lapply(s,function(x)if(is.factor(x))levels(x)[x] else x))

  # connect(...)
  con <- connection()
  suppressWarnings(doSQL(paste("create schema if not exists",
                               getOption("svyDBSchema"))))
  doSQL(paste("set search_path to",getOption("svyDBSchema")))
  dbWriteTable(con,s,
          name=name,
          overwrite=overwrite,
          indexes=indexes,
          ...
  )
  # if(make.map){
  #   copy_to(con,map,
  #         name=paste(name,"map",sep="_"),
  #         temporary=FALSE,
  #         overwrite=overwrite,
  #         indexes=list("db.name"),
  #         ...
  #   )
  #   m <- tbl(con,paste(name,"map",sep="_"))
  # } else m <- NULL
  t <- tbl(con,name)
  #dbDisconnect(getOption("svyDBConnection"))
  invisible(list(data=t,map=m))
}

# raw applies to plain data.frames, so the attributes are not checked
make.sql.names <- function(
  ns, max.len=32,raw=FALSE,remove.group=TRUE,group.delimiter='/'){
  if(remove.group)
    ns <- sub(sprintf("^.*%s",group.delimiter),"",ns)
  ns <- tolower(ns)
  ns <- ifelse(ns %in% tolower(.SQL92Keywords), paste0(ns,"_"),ns)
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

map.sql <- function(s,name){
  df <- cbind(
    apply(s,name),
    apply(s,type),
    apply(s,function(x)if(is.null(group(x)))"" else
      paste(group(x),collapse = "/")),
    apply(s,label),
    collect(
      getSQL("select column_name, data_type
             from information_schema.columns
             where table_name = 'data';")
    )
  )
}

