#' access or set the default database connection
#' 
#' @export
connection <- function(host = getOption("svyDBHost"),
                       user=getOption("svyDBUser"),
                       password=getOption("svyDBPassword"),
                       dbname=getOption("svyDBName"),
                       schema=getOption("svyDBSchema")){
  # if the default connection has not been set up or has expired,
  # set it up (again)
  if(!("svyDBConnection" %in% names(options()) &&
       DBI::dbIsValid(getOption("svyDBConnection")))){
    options(svyDBConnection=DBI::dbConnect(RPostgres::Postgres(),
                                      host = host,
                                      user = user,
                                      password = password,
                                      dbname = dbname
    ))
    setSchema(schema)
  }
  getOption("svyDBConnection")
}

#' run an sql send/get query
#' 
#' \code{doSQL} and \code{getSQL} are just wrapper functions for 
#' \code{dbSendQuery} and \{dbGetQuery} using the default \code{connection()}.
#' 
#' @param statement a string containing a valid SQL command
#' @param ... optional parameters to pass on to \code{db(Send|Get)query}
#' 
#' @rdname sql-wrappers
#' 
#' @export
doSQL <- function(..., con = connection())
  DBI::dbClearResult(
    DBI::dbSendQuery(con, paste(list(...), collapse = " "))
  )

#' @rdname sql-wrappers
#' @export
getSQL <- function(... , con = connection())
  DBI::dbGetQuery(con, paste(list(...), collapse = " "))

#' set/get the current default schema of a database connection
#' 
#' @param name string conntaining the schema
#' 
#' @rdname schema
#' @export
setSchema <- function(name=getOption("svyDBSchema", stop("need a schema")),
                      con=connection()){
  message("checking if schema exists, creating otherwise...")
  suppressMessages(doSQL(paste("create schema if not exists",
                               name)))
  message("setting default schema.")
  doSQL(paste("set search_path to", name))
  options(svyDBSchema = name)$svyDBSchema
}

#' @rdname schema
#' @export
getSchema <- function() getSQL("show search_path")$search_path

# raw applies to plain data.frames, so the attributes are not checked
make.sql.names <- function(
  ns, max.len=32,raw=FALSE,remove.group=TRUE,group.delimiter='/'){
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

