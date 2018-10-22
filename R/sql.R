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
                                      host=host,
                                      user=user,
                                      password=password,
                                      dbname=dbname
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
doSQL <- function(statement, con = connection(), ...)
  dbClearResult(
    dbSendQuery(con, statement, ...)
  )

#' @rdname sql-wrappers
#' @export
getSQL <- function(statement, con = connection(), ...)
  dbGetQuery(con, statement, ...)

#' set/get the current default schema of a database connection
#' 
#' @param name string conntaining the schema
#' 
#' @rdname schema
#' @export
setSchema <- function(name=getOption("svyDBSchema"),con=connection()){
  message("checking if schema exists, creating otherwise...")
  suppressMessages(doSQL(paste("create schema if not exists",
                               getOption("svyDBSchema"))))
  message("setting default schema.")
  doSQL(paste("set search_path to",name))
}

#' get the current schema(s)
#'
#' @rdname schema
#' @export
getSchema <- function() getSQL("show search_path")$search_path

push <- function(x,...)UseMethod("push",x)

#' push a svy object up to the database
#' 
#' @param s a \code{svy} object
#' @param name cannonical, unique survey name.  by convention:
#' <project-id>_<project-name>[__<survey-name>].
#' @param indexes ignored for now
#' @param overwrite whether to overwrite existing tables
#' @param con a \code{PqConnection} to the survey database
#' 
#' Push data and metadata of the survey up to the server.  The schema  
#' takes the name of the survey and contains three tables: 
#'  * instance, containing the collected data with one column per question 
#'  * question, containing the form structure, including group information, 
#'  name information, node, type and labels, one row per question
#'  * choice, containing the choices for multiple choice questions, including
#'  question, name, and labels, one row per question choice.
#'  
#'  @return TRUE for success, FALSE otherwise
#'  
#'  @export
push.svy <- function(s, 
                     name = getOption("svyDBSchema"),
                     prefix = NULL,
                     indexes=NULL,
                     overwrite=FALSE,
                     con = connection(),
                     ...){
  # a table with all the form data
  question <- tibble(
      question = make.sql.names(names(tb)),
      group = groups(s),
      name = names(s),
      type = types(s),
      node = node(s)
  )
  
  if(is.null(languages(s))){
    question %<>% bind_cols(
      label = labels(s))
  } else {
    question %<>% bindcols(
      languages(s) %>% 
        llply(function(l) labels(s,l)) %>% 
        structure(names = paste0("label::",languages(s)))
    )
  }
  
  push(question, prefix)

  # a table with all the choices
  choice <- s[types(s) %in% c( # just the multiple choice svqs
    "select one",
    "select all that apply"
  )] %>%
    ldply(function(q){
      df <- tibble(
        question = name(q),
        choice = {
          c <- choices(q)
          if(Hmisc::all.is.numeric(c)) as.integer(c) else c
        }
      )
      if(is.null(languages(s))){ # if monolingual
        df %<>% bind_cols(
          label = labels(q))
      } else { # if multilingual
        df %<>% bindcols(
          languages(s) %>% 
            llply(function(l)labels(q,l)) %>% 
            structure(names = paste0("label::",languages(s)))
        )
      }
      df
    })
  push(choice, prefix = prefix)
  
  # rosters, if any
  for(q in s[types(s)=="repeat"])
    q %>% 
    # rbind all the svys in q
    as_tibble %>% 
    # copy attributes from the first svy to to new svqs
    # adding a column to it to match the length, otherwise copy_atts 
    # won't descend
    copy_atts(cbind(1,q[!laply(q,is.null)][[1]])) %>% 
    # call it a svy for dispatch
    structure(., class = c("svy", class(.))) %>%
    # push it up to the database
    push("roster", name(q)) 
  
  instance <- db_format(s, prefix)
  
  dbCreateTable(connection(),paste())
  
  

}

push.data.frame <- function(df,
                            prefix = NULL,
                            name = paste0(
                              c(prefix, deparse(substitute(df))),
                              collapse = "_"
                              ),
                            con = connection(),
                            modify = FALSE){
  if( ! modify && 
     ! identical(make.sql.names(names(df)),names(df)))
    stop("some names not allowed, declining to fix.") else
      names(df) <- make.sql.names(df)
  RPostgres::dbWriteTable(con, name, df)
}

db_upload <- function(x, ...)UseMethod("db_upload", x)

db_upload.svy <- function(s, 
                          prefix = NULL,
                          con = connection()){
  # a table with all the form data
  question <- tibble(
    question = make.sql.names(names(tb)),
    group = groups(s),
    name = names(s),
    type = types(s),
    node = node(s)
  )
  
  # a table with all the choices
  choice <- choices(s)
  choice <- cbind(question = question$question[match(choice$name,names(s))],
                  choice)
  choice$name <- NULL
  if(Hmisc::all.is.numeric(choice$choice))
    choice$choice %<>% as.integer %>% structure(db_type = "integer")
  
  instance <- db_format(s, prefix)
  
  dbCreateTable(connection(),paste())

}

get_type <- function(cl){ # cycle through classes until we get a hit or fail
  switch(
    cl[1],
    POSIXct = "timestamp",
    POSIXt = "timestamp",
    integer = "integer",
    character = "text",
    factor = "text",
    matrix = "text[]",
    logical = "bool",
    list = "text",
    if(length(cl)>1) get_type(cl[-1]) 
    )
}

# db_upload.tbl <- function(tb, 
#                           name = deparse(substitute(tb)),
#                           con = connection()
#                           ){
#   tb <- llply(tb, function(col){
#     if(!is.null(db_type(col))) col else
#       db_type(col) <- get_type(col)
#       )
#   })
# }

# prepare a \code{sv(q|y)} for uploading
db_format <- function(x, ...)UseMethod("db_format", x)

#' prepare \code{svy} a data column for insertion in a database
db_format.svq <- function(q)
  switch( # convert the columns for some types
    make.names(type(q)),
    repeat. = laply(q,length),
    select.all.that.apply = aaply(q,.margins = 1, function(r)
      paste(names(r)[r], collapse = " ")),
    geopoint = aaply(q, .margins = 1, paste, collapse = " "),
    q
  )

db_format.svy <- function(s){
  t <- s %>% 
    llply(db_format) %>% 
    as_tibble %>% 
    db_format %>% 
    structure(
      names = ifelse(names(s)!="", 
              names(s), 
              names(as_tibble(s)) %>% 
                sub("^.*/", "", .)
      ) %>% make.sql.names
    )
}

db_format.data.frame <- function(df){
  structure(df, names = names(df) %>% make.sql.names) 
}

push.tbl_df <- function(df){
  
}
  
#   
#   
#   
#   
#   
#     
#     make.sql.names(sapply(names(s),function(n)
#     if(!is.null(name(s[[n]])))name(s[[n]]) else n))
# 
#     # sapply(s,function(e){
#     #   if(!is.null(group(e))) paste(paste(group(e),collapse="/"),name(e)) else
#     #     name(e)
#     # }))
#   # if(make.map)
#   #   map <- data.frame(name=names(s),
#   #                   db.name=db.names,
#   #                   type=sapply(s,type),
#   #                   label=sapply(s,label))
#   names(s) <- db.names
# 
#   # postgres does not appear to like factors
#   s <- as.data.frame(lapply(s,function(x)if(is.factor(x))levels(x)[x] else x))
# 
#   # connect(...)
#   con <- connection()
#   suppressWarnings(doSQL(paste("create schema if not exists",
#                                getOption("svyDBSchema"))))
#   doSQL(paste("set search_path to",getOption("svyDBSchema")))
#   dbWriteTable(con,s,
#           name=name,
#           overwrite=overwrite,
#           indexes=indexes,
#           ...
#   )
#   # if(make.map){
#   #   copy_to(con,map,
#   #         name=paste(name,"map",sep="_"),
#   #         temporary=FALSE,
#   #         overwrite=overwrite,
#   #         indexes=list("db.name"),
#   #         ...
#   #   )
#   #   m <- tbl(con,paste(name,"map",sep="_"))
#   # } else m <- NULL
#   t <- tbl(con,name)
#   #dbDisconnect(getOption("svyDBConnection"))
#   invisible(list(data=t,map=m))
# }

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

