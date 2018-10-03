dbUpdate <- function(name,
                     id,
                     svy.fun=identity,
                     df.fun=identity,
                     append=TRUE,
                     con=connection()
                     ){

  options(koboID = id)
  n <- name
  s <- svy()
  #do.call(options,old_opts)
  s <- svy.fun(s)
  s <- labelize(s)
  df <- as.data.frame(s)
  df <- df.fun(df)
  if(!append){
    if(db_has_table(connection(),n)){
      message("overwriting current table ",n)
      return(push(df,n,overwrite = TRUE))
    } else {
      warning(sprintf("no table %s in db but 'append' specified.
                        creating new table %s",n,n))
      return(push(df,n,overwrite=FALSE))
    }
  }
  query <- sql(sprintf("select instanceid from %s",n))
  uuid.old <- collect(tbl(connection(),query))$instanceid
  df <- df[!(df$instanceID %in% uuid.old),]
  if(nrow(df)==0){
    message(sprintf("no new data in %s.",n))
    return(tbl(connection(),n))
  }
  message(sprintf("found %d new records in %s",nrow(df),n))
  if(db_has_table(connection(),paste0(n,"_new"))){
    warning(n,"_new exists.  dropping...")
    doSQL(sprintf("drop table %s_new",n))
  }
  message("uploading new records")
  push(df,paste0(n,"_new"),overwrite = FALSE)
  doSQL(sprintf("insert into %s select * from %s_new",n,n))
  doSQL(sprintf("drop table %s_new",n))
  tbl(connection(),n)
}


filter_pilot_by_date <- function(df,cutoffs,
                     locale.header="province",
                     earliest.valid=as.Date(paste0(
                       lubridate::year(Sys.Date()),
                       "-01-01"))
                     ){
  first <- cutoffs[match(df[[locale.header]],names(cutoffs))]
  mask <- df$date>=first | df$date<earliest.valid
  mask[is.na(mask)] <- FALSE
  df <- preserve(df,function(x)x[mask,])
}

remove_records <- function(df,bad){
  if(is.logical(bad)) bad <- which(bad)
  preserve(df,function(x)df[-bad,])
}

add_date <- function(df){
  if("date" %in% names(df)){
    warning("date column already exists.")
    return(df)
  }
  df$date <- as.Date(df$start)
  df
}

add_duration <- function(df,
                         start.col="start",
                         end.col="end",
                         duration.col="duration"
                         ){
  if(!all(c(start.col,end.col)%in%names(df)))
    stop("data set must contain start and end fields")
  df[[duration.col]] <- as.numeric(df[[end.col]]-df[[start.col]])
  df
}

