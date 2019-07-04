# tools for summarizing data sets
# library(plyr)
# library(dplyr)


# summary <- function(x,...) UseMethod("summary", x)

summary.svy <- function(x,...)ldply(flatten(s),summary,.id="qid",...)

summary.svr <- function(x,...){
  s <- summary_svq$text(x)
  df <- data(x)
  s['summary'] <- sprintf("%d obs. of %d variables",nrow(df),ncol(df))
  s
}

summary.svq <- function(x,...){
  # browser()
  type <- make.names(type(x))
  if(type %in% names(summary_svq)) summary_svq[[type]](x) else
    summary_svq$text(x)
}

summary_svq <- list()

summary_svq$text <- function(x,...){
  c(
    name=name(x),
    type=type(x),
    label=label(x),
    summary=sprintf("%d unique values",length(unique(x))),
    group=paste(group(x),collapse = "/")
  )
}

num_types <- c(
  "dateTime",
  "date",
  "integer",
  "decimal",
  "start",
  "end",
  "today"
)

num_types_sum <- function(x,...){
  # browser()
  s <- summary_svq$text(x,...)
  class(x) <- class(x)[-1]
  s['summary'] <- paste("quartiles",paste(summary(x),collapse = "\n"),sep=":\n")
  s
}

summary_svq <- c(summary_svq,sapply(num_types,function(x)num_types_sum))

summary_svq$geopoint <- function(x,...){
  s <- summary_svq$text(x,...)
  s['summary'] <- sprintf("centroid: %s",
                       paste(colnames(x),
                             sprintf(c("%0.4f","%0.4f","%0.0f","%0.0f"),
                                     c(
                                       mean(x[,1],na.rm=TRUE),
                                       mean(x[,2],na.rm=TRUE),
                                       mean(x[,3],na.rm=TRUE),
                                       sqrt(sum(x[,4]^2,na.rm=TRUE)/
                                              length(x[!is.na(x)]))
                                     )
                             ), sep="=",collapse=","
                       ))
  s
}

summary_svq$select.one <- function(x,...){
  s <- summary_svq$text(x,...)
  # browser()
  ch <- choices(x)
  s['summary'] <- sprintf("%d choices, %0.0f%% = %s", length(ch),
                       100*max(table(x))/length(x),
                       levels(x)[max.col(t(table(x)))])
  s
}

summary_svq$select.all.that.apply <- function(x,...){
  s <- summary_svq$text(x,...)
  # browser()
  ch <- choices(x)
  sums <- colSums(x,na.rm=TRUE)
  s['summary'] <- sprintf("%d choices, %0.0f%% selected %s", length(ch),
                       100*max(sums)/nrow(x),
                       colnames(x)[max.col(t(sums))])
  s
}

