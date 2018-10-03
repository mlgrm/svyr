library(googlesheets)
library(xlsx)
library(plyr)

#' return a list of data.frames corresponding to the three tabs of an xlsform
xlsform <- function(form){
  if("xlsform_tmpl.rds" %in% list.files("local/"))
    tmpl <- readRDS("local/xlsform_tmpl.rds") else {
      sh <- gs_key("1xc-YUAXAm71bhYWDlGlckMzui2ANVQmh75ARgpQZ5_w")
      tmpl <- sapply(gs_ws_ls(sh), function(sn)gs_read(sh,sn),simplify=FALSE)
      saveRDS(tmpl,"local/xlsform_tmpl.rds")
    }
  out <- lapply(tmpl,function(s){
    headers <- sub("\\[language\\]", getOption("svyLang","English") , s$Item)
    headers <- headers[!grepl("^\\[",headers)]
    df <- as.data.frame(sapply(headers,function(h)character(),simplify = FALSE))
  })
  name <- function(x)name(x,use.node = FALSE)
  type <- function(x)name(x,use.node = FALSE)
  out$settings <- settings <- as.data.frame(form[names(form)!="children"])
  out$survey <- ldply(form$children,get.survey.lines,tmpl=out$survey)
}

get.survey.lines <- function(node,tmpl){
  switch(make.names(node$type),
         repeat.=getLines.group(node,tmpl,repeat.=TRUE),
         group=getLines.group(node,tmpl,repeat.=FALSE),
         default=getLines.default(node,tmpl)
         )
}

node2line <- function(node,names){
  x <- rep("",length(names))
  names(x) <- names
  x[sub(paste0("::",getOption("svyLang","English")),"",names) %in%
      names(node)] <- node[names(node)!="bind"]
  x[sub(paste0("::",getOption("svyLang","English")),"",names) %in%
      names(node$bind)] <- node$bind
  x
}

getLines.default <- function(node,tmpl)
  as.data.frame(as.list(node2line(node,names(tmpl))))


getLines.group <- function(node,tmpl,repeat.){
  first <- getLines.default(node,tmpl)
  first$type <- paste0("begin_",first$type)
  middle <- ldply(node$children,get.survey.lines,tmpl=tmpl)
  last <- getLines.default(list(),tmpl)
  last$type <- paste0("end_", node$type)
  rbind(first,middle,last)
}
