# library(curl)
# library(jsonlite)
# library(xml2)
# library(xlsx)
# source("R/local.R")

# ls_projects <- function(...)
#   get.kobo.form.list(...)[c("formid","description","num_of_submissions")]


#' connect to the kobo api server and return the connection
#' 
#' @param endpoint a string containing the suffix of the desired api url
#' (after \code{https://<kobocat-server>/api/v1/})
#' 
#' @export
kobo_curl <- function(endpoint,
                      dat = NULL,
                      method = "GET",
                      options = list(),
                      form = list(),
                      headers = list(),
                      server = getOption("koboServer"),
                      token = getOption("koboToken")){
  if (method != "GET") options$copypostfields <- method
  headers$Authorization <- paste("Token", token)
  h <- curl::new_handle()
  if(is.list(options) && length(options)>0) 
    h %<>% curl::handle_setopt(.list = options)
  if(is.list(headers) && length(headers)>0) 
    h %<>% curl::handle_setheaders(.list = headers)
  # kobo does not appear to like "GET" requests with forms in them
  if(method == "POST" && 
     is.list(form) && length(form)>0) 
    h %<>% curl::handle_setform(.list = form)
  curl::curl(paste0(getOption("koboServer"),"/api/v1/",endpoint), handle = h)
}

#' retrieve a list of all a user's projects from a kobo server
#'
#' @export
kobo_ls <- function(refresh = F,
                    cols = c("formid",
                             "description",
                             "num_of_submissions",
                             "last_submission_time"),
                    server = getOption("koboServer"),
                    token = getOption("koboToken")){
  if (!refresh &&
      !is.null(getOption("koboList")) &&
      identical(
        options()[c(
          "koboServer",
          "koboUser"
        )],
        attr(getOption("koboList"),"opts"))){
    tb <- getOption("koboList")
  }else {
    con <- kobo_curl("forms")
    tb <- readLines(con, warn = F) %>%
      jsonlite::fromJSON(simplifyVector = T) %>%
      as_tibble %>%
      structure(opts=options()[c(
        "koboServer",
        "koboUser"
      )])
    close(con)
    options(koboList = tb)
  }
  tb[cols]
}

#' retrieve a form from kobo
#' 
#' @export
kobo_form <- function(formid = getOption("koboID"),
                      server = getOption("koboServer"),
                      token = getOption("koboToken")){
  con <- kobo_curl(paste0("forms/",formid,"/form.json"))
  res <- structure(readLines(con, warn = F) %>% fromJSON(simplifyVector = F),
            class = "odk_form")
  close(con)
  res
}

#' retrieve data from kobo
kobo_data <- function(formid = getOption("koboID"),
                      server = getOption("koboServer"),
                      token = getOption("koboToken"),
                      raw = TRUE){
  con <- kobo_curl(paste0("data/",formid))
  res <- structure(readLines(con, warn = F) %>% fromJSON(simplifyVector = F),
            class = "odk_data")
  close(con)
  if(raw) res else as_tibble(res)
}

# kobo_submit <- function(file,
#                         formid = getOption("koboID"),
#                         server = getOption("koboServer"),
#                         token = getOption("koboToken")){
#   system(
#     sprintf(
#       paste("curl -X POST",
#              "-F xml_submission_file=@%s",
#              "%s/api/v1/submissions",
#              "-H \"Authorization: Token %s\""
#       ),
#       file, server, token)
#   )
# }
# 

#' submit an instance to the kobo server
#' 
#' @export
kobo_submit <- function(file,
                        formid = getOption("koboID"),
                        server = getOption("koboServer"),
                        token = getOption("koboToken")){
  con <- kobo_curl(paste0("submissions"),
                   method = "POST",
                   form = list(xml_submission_file=form_file(file))
  )
  close(con)
  res
}  

get.kobo.doc <- function(endpoint,
                     dat=NULL,
                     method="GET",
                     flags="",
                     server=getOption("koboServer"),
                     token=getOption("koboToken"),
                     fmt="json",
                     simplifyVector=F){
  # this should be modified to use Rcurl and thus platform indep.
  of <- tempfile()
  if(is.null(dat)) dat <- "" else dat <- paste("-d",dat,collapse = " ")
  cmd <- sprintf(
    "curl -X %s --ignore-content-length %s %s %s/api/v1/%s -H \"Authorization: Token %s\" > %s",
    method, paste0(flags, collapse=" "),
    dat, server, endpoint, token, of
  )

  system(cmd)
  dat <- tryCatch(
    switch(fmt,
                csv=read.csv(of),
                json=fromJSON(of, simplifyVector=simplifyVector),
                xml=read_xml(of),
                xls={
                  n <- names(getSheets(of))
                  if(length(n)==1) read.xlsx(of) else
                    sapply(n,function(n1)read.xlsx(of,n1), simplify = FALSE)
                },
                stop("unrecognized format"))
  )
  if(is(dat,"error")) stop("got error:",readLines(con=file(of)))
  dat
}

get.kobo.data <- function(dataid=getOption("koboID"),
                          server=getOption("koboServer"),
                          token=getOption("koboToken"),
                          fmt="json"){
  path <- paste0("data/", dataid,
                ifelse(fmt=="json","",paste0(".",fmt)))
  get.kobo.doc(path, method = "GET", server=server, token=token, fmt=fmt)
}

get.kobo.form <- function(pk=getOption("koboID"),
                          server=getOption("koboServer"),
                          token=getOption("koboToken"),
                          fmt="json"){
  path <- paste0("forms/", pk, "/form.", fmt)
  get.kobo.doc(path, method="GET", server=server, token=token, fmt=fmt)
}

get.kobo.form.list <- function(server=getOption("koboServer"),
                               token=getOption("koboToken")){
  fl <- get.kobo.doc("forms",simplifyVector = TRUE)
}

bubble.babble <- function(n, len=5){
  if(len%%2==0) stop("len must be odd")
  vmask <- letters %in% c('a','e','i','o','u','y')
  vowels  <- letters[vmask]
  consonants <- letters[!vmask]
  ndos <- (len-1)/2
  word <- rep(NA_character_,len)
  div <- n
  for(i in 1:ndos){
    # little-endian
    mod <- div%%length(consonants)
    word[2*i-1] <- consonants[mod+1]
    div <- (div-mod)/length(consonants)
    mod <- div%%length(vowels)
    # browser()
    word[2*i] <- vowels[mod+1]
    div <- (div-mod)/length(vowels)
  }
  word[len] <- consonants[sum(as.integer(intToBits(n)))]
  paste(word,collapse = "")
}

babble.bubble <- function(w){
  vmask <- letters %in% c('a','e','i','o','u','y')
  vowels  <- letters[vmask]
  consonants <- letters[!vmask]
  ndos <- (nchar(w)-1)/2
  chars <- strsplit(w,"")[[1]]
  checksum <- which(chars[length(chars)]==consonants)[1]
  sum <- 0
  for(i in 1:ndos){
    # browser()
    sum <- sum+(length(consonants)*length(vowels))^(i-1)*
      (which(chars[2*i-1]==consonants)[1]-1)
    sum <- sum+length(consonants)*(length(consonants)*length(vowels))^(i-1)*
      (which(chars[2*i]==vowels)[1]-1)
  }
  if(checksum!=sum(as.integer(intToBits(sum)))){
    warning("invalid word")
    return(0)
  } else return(sum)
}

l2v <- function(l){
  vmask <- letters %in% c('a','e','i','o','u','y')
  vowels  <- letters[vmask]
  consonants <- letters[!vmask]
  ndos <- length(l)/2
  v <- rep(NA_integer_,length(l)-1)
  for(i in 1:ndos){
    v[2*i-1] <- which(l[2*i-1]==consonants)
    v[2*i] <- which(l[2*i]==vowels)
  }
  v
}

pwd <- function(n, seed=1){
  system(sprintf(
    # should do this natively in R

    'apg -E \'0123456789`~!@#$%%^&*()_+|\\=-<>,.?/;:{}[]"\'"\'" -c %d -n %d',
                 seed,n),
         intern = TRUE)
}

get.clean.survey <- function(pk){
  dat <- get.kobo.doc(paste0("data/",pk))
  form <- get.kobo.doc(paste0("forms/",pk,"form.json"))
  svy <- svy(form,dat)
}

