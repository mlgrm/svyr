get_iana_langs <- function(url =
                             paste0(
                               "http://www.iana.org/assignments/",
                               "language-subtag-registry/",
                               "language-subtag-registry")){
  con <- url(url,blocking = F)
  message("waiting for data", appendLF = F)
  while(isIncomplete(con)){
    message(".", appendLF = F)
    sleep(1)
  }
  message(". done")
  txt <- readLines(con)
  close(con)
  txt %<>% paste0(collapse = "\n")
  txt <- gsub("\n\\s+","",txt)
  browser()
  txt <- strsplit(txt, "\n%%\n")[[1]][-1]
  txt %<>% lapply(function(x){
    l <- strsplit(x,"\n")[[1]]
    l <- regmatches(l, regexpr(": ",l), invert = T)
    head <- sapply(l,getElement,1)
    browser(expr=any(sapply(l,length)!=2))
    l <- sapply(l,getElement,2)
    structure(l,names = head)
  })
  head <-
    lapply(txt,names) %>%
    do.call(c,.) %>%
    unique
  browser()
  txt %<>%
    plyr::ldply(function(x){
      r <- structure(rep(NA_character_, length(head)), names = head)
      r[names(x)] <- x
      r
    }) %>%
    as_tibble
  txt
}
