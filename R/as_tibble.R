#' S3 generic for converting odk data in it's native json format to a tibble
#' 
#' @importFrom tibble as_tibble
#' @export
as_tibble.odk_data <- function(dat){
  names <- Reduce(union, lapply(dat,names))# all names that appear in any instance
    # sapply(function(n){ # for each name
    #   qd <- sapply(d,function(i) # for each instance
    #     if (n %in% names(i)) i[[n]] else NA_character_, simplify = FALSE
    #   )
    #   if(all(sapply(qd,is.character))) qd <- unlist(qd)
    #   qd
    # }, simplify = F
    # ) %>%
    # for each instance fill in missing values from names with NULL
    dat <- lapply(dat,function(i){
      all_vals <- structure(rep(list(NULL), length(names)), names=names)
      all_vals[names(i)] <- i
      all_vals
    })
    # make it tibblish
    dat <- purrr::transpose(dat)
    # convert cols with only atomics into vectors
    dat <- lapply(dat, function(col){
      len <- sapply(col,length)
      if(all(len %in% 0:1)){
        col[len==0] <- NA_character_
        col %<>% as.character      
      }
      col
    })
    tibble::as_tibble(dat, validate = FALSE)
}

#' S3 generic for converting a svq into a tibble
#' 
#' @export
as_tibble.svq <- function(x)
  switch( make.names(type(x)),
    as_tibble(structure(x, class=class(x)[-1]))
  )

#' S3 method to collapse a repeat \code{svq} into a single \code{svy}
#'
#' internally, a \code{svq} of type \code{repeat} (roster) is just a list of
#' \code{svy}s with the same \code{node} attribute.  This function rbinds
#' those tibbles into a single survey.
#' 
#' @importFrom dplyr collapse
#' @export
collapse.svq <- function(x, index = 1:NROW(x)){
  if(type(x) != "repeat") stop("collapse is only applicable to repeat types")
  names(x) <- index
  x <- x[!laply(x,is.null)]
  x %<>% 
    llply(function(s)
      llply(s, function(q)structure(q, class = class(q)[-1])) %>% as_tibble
    ) %>% #debug_pipe %>%
    bind_rows(.id = "instance") %>% #debug_pipe %>%
    copy_atts(cbind(instance=1,x[[1]])) %>% #debug_pipe %>%
    structure(., class = c("svy", class(.)))
  x$instance %<>% as.integer
  x
}

#' s3 method to convert a svy to a tibble
as_tibble.svy <- function(s)
  tibble::as_tibble(structure(s, class = class(s)[-1]), validate = F)

#' convert a svy to a dataframe, splitting matrices to multiple columns
#' 
as.data.frame.svy <- function(x, ...)
  llply(names(x),function(n)
    if(is.matrix(x[[n]])) 
      structure(
        as.data.frame(x[[n]]), 
        names = paste(n, colnames(x[[n]]), sep = ":")
      ) else if(is.list(x[[n]])) 
        structure(data.frame(laply(x[[n]], NROW)), names = n) else
        structure(data.frame(x[[n]]), names = n)
  ) %>% dplyr::bind_cols()


