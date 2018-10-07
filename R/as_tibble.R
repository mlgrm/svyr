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
    as_tibble(dat, validate = FALSE)
}

#' S3 generic for converting a svq into a tibble

as_tibble.svq <- function(x)
  switch( make.names(type(x)),
    repeat. = bind_rows(x[!sapply(x,is.null)],.id='index'),
    as_tibble(structure(x, class=class(x)[-1]))
  )
