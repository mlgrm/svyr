#' modify a function acting on a list to preserve matrix elements and 
#' some of the attributes of the list and its elements
#' 
#' @param .f the function to be wrapped
#' @param .names vector of names of attributes to be preserved
#' 
#' @details this adverb wraps attribute-destructive functions such as 
#' \code{dplyr::col_bind}
#' and all the row subsetting operations (e.g. \code{[]}, \code{[]<-}) 
#' so that \code{svy} 
#' objects and their constituent \code{svq}s do not lose their attributes
#' TODO: handle grouped tibbles.
#' 
#' @note do not wrap functions that do not return a list of the same length,
#' i.e. column operations should be performed separately from row operations.
#' 
#' @export

protectively <- function(.f, 
                         .names = getOption(
                           "preserve_atts", 
                           c("node", "group", "class", "languages", "groups")),
                           recurse = TRUE
                         ){
  function(l, ...){

    # save attributes of the list itself
    atts <- attributes(l)[names(attributes(l)) %in% .names]
    
    if(recurse){
      # save attributes of the elements of the list
      atts_list <- map(l, ~attributes(.)[names(attributes(.)) %in% .names])
      
      # save all the matrix svqs
      mats <- ungroup(l)[map_lgl(l, is.matrix)]
      # replace them with an index
      l[names(mats)] <- 1:nrow(l)
    }

    # exectute the function quietly, returning the result
    l <- quietly(.f)(l, ...)$result
    
    if(recurse){
      # subset the matrices
      mats <- map2(mats, l[names(mats)], ~.x[.y,])
      # restore the matrices
      l[names(mats)] <- mats
      
      # add the new row.names to the list attributes
      # atts$row.names <- attr(l, "row.names")
      
      # restore the attributes of the list elements
      l <- map2(
        l, atts_list,
        ~ {
          # append the attributes in atts_list, 
          # overwriting those that already exist
          attributes(.x) <- append(
            attributes(.x)[! names(attributes(.x)) %in% names(.y)],
            .y)
          .x
        }
      ) %>% as_tibble(validate = FALSE)
    }
    
    # restore the attributes of the list
    attributes(l) <- append(
      attributes(l)[! names(attributes(l)) %in% names(atts)],
      atts
    )
    
    # return the list
    l
  }
}

#' @export
preservingly <- protectively