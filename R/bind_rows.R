#' generic to extend \code{dplyr::bind_rows} to \code{svy}s
#' @export
bind_rows <- function(x, ...)UseMethod("bind_rows", x)

#' bind \code{svy}s with the same structure (form) together into a single \code{svy}
#' 
#' @details this is a method to do \code{dplyr::bind_rows()} on \code{svy}s without
#' clobbering the metadata attributes
#' 
#' @param .l a list of \code{svy}s to bind together, rowwise.  this implementation does
#' not accept the dots format used in \code{dplyr::bind_rows}.
#' 
#' @param .id the name of a (new) column to index the rows being bound
#' 
#' @param .names the names of the attributes to be preserved over the binding
#' 
#' @method bind_rows svy
#' 
bind_rows.svy <- function(.l, .id = NULL, 
                          .names = getOption(
                            "preserve_atts", 
                            c("node", "group", "class", "languages")
                          )){
  # save attributes of last svy
  slast <- .l[[length(.l)]]
  atts <- attributes(slast)[names(attributes(slast)) %in% .names]
  
  # for each new svy, overwrite atts of the previous and insert new svq atts
  # this means that the output svy will contain at least all the atts of the 
  # last (most recent) svy.
  atts_list <- list()
  for(s in .l)
    atts_list[names(s)] <- map(
      s, 
      ~attributes(.)[names(attributes(.)) %in% .names]
    )

  # combine matrices across svys
  nr <- map_int(.l, NROW)
  mats <- 
    # get all the mats for each svy
    map(.l[nr > 0], select_if, is.matrix) %>%
    # organize them by matrix svq
    transpose %>%
    # for each matrix svq
    map(~{
      # find the colnames (first non-null entry)
      cn <- colnames(.[! map_lgl(., is.null)][[1]])
      # find the type
      tp <- typeof(.[! map_lgl(., is.null)][[1]])
      # fill null mats with NA of the right type
      map2(
        ., nr, 
        ~if(is.null(.x))
          matrix(as(NA,tp), .y, length(cn), dimnames = list(, cn)) else
            .x
      )
    }) %>% 
    # rbind them together
    do.call(what = rbind, args = .)
  
  # replace matrix svqs with indices
  .l[nr > 0] <- map2(.l[nr > 0], nr[nr > 0], 
                     ~mutate_if(.x, is.matrix, function(s0)seq.int(.y)))
  
  # bind rows
  s <- quietly(bind_rows)(.l, .id = .id)$result
  
  # restore matrices
  s[names(mats)] <- mats
  
  # restore atts of svqs (but only those that have atts)
  s[names(atts_list)] <- map2_dfc(
    s[names(atts_list)], atts_list,
    ~ {
      # append the attributes in atts_list, 
      # overwriting those that already exist
      attributes(.x) <- append(
        attributes(.x)[! names(attributes(.x)) %in% names(.y)],
        .y)
      .x
    }
  )
  
  # restore atts of last survey
  attributes(s) <- append(
    attributes(s)[! names(attributes(s)) %in% names(atts)],
    atts
  )
  s
}
  # map(l, function(s){
  #   mats <- select_if(s, is.matrix)
  #   s <- mutate_if(s, is.matrix, ~1:nrow(.))
  #   structure(s, mats = mats)
  # }) %>% {
  #   # list of lists of matrices
  #   mats <- 
  #     map(., attr, "mats") %>% 
  #     transpose(.names = Reduce(union, map(., names))) %>% 
  #     map(~do.call(what = rbind, args = .))
  #   quietly(bind_rows)(., .id = .id)$result %>% #debug_pipe() %>% 
  #   {
  #     .[names(mats)] <- mats
  #     atts <- c("name", "class", "node", "languages")
  #     attributes(.)[atts] <- attributes(l[[1]])[atts]
  #     .
  #   }
  # }
    
bind_rows.default <- dplyr::bind_rows
