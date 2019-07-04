#' generic to extend \code{dplyr::bind_rows} to \code{svy}s
#' @export
bind_rows <- function(x, ...)
  UseMethod(
    generic = "bind_rows", 
    if(class(x)[1] == "list") x[[1]] else x
  )

#' bind \code{svy}s with the same structure (form) together into a single 
#' \code{svy}
#' 
#' @details this is a method to do \code{dplyr::bind_rows()} on \code{svy}s 
#' without clobbering the metadata attributes or breaking matrix columns
#' 
#' @param l a list of \code{svy}s to bind together, rowwise.  this 
#' implementation does not accept the dots format used in 
#' \code{dplyr::bind_rows}.
#' 
#' @param ... parameters to pass to dplyr::bind_cols
#' 
#' @param .id the name of a (new) column to index the rows being bound
#' 
#' @param .names the names of the attributes to be preserved over the binding
#' 
#' @method bind_rows svy
#' 
# bind_rows.svy <- function(.l, .id = NULL, 
#                           .names = getOption(
#                             "preserve_atts", 
#                             c("node", "group", "class", "languages")
#                           )){
#   # save attributes of last svy
#   slast <- .l[[length(.l)]]
#   atts <- attributes(slast)[names(attributes(slast)) %in% .names]
#   
#   # for each new svy, overwrite atts of the previous and insert new svq atts
#   # this means that the output svy will contain at least all the atts of the 
#   # last (most recent) svy.
#   atts_list <- list()
#   for(s in .l)
#     atts_list[names(s)] <- map(
#       s, 
#       ~attributes(.)[names(attributes(.)) %in% .names]
#     )
# 
#   # combine matrices across svys
#   nr <- map_int(.l, NROW)
#   mats <- 
#     # get all the mats for each svy
#     map(.l[nr > 0], select_if, is.matrix) %>%
#     # organize them by matrix svq
#     transpose %>%
#     (function(x){ browser(expr = "grid" %in% colnames(x)); x}) %>% 
#     # for each matrix svq
#     map(~{
#       # find the colnames (first non-null entry)
#       cn <- colnames(.[! map_lgl(., is.null)][[1]])
#       # find the type
#       tp <- typeof(.[! map_lgl(., is.null)][[1]])
#       # fill null mats with NA of the right type
#       map2(
#         ., nr, 
#         ~if(is.null(.x))
#           matrix(
#             as(NA,tp), 
#             nrow =.y, 
#             length(cn), 
#             dimnames = list(, cn)
#           ) else
#             .x
#       ) %>% 
#       # rowbind them together using bind_rows
#       map(as_tibble) %>% 
#       bind_rows %>% 
#       as.matrix
#       # do.call(what = rbind, args = .)
#     })
# 
#   # replace matrix svqs with indices
#   .l[nr > 0] <- map2(.l[nr > 0], nr[nr > 0], 
#                      ~mutate_if(.x, is.matrix, function(s0)seq.int(.y)))
#   
#   # bind rows
#   s <- quietly(bind_rows)(.l, .id = .id)$result
#   
#   # restore matrices
#   s[names(mats)] <- mats
#   
#   # restore atts of svqs (but only those that have atts)
#   for(n in names(atts_list)) 
#     attributes(s[[n]]) <- append(
#       attributes(s[[n]])[ ! names(attributes(s[[n]])) %in% names(atts_list[[n]]) ],
#       atts_list[[n]]
#     )
#   # s[names(atts_list)] <- map2_dfc(
#   #   s[names(atts_list)], atts_list,
#   #   ~ {
#   #     # append the attributes in atts_list, 
#   #     # overwriting those that already exist
#   #     attributes(.x) <- append(
#   #       attributes(.x)[! names(attributes(.x)) %in% names(.y)],
#   #       .y)
#   #     .x
#   #   }
#   # )
#   
#   # restore atts of last survey
#   attributes(s) <- append(
#     attributes(s)[! names(attributes(s)) %in% names(atts)],
#     atts
#   )
#   s
# }
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

bind_rows.svy <- function(l, ...,
                          .id = "id",
                          .names = getOption(
                            "preserve_atts", 
                            c("node", "group", "class", "languages")
                          )){
  # for each tibble in l, save matrix columns and
  # replace with an index
  tibbles_and_mats <- map(l, function(s){
    if(is.null(s)) return(list(s = list(), m = list()))
    browser(expr = length(s) == 0)
    matcols <- names(s)[map_lgl(s, is.matrix)]
    mats <- s[matcols]
    s[matcols] <- 1:nrow(s)
    list(s = s, m = mats)
    # attr(s, "mat_types") <- map_chr(s[matcols], typeof)
    # attr(s, "mat_choices") <- map(s[matcols], choices)
    # s[matcols] <- map_dfc(s[matcols], function(c){
    #   mat <- c
    #   c <- 1:nrow(c)
    #   attr(c, "svq_mat") <- mat
    #   c
    # })
    # s
  }) %>% transpose()

  tb <- quietly(dplyr::bind_rows)(tibbles_and_mats$s, ..., .id = .id)$result
  
  # replace indices of matrix columns with rbound matrices, filling in
  # with NA as necessary
  matcols <- map(tibbles_and_mats$m, names) %>% reduce(union)
  tb[matcols] <- map(
    transpose(tibbles_and_mats$m, .names = matcols), 
    function(mat_list){
      colnames <- map(mat_list, colnames) %>% reduce(union)
      type <- map_chr(mat_list, typeof) %>% { .[. != "NULL"][1] }
      map2(mat_list, map_int(tibbles_and_mats$s, NROW), function(mat, nrow){
        if(is.null(mat)) 
          mat <- matrix(
            as(NA, type), 
            nrow = nrow,
            ncol = length(colnames),
            dimnames = list(NULL, colnames)
          )
        as_tibble(mat)
      }) %>%
        # debug_pipe %>% 
        # use bind_rows instead of rbind because it fixes column inconsistences
        { quietly(bind_rows)(.)$result } %>% 
        as.matrix
    }
  )
  
  # attributes from last instance of each column in l to columns of dt
  att_list <- map(
    names(tb) %>% { .[! . %in% .id] } %>% set_names(., .),
    function(n){
      res <- NULL
      for(s in l) if(! is.null(s[[n]])) res <- s[[n]]
      if(is.null(res)) 
        rlang::abort(str_glue("column {n} has no data in the list of svys"))
      atts <- .names[.names %in% names(attributes(res))]
      attributes(res)[atts]
    }
  )
  
  tb <- map2(tb[! names(tb) %in% .id], att_list, function(col, att){
    attributes(col)[names(att)] <- att
    col
  }) %>% as_tibble
  
  # attributes from last svy in list to dt
  atts <- .names[
    .names %in% 
      (l[[length(l)]] %>% 
         attributes %>% 
         names)
    ]
  attributes(tb)[atts] <- attributes(l[[length(l)]])[atts]
    
  tb
}
    # function(col){
    #   map(l, function(s){
    #     if(col %in% names(s)) attr(s[[col]], "svq_mat") else 
    #     # if the matcol wasn't in the svy, create one full of NAs
    #       matrix(
    #         NA, 
    #         nrow = nrow(s), 
    #         ncol = length(attr(s, "mat_choices")[[col]]),
    #         dimnames = list(NULL, attr(s, "matchoices")[[col]])
    #       )
    #   }) %>% 
    #   # bind all the embedded matrices and attach the attributes 
    #   # of the last one. 
    #   exec(
    #     function(...){
    #       m <- rbind(...)
    #       last <- list(...)[[nargs()]]
    #       atts <- .names[.names %in% names(attributes(last))]
    #       attributes(m)[atts] <- attributes(last)[atts]
    #       m
    #     },
    #     .
    #   )
    # }
    
  
#   # bind rows returns the union of all cols in all tibbles
#   name_union <- Reduce(union, map(l, names))
#   l_union <- 
#     map(set_names(name_union, name_union), function(n){
#       for(s in l) if(n %in% names(s)) col <- s[[n]]
#       col
#     })
#   
#   tb <- map2(
#     l_union,
#     # bind_rows might have added an .id column
#     tb[names(tb) %in% names(l_union)], 
#     function(in_col, out_col){
#       atts <- .names[.names %in% names(attributes(in_col))]
#       attributes(out_col)[atts] <- attributes(in_col)[atts]
#       out_col
#     }
#   ) %>% as_tibble
#   
#   atts <- .names[.names %in% names(attributes(l[[length(l)]]))]
#   attributes(tb)[atts] <- attributes(l[[length(l)]])[atts]
#   
#   tb
# }
    
bind_rows.default <- dplyr::bind_rows
