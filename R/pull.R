pull <- function(spec = getOption("svyr_server_spec", kobo_spec())){
  # check which server type
  data <- spec$type %>% switch(
    "kobo" = kobo_data(
      formid = spec$formid,
      server = spec$url,
      token = spec$token,
      raw = TRUE
    ),
    stop("unrecognized spec type")
  ) %>% 
    transpose %>%
    flatten_linear %>% 
    as_tibble
  form <- spec$type %>% switch(
    "kobo" = kobo_form(
      formid = spec$formid,
      server = spec$url,
      token = spec$token
    ),
    stop("unrecognized spec type")
  )
  svy(data, form)
}

as_svy <- function(data, form, group = NULL, lang = "English"){
  # convert all columns with only one element sub-element lists to character
  data %<>% transpose_data()
  map(form$children, parse_node, data = data, group = group, lang = lang) %>%
    bind_cols %>% 
    structure(
      form = form,
      class = c("svy", class(tibble()))
    )
}

transpose_data <- function(data){
  data %>% map(function(inst){
    map(inst, ~ if(length(.) == 1) . else list(.)) %>% as_tibble
  }) %>% bind_rows
}

# flatten elements that are lists of single element or null vectors
flatten_linear <- function(data){
  modify_if(
    data,
    # if every (sub) element is 0 or 1 length
    function(e) every(e, ~ length(.) <= 1),
    # convert empties (length 0) to NAs and flatten
    function(e) modify_if(e, ~ length(.) == 0, ~ NA_character_) %>% 
      flatten_chr
  )
}

parse_node <- function(node, data, group, lang = "English"){
  # if our node is a group, re-run recursively on all its children, pushing
  # its name onto group
  if(node$type == "group") 
    node$childrem %>%
    map(parse_node, 
        data = data, 
        group = c(group, node$name), 
        lang = lang) %>% 
    bind_cols %>% 
    return()

  # find data column
  name <- group %>% 
    str_c(collapse = "/") %>% 
    str_c(node$name, sep = "/")
  
  datum <- if(name %in% names(data)) data[[name]] else 
    rep(NA_character_, nrow(data))

  # repeats are special: every element is a new survey
  if(node$type == "repeat") return(parse_node_repeat(datum, node, group, lang))
    
  # select function based on type
  parse_type <- case_when(
    # date-times
    node$type %in% c(
      "start",
      "end",
      "datetime"
    ) ~ "datetime",
    # dates
    node$type %in% c(
      "today",
      "date"
    ) ~ "date",
    # . == "OK"
    node$type == "acknowledge" ~ "ack",
    # integers
    node$type == "integer" ~ "int",
    # doubles
    node$type == "decimal" ~ "double",
    # take no input
    node$type %in% c(
      "note"
    ) ~ "ro",
    # gps coordinates
    node$type == "geopoint" ~ "geopoint",
    node$type == "select one" ~ "select_one",
    node$type == "select all that apply" ~ "select_multiple",
    node$type == "repeat" ~ "repeat",
    # default is string
    TRUE ~ "string"
  )
  
  # parsing funcion
  parse_datum <- get(str_c("parse_node_", parse_type), mode = "function")
  
  # parse
  parse_datum(datum, node, lang) %>% 
    structure(
      class = c("svq", class(.)),
      node = node,
      group = group
    ) %>% 
    tibble %>%
    structure(names = str_c(str_c(group, collapse = "/"), name, sep = "/"))
      
}

# parsing functions -------------------------------------------------

parse_node_datetime <- function(datum, node, lang = "English") 
  lubridate::ymd_hms(datum)

parse_node_date <- function(datum, node, lang = "English") lubridate::ymd(datum)

parse_node_ack <- function(datum, node, lang = "English") datum == "OK"

parse_node_int <- function(datum, node, lang = "English") as.integer(datum)

parse_node_double <- function(datum, node, lang = "English") as.double(datum)

parse_node_ro <- function(datum, node, lang) as.logical(datum)

parse_node_geopoint <- function(datum, node, lang = "English"){
  # split each element on space
  modify_if(
    as.list(datum), 
    ~ ! is.na(.), 
    ~ str_split(., pattern = " ")[[1]]
  ) %>%
    # missing values become four missing (NA_real_) observations
    modify_if(~ is.na(.[1]), ~ rep(NA_character_, 4)) %>% 
    map(~ as.numeric(.) %>% structure(
      names = c("latitude", "longitude", "altitude", "precision"))
    )
}

parse_node_select_one <- function(datum, node, lang = "English"){
  levels <- map_chr(node$children, ~ .$name)
  labels <- map_chr(node$children, ~ .$label[[lang]])
  factor(datum, levels = levels) %>% 
  structure(
    labels = labels
  )
}

parse_node_select_multiple <- function(datum, node, lang = "English"){
  levels <- map_chr(node$children, ~ .$name)
  labels <- map_chr(node$children, ~ .$label[[lang]])
  # split each element on space
  map(datum, str_split, pattern = " ") %>%
    # zero length chars for empty elements
    modify_if(~ length(.) == 0, ~ list(character())) %>% 
    flatten %>%
    # convert to factors
    map(factor, levels = levels) %>%
    # add labels to elements
    map(structure, labels = labels) %>%
    # add levels and labels to list
    structure(levels = levels, labels = labels)
}

parse_node_repeat <- function(datum, node, group, lang = "English"){
  # if we processed this as missing data, make a list of empty lists
  if(all(is.na(datum))) return(map(datum, ~ list()))
  map(datum, function(d){
    # instances have no names, so if they have names they are data which
    # means we have only one instance that got unlisted, so relist it
    if(!is.null(names(d))) d <- list(d)
    # null elements are empty
    if(is.null(d)) list() else
      # all others are subsurveys and can be processed as a new survey
      as_svy(
        data = d,
        form = node, 
        group = c(node$name, group),
        lang = lang
      )
  })
  
  # # if no instance had this repeat, we'll see a vector of NA's
  # if(all(is.na(datum))) return(map(datum, ~ list()))
  # map(datum, function(d){
  #   # no instances
  #   if(is.null(d)) return(NULL)
  #   # instances have no names, so if they have names they are data which
  #   # means we have only one instance that got unlisted
  #   if(!is.null(names(d))) d <- list(d)
  #   as_svy(d, node, group = c(node$name, grouplang = lang))
  # })
}

parse_node_string <- function(datum, node, lang = "English") datum


# platform-specific specs -------------------------------------------

kobo_spec <- function(formid = getOption("koboID", 
                                         stop("no formid specified")),
                      server = getOption("koboServer", 
                                         stop("no server url specified")),
                      token = getOption("koboToken", 
                                        stop("no api token specified"))){
  list(
    type = "kobo",
    formid = formid,
    url = server,
    token = token
  )
}