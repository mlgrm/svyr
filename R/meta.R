# create metadata tables
meta_question <- function(s)
  tibble(
    question = 1:length(s),
    name = svyr::all_names(s),
    group = map_chr(s, ~str_c(group(.), collapse = "/")),
    sql_name = names(s),
    node = map(s, node)
  )

get_selects <- function(s)
  which(types(s) %in% c("select one", "select all that apply"))

meta_choice <- function(s)
  map_dfr(get_selects(s), ~tibble(
    question = ., 
    name = choices(s[[.]]), 
    index = 1:length(name))
  ) %>% 
  add_column(., choice = 1:NROW(.), .before = 1)

meta_label <- function(s){
  ch <- meta_choice(s)
  map_dfr(1:length(s), function(i){
    l <- node(s[[i]])$label
    tb <- if(!is.null(l)) tibble(
      question = i, 
      type = factor("question", levels = c("question", "choice")),
      choice = NA_integer_, 
      lang = names(l),
      text = unlist(l)
    ) else tibble()
    if(i %in% get_selects(s)) tb <- bind_rows(
      tb, 
      map_dfr(
        1:length(node(s[[i]])$children),
        ~tibble(
          question = i,
          type = factor("question", levels = c("question", "choice")),
          choice = ch$choice[ch$question==i][.],
          index = .,
          lang = names(node(s[[i]])$children[[.]]$label),
          text = unlist(node(s[[i]])$children[[.]]$label)
        )
      )
    )
    tb
  })
}

  