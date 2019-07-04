db_write_svy_tables <- function(s,
                                name,
                                conn = NULL,
                                db = getOption("dbName"),
                                server = getOption("dbServer"),
                                user = getOption("dbUser"),
                                password = getOption("dbPasswd"),
                                schema = getOption("dbSchema"),
                                labelize = TRUE,
                                overwrite = FALSE,
                                append = FALSE){
  
  if(labelize) s %<>% svyr::labelize(.)
  names(s) <- sql_names(s)
  
  # split up matrices
  for(n in colnames(s)){
    if(is.matrix(s[[n]])){
      df <- as_tibble(s[[n]])
      #df <- map_df(df, ~structure(., node = node(s[[n]])))
      names(df) %<>% 
        str_replace_all('/', '_') %>% 
        paste(n, ., sep = "__") %>% 
        mkSQLnames
      # browser(expr = "_39_electricity_source_12_mths" %in% names(df))
      # need to remove original col before adding new one to avoid name collisions
      ind <- which(n == colnames(s))
      s[[n]] <- NULL
      s <- do.call(function(...)add_column(s, ..., .after = ind - 1), df)
    }
  }
  # collect rosters
  rpts <- select_if(s, function(x)
    (! is.na(svyr::type(x))) && svyr::type(x) == "repeat")
  # simplify lists of surveys (rosters)
  s <- mutate_if(
    s, 
    ~(! is.na(svyr::type(.))) && svyr::type(.) == "repeat", 
    ~as.character(1:length(.))
  )
  if(is.null(conn))
    conn <- DBI::dbConnect(
      RPostgres::Postgres(), 
      dbname = db, 
      host = server,
      user = user,
      password = password
    )
  search_path <- dbGetQuery(conn, "show search_path")[[1]]
  if(! schema %in% dbGetQuery(
    conn, 
    "select nspname from pg_catalog.pg_namespace")[[1]]
    ) dbSendQuery(conn, str_glue("create schema {schema}"))
  if(! str_detect(search_path, str_glue("^{schema},")))
    dbSendQuery(
      conn,
      str_glue("set search_path to {schema}, 
               {paste(search_path, collapse = ', ')}")
    ) 
  db_write_table(
    conn, 
    name,
    dbDataType(conn, s), 
    as_tibble(s), 
    temporary = FALSE,
    overwrite = overwrite,
    append = append
  )
  
  # set primary keys
  map(names(rpts), ~dbSendQuery(conn, str_glue(
    "
    alter table {name}
    add constraint unq_{.} unique ({.})
    ;"
    )))
  
  rptbl <- 
    map(names(rpts), function(n){
      rpt_name <- str_glue("{name}__rpt_{n}")
      tbl <- 
        bind_rows.svy(rpts[[n]], .id = n) %>% 
        { if(labelize) svyr::labelize(.) else .} %>% 
        toDB.svy(
          name = rpt_name,
          conn = conn,
          db = db,
          server = server,
          user = user,
          password = password,
          schema = schema,
          labelize = labelize,
          overwrite = overwrite,
          append = append
        )
      # dbSendQuery(
      #   conn,
      # TODO: make sure we use names mapped by mkSQLnames, not the
      # column names
      #   str_glue("alter table {rpt_name}
      #             add foreign key ({n}) references {name}({n})
      #             ;")
      # )
      tbl
    })
  invisible(c(list(tbl(conn, name)),unlist(rptbl,recursive = FALSE)))
}

toDB.svy <- db_write_svy_tables