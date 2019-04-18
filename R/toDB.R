toDB.svy <- function(s,
                     name,
                     conn = NULL,
                     db = getOption("dbName"),
                     server = getOption("dbServer"),
                     user = getOption("dbUser"),
                     password = getOption("dbPasswd"),
                     schema = getOption("dbSchema"),
                     overwrite = FALSE,
                     append = FALSE
                     ){
  names(s) <- 
    # get names from svqs
    map_chr(s, svyr::name) %>%
    # unless they have none
    { ifelse(is.na(.), names(s), .) } %>% 
    make.sql.names

  # split up matrices
  for(n in colnames(s)){
    if(is.matrix(s[[n]])){
      df <- as_tibble(s[[n]])
      #df <- map_df(df, ~structure(., node = node(s[[n]])))
      names(df) <- paste(n,names(df), sep = "__")
      s <- do.call(function(...)add_column(s, ..., .after = n), df)
      s[[n]] <- NULL
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
    ) dbGetQuery(conn, str_glue("create schema {schema}"))
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
      s <- bind_rows.svy(rpts[[n]], .id = n)
      toDB.svy(
        s,
        name = rpt_name,
        conn = conn,
        db = db,
        server = server,
        user = user,
        password = password,
        schema = schema,
        overwrite = overwrite,
        append = append
      )
      dbSendQuery(
        conn,
        str_glue("alter table {rpt_name}
                  add foreign key ({n}) references {name}({n})
                  ;")
      )
    })
  invisible(c(list(tbl(conn, name)),unlist(rptbl,recursive = FALSE)))
}