toDB.svy <- function(s,
                     name,
                     conn = NULL,
                     db = getOption("dbName"),
                     server = getOption("dbServer"),
                     user = getOption("dbUser"),
                     password = getOption("dbPasswd"),
                     schema = getOption("dbSchema")
                     ){
  names(s) <- make.unique(map_chr(s, svyr::name), sep = "_")
  # split up matrices
  for(n in colnames(s)){
    if(is.matrix(s[[n]])){
      df <- as_tibble(s[[n]])
      df <- map_df(df, ~structure(., node = node(s[[n]])))
      names(df) <- paste(n,names(df), sep = "__")
      s <- do.call(function(...)add_column(s, ..., .after = n), df)
      s[[n]] <- NULL
    }
  }
  # collect rosters
  rpts <- select_if(s, function(x)type(x) == "repeat")
  # simplify lists of surveys (rosters)
  s <- mutate_if(
    s, 
    ~type(.) == "repeat", 
    ~1:length(.)
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
    dbGetQuery(
      conn,
      str_glue("set search_path to {schema}, 
               {paste(search_path, collapse = ', ')}")
    ) 
  db_write_table(
    conn, 
    name, 
    dbDataType(conn, s), 
    as_tibble(s), 
    temporary = FALSE
  )
  map(names(rpts), function(n){
    s <- bind_rows.svy(rpts[[n]])
    db_write_table(
      conn, 
      str_glue("{name}__rpt_{n}"),
      dbDataType(conn, s),
      as_tibble(s),
      temporary = FALSE
    )
  })
  tbl(conn, name)
}