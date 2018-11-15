push <- function(x,...)UseMethod("push",x)

#' push a svy object up to the database
#' 
#' @param s a \code{svy} object
#' @param name cannonical, unique survey name.  by convention:
#' <project-id>_<project-name>[__<survey-name>].
#' @param indexes ignored for now
#' @param overwrite whether to overwrite existing tables
#' @param con a \code{PqConnection} to the survey database
#' 
#' Push data and metadata of the survey up to the server.  The schema  
#' takes the name of the survey and contains at least five tables: 
#'  * instance, containing the collected data with one column per question 
#'  * question, containing the form structure, including group information, 
#'  name information, node, type and labels, one row per question
#'  * choice, containing the choices for multiple choice questions, including
#'  question and the choice name
#'  * label, containing the text of all question labels in all 
#'  available languages, with question, language and text columns
#'  * choice_label, containing the test of all choice labels, with question,
#'  choice, language and text columns
#'  
#'  @return a list of tibbles connected to the database
#'  
#'  @export
push.svy <- function(s, 
                     indexes=NULL,
                     overwrite=FALSE,
                     ...){
  # make sure we have a schema
  if( exists("schema", inherits = FALSE) && ! is.null(schema)) 
      old_schema <- setSchema(schema) else setSchema()
  
  # insert an instance column at the front with the stripped uuid
  instance <- dplyr::bind_cols(
    instance = sub(
      "^uuid:", "", 
      structure(
        s$`meta/instanceID`, 
        class = "character",
        db_type = "uuid"
      )
    ),
    # and format the svy for uploading
    db_format(s)
  )
  push(instance, ...)
  
  # a table with all the form data
  question <- tibble(
    # don't include instance, since we made that up
    question = names(instance)[-1],
    group_ = groups(s),
    name = all_names(s),
    type = types(s),
    node = structure(laply(s, function(q)
      as.character(jsonlite::toJSON(node(q)))), 
      db_type = "json")
  )
  
  push(question, ...)
  
  # a table with all the choices
  choice <- s[types(s) %in% c( # just the multiple choice svqs
    "select one",
    "select all that apply"
  )] %>%
    ldply(function(q){
      df <- tibble(
        question = name(q),
        choice = {
          c <- choices(q)
          if(Hmisc::all.is.numeric(c)) as.integer(c) else c
        }
      )
      df
    }, .id = NULL)
  
  push(choice, ...)
  
  getq <- function(qr){
    name <- ifelse(
      is.na(qr$group_), 
      qr$name,
      paste(qr$group_, qr$name, sep = "/")
    )
    q <- s[[name]]
    if(! is(q, "svq")) stop("didn't find an svq object with the name:\n", name)
    q
  }
  
  # a table with all the question labels.
  label <- adply(question, 1, function(qr){
    sq <- getq(qr)
    if(! has_label(sq)) NULL else
      ldply(languages(s), function(l)
        tibble(
          question = qr$question,
          language_ = l,
          text = label(sq, lang = l)
        ), .id = NULL)
  }, .id = NULL)
  push(label,...)
  
  # a table with all the choice labels
  label_choice <- adply(
    question[question$type %in% c(
      "select one",
      "select all that apply"
    ),], 1, function(qr){
      sq <- getq(qr)
      if(! has_labels(sq)) NULL else
        adply(choice[choice$question == qr$question,], 1, function(ch)
          ldply(languages(s), function(l)
            tibble(
              question = qr$question,
              choice = ch$choice,
              language_ = l,
              text = unname(labels(sq, l)[ch$choice])
            ), .id = NULL
          ), .expand = FALSE, .id = NULL
        )
    }, .expand = FALSE, .id = NULL
  )
  push(label_choice, ...)
  
  doSQL("
  alter table instance
  add constraint instance_pk primary key (instance);
  ")
  doSQL("
  alter table label
  add constraint label_pk primary key (question, language_) ;
  ")
  doSQL("
  alter table label_choice
  add constraint label_choice_pk primary key (question, choice, language_);
  ")
  doSQL("
  alter table choice
  add constraint choice_pk primary key (question, choice);
  ")
  doSQL("
  alter table question
  add constraint question_pk primary key (question);
  ")
  doSQL("
  alter table choice
  add constraint choice_question_fk foreign key (question) 
    references question (question);
  ")
  doSQL("
  alter table label
  add constraint label_question_fk foreign key (question) 
    references question (question);
  ")
  doSQL("
  alter table label_choice
  add constraint label_choice_question_fk foreign key (question) 
    references question (question);
  ")
  doSQL("
  alter table label_choice
  add constraint label_choice_choice_fk foreign key (question, choice) 
    references choice (question, choice);
  ")
  doSQL("
  alter table label_choice
  add constraint label_choice_label_fk foreign key (question, language_) 
    references label (question, language_);
  ")

  # rosters, if any
  rosters <- alply(question[types(s) == "repeat", ], 1, function(qr)
    getq(qr) %>% 
      collapse(instance$instance) %>%
      push(prefix = paste0("repeat_", q["question"]))
  )
  if(length(rosters)>0) names(rosters) <- paste0("repeat_", names(rosters))
  
  c(
    list(
      instance = instance,
      question = question,
      choice = choice,
      label = label,
      label_choice = label_choice
    ),
    rosters
  )
}

push.data.frame <- function(df,
                            name = NULL,
                            prefix = NULL,
                            schema = getSchema(),
                            con = connection(),
                            modify = FALSE){
  if(is.null(name)) name <- deparse(substitute(df))
  if(! is.null(prefix)) name = paste(prefix, name, sep = "_")
  if(! is.null(getOption("svyDBSchema")) && schema != getOption("svyDBSchema")){
    browser()
    old_schema <- setSchema(schema)
  }
  # browser()  
  if( ! modify && 
      ! identical(make.sql.names(names(df)),names(df)))
    stop("some names not allowed, declining to fix.")
  if(modify) names(df) <- make.sql.names(names(df))
  RPostgres::dbWriteTable(
    conn = con, 
    name = name, 
    value = df,
    # if the field type is already set, keep it, otherwise default to RPostgres
    field.types = structure(ifelse(
      is.na(laply(df, db_type)),
      laply(df,RPostgres::dbDataType, dbObj = con),
      laply(df, db_type)
    ), names = names(df))
  )
  if(exists("old_schema", inherits = FALSE)){ browser(); setSchema(old_schema)} 
  connection() %>% dplyr::tbl(dbplyr::in_schema(schema, name))
}

