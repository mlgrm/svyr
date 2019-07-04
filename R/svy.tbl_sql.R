svy.tbl_sql <- function(dat, form = fetch_form(dat)){
  # can't handle composite tbls.  need raw data from db
  stopifnot(is(dat$ops, "op_base"))
}

fetch_form <- function(dat){}