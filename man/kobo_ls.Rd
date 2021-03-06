% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kobo.R
\name{kobo_ls}
\alias{kobo_ls}
\title{retrieve a list of all a user's projects from a kobo server}
\usage{
kobo_ls(refresh = F, cols = c("formid", "description",
  "num_of_submissions", "last_submission_time"),
  server = getOption("koboServer"), token = getOption("koboToken"))
}
\description{
retrieve a list of all a user's projects from a kobo server
}
