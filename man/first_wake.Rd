% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/first_wake.R
\name{first_wake}
\alias{first_wake}
\title{First wake datetime}
\usage{
first_wake(
  dataset,
  output_folder,
  anchor_date_table = NULL,
  before = NULL,
  after = NULL
)
}
\arguments{
\item{dataset}{a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.}

\item{output_folder}{the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").}

\item{anchor_date_table}{a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.}

\item{before}{an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.}

\item{after}{an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.}
}
\value{
output_folder/first_wake_datetime.csv
}
\description{
First wake datetime
}
\details{
First light sleep level, date and time
}
