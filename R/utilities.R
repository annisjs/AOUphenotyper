

#' Read bucket
#'
#' @param export_path
#'
#' @return a data.table
#' @export
read_bucket <- function(export_path) {
  col_types <- NULL
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  export_path <- stringr::str_glue("{bucket}/{export_path}")
  data.table::as.data.table(dplyr::bind_rows(
    purrr::map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
               function(csv) {
                 chunk <- readr::read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
                 if (is.null(col_types)) {
                   col_types <- readr::spec(chunk)
                 }
                 chunk
               })))
}

#' Download data given query
#'
#' @param a SQL query string
#'
#' @return a data.table
#' @export
#' @import data.table bigrquery
download_data <- function(query) {
  tb <- bq_project_query(Sys.getenv('GOOGLE_PROJECT'), query)
  as.data.table(bq_table_download(tb,page_size=100000))
}

#' gsutil ls
#' @export
ls_bucket <- function(folder=NULL) {
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  if (is.null(folder))
  {
    system(str_glue("gsutil ls {bucket}"),intern=TRUE)
  } else {
    system(str_glue("gsutil ls {bucket}/{folder}"),intern=TRUE)
  }
}

#' gsutil rm
#' @export
rm_bucket <- function(path)
{
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  system(str_glue("gsutil rm {bucket}/{path}"),intern=TRUE)
}

#' gsutil cp to
#' @export
cp_to_bucket <- function(from,to)
{
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  system(str_glue("gsutil cp {from} {bucket}/{to}"),intern=TRUE)
}

#' gsutil cp from
#' @export
cp_from_bucket <- function(from,to)
{
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  system(str_glue("gsutil cp {bucket}/{from} {to}"),intern=TRUE)
}

#' Recode race to White, Black, Other
#' @export
recode_race_wbo <- function(race)
{
  ifelse(race == "I prefer not to answer" |
         race == "None of these" |
         race == "None Indicated",
       NA,
       ifelse(race == "Black or African American",
              "Black",
              ifelse(race == "White",
                     "White",
                     "Other")))
}

#' Recode sex to Male, Female
#' @export
recode_sex_fm <- function(sex)
{
  ifelse(sex == "Not male, not female, prefer not to answer, or skipped" |
           sex == "No matching concept",
         NA,
         ifelse(sex == "Male",
                "Male",
                "Female"))
}

#' Cleans fitbit data
#' @export
clean_fitbit <- function(fitbit_dat,wear_time,date_of_birth)
{
  fitbit_dat <- merge(fitbit_dat,wear_time,by=c("person_id","date"))
  cat("\nInitial cohort")
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))

  cat("\n\nRemoving days where wear time < 10 hrs.")
  fitbit_dat <- fitbit_dat[wear_time >= 10]
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))

  cat("\n\nRemoving days where step count < 100.")
  fitbit_dat <- fitbit_dat[steps >= 100]
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))

  cat("\n\nRemoving days where step counts > 45,000.")
  fitbit_dat <- fitbit_dat[steps <= 45000]
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))

  cat("\n\nRemoving days where age < 18.")
  fitbit_dat <- merge(fitbit_dat,date_of_birth,by="person_id",all.x=TRUE)
  fitbit_dat[,age := as.numeric(as.Date(date) - as.Date(date_of_birth))/365.25]
  fitbit_dat <- fitbit_dat[age >= 18]
  cat("\nN: ",length(unique(fitbit_dat$person_id)))
  cat("\nDays: ",nrow(fitbit_dat))
  fitbit_dat
}

