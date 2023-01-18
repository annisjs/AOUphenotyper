

#' Read bucket
#'
#' @param export_path
#'
#' @return a data.table
#' @export
read_bucket <- function(export_path) {
  col_types <- NULL
  #Check if the bucket is provided. If not, append.
  bucket_provided <- grepl("^gs://",export_path)
  if (!bucket_provided)
  {
    bucket <- Sys.getenv("WORKSPACE_BUCKET")
    export_path <- stringr::str_glue("{bucket}/{export_path}")
  }
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

#' Mode
#'
#' @export
mode_stat <- function(x) {
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

#' Display table
#'
#' @export
display_table <- function(table1)
{
  out <- Hmisc::html(table1, caption='',
                     exclude1=F, npct='both', digits=3,long=T,
                     prmsd=TRUE, brmsd=T, longtable=T, middle.bold=T,
                     vnames = c('names'))
  IRdisplay::display_html(out)
}

#' Display ANOVA
#'
#' @export
display_anova <- function(aov,width=150)
{
  w <- paste0("width: ",width,"px")
  a <- anova(aov)
  rn <- rownames(a)
  a <- as.data.frame(as.matrix(a))
  a[,"P"] <- scales::pvalue(a[,"P"],accuracy = .0001)
  a[,1] <- round(a[,1],4)
  a <- as.data.frame(a)
  IRdisplay::display_html(htmlTable::htmlTable(a, align = "c",
                                               rnames = rn,
                                               padding.tspanner = "", ctable = TRUE,
                                               css.cell = w,
  ))
}

#' Display summary
#'
#' @export
display_summary <- function(s,width=150)
{
  w <- paste0("width: ",width,"px")
  s <- summary(s)
  rn <- rownames(s)
  s <- as.data.frame(cbind(as.matrix(s),
                           "p-value"=2*pnorm(q=abs(s[,"Effect"]/s[,"S.E."]),
                                             lower.tail=FALSE)))
  s[,"p-value"] <- scales::pvalue(s[,"p-value"],accuracy = .0001)
  s[,1:8] <- apply(s[,1:8],2,round,2)
  s <- as.data.frame(s)
  s <- s[,c(1:7,9)]
  IRdisplay::display_html(htmlTable::htmlTable(s, align = "c",
                                               rnames = rn,
                                               padding.tspanner = "", ctable = TRUE,
                                               css.cell = w,
  ))
}


backup_packages <- function()
{
  package_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"),"packages")
  system(str_glue('gsutil -m cp -r /home/jupyter/packages/* {package_path} 2>&1'), intern = TRUE)
}

restore_packages <- function()
{
  package_path <- file.path(Sys.getenv("WORKSPACE_BUCKET"),"packages")
  system(str_glue("gsutil -m cp -r {package_path}/* /home/jupyter/packages 2>&1"), intern = TRUE)
}
