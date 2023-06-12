#' First light sleep datetime
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/first_light_sleep.csv
#' @details First light sleep level, date and time
#' @import stringr bigrquery
#' @export
first_light_sleep <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- paste("
        SELECT min(start_datetime) AS first_light_sleep_datetime,
            CAST(start_datetime AS DATE) as first_light_sleep_date
        FROM sleep_level
        WHERE level = 'light'
        GROUP BY person_id, first_light_sleep_date", sep="")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/first_light_sleep_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/first_light_sleep_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[first_light_sleep_date >= min_window_date]
    result <- result[first_light_sleep_date <= max_window_date]
    result <- result[,c("person_id","first_light_sleep_datetime","first_light_sleep_date")]
  }
  fwrite(result,file="first_light_sleep.csv")
  system(str_glue("gsutil cp first_light_sleep.csv {output_folder}/first_light_sleep.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
