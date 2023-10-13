#' Asleep level data from Fitbit
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sleep_level_asleep.csv
#' @details Fitbit sleep data contains:
#' 	person_id
#' 	sleep_date
#' 	start_datetime
#' 	is_main_sleep
#' 	duration_in_min
#' @import stringr bigrquery
#' @export
sleep_level_asleep <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- paste("
        SELECT
          person_id
          , sleep_date AS sleep_level_asleep_sleep_date
          , start_datetime AS sleep_level_asleep_start_datetime
          , is_main_sleep AS sleep_level_asleep_is_main_sleep
          , duration_in_min AS sleep_level_asleep_duration_in_min
        FROM
            `sleep_level` sleep_level
        WHERE level = 'asleep'", sep="")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/sleep_level_asleep_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/sleep_level_asleep_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[sleep_level_asleep_sleep_date >= min_window_date]
    result <- result[sleep_level_asleep_sleep_date <= max_window_date]
    result <- result[,c('person_id',
                        'sleep_level_asleep_sleep_date',
                        'sleep_level_asleep_start_datetime',
                        'sleep_level_asleep_is_main_sleep',
                        'sleep_level_asleep_duration_in_min')]
  }
  fwrite(result,file="sleep_level_asleep.csv")
  system(str_glue("gsutil cp sleep_level_asleep.csv {output_folder}/sleep_level_asleep.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
