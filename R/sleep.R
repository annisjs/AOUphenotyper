#' Sleep data from Fitbit
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sleep.csv
#' @details Fitbit sleep data contains:
#' 	person_id
#' 	sleep_date
#' 	is_main_sleep
#' 	minute_in_bed
#' 	minute_a_sleep
#' 	minute_after_wakeup
#' 	minute_awake
#' 	minute_restless
#' 	minute_deep
#' 	minute_light
#' 	minute_rem
#' 	minute_wake
#' @import stringr bigrquery
#' @export
sleep <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- paste("
        SELECT
          sleep_daily_summary.person_id
          , sleep_daily_summary.sleep_date
          , sleep_daily_summary.is_main_sleep
          , sleep_daily_summary.minute_in_bed
          , sleep_daily_summary.minute_asleep
          , sleep_daily_summary.minute_after_wakeup
          , sleep_daily_summary.minute_awake
          , sleep_daily_summary.minute_restless
          , sleep_daily_summary.minute_deep
          , sleep_daily_summary.minute_light
          , sleep_daily_summary.minute_rem
          , sleep_daily_summary.minute_wake
        FROM
            `sleep_daily_summary` sleep_daily_summary", sep="")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/sleep_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/sleep_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[date >= min_window_date]
    result <- result[date <= max_window_date]
    result <- result[,c('person_id',
                        'sleep_date',
                        'is_main_sleep',
                        'minute_in_bed',
                        'minute_asleep',
                        'minute_after_wakeup',
                        'minute_awake',
                        'minute_restless',
                        'minute_deep',
                        'minute_light',
                        'minute_rem',
                        'minute_wake')]
  }
  fwrite(result,file="sleep.csv")
  system(str_glue("gsutil cp sleep.csv {output_folder}/sleep.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
