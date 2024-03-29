#' Last asleep datetime
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/last_asleep.csv
#' @details last asleep level, date and time where is_main_sleep = true
#' @import stringr bigrquery
#' @export
last_asleep <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- paste("
        SELECT person_id,
               sleep_date AS last_asleep_date,
               start_datetime AS last_asleep_datetime,
               duration_in_min AS last_asleep_duration,
               is_main_sleep AS last_asleep_is_main_sleep
        FROM (SELECT person_id, sleep_date, start_datetime, duration_in_min, is_main_sleep,
               row_number() over(partition by person_id, sleep_date order by start_datetime desc) as rn
                FROM sleep_level
                WHERE is_main_sleep = 'true' AND level != 'wake' AND level != 'awake') as t1
        WHERE rn = 1",sep="")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/last_asleep_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/last_asleep_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[last_asleep_date >= min_window_date]
    result <- result[last_asleep_date <= max_window_date]
    result <- result[,c("person_id","last_asleep_datetime","last_asleep_date","last_asleep_duration",
                        "last_asleep_is_main_sleep")]
  }
  fwrite(result,file="last_asleep.csv")
  system(str_glue("gsutil cp last_asleep.csv {output_folder}/last_asleep.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
