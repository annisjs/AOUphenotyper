#' Fitbit wear time
#' @export
#' @return output_folder/wear_time_*.csv
#' @import data.table stringr bigrquery
wear_time <- function(output_folder)
{
  query <- str_glue("
        SELECT person_id, date, SUM(has_hour) AS wear_time
            FROM (SELECT person_id, CAST(datetime AS DATE) AS date, IF(SUM(steps)>0, 1, 0) AS has_hour
                    FROM `steps_intraday`
                    GROUP BY CAST(datetime AS DATE), EXTRACT(HOUR FROM datetime), person_id) t
        GROUP BY date, person_id
")
  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/wear_time_*.csv"),
    destination_format = "CSV")
}
