#' Fitbit wear time
#' @export
#' @return output_folder/wear_time_*.csv
#' @import data.table stringr bigrquery
wear_time <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
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
    paste0(output_folder,"/aou_phenotyper/wear_time_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/wear_time_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[date >= min_window_date]
    result <- result[date <= max_window_date]
    result <- result[,c("person_id","date","wear_time")]
  }
  fwrite(result,file="wear_time.csv")
  system(str_glue("gsutil cp wear_time.csv {output_folder}/wear_time.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
