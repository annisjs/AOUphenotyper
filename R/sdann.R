#' SDANN: Standard deviation of the average NN intervals
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdann.csv
#' @details Heart rate minute level data is extracted and five-minute averages are calculated for consecutive five-minute intervals.
#' The average HR is used to calculate the average RR duration of each five-minute interval:
#' Average RR = 6000 / mean(HR)
#' Subsequently, the standard deviation of all the five-minute RR intervals is calculated, yielding the SDANN value (in ms).
#' @import stringr bigrquery
#' @export
sdann <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue(
    "
    SELECT person_id,
           sdann_date,
           STDDEV(avg_rr) AS sdann_value,
           SUM(valid_interval) AS sdann_total_valid_interval
    FROM (SELECT
            person_id,
            CAST(datetime AS DATE) AS sdann_date,
            6000 / AVG(heart_rate_value) AS avg_rr,
            IF(COUNT(*)=5,1,0) AS valid_interval
        FROM (SELECT person_id,
                     datetime,
                     heart_rate_value,
                     FLOOR((EXTRACT(MINUTE FROM datetime) + 60 * EXTRACT(HOUR FROM datetime)) / 5) AS minute_interval
              FROM {dataset}.heart_rate_minute_level
        )
        GROUP BY person_id, sdann_date, minute_interval
        HAVING valid_interval = 1
    )
    GROUP BY person_id, sdann_date
  ")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/sdann_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/sdann_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[sdann_date >= min_window_date]
    result <- result[sdann_date <= max_window_date]
    result <- result[,c("person_id","sdann_date","sdann_value","sdann_total_valid_interval")]
  }
  fwrite(result,file="sdann.csv")
  system(str_glue("gsutil cp sdann.csv {output_folder}/sdann.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
