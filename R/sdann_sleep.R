#' SDANN while asleep: Standard deviation of the average NN intervals while asleep
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdann_sleep.csv
#' @details Heart rate minute level data is extracted and five-minute averages are calculated for consecutive five-minute intervals.
#' The average HR is used to calculate the average RR duration of each five-minute interval:
#' Average RR = 6000 / mean(HR)
#' Subsequently, the standard deviation of all the five-minute RR intervals is calculated, yielding the SDANN value (in ms).
#' HR values are relegated to those while sleeping.
#' @import stringr bigrquery
#' @export
sdann_sleep <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
    SELECT person_id,
           sdann_sleep_date,
           STDDEV(avg_rr) AS sdann_sleep_value,
           SUM(valid_interval) AS sdann_sleep_total_valid_interval
    FROM (SELECT
            person_id,
            sleep_date AS sdann_sleep_date,
            6000 / AVG(heart_rate_value) AS avg_rr,
            IF(COUNT(*)=5,1,0) AS valid_interval
        FROM (WITH
                first_asleep AS (
                        SELECT person_id,
                               sleep_date,
                               start_datetime AS sleep_start_datetime
                        FROM (SELECT person_id, sleep_date, start_datetime,
                               row_number() over(partition by person_id,
                                                    sleep_date order by start_datetime asc) as rn
                                FROM {dataset}.sleep_level
                                WHERE is_main_sleep = 'true') as t1
                        WHERE rn = 1
                    ),
                last_asleep AS (
                    SELECT person_id,
                           sleep_date,
                           DATETIME_ADD(start_datetime,
                                INTERVAL CAST(duration_in_min AS INT) MINUTE) as sleep_end_datetime
                    FROM (SELECT person_id, sleep_date, start_datetime, duration_in_min, is_main_sleep,
                           row_number() over(partition by person_id,
                                                sleep_date order by start_datetime desc) as rn
                            FROM {dataset}.sleep_level
                            WHERE is_main_sleep = 'true') as t1
                    WHERE rn = 1
                )
             SELECT h.person_id,
                    la.sleep_date,
                    heart_rate_value,
                    FLOOR((EXTRACT(MINUTE FROM datetime) +
                                60 * EXTRACT(HOUR FROM datetime)) / 5) AS minute_interval
              FROM last_asleep la
                   INNER JOIN first_asleep fa ON (la.person_id = fa.person_id AND la.sleep_date = fa.sleep_date)
                   INNER JOIN {dataset}.heart_rate_minute_level AS h ON (h.person_id = fa.person_id AND
                                           h.datetime BETWEEN sleep_start_datetime AND sleep_end_datetime)
        )
        GROUP BY person_id, sdann_sleep_date, minute_interval
        HAVING valid_interval = 1
    )
    GROUP BY person_id, sdann_sleep_date
    ")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/sdann_sleep_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/sdann_sleep_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[sdann_sleep_date >= min_window_date]
    result <- result[sdann_sleep_date <= max_window_date]
    result <- result[,c("person_id","sdann_sleep_date","sdann_sleep_value","sdann_sleep_total_valid_interval")]
  }
  fwrite(result,file="sdann_sleep.csv")
  system(str_glue("gsutil cp sdann_sleep.csv {output_folder}/sdann_sleep.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
