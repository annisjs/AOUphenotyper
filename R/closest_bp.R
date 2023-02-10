#' Closest blood pressure
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @param cohort an option SQL expression representing the cohort to limit the number of BP records.
#' @export
#' @return output_folder/closest_bp.csv
#' @import data.table stringr bigrquery
closest_bp <-  function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL,cohort=NULL)
{

  if (is.null(anchor_date_table))
  {
    stop("closest_bp is not a primary variable and requires an anchor date table.")
  }

  if (!is.null(cohort))
  {
    query <- str_glue("
        WITH c AS ({cohort}),
        diatb AS (SELECT
            person_id, measurement_datetime, value_as_number AS bp_diastolic
            FROM `{dataset}.measurement` m
        WHERE
            m.measurement_source_value IN ('8462-4','8453-3')),
        systb AS (SELECT
            person_id, measurement_datetime, value_as_number AS bp_systolic
            FROM `{dataset}.measurement` m
        WHERE
            m.measurement_source_value IN ('8480-6','8459-0'))
        SELECT c.person_id,
               CAST(d.measurement_datetime AS DATE) AS measurement_date,
               bp_systolic,
               bp_diastolic
        FROM
        c
        INNER JOIN diatb d
        ON (c.person_id = d.person_id)
        INNER JOIN systb s
        ON (d.person_id = s.person_id)
        WHERE
        d.measurement_datetime = s.measurement_datetime")
  } else {
    query <- str_glue("
        WITH diatb AS (SELECT
            person_id, measurement_datetime, value_as_number AS bp_diastolic
            FROM `{dataset}.measurement` m
        WHERE
            m.measurement_source_value IN ('8462-4','8453-3')),
        systb AS (SELECT
            person_id, measurement_datetime, value_as_number AS bp_systolic
            FROM `{dataset}.measurement` m
        WHERE
            m.measurement_source_value IN ('8480-6','8459-0'))
        SELECT d.person_id,
               CAST(d.measurement_datetime AS DATE) AS measurement_date,
               bp_systolic,
               bp_diastolic
        FROM
        diatb d
        INNER JOIN systb s
        ON (d.person_id = s.person_id)
        WHERE
        d.measurement_datetime = s.measurement_datetime")
  }
  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/closest_bp_*.csv"),
    destination_format = "CSV")
  result_all <- as.data.table(read_bucket(str_glue("{output_folder}/aou_phenotyper/closest_bp_*.csv")))
  result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
  result_all[,min_window_date := anchor_date + before]
  result_all[,max_window_date := anchor_date + after]
  result_all <- result_all[measurement_date >= min_window_date]
  result_all <- result_all[measurement_date <= max_window_date]
  result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
  result_all <- result_all[order(diff)]
  result_all <- result_all[,.(closest_bp_entry_date = measurement_date[1],
                              closest_bp_systolic = bp_systolic[1],
                              closest_bp_diastolic = bp_diastolic[1]),.(person_id,anchor_date)]
  fwrite(result_all,file="closest_bp.csv")
  system(str_glue("gsutil cp closest_bp.csv {output_folder}/closest_bp.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
