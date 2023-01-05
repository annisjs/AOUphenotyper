#' Fitbit
#' @export
#' @return output_folder/fitbit_*.csv
#' @import stringr bigrquery
fitbit <- function(output_folder)
{
  query <- paste("
        SELECT
            activity_summary.person_id,
            activity_summary.date,
            activity_summary.steps,
            activity_summary.fairly_active_minutes,
            activity_summary.lightly_active_minutes,
            activity_summary.sedentary_minutes,
            activity_summary.very_active_minutes
        FROM
            `activity_summary` activity_summary
        WHERE
            activity_summary.PERSON_ID IN (
                SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (
                        SELECT
                            person_id
                        FROM
                            `cb_search_person` p
                        WHERE
                            has_fitbit = 1
                    )
                )", sep="")
  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/fitbit_*.csv"),
    destination_format = "CSV")
}
