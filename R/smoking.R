#' Smoking
#' @export
#' @return output_folder/smoking_*.csv
#' @import data.table stringr bigrquery
smoking <- function(output_folder)
{
  smoking_query <- paste("
        SELECT
            answer.person_id,
            answer.answer
        FROM
            `ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1585857
                )
            )", sep="")

  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), smoking_query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/smoking_*.csv"),
    destination_format = "CSV")
}
