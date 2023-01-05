#' Alcohol
#'
#' @param output_folder
#'
#' @return CSV saved to output_folder/alcohol_*.csv
#' @export
alcohol <- function(output_folder)
{
  alcohol_query <- paste("
        SELECT
            answer.person_id,
            answer.answer
        FROM
            `ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1586198
                )
            )", sep="")

  bigrquery::bq_table_save(
    bigrquery::bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), alcohol_query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/alcohol_*.csv"),
    destination_format = "CSV")
}
