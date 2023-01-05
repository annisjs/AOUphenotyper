#' Education
#' @export
#' @return output_folder/education_*.csv
#' @import data.table stringr bigrquery
education <- function(output_folder)
{
  edu_query <- paste("
        SELECT
            answer.person_id,
            answer.answer
        FROM
            `ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1585940
                )
            )", sep="")

  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), edu_query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/education_*.csv"),
    destination_format = "CSV")
}
