#' Age stopped smoking
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Data is from the AoU intake survey. Do you now smoke cigarettes every day, some days, or not at all?
#' @return output_folder/smoking_stop_age.csv
#' @import data.table stringr
#' @export
smoking_stop_age <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        SELECT
            answer.person_id,
            answer.answer AS smoking_stop_age_value,
            CAST(answer.survey_datetime AS DATE) AS smoking_stop_age_entry_date
        FROM
            `{dataset}.ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1585870
                )
            )")

  result <- download_data(query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[smoking_stop_age_entry_date >= min_window_date]
    result <- result[smoking_stop_age_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","smoking_stop_age_entry_date","smoking_stop_age_value")]
  fwrite(result,file="smoking_stop_age.csv")
  system(str_glue("gsutil cp smoking_stop_age.csv {output_folder}/smoking_stop_age.csv"),intern=TRUE)
}
