#' Alcohol
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date - before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/alcohol.csv
#' @details Alcohol AoU intake survey
#' @import data.table
#' @export
alcohol <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  alcohol_query <- str_glue("
        SELECT
            answer.person_id,
            answer.answer AS alcohol_status,
            CAST(answer.survey_datetime AS DATE) AS alcohol_entry_date
        FROM
            `{dataset}.ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1586198
                )
            )")

  result <- download_data(alcohol_query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[alcohol_entry_date >= min_window_date]
    result <- result[alcohol_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","alcohol_entry_date","alcohol_status")]
  fwrite(result,file="alcohol.csv")
  system(str_glue("gsutil cp alcohol.csv {output_folder}/alcohol.csv"),intern=TRUE)
}

