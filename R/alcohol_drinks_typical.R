#' Number of drinks on a typical day
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/alcohol_drinks_typical.csv
#' @details Alcohol AoU intake survey. On a typical day when you drink, how many drinks do you have?
#' @import data.table
#' @export
alcohol_drinks_typical <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  alcohol_query <- str_glue("
        SELECT
            answer.person_id,
            answer.answer AS alcohol_drinks_typical_status,
            CAST(answer.survey_datetime AS DATE) AS alcohol_drinks_typical_entry_date
        FROM
            `{dataset}.ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1586207
                )
            )")

  result <- download_data(alcohol_query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[alcohol_drinks_typical_entry_date >= min_window_date]
    result <- result[alcohol_drinks_typical_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","alcohol_drinks_typical_entry_date","alcohol_drinks_typical_status")]
  fwrite(result,file="alcohol_drinks_typical.csv")
  system(str_glue("gsutil cp alcohol_drinks_typical.csv {output_folder}/alcohol_drinks_typical.csv"),intern=TRUE)
}

