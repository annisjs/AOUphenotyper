#' Alcohol
#'
#' @param output_folder
#'
#' @return CSV saved to output_folder/alcohol.csv
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

