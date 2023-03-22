#' Employment
#' @export
#' @return output_folder/employment.csv
#' @import data.table stringr
employment <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        SELECT
            answer.person_id,
            answer.answer as employment_status,
            CAST(answer.survey_datetime AS DATE) AS employment_entry_date
        FROM
            `{dataset}.ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1585952
                )
            )")

  result <- download_data(query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[employment_entry_date >= min_window_date]
    result <- result[employment_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","employment_entry_date","employment_status")]
  fwrite(result,file="employment.csv")
  system(str_glue("gsutil cp employment.csv {output_folder}/employment.csv"),intern=TRUE)
}

