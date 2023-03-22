#' Health insurance
#' @export
#' @return output_folder/health_insurance.csv
#' @import data.table stringr
health_insurance <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        SELECT
            answer.person_id,
            answer.answer as health_insurance_status,
            CAST(answer.survey_datetime AS DATE) AS health_insurance_entry_date
        FROM
            `{dataset}.ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1585386
                )
            )")

  result <- download_data(query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[health_insurance_entry_date >= min_window_date]
    result <- result[health_insurance_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","health_insurance_entry_date","health_insurance_status")]
  fwrite(result,file="health_insurance.csv")
  system(str_glue("gsutil cp health_insurance.csv {output_folder}/health_insurance.csv"),intern=TRUE)
}

