#' Smoking
#' @export
#' @return output_folder/smoking_*.csv
#' @import data.table stringr
smoking <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  smoking_query <- str_glue("
        SELECT
            answer.person_id,
            answer.answer AS smoking_status,
            CAST(answer.survey_datetime AS DATE) AS smoking_entry_date
        FROM
            `{dataset}.ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1585857
                )
            )")

  result <- download_data(smoking_query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[smoking_entry_date >= min_window_date]
    result <- result[smoking_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","smoking_entry_date","smoking_status")]
  fwrite(result,file="smoking.csv")
  system(str_glue("gsutil cp smoking.csv {output_folder}/smoking.csv"),intern=TRUE)
}
