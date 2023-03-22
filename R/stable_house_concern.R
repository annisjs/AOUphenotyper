#' Stable house concern
#' @export
#' @return output_folder/stable_house_concern.csv
#' @import data.table stringr
#' @description In the past 6 months, have you been worried or concerned about NOT having a place to live?
stable_house_concern <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        SELECT
            answer.person_id,
            answer.answer as stable_house_concern_status,
            CAST(answer.survey_datetime AS DATE) AS stable_house_concern_entry_date
        FROM
            `{dataset}.ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1585886
                )
            )")

  result <- download_data(query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[stable_house_concern_entry_date >= min_window_date]
    result <- result[stable_house_concern_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","stable_house_concern_entry_date","stable_house_concern_status")]
  fwrite(result,file="stable_house_concern.csv")
  system(str_glue("gsutil cp stable_house_concern.csv {output_folder}/stable_house_concern.csv"),intern=TRUE)
}

