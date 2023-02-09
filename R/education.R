#' Education
#' @export
#' @return output_folder/education.csv
#' @import data.table stringr
education <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  edu_query <- str_glue("
        SELECT
            answer.person_id,
            answer.answer as education_status,
            CAST(answer.survey_datetime AS DATE) AS education_entry_date
        FROM
            `{dataset}.ds_survey` answer
        WHERE
            (
                question_concept_id IN (
                    1585940
                )
            )")

  result <- download_data(edu_query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[education_entry_date >= min_window_date]
    result <- result[education_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","education_entry_date","education_status")]
  fwrite(result,file="education.csv")
  system(str_glue("gsutil cp education.csv {output_folder}/education.csv"),intern=TRUE)
}

