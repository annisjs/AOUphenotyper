#' Top 5 Steps
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/top5_steps.csv
#' @details Top 5-minute step count for each day.
#' @import stringr bigrquery
#' @export
top5_steps <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        WITH cte AS (
        SELECT  *,
                ROW_NUMBER() OVER (PARTITION BY person_id, CAST(datetime AS DATE) ORDER BY steps DESC) as rn
        FROM `{dataset}.steps_intraday`
        )
        SELECT person_id, CAST(datetime AS DATE) AS top5_steps_date, AVG(steps) AS top5_steps_value
        FROM cte
        WHERE rn <= 5
        GROUP BY
        CAST(datetime AS DATE),person_id
    ")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/top5_steps_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/top5_steps_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[date >= min_window_date]
    result <- result[date <= max_window_date]
    result <- result[,c("person_id","top5_steps_date","top5_steps_value")]
  }
  fwrite(result,file="top5_steps.csv")
  system(str_glue("gsutil cp top5_steps.csv {output_folder}/top5_steps.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
