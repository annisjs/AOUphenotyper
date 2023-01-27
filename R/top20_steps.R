#' Top 20 Steps
#' @export
#' @return output_folder/top20_steps.csv
#' @import stringr bigrquery
top20_steps <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        WITH cte AS (
        SELECT  *,
                ROW_NUMBER() OVER (PARTITION BY person_id, CAST(datetime AS DATE) ORDER BY steps DESC) as rn
        FROM `{dataset}.steps_intraday`
        )
        SELECT person_id, CAST(datetime AS DATE) AS top20_steps_date, AVG(steps) AS top20_steps_value
        FROM cte
        WHERE rn <= 20
        GROUP BY
        CAST(datetime AS DATE),person_id
    ")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/top20_steps_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/top20_steps_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[date >= min_window_date]
    result <- result[date <= max_window_date]
    result <- result[,c("person_id","top20_steps_date","top20_steps_value")]
  }
  fwrite(result,file="top20_steps.csv")
  system(str_glue("gsutil cp top20_steps.csv {output_folder}/top20_steps.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
