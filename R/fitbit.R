#' Fitbit
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/fitbit.csv
#' @details Fitbit data contains "person_id","date","steps",
#' "fairly_active_minutes","fairly_active_minutes","lightly_active_minutes",
#' "sedentary_minutes","very_active_minutes"
#' @import stringr bigrquery
#' @export
fitbit <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- paste("
        SELECT
            activity_summary.person_id,
            activity_summary.date,
            activity_summary.steps,
            activity_summary.fairly_active_minutes,
            activity_summary.lightly_active_minutes,
            activity_summary.sedentary_minutes,
            activity_summary.very_active_minutes
        FROM
            `activity_summary` activity_summary", sep="")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/fitbit_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/fitbit_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[date >= min_window_date]
    result <- result[date <= max_window_date]
    result <- result[,c("person_id","date","steps",
                        "fairly_active_minutes","fairly_active_minutes","lightly_active_minutes",
                        "sedentary_minutes","very_active_minutes")]
  }
  fwrite(result,file="fitbit.csv")
  system(str_glue("gsutil cp fitbit.csv {output_folder}/fitbit.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
