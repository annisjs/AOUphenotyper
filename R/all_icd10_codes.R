#' All ICD10 codes
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_icd10_codes.csv
#' @import stringr bigrquery data.table
#' @export
all_icd10_codes <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
       SELECT DISTINCT co.person_id,
        co.condition_start_date as all_icd10_codes_entry_date,
        co.condition_source_value all_icd10_code_value
    FROM
        {dataset}.condition_occurrence co
        INNER JOIN
        {dataset}.concept c
        ON (co.condition_source_concept_id = c.concept_id)
    WHERE
        c.VOCABULARY_ID LIKE 'ICD10CM'
  ")

  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/all_icd10_codes_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/all_icd10_codes_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[all_icd10_codes_entry_date >= min_window_date]
    result <- result[all_icd10_codes_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","all_icd10_codes_entry_date","all_icd10_codes_value")]
  fwrite(result,file="all_icd10_codes.csv")
  system(str_glue("gsutil cp all_icd10_codes.csv {output_folder}/all_icd10_codes.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
