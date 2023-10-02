#' All HF hospitalizations
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_hf_hospitalizations.csv
#' @import stringr bigrquery data.table
#' @export
all_hf_hospitalizations <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
      SELECT  co.person_id,
              vo.visit_start_date AS all_hf_hospitalizations_entry_date
      FROM
          `{dataset}.condition_occurrence` co
          LEFT JOIN `{dataset}`.concept c ON (co.condition_source_concept_id = c.concept_id)
          LEFT JOIN `{dataset}.visit_occurrence` vo ON (co.visit_occurrence_id = vo.visit_occurrence_id)
      WHERE
          c.VOCABULARY_ID LIKE 'ICD%' AND
          (
             vo.visit_concept_id = 9201 AND
             (co.condition_type_concept_id = 38000200 OR co.condition_status_concept_id = 4230359)
          ) AND
          (co.condition_source_value LIKE '425' OR
          co.condition_source_value LIKE '425.%' OR
          co.condition_source_value LIKE '428' OR
          co.condition_source_value LIKE '428.%' OR
          co.condition_source_value LIKE 'I42' OR
          co.condition_source_value LIKE 'I42.%' OR
          co.condition_source_value LIKE 'I50' OR
          co.condition_source_value LIKE 'I50.%')
  ")

  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/all_hf_hospitalizations_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/all_hf_hospitalizations_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[all_hf_hospitalizations_entry_date >= min_window_date]
    result <- result[all_hf_hospitalizations_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","all_hf_hospitalizations_entry_date","all_hf_hospitalizations_end_date",
                      "all_hf_hospitalizations_icd_code","all_hf_hospitalizations_dx_type")]
  fwrite(result,file="all_hf_hospitalizations.csv")
  system(str_glue("gsutil cp all_hf_hospitalizations.csv {output_folder}/all_hf_hospitalizations.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
