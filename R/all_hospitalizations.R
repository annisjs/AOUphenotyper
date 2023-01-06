#' All hospitalizations
#' @export
#' @return output_folder/all_hospitalizations.csv
#' @import stringr bigrquery
all_hospitalizations <-  function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
      SELECT  co.person_id,
              vo.visit_start_date AS all_hospitalizations_entry_date,
              vo.visit_end_date AS all_hospitalizations_end_date,
              co.condition_source_value AS all_hospitalizations_icd_code,
              c2.concept_name AS all_hospitalizations_dx_type
      FROM
          `{dataset}.condition_occurrence` co
          LEFT JOIN
          `{dataset}.concept` c
          ON (co.condition_source_concept_id = c.concept_id)
          LEFT JOIN
          `{dataset}.concept` c2
          ON (co.condition_type_concept_id = c2.concept_id)
          LEFT JOIN
          `{dataset}.visit_occurrence` vo
          ON (co.visit_occurrence_id = vo.visit_occurrence_id)
      WHERE
          c.VOCABULARY_ID LIKE 'ICD%' AND
          (vo.visit_concept_id = 9201 OR vo.visit_concept_id = 9203) OR
          co.condition_type_concept_id = 38000200
  ")

  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/all_hospitalizations_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/all_hospitalizations_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[all_hospitalizations_entry_date >= min_window_date]
    result <- result[all_hospitalizations_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","all_hospitalizations_entry_date","all_hospitalizations_end_date",
                      "all_hospitalizations_icd_code","all_hospitalizations_dx_type")]
  fwrite(result,file="all_hospitalizations.csv")
  system(str_glue("gsutil cp all_hospitalizations.csv {output_folder}/all_hospitalizations.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
