#' Last Medical Encounter
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date - before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/last_medical_encounter_*.csv
#' @import stringr bigrquery data.table
#' @export
last_medical_encounter <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        WITH ehr AS (
        SELECT person_id, MAX(m.measurement_date) AS date
        FROM `measurement` AS m
        LEFT JOIN `measurement_ext` AS mm on m.measurement_id = mm.measurement_id
        WHERE LOWER(mm.src_id) LIKE 'ehr site%'
        GROUP BY person_id

        UNION DISTINCT

        SELECT person_id, MAX(m.condition_start_date) AS date
        FROM `condition_occurrence` AS m
        LEFT JOIN `condition_occurrence_ext` AS mm on m.condition_occurrence_id = mm.condition_occurrence_id
        WHERE LOWER(mm.src_id) LIKE 'ehr site%'
        GROUP BY person_id

        UNION DISTINCT

        SELECT person_id, MAX(m.procedure_date) AS date
        FROM `procedure_occurrence` AS m
        LEFT JOIN `procedure_occurrence_ext` AS mm on m.procedure_occurrence_id = mm.procedure_occurrence_id
        WHERE LOWER(mm.src_id) LIKE 'ehr site%'
        GROUP BY person_id

        UNION DISTINCT

        SELECT person_id, max(m.visit_end_date) AS date
        FROM `visit_occurrence` AS m
        LEFT JOIN `visit_occurrence_ext` AS mm on m.visit_occurrence_id = mm.visit_occurrence_id
        WHERE LOWER(mm.src_id) LIKE 'ehr site%'
        GROUP BY person_id
        )

        SELECT person_id, MAX(date) as last_medical_encounter_entry_date
        FROM ehr
        GROUP BY person_id
        ")

  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/last_medical_encounter_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/last_medical_encounter_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[last_medical_encounter_entry_date >= min_window_date]
    result <- result[last_medical_encounter_entry_date <= max_window_date]
  }
  result <- result[,c("person_id","last_medical_encounter_entry_date")]
  fwrite(result,file="last_medical_encounter.csv")
  system(str_glue("gsutil cp last_medical_encounter.csv {output_folder}/last_medical_encounter.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
