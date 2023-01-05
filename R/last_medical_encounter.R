#' Last Medical Encounter
#' @export
#' @return output_folder/last_medical_encounter_*.csv
#' @import stringr bigrquery
last_medical_encounter <- function(output_folder)
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

        SELECT person_id, MAX(date)
        FROM ehr
        GROUP BY person_id
        ")

  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/last_medical_encounter_*.csv"),
    destination_format = "CSV")
}
