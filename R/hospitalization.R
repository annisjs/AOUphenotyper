#' Hospitalization
#' @export
#' @return output_folder/hospitalization_*.csv
#' @import stringr bigrquery
hospitalization <- function(output_folder)
{
  query <- str_glue("
      WITH cohort AS (
            SELECT
                DISTINCT person_id
            FROM `{dataset}.activity_summary`
      )
      SELECT cohort.person_id,
              co.condition_start_date,
              co.condition_source_value,
              co.condition_type_concept_id,
              c2.concept_name,
              vo.visit_start_date,
              vo.visit_end_date
      FROM
          cohort
          INNER JOIN
          `{dataset}.condition_occurrence` co
          ON (cohort.person_id = co.person_id)
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
    paste0(output_folder,"/hospitalization_*.csv"),
    destination_format = "CSV")
}
