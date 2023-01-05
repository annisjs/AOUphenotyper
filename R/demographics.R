#' Demographics
#' @export
#' @return output_folder/demographics_*.csv
#' @import stringr bigrquery
demographics <- function(output_folder)
{
  dem_query <- str_glue("
    SELECT
        person.person_id,
        p_gender_concept.concept_name as gender,
        person.birth_datetime as date_of_birth,
        p_race_concept.concept_name as race,
        p_ethnicity_concept.concept_name as ethnicity,
        p_sex_at_birth_concept.concept_name as sex_at_birth
    FROM
        `person` person
    LEFT JOIN
        `concept` p_gender_concept
            ON person.gender_concept_id = p_gender_concept.concept_id
    LEFT JOIN
        `concept` p_race_concept
            ON person.race_concept_id = p_race_concept.concept_id
    LEFT JOIN
        `concept` p_ethnicity_concept
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id
    LEFT JOIN
        `concept` p_sex_at_birth_concept
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id", sep="")
  bq_table_save(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dem_query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/demographics_*.csv"),
    destination_format = "CSV")
}
