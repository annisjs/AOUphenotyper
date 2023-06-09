#' Demographics
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. The dataset will be merged with demographics.
#' @param before serves no function
#' @param after serves no function
#' @return output_folder/demographics_*.csv
#' @import stringr data.table bigrquery
#' @export
demographics <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  dem_query <- str_glue("
    SELECT
        person.person_id,
        person.birth_datetime as date_of_birth,
        p_race_concept.concept_name as race,
        p_ethnicity_concept.concept_name as ethnicity,
        p_sex_at_birth_concept.concept_name as sex
    FROM
        `person` person
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
    bq_dataset_query(dataset, dem_query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/demographics_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/demographics_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
  }
  result <- result[,c("person_id","date_of_birth","race","ethnicity","sex")]
  fwrite(result,file="demographics.csv")
  system(str_glue("gsutil cp demographics.csv {output_folder}/demographics.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
