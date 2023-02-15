#' Socioeconomic status variables
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. The dataset will be merged with demographics.
#' @param before serves no function
#' @param after serves no function
#' @return output_folder/ses.csv
#' @import stringr data.table bigrquery
#' @export
ses <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  ses_query <- str_glue("
    SELECT
        observation.person_id,
        observation.observation_datetime,
        zip_code.zip3_as_string as zip_code,
        zip_code.fraction_assisted_income as assisted_income,
        zip_code.fraction_high_school_edu as high_school_education,
        zip_code.median_income,
        zip_code.fraction_no_health_ins as no_health_insurance,
        zip_code.fraction_poverty as poverty,
        zip_code.fraction_vacant_housing as vacant_housing,
        zip_code.deprivation_index,
        zip_code.acs as american_community_survey_year,
        p.state_of_residence_source_value as state_of_residence
    FROM
        `zip3_ses_map` zip_code
    JOIN
        `observation` observation
            ON CAST(SUBSTR(observation.value_as_string,
        0,
        STRPOS(observation.value_as_string,
        '*') - 1) AS INT64) = zip_code.zip3
        AND observation_source_concept_id = 1585250
        AND observation.value_as_string NOT LIKE 'Res%'
    LEFT JOIN person p ON (observation.person_id = p.person_id)")
  bq_table_save(
    bq_dataset_query(dataset, ses_query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/ses_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/ses_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
  }
  result <- result[,c("person_id","observation_datetime","zip_code","assisted_income",
                      "high_school_education","median_income","no_health_insurance","poverty",
                      "vacant_housing","deprivation_index","american_community_survey_year",
                      "state_of_residence")]
  fwrite(result,file="ses.csv")
  system(str_glue("gsutil cp ses.csv {output_folder}/ses.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
