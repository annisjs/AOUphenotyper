#' Deprivation index
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/deprivation_index.csv
#' @details Deprivation index is a composite of many highly correlated measures, e.g., median household income and educational attainment
#' @import stringr bigrquery
#' @export
deprivation_index <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- paste("
        SELECT
            observation.person_id,
            observation.observation_date AS deprivation_index_entry_date,
            zip_code.deprivation_index AS deprivation_index_value
        FROM
            `zip3_ses_map` zip_code
        JOIN
            `observation` observation
                ON CAST(SUBSTR(observation.value_as_string,
            0,
            STRPOS(observation.value_as_string,
            '*') - 1) AS INT64) = zip_code.zip3
        WHERE
            observation.PERSON_ID IN (
                SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE
                    cb_search_person.person_id IN (
                        SELECT
                            person_id
                        FROM
                            `cb_search_person` p
                        WHERE
                            has_fitbit = 1
                    )
                )
                AND observation_source_concept_id = 1585250
                AND observation.value_as_string NOT LIKE 'Res%'", sep="")

  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/deprivation_index_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/deprivation_index_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[deprivation_index_entry_date >= min_window_date]
    result <- result[deprivation_index_entry_date <= max_window_date]
    result <- result[,c("person_id","deprivatio_index_entry_date","deprivation_index_value")]
  }
  fwrite(result,file="deprivation_index.csv")
  system(str_glue("gsutil cp deprivation_index.csv {output_folder}/deprivation_index.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}

