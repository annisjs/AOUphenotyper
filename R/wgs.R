#' Whole Genome Sequencing Flag
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/wgs.csv
#' @details wgs.csv will contain:
#' 	person_id
#'  wgs_status
#' @import stringr bigrquery
#' @export
wgs <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- paste("
    SELECT
        person.person_id
    FROM
        `person` person
    WHERE
        person.PERSON_ID IN (
            SELECT
                distinct person_id
            FROM
                `cb_search_person` cb_search_person
            WHERE cb_search_person.person_id IN (
                    SELECT
                        person_id
                    FROM
                        `cb_search_person` p
                    WHERE
                        has_whole_genome_variant = 1
                )
            )", sep="")
  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/wgs_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/wgs_*.csv"))
  result[,wgs_status := TRUE]
  fwrite(result,file="wgs.csv")
  system(str_glue("gsutil cp wgs.csv {output_folder}/wgs.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
