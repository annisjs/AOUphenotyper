#' Closest weight
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/closest_weight.csv
#' @import data.table stringr bigrquery
#' @export
closest_weight <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  if (is.null(anchor_date_table))
  {
    stop("closest_trigs is not a primary variable and requires an anchor date table.")
  }
  query <- str_glue("
        SELECT
            measurement.person_id,
            measurement.measurement_date,
            measurement.value_as_number,
            m_unit.concept_name as unit_concept_name
        FROM
            ( SELECT
                *
            FROM
                `{dataset}.measurement` measurement
            WHERE
                (
                    measurement_concept_id IN  (
                        SELECT
                            DISTINCT c.concept_id
                        FROM
                            `{dataset}.cb_criteria` c
                        JOIN
                            (
                                select
                                    cast(cr.id as string) as id
                                FROM
                                    `{dataset}.cb_criteria` cr
                                WHERE
                                    concept_id IN (
                                        3025315, 3013762
                                    )
                                    AND full_text LIKE '%_rank1]%'
                            ) a
                                ON (
                                    c.path LIKE CONCAT('%.',
                                a.id,
                                '.%')
                                OR c.path LIKE CONCAT('%.',
                                a.id)
                                OR c.path LIKE CONCAT(a.id,
                                '.%')
                                OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1
                            )
                    )
                ) measurement
                LEFT JOIN
                `{dataset}.concept` m_unit
                    ON measurement.unit_concept_id = m_unit.concept_id")

  bq_table_save(
    bq_dataset_query(dataset, query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/weight_*.csv"),
    destination_format = "CSV")
  result_all <- as.data.table(read_bucket(str_glue("{output_folder}/aou_phenotyper/weight_*.csv")))
  if (!is.null(anchor_date_table))
  {
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,min_window_date := anchor_date + before]
    result_all[,max_window_date := anchor_date + after]
    result_all <- result_all[measurement_date >= min_window_date]
    result_all <- result_all[measurement_date <= max_window_date]
  }
  result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
  result_all <- result_all[order(diff)]
  result_all <- result_all[,.(closest_weight_entry_date = measurement_date[1],
                              closest_weight_value = value_as_number[1],
                              closest_weight_unit = unit_concept_name[1]),.(person_id)]
  data.table::fwrite(result_all,file="closest_weight.csv")
  system(str_glue("gsutil cp closest_weight.csv {output_folder}/closest_weight.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
