#' Closest height
#' @export
#' @return output_folder/closest_height.csv
#' @import data.table stringr bigrquery
closest_height <- function(dataset,anchor_date_table=NULL,before=NULL,after=NULL,output_folder)
{
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
                                        3036277, 3023540
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
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/height_*.csv"),
    destination_format = "CSV")
  result_all <- as.data.table(read_bucket(str_glue("{output_folder}/height_*.csv")))
  if (!is.null(anchor_date_table))
  {
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,min_window_date := anchor_date - before]
    result_all[,max_window_date := anchor_date + after]
    result_all <- result_all[measurement_date >= min_window_date]
    result_all <- result_all[measurement_date <= max_window_date]
  }
  result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
  result_all <- result_all[order(diff)]
  result_all <- result_all[,.(closest_height_entry_date = measurement_date[1],
                              closest_height_value = value_as_number[1],
                              closest_height_unit = unit_concept_name[1]),.(person_id)]
  fwrite(result_all,file="closest_height.csv")
  system(str_glue("gsutil cp closest_height.csv {output_folder}/closest_height.csv"),intern=TRUE)
}
