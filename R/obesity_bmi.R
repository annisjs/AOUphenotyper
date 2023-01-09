#' Obesity using BMI
#' @export
#' @return output_folder/obesity_bmi.csv
#' @import data.table stringr
obesity_bmi <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        SELECT
            measurement.person_id,
            measurement.measurement_date AS bmi_date,
            measurement.value_as_number AS bmi
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
                                        3038553
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
                ) measurement")

  result_all <- as.data.table(download_data(query))
  if (!is.null(anchor_date_table))
  {
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,min_window_date := anchor_date - before]
    result_all[,max_window_date := anchor_date + after]
    result_all <- result_all[bmi_date >= min_window_date]
    result_all <- result_all[bmi_date <= max_window_date]
  }
  result_all <- result_all[order(bmi_date)]
  result_all <- result_all[bmi >= 30]
  result_all <- result_all[,.(obesity_bmi_entry_date = bmi_date[1],
                              obeisty_bmi_value = bmi[1]),
                           .(person_id)]
  fwrite(result_all,file="obesity_bmi.csv")
  system(str_glue("gsutil cp obesity_bmi.csv {output_folder}/obesity_bmi.csv"),intern=TRUE)
}
