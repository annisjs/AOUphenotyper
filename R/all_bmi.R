#' All BMI
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @export
#' @return output_folder/all_bmi.csv
#' @import data.table stringr
all_bmi <-  function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{

  bmi_query <- str_glue("
        SELECT
            measurement.person_id,
            measurement.measurement_date AS all_bmi_entry_date,
            measurement.value_as_number AS all_bmi_value
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

  # BMI
  result_bmi <- download_data(bmi_query)
  if (!is.null(anchor_date_table))
  {
    result_bmi <- as.data.table(merge(result_bmi,anchor_date_table,by="person_id",allow.cartesian=TRUE))
    result_bmi[,min_window_date := anchor_date + before]
    result_bmi[,max_window_date := anchor_date + after]
    result_bmi <- result_bmi[all_bmi_date >= min_window_date]
    result_bmi <- result_bmi[all_bmi_date <= max_window_date]
    result_bmi <- result_bmi[,c("person_id","all_bmi_entry_date","all_bmi_value")]
  }
  fwrite(result_bmi,file="all_bmi.csv")
  system(str_glue("gsutil cp all_bmi.csv {output_folder}/all_bmi.csv"),intern=TRUE)

}
