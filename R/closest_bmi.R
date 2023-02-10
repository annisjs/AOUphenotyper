#' Closest BMI
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @export
#' @return output_folder/closest_bmi.csv
#' @import data.table stringr
closest_bmi <-  function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{

  if (is.null(anchor_date_table))
  {
    stop("closest_bmi is not a primary variable and requires an anchor date table.")
  }

  query <- str_glue("
        SELECT
            measurement.person_id,
            measurement.measurement_date,
            measurement.value_as_number
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

  result_all <- download_data(query)
  result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
  result_all[,min_window_date := anchor_date + before]
  result_all[,max_window_date := anchor_date + after]
  result_all <- result_all[measurement_date >= min_window_date]
  result_all <- result_all[measurement_date <= max_window_date]
  result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
  result_all <- result_all[order(diff)]
  result_all <- result_all[,.(closest_bmi_entry_date = measurement_date[1],
                              closest_bmi_value = value_as_number[1]),.(person_id,anchor_date)]
  fwrite(result_all,file="closest_bmi.csv")
  system(str_glue("gsutil cp closest_bmi.csv {output_folder}/closest_bmi.csv"),intern=TRUE)
}
