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

  # BMI
  bmi_query <- str_glue("
        SELECT
            measurement.person_id,
            measurement.measurement_date,
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


  # Height
  height_query <- str_glue("
        SELECT
            measurement.person_id,
            measurement.measurement_date,
            measurement.value_as_number AS height
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
                ) measurement")

  # Weight
  weight_query <- str_glue("
        SELECT
            measurement.person_id,
            measurement.measurement_date,
            measurement.value_as_number AS weight
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
                ) measurement")


  # BMI
  result_bmi <- download_data(bmi_query)
  result_bmi <- as.data.table(merge(result_bmi,anchor_date_table,by="person_id",allow.cartesian=TRUE))
  result_bmi[,min_window_date := anchor_date + before]
  result_bmi[,max_window_date := anchor_date + after]
  result_bmi <- result_bmi[measurement_date >= min_window_date]
  result_bmi <- result_bmi[measurement_date <= max_window_date]
  result_bmi <- result_bmi[,c("person_id","measurement_date","bmi")]

  # Height
  bq_table_save(
    bq_dataset_query(dataset, height_query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/height_*.csv"),
    destination_format = "CSV")
  result_height <- as.data.table(read_bucket(str_glue("{output_folder}/aou_phenotyper/height_*.csv")))
  result_height <- as.data.table(merge(result_height,anchor_date_table,by="person_id",allow.cartesian=TRUE))
  result_height[,min_window_date := anchor_date + before]
  result_height[,max_window_date := anchor_date + after]
  result_height <- result_height[measurement_date >= min_window_date]
  result_height <- result_height[measurement_date <= max_window_date]
  result_height <- result_height[!duplicated(result_height)]

  # Weight
  bq_table_save(
    bq_dataset_query(dataset, weight_query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/weight_*.csv"),
    destination_format = "CSV")
  result_weight <- as.data.table(read_bucket(str_glue("{output_folder}/aou_phenotyper/weight_*.csv")))
  result_weight <- as.data.table(merge(result_weight,anchor_date_table,by="person_id",allow.cartesian=TRUE))
  result_weight[,min_window_date := anchor_date + before]
  result_weight[,max_window_date := anchor_date + after]
  result_weight <- result_weight[measurement_date >= min_window_date]
  result_weight <- result_weight[measurement_date <= max_window_date]
  result_weight <- result_weight[!duplicated(result_weight)]

  # Compute BMI from height and weight
  result_hw <- merge(result_height,result_weight,by=c("person_id","measurement_date"),allow.cartesian=TRUE)
  result_hw[, bmi := weight / (height/100)^2]
  result_hw <- result_hw[,c("person_id","measurement_date","bmi")]

  result_all <- rbind(result_bmi,result_hw)
  result_all <- merge(result_all,anchor_date_table,by="person_id",allow.cartesian=TRUE)

  result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
  result_all <- result_all[order(diff)]
  result_all <- result_all[,.(closest_bmi_entry_date = measurement_date[1],
                              closest_bmi_value = bmi[1]),.(person_id,anchor_date)]
  result_all[,anchor_date := NULL]

  fwrite(result_all,file="closest_bmi.csv")
  system(str_glue("gsutil cp closest_bmi.csv {output_folder}/closest_bmi.csv"),intern=TRUE)

}
