#' Closest fasting glucose
#' @export
#' @return output_folder/closest_fasting_glucose.csv
#' @import data.table stringr
closest_fasting_glucose <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result_all <- lab_query(dataset,c("Fasting glucose [Mass/volume] in Serum or Plasma",
                                    "Fasting glucose [Moles/volume] in Blood"))
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
  result_all <- result_all[,.(closest_fasting_glucose_entry_date = measurement_date[1],
                              closest_fasting_glucose_value = value_as_number[1]),.(person_id,anchor_date)]
  fwrite(result_all,file="closest_fasting_glucose.csv")
  system(str_glue("gsutil cp closest_fasting_glucose.csv {output_folder}/closest_fasting_glucose.csv"),intern=TRUE)
}

