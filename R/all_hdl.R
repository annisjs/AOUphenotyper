#' All HDL
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for
#'
#' "Cholesterol in HDL \[Mass/volume] in Serum or Plasma"
#' @return output_folder/all_hdl.csv
#' @import data.table stringr
#' @export
all_hdl <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result_all <- lab_query(dataset,"Cholesterol in HDL [Mass/volume] in Serum or Plasma")
  if (!is.null(anchor_date_table))
  {
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,min_window_date := anchor_date + before]
    result_all[,max_window_date := anchor_date + after]
    result_all <- result_all[measurement_date >= min_window_date]
    result_all <- result_all[measurement_date <= max_window_date]
    result_all <- result_all[, c("person_id","measurement_date","value_as_number")]
  }
  colnames(result_all) <- c("person_id","all_hdl_entry_date","all_hdl_value")
  fwrite(result_all,file="all_hdl.csv")
  system(str_glue("gsutil cp all_hdl.csv {output_folder}/all_hdl.csv"),intern=TRUE)
}
