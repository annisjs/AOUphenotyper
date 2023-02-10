#' Loop diuretics
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/loop_diuretics.csv
#' @details Meds: "bumetanide", "bumex", "ethacrynic acid", "ethacrynate", "edecrin", "sodium edicrin",
#'  "torsemide", "demadex", "furosemide", "lasix"
#' @import data.table stringr
#' @export
loop_diuretics <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  meds <- c("bumetanide", "bumex", "ethacrynic acid", "ethacrynate", "edecrin", "sodium edicrin", "torsemide", "demadex", "furosemide", "lasix")
  result <- med_query(dataset,meds)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[drug_exposure_start_date >= min_window_date]
    result <- result[drug_exposure_start_date <= max_window_date]
    result <- result[,c("person_id","drug_exposure_start_date")]
  }
  result <- setDT(result)[,.(loop_diuretics_entry_date = min(drug_exposure_start_date)),.(person_id)]
  fwrite(result,file="loop_diuretics.csv")
  system(str_glue("gsutil cp loop_diuretics.csv {output_folder}/loop_diuretics.csv"),intern=TRUE)
}
