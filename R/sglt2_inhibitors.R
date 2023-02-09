#' SGLT2 Inhibitors
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date - before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sglt2_inhibitors.csv
#' @details Meds: "empagliflozin","jardiance","canagliflozin","invokana","dapagliflozin","farxiga","ertugliflozin","steglatro"
#' @import data.table stringr
#' @export
sglt2_inhibitors <- function(dataset,output_path,anchor_date_table=NULL,before=NULL,after=NULL)
{
  meds <- c("empagliflozin","jardiance","canagliflozin","invokana","dapagliflozin","farxiga","ertugliflozin","steglatro")
  result <- med_query(dataset,meds)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[drug_exposure_start_date >= min_window_date]
    result <- result[drug_exposure_start_date <= max_window_date]
    result <- result[,c("person_id","drug_exposure_start_date")]
  }
  result <- setDT(result)[,.(sglt2_inhibitors_entry_date = min(drug_exposure_start_date)),.(person_id)]
  fwrite(result,file="sglt2_inhibitors.csv")
  system(str_glue("gsutil cp sglt2_inhibitors.csv {output_folder}/sglt2_inhibitors.csv"),intern=TRUE)
}
