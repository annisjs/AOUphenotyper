#' Hyperlipidemia#'
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hyperlipidemia.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "272.%"
#' ICD10: "E78.%", "E88.%"
#' @import data.table stringr
#' @export
hyperlipidemia <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("272.%")
  icd10_codes <- c("E78.%", "E88.%")
  result_icd9 <- icd9_query(dataset,icd9_codes)
  result_icd10 <- icd10_query(dataset,icd10_codes)
  result_all <- rbind(result_icd9,result_icd10)
  if (!is.null(anchor_date_table))
  {
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,min_window_date := anchor_date + before]
    result_all[,max_window_date := anchor_date + after]
    result_all <- result_all[condition_start_date >= min_window_date]
    result_all <- result_all[condition_start_date <= max_window_date]
  }
  result_all <- setDT(result_all)[,.(hyperlipidemia_entry_date = min(condition_start_date),
                                     hyperlipidemia_status = length(condition_start_date) > 0),
                                  .(person_id)]
  data.table::fwrite(result_all,file="hyperlipidemia.csv")
  system(str_glue("gsutil cp hyperlipidemia.csv {output_folder}/hyperlipidemia.csv"),intern=TRUE)
}
