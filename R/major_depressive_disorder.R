#' Major depressive disorder
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/major_depressive_disorder.csv
#' @details Need at least 1 ICD code:
#'
#' ICD9: "296.21","296.22","296.23","296.24","296.25","296.26","296.2","296.31","296.32",
#' "296.33","296.34","296.35","296.36","296.3","300.4","293.83","311"
#'
#' ICD10: "F32.0","F32.1","F32.2","F32.3","F32.4","F32.5","F32.9","F33","F33.1","F33.2",
#' "F33.3","F33.41","F33.42","F33.9","F34.1","F06.30","F32.9"
#' @import data.table stringr
#' @export
major_depressive_disorder <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("296.21","296.22","296.23","296.24","296.25","296.26","296.2","296.31","296.32","296.33","296.34","296.35","296.36","296.3","300.4","293.83","311")
  icd10_codes <- c("F32.0","F32.1","F32.2","F32.3","F32.4","F32.5","F32.9","F33","F33.1","F33.2","F33.3","F33.41","F33.42","F33.9","F34.1","F06.30","F32.9")
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
  result_all <- setDT(result_all)[,.(major_depressive_disorder_entry_date = min(condition_start_date),
                                     major_depressive_disorder_status = length(condition_start_date) > 0),
                                  .(person_id)]
  fwrite(result_all,file="major_depressive_disorder.csv")
  system(str_glue("gsutil cp major_depressive_disorder.csv {output_folder}/major_depressive_disorder.csv"),intern=TRUE)
}
