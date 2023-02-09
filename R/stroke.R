#' Stroke
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date - before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/stroke.csv
#' @details At lest 1 ICD code:
#'
#' ICD9: "433.01","433.11","433.21","433.31","433.81","433.91","434.01","434.11","434.91"
#'
#' ICD10: "I63","I63.%"
#' @import data.table stringr
#' @export
stroke <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{

  icd9_codes <- c("433.01","433.11","433.21","433.31","433.81","433.91","434.01","434.11","434.91")
  icd10_codes <- c("I63","I63.%")
  result_icd9 <- icd9_query(dataset,icd9_codes)
  result_icd10 <- icd10_query(dataset,icd10_codes)
  result_all <- rbind(result_icd9,result_icd10)
  if (!is.null(anchor_date_table))
  {
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,min_window_date := anchor_date - before]
    result_all[,max_window_date := anchor_date + after]
    result_all <- result_all[condition_start_date >= min_window_date]
    result_all <- result_all[condition_start_date <= max_window_date]
  }
  result_all <- setDT(result_all)[,.(stroke_entry_date = min(condition_start_date),
                                     stroke_status = length(condition_start_date) > 0),
                                  .(person_id)]
  fwrite(result_all,file="stroke.csv")
  system(str_glue("gsutil cp stroke.csv {output_folder}/stroke.csv"),intern=TRUE)
}
