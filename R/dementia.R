#' Dementia
#' @export
#' @return output_folder/dementia.csv
#' @import data.table stringr
dementia <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("530.11","530.81")
  icd10_codes <- c("K21","K21.%")
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
  result_all <- setDT(result_all)[,.(dementia_status = length(condition_start_date) > 0,
                                     dementia_entry_date = min(condition_start_date)),
                                  .(person_id)]
  fwrite(result_all,file="dementia.csv")
  system(str_glue("gsutil cp dementia.csv {output_folder}/dementia.csv"),intern=TRUE)
}
