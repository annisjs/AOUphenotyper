#' Dementia
#' @export
#' @return output_folder/dementia.csv
#' @import data.table stringr
dementia <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("331.0","331.82","290.4","290.40","290.41","290.42","290.43")
  icd10_codes <- c("G30","F00","G30.0","G30.1","G30.8","G30.9","F00.0","F00.1","F00.2","F00.9","G31.8","F02.8","F01","F01.0","F01.1","F01.2","F01.3","F01.8","F01.9")
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
  result_all <- setDT(result_all)[,.(dementia_entry_date = min(condition_start_date),
                                     dementia_status = length(condition_start_date) > 0),
                                  .(person_id)]
  fwrite(result_all,file="dementia.csv")
  system(str_glue("gsutil cp dementia.csv {output_folder}/dementia.csv"),intern=TRUE)
}
