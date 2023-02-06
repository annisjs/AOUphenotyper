#' Myocardial Infarction
#' @export
#' @return output_folder/mi.csv
#' @import data.table stringr
mi <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("410","410.%","411","411.%")
  icd10_codes <- c("I21","I21.%","I22","I22.%","I23","I23.%","I24","I24.%","I25","I25.%")
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
  result_all <- setDT(result_all)[,.(mi_status = length(condition_start_date) > 0,
                                     mi_entry_date = min(condition_start_date)),
                                  .(person_id)]
  fwrite(result_all,file="mi.csv")
  system(str_glue("gsutil cp mi.csv {output_folder}/mi.csv"),intern=TRUE)
}
