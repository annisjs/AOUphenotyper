#' Sleep Apnea
#' @export
#' @return output_folder/sleep_apnea.csv
#' @import data.table stringr
sleep_apnea <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{

  icd9_codes <- c("780.51","780.53","780.57","327.2%")
  icd10_codes <- c("G47.3%")
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
  result_all <- setDT(result_all)[,.(sleep_apnea_status = length(condition_start_date) > 0,
                                     sleep_apnea_entry_date = min(condition_start_date)),
                                  .(person_id)]
  fwrite(result_all,file="sleep_apnea.csv")
  system(str_glue("gsutil cp sleep_apnea.csv {output_folder}/sleep_apnea.csv"),intern=TRUE)
}
