#' Asthma
#' @export
#' @return output_folder/asthma.csv
#' @import data.table stringr
asthma <- function(dataset,anchor_date_table=NULL,before=NULL,after=NULL,output_folder)
{
  icd9_codes <- c("493","493.%")
  icd10_codes <- c("J45","J45.%")
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
  result_all <- setDT(result_all)[,.(asthma_status = length(condition_start_date) > 0,
                                     asthma_entry_date = min(condition_start_date)),
                                  .(person_id)]
  fwrite(result_all,file="asthma.csv")
  system(str_glue("gsutil cp asthma.csv {output_folder}/asthma.csv"),intern=TRUE)
}
