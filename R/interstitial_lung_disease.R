#' Interstitial lung disease
#' @export
#' @return output_folder/interstitial_lung_disease.csv
#' @import data.table stringr
interstitial_lung_disease <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{

  icd9_codes <- c("515.%","516.%","135","501","508.1","518.89")
  icd10_codes <- c("J84.%","D86.0","J61","J70.1","J98.4")
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
  result_all <- setDT(result_all)[,.(interstitial_lung_disease_status = length(condition_start_date) > 0,
                                     interstitial_lung_disease_entry_date = min(condition_start_date)),
                                  .(person_id)]
  fwrite(result_all,file="interstitial_lung_disease.csv")
  system(str_glue("gsutil cp interstitial_lung_disease.csv {output_folder}/interstitial_lung_disease.csv"),intern=TRUE)
}
