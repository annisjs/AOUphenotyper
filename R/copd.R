#' COPD
#' @export
#' @return output_folder/copd.csv
#' @import data.table stringr
copd <- function(dataset,anchor_date_table=NULL,before=NULL,after=NULL,output_folder)
{
  icd9_codes <- c("491","491.0","491.1","491.2","491.20","491.21","491.22","491.8","491.9","492","492.0","492.8","496","496.0","493.21","493.22")
  icd10_codes <- c("J44.%","J43.%","J42","J41.%")
  meds <- c("roflumilast","daliresp","tiotropium","spiriva","ipratropium","atrovent","theophylline","slo-bid","slo-phyllin","theo-dur","theo 24","theo24","theo-24","uniphyl","salmeterol","serevent","formoterol","foradil","albuterol + ipratropium","albuterol / ipratropium","ipratropium bromide / albuterol sulfate","ipratropium bromide with albuterol sulfate","combivent","duoneb","albuterol","proventil","proair","ventolin","fluticasone","salmeterol","advair","budesonide/formoterol","budesonide / formoterol","budesonide - formoterol","budesonide-formoterol","mometasone/formoterol","mometasone / formoterol","dulera","beclomethaosne","qvar","budesonide","pulmicort","fluticasone","flovent","mometasone","asmanex")
  result_icd9 <- icd9_query(dataset,icd9_codes)
  result_icd10 <- icd10_query(dataset,icd10_codes)
  result_med <- med_query(dataset,meds)
  result_icd <- rbind(result_icd9,result_icd10)
  if (!is.null(anchor_date_table))
  {
    result_icd <- as.data.table(merge(result_icd,anchor_date_table,by="person_id"))
    result_icd[,min_window_date := anchor_date - before]
    result_icd[,max_window_date := anchor_date + after]
    result_icd <- result_icd[condition_start_date >= min_window_date]
    result_icd <- result_icd[condition_start_date <= max_window_date]

    result_med <- as.data.table(merge(result_med,anchor_date_table,by="person_id"))
    result_med[,min_window_date := anchor_date - before]
    result_med[,max_window_date := anchor_date + after]
    result_med <- result_med[drug_exposure_start_date >= min_window_date]
    result_med <- result_med[drug_exposure_start_date <= max_window_date]
  }
  result_icd_agg <- setDT(result_icd)[,.(icd_status = length(condition_start_date) > 1,
                                         icd_entry_date = min(condition_start_date)),
                                      .(person_id)]
  result_med_agg <- setDT(result_med)[,.(med_status = length(drug_exposure_start_date) > 0,
                                         med_entry_date = min(drug_exposure_start_date)),
                                      .(person_id)]

  result_all <- merge(result_icd_agg,result_med_agg,by="person_id",all.x=TRUE)
  result_all <- result_all[,.(copd_entry_date = min(c(icd_entry_date,med_entry_date)),
                              copd_status = med_status & icd_status),
                           .(person_id)]
  fwrite(result_all,file="copd.csv")
  system(str_glue("gsutil cp copd.csv {output_folder}/copd.csv"),intern=TRUE)
}
