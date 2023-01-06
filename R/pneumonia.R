#' Pneumonia
#' @export
#' @return output_folder/asthma.csv
#' @import data.table stringr
pneumonia <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("480","480.%","481","481.%","482","482.%","483","483.%","484","484.%","485","485.%","486","486.%","487","487.%","488","488.%")
  icd10_codes <- c("J18.%")
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
  result_all <- setDT(result_all)[,.(pneumonia_status = length(condition_start_date) > 0,
                                     pneumonia_entry_date = min(condition_start_date)),
                                  .(person_id)]
  fwrite(result_all,file="pneumonia.csv")
  system(str_glue("gsutil cp pneumonia.csv {output_folder}/pneumonia.csv"),intern=TRUE)
}
