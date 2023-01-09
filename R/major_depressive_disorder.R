#' Major depressive disorder
#' @export
#' @return output_folder/major_depressive_disorder.csv
#' @import data.table stringr
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
    result_all[,min_window_date := anchor_date - before]
    result_all[,max_window_date := anchor_date + after]
    result_all <- result_all[condition_start_date >= min_window_date]
    result_all <- result_all[condition_start_date <= max_window_date]
  }
  result_all <- setDT(result_all)[,.(major_depressive_disorder_status = length(condition_start_date) > 0,
                                     major_depressive_disorder_entry_date = min(condition_start_date)),
                                  .(person_id)]
  fwrite(result_all,file="major_depressive_disorder.csv")
  system(str_glue("gsutil cp major_depressive_disorder.csv {output_folder}/major_depressive_disorder.csv"),intern=TRUE)
}
