#' CAD
#' @export
#' @return output_folder/cad.csv
#' @import data.table stringr
cad <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes = c("410","410.%","411","411.%","412","412.%","413","413.%","414","414.%","V45.82")
  icd10_codes = c("I25.1%")
  cpt_codes = c("33534","33535","33536","33510","33511","33512","33513","33514","33515","33516","33517","33518","33519","33520","33521","33522","33523","92980","92981","92982","92984","92995","92996")

  result_icd9 <- icd9_query(dataset,icd9_codes)
  result_icd10 <- icd10_query(dataset,icd10_codes)
  result_cpt <- cpt_query(dataset,cpt_codes)
  result_icd <- rbind(result_icd9,result_icd10)
  if (!is.null(anchor_date_table))
  {
    result_icd <- as.data.table(merge(result_icd,anchor_date_table,by="person_id"))
    result_icd[,min_window_date := anchor_date - before]
    result_icd[,max_window_date := anchor_date + after]
    result_icd <- result_icd[condition_start_date >= min_window_date]
    result_icd <- result_icd[condition_start_date <= max_window_date]

    result_cpt <- as.data.table(merge(result_cpt,anchor_date_table,by="person_id"))
    result_cpt[,min_window_date := anchor_date - before]
    result_cpt[,max_window_date := anchor_date + after]
    result_cpt <- result_cpt[entry_date >= min_window_date]
    result_cpt <- result_cpt[entry_date <= max_window_date]
  }

  result_icd_agg <- setDT(result_icd)[,.(icd_status = length(condition_start_date) > 1,
                                         icd_entry_date = min(condition_start_date)),.(person_id)]
  result_cpt_agg <- setDT(result_cpt)[,.(cpt_status = length(entry_date) > 0,
                                         cpt_entry_date = min(entry_date)),.(person_id)]

  result_all <- merge(result_icd_agg,result_cpt_agg,by="person_id",all.x=TRUE,all.y=TRUE)
  result_all$cpt_status[is.na(result_all$cpt_status)] <- FALSE
  result_all$icd_status[is.na(result_all$icd_status)] <- FALSE
  result_all <- result_all[,.(cad_entry_date = min(c(icd_entry_date,cpt_entry_date),na.rm=T),
                              cad_status = cpt_status | icd_status),.(person_id)]
  result_all$cad_entry_date[result_all$cad_status == FALSE] <- NA

  fwrite(result_all,file="cad.csv")
  system(str_glue("gsutil cp cad.csv {output_folder}/cad.csv"),intern=TRUE)
}
