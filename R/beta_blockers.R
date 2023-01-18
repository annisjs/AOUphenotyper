#' Beta blockers
#' @export
#' @return output_folder/beta_blockers.csv
#' @import data.table stringr
beta_blockers <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  meds <- c("betaxolol", "kerlone", "betoptic", "acebutolol", "sectral", "atenolol", "tenormin", "metoprolol", "metoprolol succinate", "toprol-xl", "metoprolol tartrate", "lopressor", "metoprolol succinate er", "metoprolol tartrate", "dutoprolol", "nebivolol", "bystolic", "bisoprolol", "zebeta", "esmolol", "brevibloc", "propranolol", "innopran xl", "inderal", "inderal la", "nadolol", "corgard", "carvedilol", "coreg", "coreg cr", "labetalol", "normodyne", "trandate", "timolol", "blocadren", "timoptic", "istalol", "betimol", "carteolol", "cartrol", "penbutolol", "levatol", "pindolol", "visken")
  result <- med_query(dataset,meds)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date - before]
    result[,max_window_date := anchor_date + after]
    result <- result[drug_exposure_start_date >= min_window_date]
    result <- result[drug_exposure_start_date <= max_window_date]
    result <- result[,c("person_id","drug_exposure_start_date")]
  }
  result <- setDT(result)[,.(beta_blockers_entry_date = min(drug_exposure_start_date)),.(person_id)]
  fwrite(result,file="beta_blockers.csv")
  system(str_glue("gsutil cp beta_blockers.csv {output_folder}/beta_blockers.csv"),intern=TRUE)
}
