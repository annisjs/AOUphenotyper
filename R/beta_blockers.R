#' Beta blockers
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/beta_blockers.csv
#' @details Meds: "betaxolol", "kerlone", "betoptic", "acebutolol", "sectral",
#' "atenolol", "tenormin", "metoprolol", "metoprolol succinate",
#' "toprol-xl", "metoprolol tartrate", "lopressor", "metoprolol succinate er",
#' "metoprolol tartrate", "dutoprolol", "nebivolol", "bystolic", "bisoprolol", "zebeta", "esmolol",
#' "brevibloc", "propranolol", "innopran xl", "inderal", "inderal la", "nadolol",
#' "corgard", "carvedilol", "coreg", "coreg cr", "labetalol", "normodyne",
#' "trandate", "timolol", "blocadren", "timoptic", "istalol", "betimol", "carteolol",
#' "cartrol", "penbutolol", "levatol", "pindolol", "visken"
#' @import data.table stringr
#' @export
beta_blockers <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  meds <- c("betaxolol", "kerlone", "betoptic", "acebutolol", "sectral", "atenolol", "tenormin", "metoprolol", "metoprolol succinate", "toprol-xl", "metoprolol tartrate", "lopressor", "metoprolol succinate er", "metoprolol tartrate", "dutoprolol", "nebivolol", "bystolic", "bisoprolol", "zebeta", "esmolol", "brevibloc", "propranolol", "innopran xl", "inderal", "inderal la", "nadolol", "corgard", "carvedilol", "coreg", "coreg cr", "labetalol", "normodyne", "trandate", "timolol", "blocadren", "timoptic", "istalol", "betimol", "carteolol", "cartrol", "penbutolol", "levatol", "pindolol", "visken")
  result <- med_query(dataset,meds)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[drug_exposure_start_date >= min_window_date]
    result <- result[drug_exposure_start_date <= max_window_date]
    result <- result[,c("person_id","drug_exposure_start_date")]
  }
  result <- setDT(result)[,.(beta_blockers_entry_date = min(drug_exposure_start_date)),.(person_id)]
  fwrite(result,file="beta_blockers.csv")
  system(str_glue("gsutil cp beta_blockers.csv {output_folder}/beta_blockers.csv"),intern=TRUE)
}
