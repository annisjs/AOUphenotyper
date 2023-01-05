#' SGLT2 Inhibitors
#' @export
#' @return output_folder/sglt2_inhibitors.csv
#' @import data.table stringr
sglt2_inhibitors <- function(dataset,anchor_date_table=NULL,before=NULL,after=NULL,output_path)
{
  require(data.table)
  require(stringr)
  meds <- c("empagliflozin","jardiance","canagliflozin","invokana","dapagliflozin","farxiga","ertugliflozin","steglatro")
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
  result <- setDT(result)[,.(sglt2_inhibitors_entry_date = min(drug_exposure_start_date)),.(person_id)]
  fwrite(result,file="sglt2_inhibitors.csv")
  system(str_glue("gsutil cp sglt2_inhibitors.csv {output_folder}/sglt2_inhibitors.csv"),intern=TRUE)
}
