#' Mineralocorticoid Antagonists
#' @export
#' @return output_folder/mineralocorticoid_antagonists.csv
#' @import data.table stringr
mineralocorticoid_antagonists <- function(dataset,output_path,anchor_date_table=NULL,before=NULL,after=NULL)
{
  require(data.table)
  require(stringr)
  meds <- c("spironolactone","aldactone","eplerenone","inspra")
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
  result <- setDT(result)[,.(mineralocorticoid_antagonists_entry_date = min(drug_exposure_start_date)),
                          .(person_id)]
  fwrite(result,file="mineralocorticoid_antagonists.csv")
  system(str_glue("gsutil cp mineralocorticoid_antagonists.csv {output_folder}/mineralocorticoid_antagonists.csv"),intern=TRUE)
}
