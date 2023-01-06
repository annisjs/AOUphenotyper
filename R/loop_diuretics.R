#' Loop diuretics
#' @export
#' @return output_folder/loop_diuretics.csv
#' @import data.table stringr
loop_diuretics <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  require(data.table)
  require(stringr)
  meds <- c("bumetanide", "bumex", "ethacrynic acid", "ethacrynate", "edecrin", "sodium edicrin", "torsemide", "demadex", "furosemide", "lasix")
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
  result <- setDT(result)[,.(loop_diuretics_entry_date = min(drug_exposure_start_date)),.(person_id)]
  fwrite(result,file="loop_diuretics.csv")
  system(str_glue("gsutil cp loop_diuretics.csv {output_folder}/loop_diuretics.csv"),intern=TRUE)
}
