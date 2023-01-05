#' ACEI/ARB Meds
#'
#' @return CSV saved to output_folder/acei_arb_meds.csv
#' @export
#'
#' @import data.table stringr
acei_arb_meds <- function(dataset,anchor_date_table=NULL,before=NULL,after=NULL,output_path)
{
  #acei
  meds <- c("fosinopril", "fosinopril sodium","monopril", "ramipril", "altace", "captopril", "capoten", "moexipril", "univasc", "lisinopril", "zestril", "prinivil", "enalapril", "vasotec", "epaned", "quinapril", "accupril", "trandolapril", "mavik", "gopten", "odrik", "benazepril", "lotensin", "perindopril", "aceon")
  #arb
  meds <- c(meds,"eprosartan", "teveten", "azilsartan", "medoxomil", "edarbi", "olmesartan", "benicar", "valsartan", "diovan", "telmisartan", "micardis", "losartan", "cozaar", "candesartan", "atacand", "irbesartan", "avapro")
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
  result <- setDT(result)[,.(acei_arb_meds_entry_date = min(drug_exposure_start_date)),.(person_id)]
  fwrite(result,file="acei_arb_meds.csv")
  system(str_glue("gsutil cp acei_arb_meds.csv {output_folder}/acei_arb_meds.csv"),intern=TRUE)
}
