#' Heart failure
#' @export
#' @return output_folder/heart_failure.csv
#' @import data.table stringr
heart_failure <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  # Pull inpatient and outpatient codes
  codes <- c("425","425.%","428","428.%","I42","I42.%","I50","I50.%")
  hf_inpt <- inpatient_icd_query(dataset,codes)
  hf_outpt <- outpatient_icd_query(dataset,codes)
  if (!is.null(anchor_date_table))
  {
    hf_inpt <- as.data.table(merge(hf_inpt,anchor_date_table,by="person_id"))
    hf_inpt[,min_window_date := anchor_date - before]
    hf_inpt[,max_window_date := anchor_date + after]
    hf_inpt <- hf_inpt[condition_start_date >= min_window_date]

    hf_outpt <- as.data.table(merge(hf_inpt,anchor_date_table,by="person_id"))
    hf_outpt[,min_window_date := anchor_date - before]
    hf_outpt[,max_window_date := anchor_date + after]
    hf_outpt <- hf_outpt[condition_start_date >= min_window_date]
  }
  # Sort by date
  hf_inpt_sorted <- setDT(hf_inpt)[order(condition_start_date)]
  hf_outpt_sorted <- setDT(hf_outpt)[order(condition_start_date)]
  # De-duplicate
  hf_inpt_sorted <- hf_inpt_sorted[!duplicated(hf_inpt_sorted)]
  hf_outpt_sorted <- hf_outpt_sorted[!duplicated(hf_outpt_sorted)]
  # Get counts and dates (2nd date for outpatient since we need two codes)
  hf_inpt_count <- hf_inpt_sorted[,.(hf_inpt_count = length(unique(condition_start_date)),
                                     hf_inpt_date = condition_start_date[1]),.(person_id)]
  hf_outpt_count <- hf_outpt_sorted[,.(hf_outpt_count = length(unique(condition_start_date)),
                                       hf_outpt_date = condition_start_date[2]),.(person_id)]
  # Outer join the inpatient and outpatient datasets
  hf_counts <- merge(hf_inpt_count,hf_outpt_count,by="person_id",all.x=TRUE,all.y=TRUE)
  # HF definition: If 1 inpatient or 2 outpatient
  hf_counts[,hf_inpt_status := ifelse(is.na(hf_inpt_count),FALSE,hf_inpt_count >= 1)]
  hf_counts[,hf_outpt_status := ifelse(is.na(hf_outpt_count),FALSE,hf_outpt_count >= 2)]
  hf_counts[,heart_failure_status := hf_inpt_status | hf_outpt_status]
  # Get the heart failure entry date (min date)
  hf_counts[,heart_failure_entry_date := pmin(hf_inpt_date,hf_outpt_date,na.rm = TRUE)]
  hf_counts[,heart_failure_entry_date := lubridate::as_date(
    ifelse(heart_failure_status == FALSE,NA,
           heart_failure_entry_date))]
  # Save data to bucket
  fwrite(hf_counts,"heart_failure.csv")
  system(str_glue("gsutil cp heart_failure.csv {output_folder}/heart_failure.csv"),intern=TRUE)
}
