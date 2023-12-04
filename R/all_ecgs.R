#' All ECGs
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for ECG measurements
#' @return output_folder/all_ecgs.csv
#' @import data.table stringr
#' @export
all_ecgs <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  ecg_vars <- c("Q-T interval","P-R Interval","T wave axis","R wave axis","P wave axis","QRS duration",
                "Q-T interval corrected","QRS complex Ventricles by EKG","P wave Atrium by EKG","Heart rate.beat-to-beat by EKG",
                "Q-T interval corrected based on Bazett formula","QRS duration {Electrocardiograph lead}","QRS axis",
                "R-R interval by EKG")
  ecg_terms <- paste('c.concept_name LIKE ',"'",ecg_vars,"'",collapse=' OR ',sep="")
  query <- str_glue("
    SELECT person_id, measurement_date, value_as_number, c.concept_name
    FROM `{dataset}.measurement` m
    INNER JOIN `{dataset}.concept` c ON (m.measurement_concept_id = c.concept_id)
    WHERE
    ({ecg_terms})
  ")
  result_all <- download_data(query,page_size=100000)
  result_all <- result_all[!duplicated(result_all[,c("person_id","measurement_date","concept_name")])]
  result_all[, concept_name := gsub("[{]|[}]|_by_ekg","",gsub(" |-|[.]","_",tolower(concept_name)))]
  result_all <- result_all[!duplicated(result_all[,c("person_id","measurement_date","concept_name")])]
  result_all <- dcast(result_all, person_id + measurement_date ~ concept_name, value.var = "value_as_number")
  if (!is.null(anchor_date_table))
  {
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,min_window_date := anchor_date + before]
    result_all[,max_window_date := anchor_date + after]
    result_all <- result_all[measurement_date >= min_window_date]
    result_all <- result_all[measurement_date <= max_window_date]
    result_all[, min_window_date := NULL]
    result_all[, max_window_date := NULL]
    result_all[, before := NULL]
    result_all[, after := NULL]
    result_all[, anchor_date := NULL]
  }
  coln <- colnames(result_all)
  coln <- coln[which(coln != "person_id")]
  coln <- paste0("all_ecgs_",coln)
  colnames(result_all) <- c("person_id",coln)
  fwrite(result_all,file="all_ecgs.csv")
  system(str_glue("gsutil cp all_ecgs.csv {output_folder}/all_ecgs.csv"),intern=TRUE)
}

