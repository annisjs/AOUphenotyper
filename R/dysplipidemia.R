#' Dyslipidemia
#' @param dataset a string returned by Sys.getenv("WORKSPACE_CDR"). Can also use another dataset, but this is not recommended.
#' @param output_folder the folder to write the output to. Must be preceded by the workspace bucket location given by Sys.getenv("WORKSPACE_BUCKET").
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dyslipidemia.csv
#' @details Meets at least one condition:
#'
#' If male and HDL < 40
#'
#' If female and HDL < 50
#'
#' LDL > 160
#'
#' Triglycerides > 150
#'
#' Cholesterol > 240
#' @import data.table stringr
#' @export
dyslipidemia <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{

  hdl <- lab_query(dataset,"Cholesterol in HDL [Mass/volume] in Serum or Plasma")
  colnames(hdl) <- c("person_id","hdl_date","hdl_value")
  if (!is.null(anchor_date_table))
  {
    hdl <- as.data.table(merge(hdl,anchor_date_table,by="person_id"))
    hdl[,min_window_date := anchor_date + before]
    hdl[,max_window_date := anchor_date + after]
    hdl <- hdl[hdl_date >= min_window_date]
    hdl <- hdl[hdl_date <= max_window_date]
  }

  ldl <- lab_query(dataset,c("Cholesterol in LDL [Mass/volume] in Serum or Plasma by calculation",
                             "Cholesterol in LDL [Mass/volume] in Serum or Plasma",
                             "Cholesterol in LDL [Mass/volume] in Serum or Plasma by Direct assay",
                             "Cholesterol in LDL [Mass/volume] in Serum or Plasma by Electrophoresis"))
  colnames(ldl) <- c("person_id","ldl_date","ldl_value")
  if (!is.null(anchor_date_table))
  {
    ldl <- as.data.table(merge(ldl,anchor_date_table,by="person_id"))
    ldl[,min_window_date := anchor_date + before]
    ldl[,max_window_date := anchor_date + after]
    ldl <- ldl[ldl_date >= min_window_date]
    ldl <- ldl[ldl_date <= max_window_date]
  }

  trigs <- lab_query(dataset,c("Triglyceride [Mass/volume] in Serum or Plasma",
                               "Triglyceride [Mass/volume] in Blood"))
  colnames(trigs) <- c("person_id","trigs_date","trigs_value")
  if (!is.null(anchor_date_table))
  {
    trigs <- as.data.table(merge(trigs,anchor_date_table,by="person_id"))
    trigs[,min_window_date := anchor_date + before]
    trigs[,max_window_date := anchor_date + after]
    trigs <- trigs[trigs_date >= min_window_date]
    trigs <- trigs[trigs_date <= max_window_date]
  }

  chol <- lab_query(dataset,c("Cholesterol [Mass/volume] in Serum or Plasma"))
  colnames(chol) <- c("person_id","chol_date","chol_value")
  if (!is.null(anchor_date_table))
  {
    chol <- as.data.table(merge(chol,anchor_date_table,by="person_id"))
    chol[,min_window_date := anchor_date + before]
    chol[,max_window_date := anchor_date + after]
    chol <- chol[chol_date >= min_window_date]
    chol <- chol[chol_date <= max_window_date]
  }

  sex_query <- str_glue("
  SELECT
      person.person_id,
      p_sex_at_birth_concept.concept_name as sex
  FROM
      {dataset}.`person` person
  LEFT JOIN
      {dataset}.`concept` p_sex_at_birth_concept
          ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id", sep="")
  #HDL
  dem <- download_data(sex_query)
  dem[,sex := recode_sex_fm(sex)]
  dem <- dem[!is.na(sex)]
  hdl_merged <- merge(hdl,dem,by="person_id",all.x=TRUE)
  hdl_merged[,hdl_status := ifelse(sex == "Male",
                                   hdl_value < 40,
                                   ifelse(sex == "Female",
                                          hdl_value < 50,NA))]
  hdl_merged <- hdl_merged[hdl_status == TRUE]
  hdl_merged <- hdl_merged[order(hdl_date,decreasing = FALSE)]
  hdl_agg <- hdl_merged[,.(hdl_status = hdl_status[1],
                           hdl_date = hdl_date[1]),.(person_id)]
  #LDL
  ldl[,ldl_status := ldl_value > 160]
  ldl <- ldl[ldl_status == TRUE]
  ldl <- ldl[order(ldl_date,decreasing = FALSE)]
  ldl_agg <- ldl[,.(ldl_status = ldl_status[1],
                    ldl_date = ldl_date[1]),.(person_id)]
  #Trigs
  trigs[,trigs_status := trigs_value > 150]
  trigs <- trigs[trigs_status == TRUE]
  trigs <- trigs[order(trigs_date,decreasing = FALSE)]
  trigs_agg <- trigs[,.(trigs_status = trigs_status[1],
                        trigs_date = trigs_date[1]),.(person_id)]
  #Chol
  chol[,chol_status := chol_value > 240]
  chol <- chol[chol_status == TRUE]
  chol <- chol[order(chol_date,decreasing = FALSE)]
  chol_agg <- chol[,.(chol_status = chol_status[1],
                      chol_date = chol_date[1]),.(person_id)]
  #Merge
  result_all <- merge(hdl_agg,ldl_agg,by="person_id",all.x=T)
  result_all <- merge(result_all,trigs_agg,by="person_id",all.x=T)
  result_all <- merge(result_all,chol_agg,by="person_id",all.x=T)
  result_all[,dyslipidemia_status := hdl_status | ldl_status | trigs_status | chol_status]
  result_all[,dyslipidemia_status := ifelse(is.na(dyslipidemia_status),FALSE,dyslipidemia_status)]
  result_all[,dyslipidemia_entry_date := pmin(hdl_date,ldl_date,trigs_date,chol_date,na.rm=T)]
  result_all <- result_all[order(dyslipidemia_entry_date,decreasing=T)]
  result_all <- result_all[,.(dyslipidemia_status = dyslipidemia_status[1],
                              dyslipidemia_entry_date = dyslipidemia_entry_date[1]),
                           .(person_id)]
  data.table::fwrite(result_all,file="dyslipidemia.csv")
  system(str_glue("gsutil cp dyslipidemia.csv {output_folder}/dyslipidemia.csv"),intern=TRUE)
}

