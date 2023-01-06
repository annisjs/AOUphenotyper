#' Impute covariates for time-varying fitbit dataset
#'
#' @param fitbit_formatted the data.table output from format_fitbit_cox
#' @param merged_covariates a data.table with a "person_id" column and any other covariates. Must have same person_ids as fitbit_formatted.
#'
#' @return a mids object object
#' @export
#'
impute_fitbit_covariates <- function(fitbit_formatted,merged_covariates)
{
  set.seed(0)
  merged_cox_agg_cov <- fitbit_formatted[,.(status=any(status),
                                            mean_steps=mean(mean_steps),
                                            baseline_steps = baseline_steps[1],
                                            baseline_sedentary_minutes = baseline_sedentary_minutes[1],
                                            baseline_lightly_active_minutes = baseline_lightly_active_minutes[1],
                                            baseline_fairly_active_minutes = baseline_fairly_active_minutes[1],
                                            baseline_very_active_minutes = baseline_very_active_minutes[1],
                                            mean_sedentary_minutes=mean(mean_sedentary_minutes),
                                            mean_lightly_active_minutes=mean(mean_lightly_active_minutes),
                                            mean_fairly_active_minutes=mean(mean_fairly_active_minutes),
                                            mean_very_active_minutes=mean(mean_very_active_minutes)),.(person_id)]
  merged_cox_agg_cov <- merge(merged_cox_agg_cov,merged_covariates,by="person_id",all.x=TRUE)

  vars <- colnames(merged_cox_agg_cov)
  vars <- vars[-which(vars=="person_id")]
  f <- as.formula(paste0("~",paste0(vars,collapse = "+")))
  xtrans <- Hmisc::aregImpute(f,data = merged_cox_agg_cov, n.impute = 5, pr = TRUE)

  datc_list<- lapply(1:5,function(x) as.data.frame(Hmisc::impute.transcan(xtrans,
                                                                          imputation=x,
                                                                          data=merged_cox_agg_cov,
                                                                          list.out=TRUE,
                                                                          pr=FALSE,
                                                                          check=FALSE)))
  datc_list <- lapply(datc_list,function(x) data.frame(cbind(person_id = merged_cox_agg_cov$person_id, x)))

  dat_list <- list()
  #original dataset
  dat_list[[1]] <- merge(fitbit_formatted,merged_covariates,by="person_id")

  cov_vars <- colnames(merged_covariates)
  dat_list[[1]]$.imp <- 0
  for (j in 2:(length(datc_list)+1))
  {
    #attach covariates
    dat_list[[j]] <- merge(fitbit_formatted,datc_list[[j-1]][,cov_vars],by="person_id")
    dat_list[[j]]$.imp <- j-1
  }

  print(xtrans)

  dat_imp_long <- lapply(dat_list,function(x) data.frame(x))
  dat_imp_long <- do.call(rbind,dat_imp_long)
  dat_mids <- as.mids(dat_imp_long)
  return(dat_mids)
}
