#' Impute covariates for time-varying dataset
#'
#' @param dat_long a data.table representing a time-varying dataset
#' @param merged_covariates a data.table with a "person_id" column and any other covariates. Must have same person_ids as dat_long.
#'
#' @return a mids object object
#' @import Hmisc mice
#' @export
#'
impute_long_data <- function(dat_long,covariates)
{
  set.seed(0)
  vars <- colnames(covariates)
  vars <- vars[-which(vars=="person_id")]
  f <- as.formula(paste0("~",paste0(vars,collapse = "+")))
  xtrans <- Hmisc::aregImpute(f,data = covariates, n.impute = 5, pr = TRUE)

  datc_list<- lapply(1:5,function(x) as.data.frame(Hmisc::impute.transcan(xtrans,
                                                                          imputation=x,
                                                                          data=covariates,
                                                                          list.out=TRUE,
                                                                          pr=FALSE,
                                                                          check=FALSE)))
  datc_list <- lapply(datc_list,function(x) data.frame(cbind(person_id = covariates$person_id, x)))

  dat_list <- list()
  #original dataset
  dat_list[[1]] <- merge(dat_long,covariates,by="person_id")

  dat_list[[1]]$.imp <- 0
  for (j in 2:(length(datc_list)+1))
  {
    #attach covariates
    dat_list[[j]] <- merge(dat_long,datc_list[[j-1]],by="person_id")
    dat_list[[j]]$.imp <- j-1
  }

  print(xtrans)

  dat_imp_long <- lapply(dat_list,function(x) data.frame(x))
  dat_imp_long <- do.call(rbind,dat_imp_long)
  dat_mids <- as.mids(dat_imp_long)
  return(dat_mids)
}
