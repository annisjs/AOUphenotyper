#' Death
#' @export
#' @return output_folder/death.csv
#' @import data.table stringr
death <- function(dataset,output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  query <- str_glue("
        SELECT
            person_id,
            death_entry_date
        FROM
            {dataset}.death")
  result <- download_data(query)
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[death_date >= min_window_date]
    result <- result[death_date <= max_window_date]
  }
  result <- result[,c("person_id","death_date")]
  fwrite(result,file="death.csv")
  system(str_glue("gsutil cp death.csv {output_folder}/death.csv"),intern=TRUE)
}

