#' Read bucket
#'
#' @param export_path
#'
#' @return a data.table
#' @export
read_bucket <- function(export_path) {
  col_types <- NULL
  as.data.table(dplyr::bind_rows(
    purrr::map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          chunk <- readr::read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- readr::spec(chunk)
          }
          chunk
        })))
}

download_data <- function(query) {
  tb <- bigrquery::bq_project_query(Sys.getenv('GOOGLE_PROJECT'), query)
  bigrquery::bq_table_download(tb,page_size=100000)
}

#' gsutil ls
#' @export
ls_bucket <- function(folder=NULL) {
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  if (is.null(folder))
  {
    system(str_glue("gsutil ls {bucket}"),intern=TRUE)
  } else {
    system(str_glue("gsutil ls {bucket}/{folder}"),intern=TRUE)
  }
}

#' gsutil rm
#' @export
rm_bucket <- function(path)
{
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  system(str_glue("gsutil rm {bucket}/{path}"),intern=TRUE)
}

#' gsutil cp to
#' @export
cp_to_bucket <- function(from,to)
{
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  system(str_glue("gsutil cp {from} {bucket}/{to}"),intern=TRUE)
}

#' gsutil cp from
#' @export
cp_from_bucket <- function(from,to)
{
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  system(str_glue("gsutil cp {bucket}/{from} {to}"),intern=TRUE)
}
