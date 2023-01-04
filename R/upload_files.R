# upload_files.R
# =========================================

#' Upload a character vector
#' @param client Benchling Python SDK object
#' @param file Character vector, list, or data frame with file names
#' to be uploaded.
#' @param blob_link_cols If a data frame is provided, the names of
#' the blob link columns must be specified. 
#' @keywords internal
upload_files <- function(file, client, blob_link_cols=NULL) {
  UseMethod("upload_files")
}


#' @keywords internal
upload_files.character <- function(file, client, blob_link_cols=NULL) {
  pathlib <- reticulate::import("pathlib")
  res <- vector("list", length=length(file))
  for (f in 1:length(file)) {
    res[[f]] <- client$blobs$create_from_file(pathlib$Path(file[f]))
  }
  return(res)
}


#' @keywords internal
upload_files.list <- function(file, client, blob_link_cols=NULL) {
  pathlib <- reticulate::import("pathlib")
  res <- vector("list", length=length(file))
  
  for (f in 1:length(file)) {
    res[[f]] <- vector("list", length=length(file[[f]]))
    for (j in 1:length(file[[f]])) {
      res[[f]][[j]] <- client$blobs$create_from_file(
        pathlib$Path(file[[f]][j]))
    }
  }
  return(res)
}

#' Upload files represented by one or more columns in a data frame containing
#' file paths.
#' @param file data.frame with one or more columns containing files 
#' to be uploaded.
#' @param blob_link_cols Character vector of column names containing files
#' to be uploaded
#' @keywords internal
#' @examples \dontrun{
#' client <- benchlingr::benchling_api_auth(
#' tenant="https://hemoshear-dev.benchling.com",
#' api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))
#' df <- data.frame(file = "upload_files.R")
#' upload_files.data.frame(file=df, client=client, blob_link_cols="file")
#' }

upload_files.data.frame <- function(file, client, blob_link_cols=NULL) {
  
  for (blob_link_col in blob_link_cols) {
    blobs <- upload_files(
      file=file[[blob_link_col]],
      client=client)
    n_elements <- purrr::map_int(blobs, ~ length(.))
    if (all(n_elements < 2)) {
      blob_ids <- purrr::map(blobs, ~ .$id)
    } else {
      blob_ids <- vector("list", length=length(blobs))
      for (b in 1:length(blobs)) {
        for (j in 1:length(blobs[[b]]))
          blob_ids[[b]] <- purrr::map(blobs[[b]], ~ .$id)
      }
    }
    file[[blob_link_col]] <- blob_ids
  }
  return(file)
}
