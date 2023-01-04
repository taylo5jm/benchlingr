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

