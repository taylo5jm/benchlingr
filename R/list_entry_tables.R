# list_entry_tables.R

#' Find unstructured tables (not registry, results, or look-up) in a notebook entry.
#' 
#' This function returns the locations of unstructured tables in a notebook entry.
#' It does not show the user where any registration, results, or look-up tables might be 
#' located in a notebook entry, as information in those tables must be retrieved
#' from other API endpoints / data warehouse tables. 
#' 
#' @param entry Notebook entry retrieved with the `entries$get_entry_by_id` method
#' of the Benchling Python SDK facade object created by `benchling::connect_sdk`.
#' @param min_rows Keep tables with this number of rows or more.
#' @return List where names are days and elements are indices in the `notes` attribute that
#'    correspond to tables in the entry.
#' @examples \dontrun{
#' # Initialize a Benchling Python SDK facade object.
#' client <- benchlingr::connect_sdk(
#'     tenant="https://hemoshear-dev.benchling.com",
#'     api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))
#' # Retrieve notebook entries with the `entries$get_entry_by_id` method.
#' entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
#' # Find the locations of the unstructured tables in the notebook entry.
#' benchlingr::list_entry_tables(entry)
#' }
#' @export

list_entry_tables <- function(entry, min_rows=NULL) {
  if (missing(entry)) {
    stop("Entry input is missing.")
  }
  
  if (length(class(entry)) != 2 | class(entry)[1] != "benchling_api_client.v2.stable.models.entry.Entry" |
      class(entry)[2] != "python.builtin.object") {
    stop("Entry input is invalid.")
  }
  
  .find_tables <- function(entry, min_rows=NULL) {
    tables <- which(purrr::map_lgl(entry$notes, ~ 'table' %in% names(.)))
    table_lengths <- purrr::map_dbl(entry$notes[tables], 
                                    ~ length(.$table$rows))
    if (!is.null(min_rows)) {
      tables <- tables[which(table_lengths >= min_rows)]
    }
    
    return(tables)
  }
  
  res <- purrr::map(entry$days, ~ .find_tables(., min_rows=min_rows))
  for (i in 1:length(res)) {
    names(res)[[i]] <- i
    if (!identical(res[[i]], integer(0))) {
      for (j in 1:length(res[[i]])) {
        names(res[[i]])[j] <- entry$days[[i]]$notes[[res[[i]][j]]]$table$name
      } 
    } else {
      next
    }
  }
  
  if (all(purrr::map_lgl(res, ~ length(.) == 0))) {
    res <- NA
    warning("No tables were found in entry provided.")
  }
  
  return(res)
}


