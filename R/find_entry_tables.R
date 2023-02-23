# find_entry_tables.R

#' Find unstructured tables (not registry or results) in a notebook entry.
#' 
#' This function returns the locations of unstructured tables in a notebook entry.
#' It does not show the user where any registration or results tables might be 
#' located in a notebook entry, as information in those tables must be retrieved
#' from other API endpoints / data warehouse tables. 
#' 
#' @param entry entry response from GET /entry/{entry_id}. *** Link the appropriate
#' Benchling API endpoint here. 
#' @param min_rows Keep tables with this number of rows or more.
#' @return List where names are days and elements are indices in the `notes` attribute that
#'    correspond to tables in the entry.
#' @examples \dontrun{
#' client <- benchlingr::benchling_api_auth(
#'     tenant="https://hemoshear-dev.benchling.com",
#'     api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))
#' entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
#' benchlingr::find_entry_tables(entry)
#' }
#' @export

find_entry_tables <- function(entry, min_rows=NULL) {
  if (missing(entry)) {
    stop("'entry' input is missing. See ?benchlingr::get_entry.")
  } 
  
  if (!all(class(entry) %in% c("benchling_api_client.v2.stable.models.entry.Entry", 
                               "python.builtin.object"))) {
    stop("'entry' input is invalid. See ?benchlingr::get_entry.")
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
  
  if (length(res) == 0) {
    res <- NA
    warning("No tables were found in notebook entry.")
  }
  
  res
}


