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
#' entry <- client$entries$get_entry_by_id("etr_IWLMFYhR")
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
    names(tables) <- purrr::map_chr(tables, ~ entry$notes[[.]]$table$name)
    return(tables)
  }
  
  res <- purrr::map(entry$days, ~ .find_tables(., min_rows=min_rows))
  if (all(purrr::map_lgl(res, ~ length(.) == 0))) {
    res <- NA
    warning("No tables were found in the notebook entry provided.")
  }
  
  return(res)

}


