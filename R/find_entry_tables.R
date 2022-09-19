# find_entry_tables.R

#' Find unstructured tables (not registry or results) in a notebook entry.
#' 
#' This function returns the locations of unstructured tables in a notebook entry.
#' It does not show the user where any registration or results tables might be 
#' located in a notebook entry, as information in those tables must be retrieved
#' from other API endpoints / data warehouse tables. 
#' 
#' @param json JSON response from GET /entry/{entry_id}. *** Link the appropriate
#' Benchling API endpoint here. 
#' @param min_rows Keep tables with this number of rows or more.
#' @return List where names are days and elements are indices in the `notes` attribute that
#'    correspond to tables in the entry.
#' @examples \dontrun{
#' conn <- warehouse_connect("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' entry <- get_entry(id="xxxxxx", download=FALSE)
#' find_tables(entry)
#' DBI::dbDisconnect(conn)
#' }
#' @export
find_entry_tables <- function(json, min_rows=NULL) {
  .find_tables <- function(json, min_rows=NULL) {
    tables <- which(purrr::map_lgl(json$notes, ~ 'table' %in% names(.)))
    table_lengths <- purrr::map_dbl(json$notes[tables], 
                                    ~ length(.$table$rows))
    if (!is.null(min_rows)) {
      tables <- tables[which(table_lengths >= min_rows)]
    }
    return(tables)
  }
  res <- purrr::map(json$days, ~ .find_tables(., min_rows=min_rows))
  res
}