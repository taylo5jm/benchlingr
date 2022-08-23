# find_entry_tables.R

#' Find unstructured tables (not registry or results) in a notebook entry.
#' 
#' @param json JSON response from GET /entry/{entry_id}
#' @param min_rows Keep tables with this number of rows or more.
#' @return List where names are days and elements are indices in the `notes` attribute that
#'    correspond to tables in the entry.
#' @examples
#' conn <- warehouse_connect("hemoshear")
#' entry <- get_entry(id="xxxxxx", download=FALSE)
#' find_tables(entry)
#' DBI::dbDisconnect(conn)
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