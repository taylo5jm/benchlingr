# get_entry_table.R

#' Read unstructured table in a notebook entry
#' 
#' @importFrom magrittr %>%
#' @param entry Notebook entry in JSON format.
#' @param day Integer for the day in the notebook entry. See `list_entry_tables`.
#' @param table_position Integer for the position of the table in the notebook entry list. 
#' See `list_entry_tables`.
#' @param return_table_name bool Determines how the name of the table in the notebook 
#' entry is included in the output.
#' If `TRUE` is selected, the table name will be added as a new column
#' to the data frame. If `FALSE`, then the table name will be ignored. 
#' @return Data frame representing the unstructured table in the notebook
#' entry.
#' @keywords internal
#' @examples \dontrun{
#' client <- benchlingr::connect_sdk(
#'     tenant="https://hemoshear-dev.benchling.com",
#'     api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))
#' entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
#' 
#' table_indices <- benchlingr:::list_entry_tables(entry)
#' print(table_indices)
#' a_table <- get_entry_table(entry, day=1, table_position=2)
#' }

get_entry_table <- function(entry, day=NULL, table_position=NULL, 
                             return_table_name=TRUE) {
  
  if (!is.numeric(day) & !is.numeric(table_position)) {
    stop("'day' and 'table_position' should be integers that represent the day and location of the unstructured table in the notebook entry. 
         Use 'list_entry_tables(entry)' to locate the unstructured tables in the notebook entry.")
  }
  
  a_table <- entry$days[[day]]$notes[[table_position]]

  direct_from_api <- FALSE
  if ((class(a_table)[1] == "benchling_api_client.v2.stable.models.table_note_part.TableNotePart")) {
    if (is.character(a_table$table$column_labels)) {
      columns <- snakecase::to_snake_case(a_table$table$column_labels)
    } else { # in this case, the column_labels slot is a list of NULLs.
      columns <- NA
    }
    direct_from_api <- TRUE
  } else {
    columns <- tryCatch({
      snakecase::to_snake_case(a_table[[1]][[1]])},
      error = function(e) {NA})
  }
  
  .extract_rows <- function(rows, from_api) {
    .extract_row <- function(row) {
      if (class(row[[1]])[1] == "benchling_api_client.v2.stable.models.entry_table_row.EntryTableRow") {
        purrr::map(row, ~ lapply(.$cells, function(x) (x['text'])) %>%
                     unlist)
      } else {
        purrr::map(row, ~ .)
      }
    }
    if (from_api) {
      .extract_row(rows)
    } else {
      purrr::map(rows, ~ .[[1]] %>% unlist)
    }
  }
  res <- a_table$table$rows %>%
    .extract_rows(., direct_from_api) %>%
    do.call('rbind', .) %>% 
    as.data.frame()
  if (!all(is.na(columns))) {
    colnames(res) <- columns
  }
  # Add the table name as a column
  if (return_table_name) {
    res$return_table_name <- a_table$table$name
  }
  res
}

