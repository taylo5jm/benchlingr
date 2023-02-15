read_entry_table <- function(entry, day, table_position, table_name,
                             return_table_name=TRUE) {
  if (!is.numeric(day) & !is.numeric(table_position)) {
    stop("'day' and 'table_position' should be integers that represent the day and location of the unstructured table in the notebook entry. 
         Use 'find_entry_tables(entry)' to locate the unstructured tables in the notebook entry.")
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