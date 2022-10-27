# read_entry_tables.R

#' Read unstructured table in a notebook entry
#' 
#' @importFrom magrittr %>%
#' @param entry Notebook entry in JSON format.
#' @param day Integer for the day in the notebook entry. See `find_entry_tables`.
#' @param table_index Integer for the position of the table in the notebook entry list. 
#' See `find_entry_tables`.
#' @param table_name bool Determines how
#' the name of the table in the notebook entry is included in the output.
#' If `TRUE` is selected, the table name will be added as a new column
#' to the data frame. If `FALSE`, then the table name will be ignored. 
#' @return Data frame representing the unstructured table in the notebook
#' entry.
#' @export
#' @examples \dontrun{
#' client <- benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
#'                              api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
#' entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
#' table_indices <- benchlingr:::find_entry_tables(entry)
#' print(table_indices)
#' a_table <- read_entry_table(entry, day=1, table_index=2)
#' }

read_entry_table <- function(entry, day, table_index,
                             table_name=TRUE) {
  a_table <- entry$days[[day]]$notes[[table_index]]

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
  if (table_name) {
    res$table_name <- a_table$table$name
  }
  res
}


#' Read all unstructured tables in a notebook entry. 
#' 
#' This function will read all unstructured tables in a notebook entry,
#' returning a list of data frames. 
#' 
#' @include find_entry_tables.R
#' @param entry Notebook entry in JSON format. See `get_entry`.
#' @param table_name If table_name is TRUE, then the names of the tables
#' in the notebook entry will be returned as names in the output list. 
#' @param verbose If verbose, then the function will alert the user
#' if no tables can be found for some days in the notebook. 
#' @return List of data frames representing the unstructured tables in
#' the notebook entry.
#' @examples \dontrun{
#' client <- benchling_api_auth(tenant="https://hemoshear.benchling.com")
#' client$entries$get_entry_by_id
#' entry <- get_entry(id = "etr")
#' tables <- read_tables(entry)
#' }
#' @export

read_entry_tables <- function(entry, day=NULL, table_position=NULL,
                              table_name=TRUE, verbose=FALSE) {
  if (is.null(day) & is.null(table_position)) {
    table_indices <- find_entry_tables(entry)
    res <- list(); k <- 1;
    for (i in 1:length(table_indices)) {
      if (length(table_indices[[i]]) > 0) {
        for (j in 1:length(table_indices[[i]])) {
          res[[k]] <- read_entry_table(entry, day=i, 
                                       table_index=table_indices[[i]][j],
                                       table_name=table_name)
          k <- k + 1
        }
        # If table_name is TRUE, then make the table names the names of the list
        # itself and remove them from the original data frames. 
        if (table_name) {
          names(res) <- purrr::map(res, ~ unique(.$table_name)) %>%
            unlist
          for (i in 1:length(res)) {
            res[[i]]$table_name <- NULL
          }
        }
        
      } else {
        if (verbose) {
          cat(glue::glue("No tables found for day {`i`}\n"))
        }
      }
    }
  } else {
    res <- read_entry_table(entry, day=day, table_index=table_position,
                     table_name=table_name)
  }
  res 
}