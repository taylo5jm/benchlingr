# read_entry_tables.R

#' Read unstructured table in a notebook entry
#' 
#' @importFrom magrittr %>%
#' @param entry Notebook entry in JSON format.
#' @param day Integer for the day in the notebook entry. See `find_entry_tables`.
#' @param table_position Integer for the position of the table in the notebook entry list. 
#' See `find_entry_tables`.
#' @param return_table_name bool Determines how the name of the table in the notebook 
#' entry is included in the output.
#' If `TRUE` is selected, the table name will be added as a new column
#' to the data frame. If `FALSE`, then the table name will be ignored. 
#' @return Data frame representing the unstructured table in the notebook
#' entry.
#' @keywords internal
#' @examples \dontrun{
#' client <- benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
#'                              api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
#' entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
#' table_indices <- benchlingr:::find_entry_tables(entry)
#' print(table_indices)
#' a_table <- read_entry_table(entry, day=1, table_position=2)
#' }

read_entry_table <- function(entry, day, table_position, table_name,
                             return_table_name=TRUE) {
  
  entry_tables <- list()
  counter1 <- 0
  table_check <- list()
  for (i in 1:length(entry$days)) {
    entry_tables[[i]] <- list()
    for (j in 1:length(entry$days[[i]]$notes)) {
      entry_tables[[i]][[j]] <- entry$days[[i]]$notes[[j]]
      if (entry$days[[i]]$notes[[j]]$type$value == "table") {
        counter1 <- counter1 + 1
        table_check[[counter1]] <- entry$days[[i]]$notes[[j]]
      }
    }
  }
  
  if (length(table_check) == 0) {
    stop("Entry is empty and contains no tables.")
  }
  
  if (table_name != "" | !is.na(table_name) | !is.null(table_name)) {
    a_table <- "empty"
    entry_table_name_list <- list()
    counter2 <- 0
    for (i in 1:length(entry_tables)) {
      for (j in 1:length(entry_tables[[i]])) {
        if (entry_tables[[i]][[j]]$type$value == "table") {
          counter2 <- counter2 + 1
          entry_table_name_list[[counter2]] <- entry_tables[[i]][[j]]$table$name
          if (entry_tables[[i]][[j]]$table$name == table_name) {
            a_table <- entry_tables[[i]][[j]]
          } else {
            next
          }
        } else {
          next
        }
      }
    }
    
    if (!(table_name %in% entry_table_name_list)) {
      stop(paste0(paste(c("Name for table could not be found. If possible try to find actual table name from the following list of tables found in the entry:", entry_table_name_list), collapse=" "),"."))
    }
    
    if (is.numeric(day) & is.numeric(table_position)) {
      warning("'day' and 'table_position' arguments will be ignored in favor of 'table_name.'")
    }
    
    if (is.numeric(day) & !is.numeric(table_position)) {
      warning("'day' argument will be ignored in favor of 'table_name.' Also 'table_position' should either be written as an integer as it designates the location of the unstructured table in the notebook entry or kept blank along with 'day.'")
    }
    
    if (!is.numeric(day) & is.numeric(table_position)) {
      warning("'table_position' argument will be ignored in favor of 'table_name.' Also 'day' should either be written as an integer as it designates the day of the unstructured table in the notebook entry or kept blank along with 'day.'")
    }
    
    if ((!is.numeric(day) & !is.na(day) & !is.null(day) & day != "") & 
        (!is.numeric(table_position) & !is.na(table_position) & !is.null(table_position) & 
         table_position != "")) {
      warning("'day' and 'table_position' arguments will be ignored in favor of 'table_name.' Also 'day' and 'table_position' should either be integers as they represent the day and location of the unstructured table in the notebook entry or be rendered blank.")
    }
  } else {
    if (!is.numeric(day) & is.numeric(table_position)) {
      stop("'day' should be an integer that represents the day of the unstructured table in the notebook entry. 
           Either use 'find_entry_tables(entry)' to locate the unstructured tables in the notebook entry or specify a name for 'table_name' and make 'day' and 'table_position' blank either by NULL, NA or by writing them as empty strings.")
    }
    if (is.numeric(day) & !is.numeric(table_position)) {
      stop("'table_position' should be an integer that represents the location of the unstructured table in the notebook entry. 
           Either use 'find_entry_tables(entry)' to locate the unstructured tables in the notebook entry or specify a name for 'table_name' and make 'day' and 'table_position' blank either by NULL, NA or by writing them as empty strings.")
    }
    if (!is.numeric(day) & !is.numeric(table_position)) {
      stop("'day' and 'table_position' should be integers that represent the day and location of the unstructured table in the notebook entry. 
           Either use 'find_entry_tables(entry)' to locate the unstructured tables in the notebook entry or specify a name for 'table_name' and make 'day' and 'table_position' blank either by NULL, NA or by writing them as empty strings.")
    }
    a_table <- entry_tables[[day]][[table_position]]
  }
  
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

#' Read all unstructured tables in a notebook entry. 
#' 
#' This function will read all unstructured tables in a notebook entry,
#' returning a list of data frames. 
#' 
#' @include find_entry_tables.R
#' @param entry Notebook entry in JSON format. See `get_entry`.
#' @param day Integer for the day in the notebook entry. See `find_entry_tables`.
#' @param table_position Integer for the position of the table in the notebook entry list. 
#' See `find_entry_tables`.
#' @param return_table_name If return_table_name is TRUE, then the names of the tables
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

read_entry_tables <- function(entry, day=NULL, table_position=NULL, table_name=NULL,
                              return_table_name=TRUE, verbose=FALSE) {
  if (is.null(day) & is.null(table_position)) {
    table_indices <- find_entry_tables(entry)
    res <- list(); k <- 1;
    for (i in 1:length(table_indices)) {
      if (length(table_indices[[i]]) > 0) {
        for (j in 1:length(table_indices[[i]])) {
          res[[k]] <- read_entry_table(entry, day=i, 
                                       table_position=table_indices[[i]][j],
                                       return_table_name=return_table_name)
          k <- k + 1
        }
        # If return_table_name is TRUE, then make the table names the names of the list
        # itself and remove them from the original data frames. 
        if (return_table_name) {
          names(res) <- purrr::map(res, ~ unique(.$return_table_name)) %>%
            unlist
          for (i in 1:length(res)) {
            res[[i]]$return_table_name <- NULL
          }
        }
        
      } else {
        if (verbose) {
          cat(glue::glue("No tables found for day {`i`}\n"))
        }
      }
    }
  } else {
    res <- read_entry_table(entry, day=day, table_position=table_position, 
                            table_name=table_name,
                            return_table_name=return_table_name)
  }
  res 
}
