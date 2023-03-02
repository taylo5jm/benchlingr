# read_entry_tables.R

#' @keywords internal
.read_entry_table_by_name <- function(entry, table_name, table_indices, return_table_name) {
    if (!is.character(table_name)) {
      stop("'table_name' input is invalid.")
    }
    # Find the days and table positions that correspond to the
    # table names provided. 
    table_name_lookup <- list()
    for (i in 1:length(table_name)) {
      table_name_lookup[[i]] <- purrr::map(
        table_indices, ~ .[which(names(.) == table_name[i])])
      names(table_name_lookup[[i]]) <- 1:length(table_indices)
      table_name_lookup[[i]] %<>% Filter(function(x) (length(x) > 0), .)
    }
    names(table_name_lookup) <- table_name
    # Check to see if any tables are missing
    missing_tables <- which(
      purrr::map_int(table_name_lookup, ~ length(.)) == 0)
    if (length(missing_tables) > 0) {
      stop(glue::glue(
        "'table_name' input {paste0(names(missing_tables), collapse=',')}does not exist in notebook entry."))
    }
    # Get the days and positions that correspond to the
    # table names provided by the user. 
    positions <- purrr::map_int(table_name_lookup, ~ .[[1]][[1]])
    # purrr::map(table_name_lookup, ~ names(.))
    days <- purrr::map_int(table_name_lookup, ~ names(.) %>%
                     as.integer)
    # Call the read_entry_table function over the positions/days we have.
    res <- purrr::map2(
      days, positions, ~ read_entry_table(
        entry=entry, day=.x, table_position=.y,
        return_table_name=return_table_name))
    return(res)
}
    
#' @keywords internal
.read_entry_table_by_location <- function(entry, day, table_position, table_indices,
                                          return_table_name) {
    if (is.null(day) & !is.null(table_position)) {
      if (!is.numeric(table_position)) {
        stop("Both 'day' and 'table_position' inputs have to be defined as integers while 'table_name' is defined as NULL.")
      }
      stop("'table_position' input cannot be defined as an integer while 'day' input is defined as NULL. Either provide a numeric input for 'day' or provide a properly defined input for 'table_name.'")
    }
    if (!(day %in% names(table_indices))) {
      stop("'day' input does not exist in notebook entry.")
    }
    if (!(table_position %in% table_indices[[day]])) {
      stop("'table_position' input does not correspond to a table in the notebook entry.")
    } 
    res <- purrr::map(
      table_position, ~ read_entry_table(
        entry, day=day, table_position=.,
        return_table_name=return_table_name))
    return(res)
}
  
#' @keywords internal
.read_all_entry_tables <- function(entry, table_indices, return_table_name) {
  
    res <- list(); k <- 1;
    for (i in 1:length(table_indices)) {
      if (length(table_indices[[i]]) > 0) {
        for (j in 1:length(table_indices[[i]])) {
          res[[k]] <- read_entry_table(entry, day=i, 
                                       table_position=table_indices[[i]][j],
                                       table_name=NULL,
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
          cat(glue::glue("No tables were found for day {`i`}\n"))
        }
      }
    }
    
    return(res)
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
#' @param table_name A specific table can be read by name with the `table_name` argument.
#' If `table_name` is provided, then `day` and `table_position` are ignored.  
#' @param return_table_name If return_return_table_name is TRUE, then the names of the tables
#' in the notebook entry will be returned as names in the output list. 
#' @param verbose If verbose, then the function will alert the user
#' if no tables can be found for some days in the notebook. 
#' @return List of data frames representing the unstructured tables in
#' the notebook entry.
#' @examples \dontrun{
#' client <- benchlingr::benchling_api_auth(
#'     tenant="https://hemoshear-dev.benchling.com",
#'     api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))
#' entry <- client$entries$get_entry_by_id("etr_IWLMFYhR")
#' 
#' entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
#' tables <- read_entry_tables(entry, table_name='Experimental Conditions')
#' entry <- client$entries$get_entry_by_id("etr_aTa6eDWA")
#' tables <- read_entry_tables(entry, table_name=c("TableA", "TableB"))
#' }
#' @export

read_entry_tables <- function(entry, day=NULL, table_position=NULL, 
                              table_name=NULL, return_table_name=TRUE, 
                              verbose=FALSE) {
  if (missing(entry)) {
    stop("'entry' input is missing. See ?benchlingr::get_entry. ")
  }
  
  if (!all(class(entry) %in% c("benchling_api_client.v2.stable.models.entry.Entry", 
                            "python.builtin.object"))) {
    stop("'entry' input is invalid. See ?benchlingr::get_entry.")
  }
  
  if (is.character(table_name) & (!is.null(day) | !is.null(table_position))) {
    warning("'day' and 'table_position' will be ignored in favor of 'table_name'.")
  }
  
  table_indices <- find_entry_tables(entry)
  
  if (length(table_indices) == 1) {
    if (is.na(table_indices)) {
      stop("No tables were found in notebook entry.")
    }
  }

  if (!is.null(table_name)) {
    res <- .read_entry_table_by_name(
      entry=entry, table_name=table_name, table_indices=table_indices, 
      return_table_name=return_table_name)
  } else if (!is.null(day) & !is.null(table_position)) {
    res <- .read_entry_table_by_location(
        entry, day=day, table_position = table_position, 
        return_table_name=return_table_name)
  } else if (is.null(day) & is.null(table_position)) {
    res <- .read_all_entry_tables(
      entry, table_indices, 
      return_table_name=return_table_name)
  }
  res 
}

