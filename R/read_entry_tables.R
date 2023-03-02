# read_entry_tables.R

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
#' tables <- read_entry_tables(entry)
#' }
#' @export

read_entry_tables <- function(entry, day=NULL, table_position=NULL, 
                              table_name=NULL, return_table_name=TRUE, 
                              verbose=FALSE) {
  if (missing(entry)) {
    stop("'entry' input is missing.") # See ?benchlingr::get_entry.
  }
  
  if (missing(day)) {
    day <- NULL
  } else {
    if (!missing(day) & !is.null(day)) {
      if (is.na(day) | day == "") {
        day <- NULL
      }
    }
  }
  
  if (missing(table_position)) {
    table_position <- NULL
  } else {
    if (!missing(table_position) & !is.null(table_position)) {
      if (is.na(table_position) | table_position == "") {
        table_position <- NULL
      }
    }
  }
  
  if (missing(table_name)) {
    table_name <- NULL
  } else {
    if (!missing(table_name) & !is.null(table_name)) {
      if (is.na(table_name) | table_name == "") {
        table_name <- NULL
      }
    }
  }
  
  if (!all(class(entry) %in% c("benchling_api_client.v2.stable.models.entry.Entry", 
                               "python.builtin.object"))) {
    stop("'entry' input is invalid.") # See ?benchlingr::get_entry.
  }
  
  if (is.character(table_name) & (!is.null(day) | !is.null(table_position))) {
    warning("'day' and 'table_position' will be ignored in favor of 'table_name'. ")
    if (!is.null(day) & !is.numeric(day)) {
      warning("'day' input is invalid as it should either be defined as NULL or as an integer along with 'table_position' when 'table_name' is defined.")
    }
    if (!is.null(table_position) & !is.numeric(table_position)) {
      warning("'table_position' input is invalid as it should either be defined as NULL or as an integer along with 'table_position' when 'table_name' is defined.")
    }
  }
  
  table_indices <- find_entry_tables(entry)
  
  if (length(table_indices) == 1) {
    if (is.na(table_indices)) {
      stop("No tables were found in notebook entry.")
    }
  }
  
  if (!is.null(table_name)) {
    if (!is.character(table_name)) {
      stop("'table_name' input is invalid.")
    }
    
    table_name_check <- FALSE
    
    for (i in 1:length(table_indices)) {
      if (table_name %in% names(table_indices[[i]])) {
        table_name_check <- TRUE
      } else {
        next
      }
    }
    
    if (table_name_check == FALSE) {
      stop("'table_name' input does not exist in notebook entry.")
    } else {
      res <- read_entry_table(entry=entry, day=NULL, table_position=NULL,table_name=table_name,
                              return_table_name=return_table_name) 
    }
    
  } else {
    if (is.null(day) & !is.null(table_position)) {
      if (!is.numeric(table_position)) {
        warning("'table_position' input is invalid.")
        stop("Both 'day' and 'table_position' inputs have to be defined as integers while 'table_name' is defined as NULL.")
      }
      stop("'table_position' input cannot be defined as an integer while 'day' input is defined as NULL. Either provide a numeric input for 'day' or provide a properly defined input for 'table_name.'")
    }
    
    if (!is.null(day) & !is.null(table_position)) {
      if (!is.numeric(day) & !is.numeric(table_position)) {
        stop("'day' and 'table_position' inputs are invalid.")
      }
      
      if (!is.numeric(day) & is.numeric(table_position)) {
        stop("'day' input is invalid.")
      }
      
      if (is.numeric(day) & !is.numeric(table_position)) {
        stop("'table_position' input is invalid.")
      }
      
      if (day %in% names(table_indices)) {
        if (!(table_position %in% table_indices[[day]])) {
          stop("'table_position' input does not exist in notebook entry.")
        } 
      } else {
        stop("'day' input does not exist in notebook entry.")
      }
      
      res <- read_entry_table(entry, day=day, table_position=table_position,table_name=NULL,
                              return_table_name=return_table_name)
    }
    
    if (!is.null(day) & is.null(table_position)) {
      if (!is.numeric(day)) {
        stop("'day' input is invalid.")
      }
      
      if (!(day %in% names(table_indices))) {
        stop("'day' input does not exist in notebook entry.")
      }
      
      if (identical(table_indices[[day]],integer(0))) {
        stop(glue::glue("No tables were found for {day} in notebook entry."))
      }
      
      res <- list()
      for (i in 1:length(table_indices[[day]])) {
        res[[i]] <- read_entry_table(entry, day=day,
                                     table_position=table_indices[[day]][i],
                                     table_name=NULL,
                                     return_table_name=return_table_name)
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
    } 
    
    if (is.null(day) & is.null(table_position)) {
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
    }
  }
  res 
}

