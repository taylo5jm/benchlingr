


check_inputs <- function(entity_id, benchling_api_key) {
  if (benchling_api_key == "") { # Check if the Benchling API Key is not defined or is missing from the .Renviron file.
    stop("Benchling API key is missing or empty.") # Stop the function if the Benchling API Key is missing or not defined in the .Renviron file.
  }
  
  if (missing(entity_id)) { # Check if the 
    stop("'entity_id' input is missing. Must be a 1-D list or character vector")
  }
  
  if (is.null(entity_id) | length(entity_id) == 0) {
    stop("'entity_id' input is invalid. Must be a 1-D list or character vector")
  } 
  
  for (i in 1:length(entity_id)){
    if (length(entity_id[[i]]) != 1) {
      stop("'entity_id' contains an invalid identifier. Must be a 1-D list or character vector.")
    }
    if (!is.character(entity_id[[i]])) {
      stop("'entity_id' contains an invalid identifier. Must be a 1-D list or character vector.")
    }
    if (is.na(entity_id[[i]])) {
      stop("'entity_id' contains an invalid identifier. Must be a 1-D list or character vector and must not contain any values labeled as NA.")
    }
    if (grepl("_", entity_id[[i]], fixed = TRUE) == FALSE) {
      stop(glue::glue("'entity_id' contains an invalid identifier. {entity_id[[i]]} is not valid and is written in an invalid format."))
    }
    if (stringr::str_count(entity_id[[i]], "_") != 1) {
      stop(glue::glue("'entity_id' contains an invalid identifier. {entity_id[[i]]} is not valid and is written in an invalid format."))
    }
  }
}
  