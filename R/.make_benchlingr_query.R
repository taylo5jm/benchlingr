.make_benchlingr_query <- function(entity_id_set, query_format, api_url) {
  if (length(entity_id_set) == 0) { # Checks the length of entity_id_set.
    stop("'entity_id_set' input is invalid. Must have a length greater than 0.")
  } 
  
  query_string <- paste(entity_id_set, collapse="%2C") # Joins all the entity identifiers into a string formatted so that will be used inside the query URL.
  
  if (query_format=="list contents" | query_format=="single-get endpoint" | # Only accepts 3 parameters for query_format: 'list contents', 
                                                                            # 'single-get endpoint' or 'bulk-get endpoints.'
      query_format=="bulk-get endpoints") {
    if (query_format=="list contents" | query_format=="bulk-get endpoints") {
     query_url <- gsub("ENTITY_IDS", query_string, api_url)
    } 
    if (query_format=="single-get endpoint") {
      query_url <- gsub("ENTITY_ID", query_string, api_url)
    }
  } else { # Stops the function if the input for query_format is not either 'list contents', 'single-get endpoint' 
           # or 'bulk-get endpoints.'
    stop("query_format input is invalid. Must be defined either as 'list contents', 'single-get endpoint' 
         or 'bulk-get endpoints.'")
  }
  return(query_url)
}
