.entity_id_list_check <- function(entity_type, entity_id, api_url, 
                                  benchling_api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (!is.character(entity_id) | length(entity_id) == 0) { # Checks if entity_id is a character vector with a length greater than 0.
    stop("'entity_id' input is invalid. Must be a character vector with a length greater than 0.")
  } 
  
  if (benchling_api_key == "") { # Check if the benchling api key is valid or not
    stop("Benchling API key is missing or empty.")
  }
  entity_id_sets <- split(entity_id, ceiling(seq_along(entity_id)/50)) # Splits a vector containing entity identifiers into a 
                                                                       # number of sets based on the maximum size of each set
  list_queries <- purrr::map(entity_id_sets, ~ .make_benchlingr_query(entity_id_set=.,
                                                                      query_format="list contents",
                                                                      api_url=api_url))
  response_list <- purrr::map(list_queries, ~ httr::content(httr::GET(., httr::authenticate(benchling_api_key, ''))))
  
  return(response_list) # Finish!
  }


