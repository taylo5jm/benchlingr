.submit_queries <- function(api_queries, benchling_api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (benchling_api_key == "") { # Check if the benchling api key is valid or not
    stop("Benchling API key is missing or empty.")
  }
  
  response_list <- purrr::map(api_queries, ~ 
                              httr::content(httr::GET(., httr::authenticate(benchling_api_key, '')))) # Sends each URL query in the input to benchling 
                                                                                                      # and retrieves the response for each one. 
  names(response_list) <- names(api_queries) # Re-assigns the names using the names of the entity identifiers.

  return(response_list)
}
  