# .submit_queries.R

#' Uses each element to call the Single-Get API endpoint in Benchling for a specific entity identifier.
#' 
#' .submit_queries.R takes a named character vector with one or more elements where the names are entity 
#' identifiers and the elements are each identifier's respective Single-Get API endpoint URL and uses them 
#' to call that identifier's Single-Get API endpoint in Benchling and extract the response contents.
#' 
#' @param api_queries A named character vector where the names are entity identifiers and the elements are 
#' the Single-Get API endpoint URLs for each identifier.
#' @param api_key Benchling API key. 
#' @return A named list where the names are entity identifiers and each element is a list that contains the 
#' response contents from the call made to that identifier's Single-Get API endpoint in Benchling.
#' @examples \dontrun{
#' api_queries <- c("seq_Cuf0bmCm"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences/seq_Cuf0bmCm", 
#' "bfi_Q1PMlXkf"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/bfi_Q1PMlXkf", 
#' "box_7YutniM0"="https://hemoshear-dev.benchling.com/api/v2/boxes/box_7YutniM0",
#' "bfi_Ks908uWV"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/bfi_Ks908uWV")
#' res <- .submit_queries(api_queries=api_queries, api_key=Sys.getenv("BENCHLING_API_KEY"))
#' }
#' @export
#' @keywords internal

.submit_queries <- function(api_queries, api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (api_key == "") { # Check if the benchling api key is valid or not
    stop("Benchling API key is missing or empty.")
  }
  
  response_list <- purrr::map(api_queries, ~ 
                              httr::content(httr::GET(., httr::authenticate(api_key, '')))) # Sends each URL query in the input to benchling 
                                                                                                      # and retrieves the response for each one. 
  names(response_list) <- names(api_queries) # Re-assigns the names using the names of the entity identifiers.

  return(response_list)
}
  
