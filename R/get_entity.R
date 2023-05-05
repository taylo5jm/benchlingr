# .get_api_endpoints.R

#' Match each element to its corresponding Single-Get API endpoint URL format
#' 
#' .get_api_endpoints.R takes a named character vector with one or more elements where the names are entity 
#' identifiers and the elements are the entity types for those identifiers and matches each element 
#' to their respective Single-Get API endpoint URL formats according to which entity the schema correlates 
#' with.
#' 
#' @param entity_id A named character vector where the names are entity identifiers and the elements 
#' are the entity types associated with those identifiers.
#' @param entity_list A list of vectors where each vector designates the schema type and API options for 
#' the entities we are interested in and the names are the first characters seen in the identifiers for
#' each entity. If NULL (the default), the function will use a default list.
#' 
#' Alternatively, a custom or more specified list can be used based on the purpose and intent of the list
#' and overall application of the function.
#' 
#' @return A named character vector where the names are entity identifiers and the elements are the
#' Single-Get API endpoint URL formats associated with them based on the respective entities each identifier 
#' correlates to and matches with.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm"="dna_sequence", "bfi_Q1PMlXkf"="custom_entity", "box_7YutniM0"="box")
#' res1 <- .get_api_endpoints(entity_id=entity_id1, entity_list=NULL)
#' 
#' entity_id2 <- c("bfi_Ur5DfvGJ"="custom_entity", "seq_Gju61mCm"="dna_sequence", 
#' "bfi_Q13AlXkf"="custom_entity", "bfi_Ks908uWV"="custom_entity", "ent_Ec76qX9f"="user", 
#' "ent_sPrxBNOh"="user", "box_K9950IQ8"="box")
#' res2 <- .get_api_endpoints(entity_id=entity_id2, entity_list=NULL)
#' }
#' @keywords internal

.get_api_endpoints <- function(entity_id, entity_list=NULL) {
  if (is.null(entity_list)) { # Checks if entity_list has not been defined.
    entity_list <- .list_api_contents(contents="all") # Defines entity_list if left as NULL using .list_api_contents.R.
  }
  
  entity_elements <- purrr::map(entity_list, ~ .[c(2,3,4)]) # Extracts API contents for each entity.
  names(entity_elements) <- purrr::map_chr(entity_list, ~ .[1]) 
  
  entity_single_get_endpoints <- sapply(entity_id, function(x) entity_elements[[x]][2]) 
  
  return(entity_single_get_endpoints)
}

# .craft_api_queries.R

#' Format each element using its entity identifier's name to generate proper Single-Get API endpoint URLs for 
#' that identifier
#' 
#' .craft_api_queries.R takes a named character vector with one or more elements where the names are entity 
#' identifiers and the elements are each identifier's respective Single-Get API endpoint URL formats and 
#' rewrites the element using the name of the identifier to generate proper Single-Get API endpoint URLs.
#' 
#' @param entity_single_get_endpoints A named character vector where the names are entity identifiers and the 
#' elements are the Single-Get API endpoint URL formats for each identifier.
#' @return A named character vector where the names are entity identifiers and the elements are the 
#' respective Single-Get API endpoint URLs for each identifier.
#' @examples \dontrun{
#' entity_single_get_endpoints <- c("seq_Cuf0bmCm"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences/ENTITY_ID", 
#' "bfi_Q1PMlXkf"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID", 
#' "box_7YutniM0"="https://hemoshear-dev.benchling.com/api/v2/boxes/ENTITY_ID",
#' "bfi_Ks908uWV"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID")
#' res <- .craft_api_queries(entity_single_get_endpoints=entity_single_get_endpoints)
#' }
#' @keywords internal

.craft_api_queries <- function(entity_single_get_endpoints) {
  api_queries <- sapply(names(entity_single_get_endpoints),
                        function(x) gsub("ENTITY_ID", x, entity_single_get_endpoints[x])) # Prepares single-get API endpoint 
  # URL queries for each entity 
  # identifier not assigned with a 
  # value of NA.
  names(api_queries) <- names(entity_single_get_endpoints) # Re-assigns entity identifiers as names
  # to each corresponding URL query.
  return(api_queries)
}

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

# get_entity.R

#' Calls each element's Single-Get API endpoints in Benchling and extracts the response.
#' 
#' get_entity.R takes a character vector with one or more elements and for each element calls its Single-Get API 
#' endpoints in Benchling and extracts the response contents and stores it in a list.
#' 
#' @param entity_id A character vector with 1 or more elements.
#' @param api_key Benchling API key. 
#' @return A named list where the names are entity identifiers and each element is a list that contains
#' the response contents from the call made to that identifier's Single-Get API endpoint in Benchling.
#' @examples \dontrun{
#' entity_id <- c("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", "bfi_Ks908uWV", "ent_Ec76qX9f", 
#' "ent_sPrxBNOh", "box_K9950IQ8", "box_923aklum")
#' res <- get_entity(entity_id=entity_id, api_key=Sys.getenv("BENCHLING_API_KEY"))
#' }
#' @export

get_entity <-  function(entity_id, api_key=Sys.getenv("BENCHLING_API_KEY")) {
  inferred_entity_ids <- infer_entity_type(entity_id=entity_id, entity_list=NULL) # Infers the entity schemas for each element
  entity_single_get_endpoints  <- .get_api_endpoints(entity_id=inferred_entity_ids, entity_list=NULL) # Extracts the Single-Get API endpoint URL format for each entity identifier according to its respective schema element
  api_queries <- .craft_api_queries(entity_single_get_endpoints=entity_single_get_endpoints) # Rewrites the Single-Get API endpoint URL formats into proper URL strings for each identifier
  response_list <- .submit_queries(api_queries=api_queries, api_key=api_key) # Uses the URL strings to call the entity identifier's Single-Get API endpoint in Benchling and extracts the contents of the response
  return(response_list)
}
