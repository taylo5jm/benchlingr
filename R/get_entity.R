# get_entity.R

#' Calls each element's Single-Get API endpoints in Benchling and extracts the response.
#' 
#' get_entity.R takes a character vector with one or more elements and for each element calls its Single-Get API 
#' endpoints in Benchling and extracts the response contents and stores it in a list.
#' 
#' @include infer_entity_type.R
#' @include .get_api_endpoints.R
#' @include .craft_api_queries.R
#' @include .submit_queries.R
#' @param entity_id A character vector with 1 or more elements.
#' @param entity_list A list of vectors where each vector designates the schema type, list API contents, 
#' Single-Get API endpoint contents and Bulk-Get API endpoints contents for the entities we are interested 
#' in and the names are the first characters seen in the identifiers for each entity. 
#' @param benchling_api_key Benchling API key. 
#' @return A named list where the names are entity identifiers and each element is a list that contains
#' the response contents from the call made to that identifier's Single-Get API endpoint in Benchling.
#' @examples \dontrun{
#' entity_id <- c("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", "bfi_Ks908uWV", "ent_Ec76qX9f", 
#' "ent_sPrxBNOh", "box_K9950IQ8", "box_923aklum")
#' res <- get_entity(entity_id=entity_id, entity_list=NULL, benchling_api_key=Sys.getenv("BENCHLING_API_KEY"))
#' }
#' @export

get_entity <-  function(entity_id, entity_list=NULL, benchling_api_key=Sys.getenv("BENCHLING_API_KEY")) {
  inferred_entity_ids <- infer_entity_type(entity_id=entity_id, entity_list=entity_list) # Infers the entity schemas for each element
  entity_single_get_endpoints  <- .get_api_endpoints(entity_id=inferred_entity_ids, entity_list=entity_list) # Extracts the Single-Get API endpoint URL format for each entity identifier according to its respective schema element
  api_queries <- .craft_api_queries(entity_single_get_endpoints=entity_single_get_endpoints) # Rewrites the Single-Get API endpoint URL formats into proper URL strings for each identifier
  response_list <- .submit_queries(api_queries=api_queries, benchling_api_key=benchling_api_key) # Uses the URL strings to call the entity identifier's Single-Get API endpoint in Benchling and extracts the contents of the response
  return(response_list)
}
