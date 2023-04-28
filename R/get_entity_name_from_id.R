# get_entity_name_from_id.R

#' Retrieves the name attribute from each element
#' 
#' get_entity_name_from_id.R takes a named list where the names are entity identifiers and each element in the list
#' is a list as well that contains the contents of the response from the call to each identifier's Single-Get API endpoint 
#' in Benchling and extracts the name associated with it.
#' 
#' @param entity_response_list A named list where the names are entity identifiers and each element is a list
#' containing the response contents from the call made to that identifier's Single-Get API endpoint in Benchling.
#' @return A named character vector where the names are entity identifiers and the elements are the names
#' corresponding to those identifiers.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0bmCm", "bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", "bfi_Ks908uWV", 
#' "ent_Ec76qX9f", "ent_sPrxBNOh", "box_K9950IQ8", "box_923aklum")
#' entity_responses <- get_entity(entity_id=entity_id, api_key=Sys.getenv("BENCHLING_API_KEY"))
#' 
#' res <- get_entity_name_from_id(entity_response_list=entity_responses)
#' }
#' @export

get_entity_name_from_id <- function(entity_response_list) { 
  entity_names <- purrr::map(entity_response_list, ~ .[['name']]) # Extracts the name attribute from each response if it exists
  entity_names[which(purrr::map(entity_names, ~ is.null(.)) == TRUE)] <- NA # Replaces all null values with NA
  entity_names <- unlist(entity_names) # Converts entity_names from a named list to a named character vector
  return(entity_names)
}
