# .get_api_endpoints.R

#' Match each element to its corresponding Single-Get API endpoint URL format
#' 
#' .get_api_endpoints.R takes a named character vector with one or more elements where the names are entity 
#' identifiers and the elements are the entity types for those identifiers and matches each element 
#' to their respective Single-Get API endpoint URL formats according to which entity the schema correlates 
#' with.
#' 
#' @include .list_api_contents.R
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
#' @export
#' @keywords internal

.get_api_endpoints <- function(entity_id, entity_list=NULL) {
  if (is.null(entity_list)) { # Checks if entity_list has not been defined.
    entity_list <- .list_api_contents(contents="all") # Defines entity_list if left as NULL using list_api_contents.R.
  }
  
  entity_elements <- purrr::map(entity_list, ~ .[c(2,3,4)]) # Extracts API contents for each entity.
  names(entity_elements) <- purrr::map_chr(entity_list, ~ .[1]) 
  
  entity_single_get_endpoints <- sapply(entity_id, function(x) entity_elements[[x]][2]) 
  
  return(entity_single_get_endpoints)
}
