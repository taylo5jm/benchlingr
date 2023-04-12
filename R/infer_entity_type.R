# infer_entity_type.R

#' Infer the entity types of elements contained in a vector or list
#' 
#' infer_entity_type.R takes either a character vector or list of character strings and 
#' for each element tries to infer the schema type.
#' 
#' @param entity_id Either a character vector or list that contains the entities. 
#' All entities in the list or character vector must be character strings.
#' @param entity_list A list of vectors where each vector designates the schema type and API URLs for 
#' the entities we are interested in and the names are the first characters seen in the identifiers for
#' each entity. 
#' @return A named list, where the names are the identifiers or elements from entity_id
#' and the values are the schema types.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm","bfi_Q1PMlXkf","ver_io98720u")
#' res1 <- infer_entity_type(entity_id1)
#' 
#' entity_id2 <- list("bfi_Ur5DfvGJ","seq_Gju61mCm","bfi_Q13AlXkf","bfi_Ks908uWV",
#' "ent_Ec76qX9f","ent_sPrxBNOh","box_K9950IQ8","dis_89mkooip","bxo_923aklum")
#' res2 <- infer_entity_type(entity_id2)
#' }
#' @export
#' @keywords internal

infer_entity_type <- function(entity_id, entity_list=NULL) {
  if (is.null(entity_list)) { # Check if entity_list has not been defined or has been rendered NULL
    entity_list <- list_api_contents(contents="all") # Define entity_list if left as NULL using list_api_contents.R and setting contents to "all"
  }
  
  res <- unlist(purrr::map(entity_id, ~ entity_lookup(entity_id = ., entity_list = entity_list))) # Run entity_id through entity_lookup.R in order to see which elements in entity_id can be matched with an appropriate entity type according to entity_list and convert the result into a character vector  
  names(res) <- unlist(entity_id) # Name the character vector with their respective elements in entity_id
  return(res)
}

