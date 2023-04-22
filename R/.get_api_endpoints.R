.get_api_endpoints <- function(entity_id, entity_list=NULL) {
  if (is.null(entity_list)) { # Checks if entity_list has not been defined.
    entity_list <- .list_api_contents(contents="all") # Defines entity_list if left as NULL using list_api_contents.R.
  }
  
  entity_elements <- purrr::map(entity_list, ~ .[c(2,3,4)]) # Extracts API contents for each entity.
  names(entity_elements) <- purrr::map_chr(entity_list, ~ .[1]) 
  
  entity_get_endpoints <- sapply(entity_id, function(x) entity_elements[[x]][2]) 
  
  return(entity_get_endpoints)
}
