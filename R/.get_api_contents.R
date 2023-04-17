# .get_api_contents.R

#' Attaches API URLs for an entity's list contents, single-get API endpoint contents and bulk-get API 
#' endpoints contents to input.
#' 
#' .get_api_contents.R takes a named list where the names are entity schemas and the elements are vectors
#' that contain 



.get_api_contents <- function(entity_id, entity_list) {
  API_URLs <- purrr::map(entity_list, ~ .[c(2,3,4)])
  names(API_URLs) <- purrr::map_chr(entity_list, ~ .[1])
  entity_id_types <- names(entity_id)
  entity_id_info <- purrr::map(entity_id_types, ~ list("entity_identifiers"=entity_id[[.]],"api_urls"=API_URLs[[.]]))
  names(entity_id_info) <- entity_id_types
  return(entity_id_info)
}