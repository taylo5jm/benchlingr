.get_api_endpoints <- function(entity_id, entity_list=NULL, contents="all") {
  if (is.null(entity_list)) { # Checks if entity_list has not been defined.
    entity_list <- .list_api_contents(contents="all") # Defines entity_list if left as NULL using list_api_contents.R.
  }
  
  entity_elements <- purrr::map(entity_list, ~ .[c(2,3,4)]) # Extracts API contents for each entity.
  names(entity_elements) <- purrr::map_chr(entity_list, ~ .[1]) 
  
  if (contents=="all") {
    entity_contents <- purrr::map(entity_id, ~ entity_elements[[.]])
    return(entity_contents)
  } else if (contents=="list contents") {
    entity_contents <- purrr::map(entity_id, ~ entity_elements[[.]][1])
    return(entity_contents)
  } else if (contents=="single-get endpoint") {
    entity_contents <- purrr::map(entity_id, ~ entity_elements[[.]][2])
    return(entity_contents)
  } else if (contents=="bulk-get endpoints") {
    entity_contents <- purrr::map(entity_id, ~ entity_elements[[.]][3])
    return(entity_contents)
  } else {
    stop("Invalid argument for contents. Input for contents should either be defined as 'all', 
         'list contents', 'single-get endpoint', 'bulk-get endpoints' or left as NULL.")
  }
}
