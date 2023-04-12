entity_lookup <- function(entity_id, entity_list) {
  if (substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1) %in% names(entity_list)) {
    output <- entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][1]
  } else {
    output <- NA
  }
  return(output)
}
