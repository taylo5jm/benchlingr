.valid_entity_sort <- function(entity_id) {
  if (length(entity_id[is.na(entity_id)]) > 0) {
    stop("'entity_id' input is invalid. Must not contain entity identifiers with NA values as only 
         valid entity identifiers will be accepted for this function.")
  }
  entity_id_contents <- .get_api_endpoints(entity_id=entity_id,
                                           entity_list=NULL,
                                           contents="all")
  valid_id_check <- sapply(entity_id_contents, function(x) !is.na(x[2]) | !is.na(x[3]))
  valid_ids <- entity_id[valid_id_check]

  testable_entity_list_options <- sapply(names(valid_ids), function(x) !is.na(entity_id_contents[[x]][1]))
  testable_ids <- valid_ids[testable_entity_list_options]
  testable_ids <- split(names(testable_ids), testable_ids)
  testable_id_types <- names(testable_ids)
  testable_ids <- purrr::map(testable_id_types, ~ list("entity_identifiers"=testable_ids[[.]]))
  names(testable_ids) <- testable_id_types
  
  nontestable_ids <- valid_ids[!testable_entity_list_options]
  nontestable_ids <- split(names(nontestable_ids), nontestable_ids)
  nontestable_id_types <- names(nontestable_ids)
  nontestable_ids <- purrr::map(nontestable_id_types, ~ list("entity_identifiers"=nontestable_ids[[.]]))
  names(nontestable_ids) <- nontestable_id_types
  
  invalid_ids <- names(entity_id)[!valid_id_check]
  invalid_ids <- list("invalid_entity"=list("entity_identifiers"=invalid_ids)) 
  
  res <- list("testable_ids"=testable_ids,
              "nontestable_ids"=nontestable_ids,
              "invalid_ids"=invalid_ids)
  
  return(res)
}
