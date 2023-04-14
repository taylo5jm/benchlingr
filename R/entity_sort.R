# entity_sort.R

#' Sort a named character vector of entity identifiers based on whether the entity identifiers are
#' associated with valid entities, cannot be tested or are invalid

entity_sort <- function(entity_id) {
  entity_id[which(is.na(entity_id))] <- "invalid"
  entity_id_types <- unique(entity_id)
  valid_entity_id_types <- entity_id_types[entity_id_types != "invalid"]
  sorted_entity_ids <- purrr::map(entity_id_types, ~ names(entity_id[which(entity_id == .)]))
  names(sorted_entity_ids) <- entity_id_types
  
  entity_list2 <- purrr::map(entity_list, ~ .[c(2,3,4)])
  names(entity_list2) <-  unlist(purrr::map(entity_list, ~ .[1]), use.names = FALSE)
  
  
  invalid_entity_id_types <- valid_entity_id_types[which(is.na(entity_list2[valid_entity_id_types][3]))]

                                                   valid_entity_id_types == "dna_sequence"]
  purrr::map(valid_entity_id_types, ~ entity_list2[.][[1]])
  invalid_ids1 <- sorted_entity_ids[names(sorted_entity_ids) == "invalid"]
  invalid_ids2 <- 

  
  entity_indexes <- purrr::map(entity_id_types[which(entity_id_types != "invalid")], 
                               ~ which(entity_types == .))
  
  entity_indexes <- unlist(entity_indexes, use.names = FALSE)
  entity_sublist <- entity_list[entity_indexes]
  names(entity_sublist) <- purrr::map_chr(entity_sublist, ~ .[1])
  
  invalid_entities1 <- sorted_entity_ids[names(sorted_entity_ids) == "invalid"]
  invalid_entities2 <- sorted_entity_ids[names(sorted_entity_ids) != "invalid"]
  invalid_entities2
  

  
  entity
  entity_sublist <- purrr::map_chr(unique(entity_id), ~ entity_list)
  entity_id <- setNames(names(entity_id), entity_id)
  entity_id <- unique(names(entity_ids))
  
  invalid_entities <- 
}