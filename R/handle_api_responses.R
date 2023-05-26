# .handle_api_response.R

.handle_api_responses <- function(entity_id_responses) { 
  invalid_entity_ids <- names(entity_id_responses[which(purrr::map(entity_id_responses, 
                                                  ~ !is.null(.$error)) == TRUE)])
  if (length(invalid_entity_ids) > 0 & 
      length(invalid_entity_ids) < length(entity_id_responses)) {
    error_messages <- unlist(purrr::map(invalid_entity_ids, 
                                        ~ as.character(entity_id_responses[[.]]$error$message)))
    names(error_messages) <- invalid_entity_ids
    error_messages <- split(names(error_messages), error_messages)
    error_messages <- unlist(purrr::map(error_messages, ~ toString(.)))
    warning(paste0(
      "The following entity identifier(s) contain errors in their API response outputs: ",
                   error_messages, ". ", ))
  
  
}


invalid_entity_ids <- names(entity_id_responses[which(purrr::map(entity_id_responses, 
                                                            ~ !is.null(.$error)) == TRUE)])
# Finds all the entity identifiers that generated an error in their response.
if (length(invalid_entity_ids) > 0 & 
    length(invalid_entity_ids) < length(entity_id_responses)) {
  # Checks for any entity identifiers that generated an error in their response
  # while making sure that every entity identifier is not invalid.
  error_messages <- unlist(purrr::map(invalid_entity_ids, ~ entity_id_responses[[.]]$error$message))
  # Extracts all the error messages found in each entity identifier with an error
  # response.
  warning(paste0("Error in API response output for ", 
                 paste0(invalid_entity_ids), ". ", 
                 paste0(error_messages), "\n"))
  # Generates a warning for each entity identifier found with an error 
  # in its response and also prints out the error message found in its 
  # response.
  entity_id_responses <- entity_id_responses[! names(entity_id_responses) %in% invalid_entity_ids]
  # Removes all entity identifiers that have an error in their response from
  # entity_id_responses.
} else if (length(invalid_entity_ids) == length(entity_id_responses)) {
  # Checks if all the entity identifiers have an error in their response.
  error_messages <- unlist(purrr::map(invalid_entity_ids, ~ entity_id_responses[[.]]$error$message))
  # Extracts all the error messages found in each entity identifier's 
  # response.
  stop(paste0("Error in API response output for ", 
              paste0(invalid_entity_ids), ". ", 
              paste0(error_messages), "\n"))
  # Stops the function and generates a warning containing an error message 
  # for each entity identifier.
}