# .handle_api_response.R

#'

.handle_api_responses <- function(entity_id_responses) { 
  invalid_entity_ids <- names(entity_id_responses[which(purrr::map(entity_id_responses, 
                                                  ~ !is.null(.$error)) == TRUE)])
  # Finds all the entity identifiers that generated an error in their response.
  if (length(invalid_entity_ids) > 0 & 
      length(invalid_entity_ids) < length(entity_id_responses)) {
    # Checks for any entity identifiers that generated an error in their response
    # while making sure that every entity identifier is not invalid.
    error_messages <- unlist(purrr::map(invalid_entity_ids, 
                             ~ as.character(entity_id_responses[[.]]$error$message)))
    # Extracts all the error messages found in each entity identifier with an error
    # response.
    names(error_messages) <- invalid_entity_ids
    # Names each error message after the entity identifier associated with it.
    error_messages <- split(names(error_messages), error_messages)
    # Groups and re-organizes the error messages so that entity identifiers with the 
    # same error message would be placed together in a list with the name for that
    # list being the error message.
    single_id_error_messages <- error_messages[which(purrr::map(error_messages, 
                                                                ~ length(.) == 1) == TRUE)]
    # Extracts all error messages that are unique to only a single entity 
    # identifier.
    single_id_error_messages <- unlist(purrr::map(names(single_id_error_messages), 
                                                  ~ paste0("Errors were found in the API response output for ",
                                                           toString(single_id_error_messages[[.]]),
                                                           " with the following error message: ", as.character(.), 
                                                           " Therefore, the entity identifier along with its ", 
                                                           "respective API response output was removed.")))
    # Rewrites the error messages associated with a single entity identifier into
    # custom messages so that they can also be utilized as warnings.
    multiple_id_error_messages <- error_messages[which(purrr::map(error_messages, 
                                                                  ~ length(.) > 1) == TRUE)]
    # Extracts all error messages that are shared by more than one entity 
    # identifier.
    multiple_id_error_messages <- unlist(purrr::map(names(multiple_id_error_messages), 
                                                           ~ paste0("Errors were found in the API response outputs for ",
                                                                    toString(multiple_id_error_messages[[.]]),
                                                                    " with the following error message: ", as.character(.), 
                                                                    " Therefore, the entity identifiers along with their ", 
                                                                    "respective API response outputs were removed.")))
    # Rewrites the error messages associated with multiple entity identifiers into
    # custom messages so that they can also be utilized as warnings.
    error_messages <- c(single_id_error_messages, multiple_id_error_messages)
    # Rewrites error_messages to store the custom messages. 
    warning(paste0(error_messages, "\n"))
    # Generates warnings using the customized error messages.
    entity_id_responses <- entity_id_responses[! names(entity_id_responses) 
                                               %in% invalid_entity_ids]
    # Removes the entity identifiers with errors in their API responses from 
    # entity_id_responses.
  
  } else if (length(invalid_entity_ids) == length(entity_id_responses)) {
    # Checks if all the entity identifiers have an error in their response.
    error_messages <- unlist(purrr::map(invalid_entity_ids, 
                                        ~ entity_id_responses[[.]]$error$message))
    # Extracts all the error messages found in each entity identifier's 
    # response.
    names(error_messages) <- invalid_entity_ids
    # Names each error message after the entity identifier associated with it.
    error_messages <- split(names(error_messages), error_messages)
    # Groups and re-organizes the error messages so that entity identifiers with the 
    # same error message would be placed together in a list with the name for that
    # list being the error message.
    single_id_error_messages <- error_messages[which(purrr::map(error_messages, 
                                                                ~ length(.) == 1) == TRUE)]
    # Extracts all error messages that are unique to only a single entity 
    # identifier.
    single_id_error_messages <- unlist(purrr::map(names(single_id_error_messages), 
                                                  ~ paste0("Errors were found in the API response output for ",
                                                           toString(single_id_error_messages[[.]]),
                                                           " with the following error message: ", as.character(.), 
                                                           " Therefore, the entity identifier along with its ", 
                                                           "respective API response output was removed.")))
    # Rewrites the error messages associated with a single entity identifier into
    # custom messages so that they can also be utilized as warnings.
    multiple_id_error_messages <- error_messages[which(purrr::map(error_messages, 
                                                                  ~ length(.) > 1) == TRUE)]
    # Extracts all error messages that are shared by more than one entity 
    # identifier.
    multiple_id_error_messages <- unlist(purrr::map(names(multiple_id_error_messages), 
                                                    ~ paste0("Errors were found in the API response outputs for ",
                                                             toString(multiple_id_error_messages[[.]]),
                                                             " with the following error message: ", as.character(.), 
                                                             " Therefore, the entity identifiers along with their ", 
                                                             "respective API response outputs were removed.")))
    # Rewrites the error messages associated with multiple entity identifiers into
    # custom messages so that they can also be utilized as warnings.
    error_messages <- c(single_id_error_messages, multiple_id_error_messages)
    # Rewrites error_messages to store the custom messages. 
    stop(glue::glue("Errors were found in all API response outputs. \n {paste0(error_messages,'\n')}"))
    # Stops the function and prints out a statement containing all the 
    # customized error messages generated for each error found.
  }
  return(entity_id_responses)
  # Returns entity_id_responses either unchanged or with all the entity 
  # identifiers containing errors in their responses removed.
}

