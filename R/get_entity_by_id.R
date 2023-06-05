# .get_api_endpoints.R

#' Match each element to its corresponding Single-Get API endpoint URL format
#' 
#' .get_api_endpoints.R takes a named character vector with one or more elements 
#' where the names are entity identifiers and the elements are the entity types 
#' for those identifiers and matches each element to their respective Single-Get 
#' API endpoint URL formats according to which entity the schema correlates 
#' with.
#' 
#' @include list_api_contents.R
#' @param entity_id A named character vector where the names are entity 
#' identifiers and the elements are the entity types associated with those 
#' identifiers.
#' @param tenant A character string containing the name of the Benchling tenant 
#' for which we want to use to extract information for at least one entity or 
#' element. 
#' @return A named character vector where the names are entity identifiers and 
#' the elements are the Single-Get API endpoint URL formats associated with them 
#' based on the respective entities each identifier correlates to and matches 
#' with.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf", "box_7YutniM0")
#' inferred_id1 <- infer_entity_type(entity_id=entity_id1, tenant=
#' "hemoshear-dev")
#' res1 <- .get_api_endpoints(entity_id=inferred_id1, tenant="hemoshear-dev")
#' 
#' entity_id2 <- c("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", 
#' "bfi_Ks908uWV", "ent_Ec76qX9f", "ent_sPrxBNOh", "box_K9950IQ8")
#' inferred_id2 <- infer_entity_type(entity_id=entity_id2, tenant=
#' "hemoshear-dev")
#' res2 <- .get_api_endpoints(entity_id=inferred_id2, tenant="hemoshear-dev")
#' }
#' @keywords internal

.get_api_endpoints <- function(entity_id, tenant) {
  entity_list <- .list_api_contents(tenant=tenant, contents="all") 
    # Defines entity_list using .list_api_contents.R.
  
  entity_elements <- purrr::map(entity_list, ~ .[c(2,3,4)]) 
    # Extracts API contents for each entity.
  
  names(entity_elements) <- purrr::map_chr(entity_list, ~ .[1])
    # Names API contents after the entity names they are referenced under.
  
  entity_single_get_endpoints <- sapply(entity_id, function(x) 
    entity_elements[[x]][2]) 
    # Extracts the Single-Get API endpoint contents associated with each entity
    # identifier in 'entity_id' input from 'entity_elements.'
  
  return(entity_single_get_endpoints)
}

# .craft_api_queries.R

#' Format each element using its entity identifier's name to generate proper 
#' Single-Get API endpoint URLs for that identifier
#' 
#' .craft_api_queries.R takes a named character vector with one or more elements 
#' where the names are entity identifiers and the elements are each identifier's 
#' respective Single-Get API endpoint URL formats and rewrites the element using 
#' the name of the identifier to generate proper Single-Get API endpoint URLs.
#' 
#' @param entity_single_get_endpoints A named character vector where the names 
#' are entity identifiers and the elements are the Single-Get API endpoint URL 
#' formats for each identifier.
#' @return A named character vector where the names are entity identifiers and 
#' the elements are the respective Single-Get API endpoint URLs for each 
#' identifier.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf", "box_7YutniM0", 
#' "bfi_Ks908uWV")
#' 
#' inferred_id <- infer_entity_type(entity_id=entity_id, tenant="hemoshear-dev")
#' 
#' entity_single_get_endpoints <- .get_api_endpoints(entity_id=inferred_id,
#' tenant="hemoshear-dev")
#' 
#' res <- .craft_api_queries(entity_single_get_endpoints=
#' entity_single_get_endpoints)
#' }
#' @keywords internal

.craft_api_queries <- function(entity_single_get_endpoints) {
  api_queries <- sapply(names(entity_single_get_endpoints),
    function(x) gsub("ENTITY_ID", x, entity_single_get_endpoints[x])) 
    # Prepares single-get API endpoint URL queries for each entity identifier 
    # not assigned with a value of NA.
  
  names(api_queries) <- names(entity_single_get_endpoints) 
    # Re-assigns entity identifiers as names to each corresponding URL query.
  
  return(api_queries)
}

# .submit_queries.R

#' Uses each element to call the Single-Get API endpoint in Benchling for a 
#' specific entity identifier.
#' 
#' .submit_queries.R takes a named character vector with one or more elements 
#' where the names are entity identifiers and the elements are each identifier's 
#' respective Single-Get API endpoint URL and uses them to call that 
#' identifier's Single-Get API endpoint in Benchling and extract the response 
#' contents.
#' 
#' @param api_queries A named character vector where the names are entity 
#' identifiers and the elements are the Single-Get API endpoint URLs for each 
#' identifier.
#' @param api_key Benchling API key. 
#' @return A named list where the names are entity identifiers and each element 
#' is a list that contains the response contents from the call made to that 
#' identifier's Single-Get API endpoint in Benchling.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf", "box_7YutniM0", 
#' "bfi_Ks908uWV", "box_Api455op", "box_t99a7IQ8", "con_m1dmbdV8", 
#' "bfi_ztXInwdh", "bfi_Y7ORWDSz", "bfi_vrXl1zIm", "etr_lu8wTLfL", 
#' "etr_9012RkiU")
#' 
#' inferred_id <- infer_entity_type(entity_id=entity_id, tenant="hemoshear-dev")
#' 
#' entity_single_get_endpoints <- .get_api_endpoints(entity_id=inferred_id,
#' tenant="hemoshear-dev")
#' 
#' api_queries <- .craft_api_queries(entity_single_get_endpoints=
#' entity_single_get_endpoints)
#' 
#' res <- .submit_queries(api_queries=api_queries, 
#' api_key=Sys.getenv("BENCHLING_API_KEY"))
#' }
#' @keywords internal

.submit_queries <- function(api_queries, api_key=
                            Sys.getenv("BENCHLING_API_KEY")) {
  if (api_key == "") { 
    # Checks if the benchling api key is valid or not.
    stop("Benchling API key is missing or empty.")
    # Stops the function if the Benchling API Key is missing or empty.
  }
  
  response_list <- purrr::map(api_queries, ~ 
    httr::content(httr::GET(., httr::authenticate(api_key, '')))) 
    # Sends each URL query in the input to benchling and retrieves the response 
    # for each one. 
  
  names(response_list) <- names(api_queries) 
    # Re-assigns the names using the names of the entity identifiers.
  
  return(response_list)
}

# .handle_api_response.R

#' Check each element's API response and remove any elements that have errors in
#' their responses.
#' 
#' .handle_api_response.R takes a named list where the names are entity 
#' identifiers and the elements are each identifier's Single-Get API endpoint 
#' response contents extracted from Benchling and analyzes it to see if any 
#' entity identifiers contain errors in their Single-Get API endpoint responses 
#' and removes them from the list.
#' 
#' @param entity_id_responses A named list where the names are entity 
#' identifiers and the elements are each entity identifier's Single-Get API 
#' endpoint response contents from Benchling.
#' @return A named list where the names are valid entity identifiers that do not 
#' possess errors in their Single-Get API endpoint response contents and the
#' elements are said identifier's Single-Get API endpoint response contents.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf", "box_7YutniM0", 
#' "bfi_Ks908uWV", "box_Api455op", "box_t99a7IQ8", "con_m1dmbdV8", 
#' "bfi_ztXInwdh", "bfi_Y7ORWDSz", "bfi_vrXl1zIm", "etr_lu8wTLfL", 
#' "etr_9012RkiU")
#' 
#' inferred_id <- infer_entity_type(entity_id=entity_id, tenant="hemoshear-dev")
#' 
#' entity_single_get_endpoints <- .get_api_endpoints(entity_id=inferred_id,
#' tenant="hemoshear-dev")
#' 
#' api_queries <- .craft_api_queries(entity_single_get_endpoints=
#' entity_single_get_endpoints)
#' 
#' entity_id_responses <- .submit_queries(api_queries=api_queries, 
#' api_key=Sys.getenv("BENCHLING_API_KEY"))
#' 
#' res <- .handle_api_responses(entity_id_responses=entity_id_responses)
#' }
#' @keywords internal

.handle_api_responses <- function(entity_id_responses) { 
  invalid_entity_ids <- names(entity_id_responses[which(purrr::map(
    entity_id_responses, ~ !is.null(.$error)) == TRUE)])
    # Finds all the entity identifiers that generated an error in their 
    # response.
  
  if (length(invalid_entity_ids) > 0 & 
      length(invalid_entity_ids) < length(entity_id_responses)) {
      # Checks for any entity identifiers that generated an error in their 
      # response while making sure that every entity identifier is not invalid.
    error_messages <- unlist(purrr::map(invalid_entity_ids, 
      ~ as.character(entity_id_responses[[.]]$error$message)))
      # Extracts all the error messages found in each entity identifier with an 
      # error response.
    names(error_messages) <- invalid_entity_ids
      # Names each error message after the entity identifier associated with it.
    error_messages <- split(names(error_messages), error_messages)
      # Groups and re-organizes the error messages so that entity identifiers 
      # with the same error message would be placed together in a list with the 
      # name for that list being the error message.
    single_id_error_messages <- error_messages[which(purrr::map(error_messages, 
      ~ length(.) == 1) == TRUE)]
      # Extracts all error messages that are unique to only a single entity 
      # identifier.
    single_id_error_messages <- unlist(purrr::map(
      names(single_id_error_messages), ~ paste0("Errors were found in the API ",
      "response output for ", toString(single_id_error_messages[[.]]), " with ",
      "the following error message: ", as.character(.), " Therefore, the ",
      "entity identifier along with its respective API response output was ",
      "removed.")))
      # Rewrites the error messages associated with a single entity identifier 
      # into custom messages so that they can also be utilized as warnings.
    multiple_id_error_messages <- error_messages[which(purrr::map(
      error_messages, ~ length(.) > 1) == TRUE)]
      # Extracts all error messages that are shared by more than one entity 
      # identifier.
    multiple_id_error_messages <- unlist(purrr::map(
      names(multiple_id_error_messages), ~ paste0("Errors were found in the ",
      "API response outputs for ", toString(multiple_id_error_messages[[.]]), 
      " with the following error message: ", as.character(.), " Therefore, ",
      "the entity identifiers along with their respective API response ",
      "outputs were removed.")))
      # Rewrites the error messages associated with multiple entity identifiers 
      # into custom messages so that they can also be utilized as warnings.
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
      # Groups and re-organizes the error messages so that entity identifiers 
      # with the same error message would be placed together in a list with the 
      # name for that list being the error message.
    single_id_error_messages <- error_messages[which(purrr::map(error_messages, 
      ~ length(.) == 1) == TRUE)]
      # Extracts all error messages that are unique to only a single entity 
      # identifier.
    single_id_error_messages <- unlist(purrr::map(
      names(single_id_error_messages), ~ paste0("Errors were found in the API ",
      "response output for ", toString(single_id_error_messages[[.]]), " with ",
      "the following error message: ", as.character(.))))
      # Rewrites the error messages associated with a single entity identifier 
      # into custom messages so that they can also be utilized as warnings.
    multiple_id_error_messages <- error_messages[which(purrr::map(
      error_messages, ~ length(.) > 1) == TRUE)]
      # Extracts all error messages that are shared by more than one entity 
      # identifier.
    multiple_id_error_messages <- unlist(purrr::map(
      names(multiple_id_error_messages), ~ paste0("Errors were found in the ",
      "API response outputs for ", toString(multiple_id_error_messages[[.]]),
      " with the following error message: ", as.character(.))))
      # Rewrites the error messages associated with multiple entity identifiers 
      # into custom messages so that they can also be utilized as warnings.
    error_messages <- c(single_id_error_messages, multiple_id_error_messages)
      # Rewrites error_messages to store the custom messages. 
    stop(cat("Errors were found in all API response outputs.\n",
      paste0(error_messages,'\n')))
      # Stops the function and prints out a statement containing all the 
      # customized error messages generated for each error found.
  }
  return(entity_id_responses)
    # Returns entity_id_responses either unchanged or with all the entity 
    # identifiers containing errors in their responses removed.
}

# .bundle_api_results.R

#' Generate a named list of tibble-formatted data tables using the response 
#' contents from Benchling for specific entity identifiers.
#' 
#' .bundle_api_results.R takes a named list where the names are entity 
#' identifiers and each element is a list containing that identifier's 
#' Single-Get API endpoint response contents from Benchling and sorts the data 
#' into a named list where the names are the collective entity types for all of 
#' the entity identifiers in the input and each element is also a named list 
#' where the names are entity schema names and the elements are tibble-formatted 
#' data tables containing the Single-Get API endpoint response contents data for 
#' each entity identifier associated with a specific schema name and entity 
#' type.
#' 
#' @include infer_entity_type.R
#' @param entity_responses A named list where the names are entity identifiers 
#' and each element is a list that contains the response contents from the call 
#' made to that identifier's Single-Get API endpoint in Benchling.
#' @param tenant A character string containing the name of the Benchling tenant 
#' for which we want to use to extract information for at least one entity or 
#' element.
#' @return A named list where the names are entity types and each element is 
#' another named list where the names are entity schema names and each element 
#' is a tibble-formatted data table displaying the Single-Get API endpoint 
#' response contents data extracted Benchling for each entity identifier 
#' corresponding to a specific entity schema name and entity type.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0bmCm", "sfs_VUhew7oD", "box_7YutniM0",
#' "bfi_Ks908uWV", "box_Api455op", "box_t99a7IQ8", "con_m1dmbdV8",
#' "bfi_ztXInwdh", "bfi_Y7ORWDSz", "bfi_vrXl1zIm", "etr_lu8wTLfL",
#' "etr_9012RkiU")
#' 
#' inferred_id <- infer_entity_type(entity_id=entity_id, tenant="hemoshear-dev")
#' 
#' entity_single_get_endpoints <- .get_api_endpoints(entity_id=inferred_id,
#' tenant="hemoshear-dev")
#' 
#' api_queries <- .craft_api_queries(entity_single_get_endpoints=
#' entity_single_get_endpoints)
#' 
#' entity_responses <- .submit_queries(api_queries=api_queries,
#' api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
#'
#' res <- .bundle_api_results(entity_responses=entity_responses tenant=
#' "hemoshear-dev")
#' }
#' @keywords internal

.bundle_api_results <- function(entity_responses, tenant) {
  entity_ids <- names(entity_responses) 
    # Extracts a vector containing the entity identifiers in the input.
  
  inferred_entity_types <- infer_entity_type(entity_id=entity_ids,
    tenant=tenant) 
    # Infers the entity types for the entity identifiers. 
  
  entity_id_schema_ids <- purrr::map(entity_responses, ~ .$schema$id) 
    # Extracts the entity schema ids in each entity identifier's response if 
    # listed.
  
  entity_id_schema_ids[which(purrr::map(entity_id_schema_ids, 
    ~ is.null(.)) == TRUE)] <- "none" 
    # Generates a value of "none" for each entity identifier's schema id that
    # have a value of NULL.
  
  entity_id_schema_ids <- unlist(entity_id_schema_ids) 
    # Converts entity_id_schema_ids into a vector.
  
  entity_id_schema_names <- purrr::map(entity_responses, ~ .$schema$name) 
    # Extracts the entity schema names in each entity identifier's response if 
    # listed.
  
  entity_id_schema_names[which(purrr::map(entity_id_schema_names, 
    ~ is.null(.)) == TRUE)] <- inferred_entity_types[which(purrr::map(
    entity_id_schema_names, ~ is.null(.)) == TRUE)] 
    # Assigns to each entity identifier that do not have an error value in their 
    # response but still feature a value of NULL for their schema name their 
    # respective entity type as a substitute value for their schema name.
  
  entity_id_schema_names <- unlist(entity_id_schema_names) 
    # Converts entity_id_schema_names into a vector.
  
  .api_response_sort <- function(entity_id_response) { 
    # Creates a function that modifies each entity identifier's API response.
    if (length(names(entity_id_response)) == 1) {
      if (names(entity_id_response) == "entry") {
        entity_id_response <- entity_id_response$entry[which(
        names(entity_id_response$entry) != "days")]
          # Removes the "days" element from an entity identifier's entry 
          # attribute due to how repetitive the information is and how it cannot 
          # be organized in a unique way when integrating the information into a 
          # data frame or a table.
      }
    }
    entity_id_response <- unlist(entity_id_response) 
      # Unlists each entity identifier's response. 
    entity_id_response <- entity_id_response[! (substr(names(entity_id_response), 
                                                       1, 7) == "fields." & 
                                                substr(names(entity_id_response), 
                                                       nchar(names(entity_id_response))-5, 
                                                       nchar(names(entity_id_response))) != ".value" & 
                                                substr(names(entity_id_response), 
                                                       nchar(names(entity_id_response))-4,
                                                       nchar(names(entity_id_response))) != ".type")]
      # Extracts only the type and value elements from each entity identifier's 
      # field attribute.
    names(entity_id_response) <- gsub("fields.", "", names(entity_id_response)) 
      # Removes "fields." from the names of each entity identifier's 
      # response attributes.
    names(entity_id_response) <- gsub("registrationOrigin.", "", 
                                      names(entity_id_response)) 
      # Removes "registrationOrigin." from the names of each entity identifier's 
      # response attributes.
    names(entity_id_response) <- gsub("\\.", "_", names(entity_id_response)) 
      # Replaces all instances of "." in the names of each entity identifier's 
      # response attributes with "_".
    entity_id_response[which(grepl("^\\s*$", entity_id_response))] <- NA
      # Replaces all blank values in each entity identifier's response with a 
      # value of NA.
    return(entity_id_response)
  }
  
  entity_responses <- purrr::map(entity_responses, ~ .api_response_sort(.)) 
    # Runs each entity identifier's response through the .api_response_sort() 
    # function in order to modify them.
  
  entity_responses <- purrr::map(entity_responses, 
    ~ .[names(.) != "id" & names(.) != "schema_id" & 
        names(.) != "schema_name" & names(.) != "error_invalidId"])
  
  entity_responses <- purrr::map(entity_ids, 
    ~ c("id" = ., "entityType" = inferred_entity_types[[.]], 
        "schema_id" = entity_id_schema_ids[[.]], 
        "schema_name" = entity_id_schema_names[[.]], entity_responses[[.]]))
    # Adds to the beginning of each entity identifier's API response their 
    # entity id, entity type and schema id and schema name as defined by 
    # entity_id_schema_ids and entity_id_schema_names after removing all prior 
    # instances of their entity id, schema id and schema name when referenced 
    # using "id," "error_invalidId," "schema_id" and "schema_name".
  
  names(entity_responses) <- entity_ids 
    # Renames each entity identifier's response using their entity id.
  
  entity_response_attributes <- purrr::map(entity_responses, ~ names(.))
    # Extracts the response attributes for each entity identifier's response.
  
  sorted_entity_types <- split(names(inferred_entity_types), 
    inferred_entity_types) 
    # Sorts the entity identifiers according to which entity type they belong 
    # to.
  
  sorted_entity_responses <- purrr::map(sorted_entity_types, 
    ~ entity_id_schema_names[which(names(entity_id_schema_names) %in% .)]) 
    # Matches each entity identifier listed underneath an entity type with their 
    # respective entity schema name.
  
  sorted_entity_responses <- purrr::map(sorted_entity_responses, 
    ~ split(names(.), .))
    # Further organizes sorted_entity_responses by grouping all the entity 
    # identifiers listed under a certain entity type by their entity schema 
    # name.
  
  sorted_entity_response_attributes <- lapply(sorted_entity_responses, 
    function(x) purrr::map(x, ~ unique(unlist(entity_response_attributes[
    which(names(entity_response_attributes) %in% .)], use.names = FALSE))))
    # Organizes the response attributes for entity identifiers with the same 
    # entity type and schema name and sorts them together.
  
  sorted_entity_id_response_attributes <- lapply(entity_ids, function(x)
    names(unlist(sorted_entity_responses, recursive = FALSE)[which(purrr::map(
    unlist(sorted_entity_responses, recursive = FALSE), 
    ~ any(. %in% x)) == TRUE)]))
    # Generates a list called sorted_entity_id_response_attributes which 
    # attaches the sorted response attributes for entity identifiers with the 
    # same entity type and schema name to its corresponding entity identifier.
  
  names(sorted_entity_id_response_attributes) <- entity_ids
    # Names each list of sorted response attributes with the name of the entity
    # identifier it is associated with.
  
  sorted_entity_id_response_attributes <- purrr::map(
    sorted_entity_id_response_attributes, ~ 
    unlist(sorted_entity_response_attributes, recursive = FALSE)[[.]])
    # Attaches to each entity identifier in entity_ids a vector containing the
    # collective and unique response attributes found for not only that specific
    # entity identifier but also for all entity identifiers with the same entity
    # type and schema name.
  
  entity_responses <- purrr::map(entity_ids, 
   ~ entity_responses[[.]][sorted_entity_id_response_attributes[[.]]])
    # Runs each entity identifier through all the response attributes possessed 
    # by not only it but also any other entity identifier with the same entity 
    # type and schema name.
  
  names(entity_responses) <- entity_ids
    # Renames the response outputs for each entity identifier by the names of 
    # the entity identifiers.  
  
  entity_responses <- purrr::map(entity_ids,
    ~ setNames(entity_responses[[.]], 
               sorted_entity_id_response_attributes[[.]]))
    # Names each element in each entity identifier's response output after its 
    # respective response attribute.
  
  names(entity_responses) <- entity_ids
    # Renames the response outputs for each entity identifier again by the names 
    # of the entity identifiers since the previous command removes the names.
  
  res <- lapply(sorted_entity_responses, function(x) 
    purrr::map(x, ~ tibble::as_tibble(as.data.frame(do.call(rbind, 
                    entity_responses[which(names(entity_responses) 
                    %in% .)])))))
    # Matches each entity identifier listed for a specific schema name with 
    # their API response data and re-organizes it all into a single 
    # tibble-formatted data table where each row shows the API response data for 
    # a specific entity identifier matched to a specific schema name for a 
    # certain entity type. 
  
  return(res)
}

# get_entity_by_id.R

#' Calls each element's Single-Get API endpoints in Benchling and extracts the 
#' response.
#' 
#' get_entity_by_id.R takes a character vector with one or more elements and for 
#' each element calls its Single-Get API endpoints in Benchling and extracts the 
#' response contents and stores it in a list.
#' 
#' @include infer_entity_type.R
#' @param entity_id A character vector with 1 or more elements.
#' @param api_key Benchling API key. 
#' @param tenant A character string containing the name of the Benchling tenant 
#' for which we want to use to extract information for at least one entity or 
#' element. 
#' @param bundle A boolean that determines how the output is organized.
#' If `TRUE` is selected, the output will be organized into a named list where 
#' the names are entity types and each element is a named list of 
#' tibble-formatted data tables where the names are schema names and each data
#' table displays the API response contents output extracted from Benchling for 
#' each entity identifier inside. If `FALSE`, then the output will be made into 
#' a named list where the names are entity identifiers and each element contains
#' the API response contents from the call made to that identifier's Single-Get 
#' API endpoint in Benchling.
#' @return Either a named list where the names are entity identifiers and the 
#' elements are each identifier's API response contents extracted from 
#' Benchling or a named list where the names are entity types and each element 
#' is a named list of tibble-formatted tables where the names are schema names
#' and each table features the API response contents for the identifiers 
#' specified under both the entity type and schema name.
#' @examples \dontrun{
#' entity_id <- c("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", 
#' "bfi_Ks908uWV", "ent_Ec76qX9f", "ent_sPrxBNOh", "box_K9950IQ8", 
#' "box_923aklum", "ver_asd89230", "ysq_983mnK4e")
#' res <- get_entity_by_id(entity_id=entity_id, 
#' api_key=Sys.getenv("BENCHLING_API_KEY"), tenant="hemoshear-dev",
#' bundle=TRUE)
#' }
#' @export

get_entity_by_id <-  function(entity_id, 
                              api_key=Sys.getenv("BENCHLING_API_KEY"),
                              tenant, bundle=TRUE) {
  inferred_entity_ids <- infer_entity_type(entity_id=entity_id, 
                                           tenant=tenant) 
    # Infers the entity schemas for each element.
  
  invalid_entity_ids <- names(inferred_entity_ids[
    which(is.na(inferred_entity_ids))]) 
    # Finds all the invalid entity identifiers whose values are NA.
  
  if (length(invalid_entity_ids) == 1 &
      length(invalid_entity_ids) < length(inferred_entity_ids)) { 
    # Checks if only a single entity identifier is invalid while making sure 
    # that not every entity identifier is invalid.
    warning(paste0("The following entity identifier cannot be matched to an ",
      "entity type and has therefore been removed: ",
      toString(invalid_entity_ids), "."))
      # Generates a warning for any invalid entity identifier found.
    inferred_entity_ids <- inferred_entity_ids[! names(inferred_entity_ids) 
      %in% invalid_entity_ids]
      # Removes the invalid entity identifier from inferred_entity_ids.
  
  } else if (length(invalid_entity_ids) > 1 & 
             length(invalid_entity_ids) < length(inferred_entity_ids)) { 
      # Checks if multiple entity identifiers are invalid while making sure that not 
      # every entity identifier is invalid.
    warning(paste0("The following entity identifiers cannot be matched to an ",
      "entity type and have therefore been removed: ",
      toString(invalid_entity_ids), "."))
      # Generates a warning for any invalid entity identifiers found.
    inferred_entity_ids <- inferred_entity_ids[! names(inferred_entity_ids) 
      %in% invalid_entity_ids]
      # Removes the invalid entity identifiers from inferred_entity_ids.
  
  } else if (length(invalid_entity_ids) == length(inferred_entity_ids)) {
      # Checks if all the entity identifiers are invalid.
    stop(paste0("None of the entity identifiers can be matched to an entity ",
      "type and are therefore all invalid."))
      # Stops the function.
  }
  entity_single_get_endpoints  <- .get_api_endpoints(entity_id=
    inferred_entity_ids, tenant=tenant) 
    # Extracts the Single-Get API endpoint URL format for each entity identifier 
    # according to its respective schema element.
  
  api_queries <- .craft_api_queries(entity_single_get_endpoints=
                                    entity_single_get_endpoints) 
    # Rewrites the Single-Get API endpoint URL formats into proper URL 
    # strings for each identifier.
  
  response_list <- .submit_queries(api_queries=api_queries, 
                                   api_key=api_key) 
    # Uses the URL strings to call the entity identifier's Single-Get 
    # API endpoint in Benchling and extracts the contents of the response.
  
  final_response_list <- .handle_api_responses(entity_id_responses=
    response_list)
    # Locates any entity identifiers with errors in their API response contents
    # and removes them from the list.
  
  if (bundle) {
    # Checks if bundle is left as default or defined as true.
    final_response_list <- .bundle_api_results(
      entity_responses=final_response_list, tenant=tenant)
      # Converts final_response_list into a named list where the names are 
      # entity types and each element is a named list of tibble-formatted data
      # data tables where the names are schema names and the tables show the API
      # response contents data for entity identifiers.
  }
  
  return(final_response_list)
}

