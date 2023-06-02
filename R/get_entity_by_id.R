# .get_api_endpoints.R

#' Match each element to its corresponding Single-Get API endpoint URL format
#' 
#' .get_api_endpoints.R takes a named character vector with one or more elements 
#' where the names are entity identifiers and the elements are the entity types 
#' for those identifiers and matches each element to their respective Single-Get 
#' API endpoint URL formats according to which entity the schema correlates 
#' with.
#' 
#' @param entity_id A named character vector where the names are entity 
#' identifiers and the elements are the entity types associated with those 
#' identifiers.
#' @param entity_list A list of vectors where each vector designates the schema 
#' type and API options for the entities we are interested in and the names are 
#' the first characters seen in the identifiers for each entity. If NULL 
#' (the default), the function will use a default list.
#' 
#' Alternatively, a custom or more specified list can be used based on the 
#' purpose and intent of the list and overall application of the function.
#' 
#' @return A named character vector where the names are entity identifiers and 
#' the elements are the Single-Get API endpoint URL formats associated with them 
#' based on the respective entities each identifier correlates to and matches 
#' with.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm"="dna_sequence", "bfi_Q1PMlXkf"=
#' "custom_entity", "box_7YutniM0"="box")
#' res1 <- .get_api_endpoints(entity_id=entity_id1, entity_list=NULL)
#' 
#' entity_id2 <- c("bfi_Ur5DfvGJ"="custom_entity", "seq_Gju61mCm"="dna_sequence", 
#' "bfi_Q13AlXkf"="custom_entity", "bfi_Ks908uWV"="custom_entity", 
#' "ent_Ec76qX9f"="user", "ent_sPrxBNOh"="user", "box_K9950IQ8"="box")
#' res2 <- .get_api_endpoints(entity_id=entity_id2, entity_list=NULL)
#' }
#' @keywords internal

.get_api_endpoints <- function(entity_id, entity_list=NULL) {
  if (is.null(entity_list)) { 
    # Checks if entity_list has not been defined.
    
    entity_list <- .list_api_contents(contents="all") 
    # Defines entity_list if left as NULL using .list_api_contents.R.
  }
  
  entity_elements <- purrr::map(entity_list, ~ .[c(2,3,4)]) 
  # Extracts API contents for each entity.
  
  names(entity_elements) <- purrr::map_chr(entity_list, ~ .[1]) 
  
  entity_single_get_endpoints <- sapply(entity_id, function(x) 
                                        entity_elements[[x]][2]) 
  
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
#' entity_single_get_endpoints <- c(
#' "seq_Cuf0bmCm"=
#' "https://hemoshear-dev.benchling.com/api/v2/dna-sequences/ENTITY_ID", 
#' "bfi_Q1PMlXkf"=
#' "https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID", 
#' "box_7YutniM0"=
#' "https://hemoshear-dev.benchling.com/api/v2/boxes/ENTITY_ID",
#' "bfi_Ks908uWV"=
#' "https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID")
#' res <- .craft_api_queries(entity_single_get_endpoints=
#'                           entity_single_get_endpoints)
#' }
#' @keywords internal

.craft_api_queries <- function(entity_single_get_endpoints) {
  api_queries <- sapply(names(entity_single_get_endpoints),
                        function(x) gsub("ENTITY_ID", x, 
                                         entity_single_get_endpoints[x])) 
  # Prepares single-get API endpoint 
  # URL queries for each entity 
  # identifier not assigned with a 
  # value of NA.
  names(api_queries) <- names(entity_single_get_endpoints) # Re-assigns entity identifiers as names
  # to each corresponding URL query.
  return(api_queries)
}

# .submit_queries.R

#' Uses each element to call the Single-Get API endpoint in Benchling for a specific entity identifier.
#' 
#' .submit_queries.R takes a named character vector with one or more elements where the names are entity 
#' identifiers and the elements are each identifier's respective Single-Get API endpoint URL and uses them 
#' to call that identifier's Single-Get API endpoint in Benchling and extract the response contents.
#' 
#' @param api_queries A named character vector where the names are entity identifiers and the elements are 
#' the Single-Get API endpoint URLs for each identifier.
#' @param api_key Benchling API key. 
#' @return A named list where the names are entity identifiers and each element is a list that contains the 
#' response contents from the call made to that identifier's Single-Get API endpoint in Benchling.
#' @examples \dontrun{
#' api_queries <- c(
#' "seq_Cuf0bmCm"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences/seq_Cuf0bmCm", 
#' "bfi_Q1PMlXkf"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/bfi_Q1PMlXkf", 
#' "box_7YutniM0"="https://hemoshear-dev.benchling.com/api/v2/boxes/box_7YutniM0",
#' "bfi_Ks908uWV"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/bfi_Ks908uWV")
#' res <- .submit_queries(api_queries=api_queries, api_key=Sys.getenv("BENCHLING_API_KEY"))
#' }
#' @keywords internal

.submit_queries <- function(api_queries, api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (api_key == "") { # Check if the benchling api key is valid or not
    stop("Benchling API key is missing or empty.")
  }
  
  response_list <- purrr::map(api_queries, ~ 
                                httr::content(httr::GET(., httr::authenticate(api_key, '')))) # Sends each URL query in the input to benchling 
  # and retrieves the response for each one. 
  names(response_list) <- names(api_queries) # Re-assigns the names using the names of the entity identifiers.
  
  return(response_list)
}

# .bundle_api_results.R

#' Generate a named list of tibble-formatted data tables using the response 
#' contents from Benchling for specific entity identifiers.
#' 
#' .bundle_api_results.R takes a named list where the names are entity 
#' identifiers and each element is a list containing that identifier's 
#' Single-Get API endpoint response contents from Benchling and sorts the 
#' data into a named list where the names are the collective entity types 
#' for all of the entity identifiers in the input and each element is also a 
#' named list where the names are entity schema names and the elements are 
#' tibble-formatted data tables containing the Single-Get API endpoint response 
#' contents data for each entity identifier associated with a specific schema 
#' name and entity type.
#' 
#' @include infer_entity_type.R
#' @param entity_responses A named list where the names are entity identifiers 
#' and each element is a list that contains the response contents from the call 
#' made to that identifier's Single-Get API endpoint in Benchling.
#' @return A named list where the names are entity types and each element is 
#' another named list where the names are entity schema names and each element 
#' is a tibble-formatted data table displaying the Single-Get API endpoint 
#' response contents data extracted Benchling for each entity identifier 
#' corresponding to a specific entity schema name and entity type.
#' @examples \dontrun{
#' entity_responses <- get_entity(entity_id = c("con_m1dmbdV8", "bfi_Ur5DfvGJ", 
#' "seq_Cuf0bmCm", "bfi_Q13AlXkf", "ent_Ec76qX9f", "ent_sPrxBNOh"), 
#' api_key=Sys.getenv("BENCHLING_API_KEY"))
#' res <- .bundle_api_results(entity_responses=entity_responses)
#' }
#' @keywords internal

.bundle_api_results <- function(entity_responses) {
  entity_ids <- names(entity_responses) 
  # Extracts a vector containing the entity identifiers in the input.
  
  inferred_entity_types <- infer_entity_type(entity_id=entity_ids,entity_list=NULL) 
  # Infers the entity types for the entity identifiers. 
  
  entity_id_schema_ids <- purrr::map(entity_responses, ~ .$schema$id) 
  # Extracts the entity schema ids in each entity identifier's response if listed.
  entity_id_schema_ids[which(purrr::map(entity_id_schema_ids, 
                                        ~ is.null(.)) == TRUE)] <- "none" 
  # Generates a value of "none" for each entity identifier's schema id that have 
  # a value of NULL.
  entity_id_schema_ids <- unlist(entity_id_schema_ids) 
  # Converts entity_id_schema_ids into a vector.
  
  entity_id_schema_names <- purrr::map(entity_responses, ~ .$schema$name) 
  # Extracts the entity schema names in each entity identifier's response 
  # if listed.
  entity_id_schema_names[which(purrr::map(entity_id_schema_names, 
                                          ~ is.null(.)) == TRUE)] <- inferred_entity_types[which(purrr::map(
                                            entity_id_schema_names, ~ is.null(.)) == TRUE)] 
  # Assigns to each entity identifier that do not have an error value in 
  # their response but still feature a value of NULL for their schema name 
  # their respective entity type as a substitute value for their schema name.
  entity_id_schema_names <- unlist(entity_id_schema_names) 
  # Converts entity_id_schema_names into a vector.
  
  .api_response_sort <- function(entity_id_response) { 
    # Creates a function that modifies each entity identifier's API response.
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
    entity_id_response[entity_id_response == '' | 
                         entity_id_response == ' ' | 
                         entity_id_response == "" | 
                         entity_id_response == " "] <- NA 
    return(entity_id_response)
    # Replaces all '', ' ', "" and " " values in each entity 
    # identifier's response with a value of NA.
  }
  
  entity_responses <- purrr::map(entity_responses, ~ .api_response_sort(.)) 
  # Runs each entity identifier's response through the .api_response_sort() 
  # function in order to modify them.
  entity_responses <- purrr::map(entity_responses, 
                                 ~ .[names(.) != "id" & names(.) != "schema_id" & 
                                       names(.) != "schema_name"])
  entity_responses <- purrr::map(entity_ids, ~ c("id" = ., 
                                                 "entityType" = inferred_entity_types[[.]],
                                                 "schema_id" = entity_id_schema_ids[[.]],
                                                 "schema_name" = entity_id_schema_names[[.]],
                                                 entity_responses[[.]]))
  # Adds to the beginning of each entity identifier's API response their 
  # entity id, entity type and schema id and schema name as defined by 
  # entity_id_schema_ids and entity_id_schema_names after removing all prior 
  # instances of their entity id, schema id and schema name when referenced 
  # using "id," "error_invalidId," "schema_id" and "schema_name".
  
  names(entity_responses) <- entity_ids 
  # Renames each entity identifier's response using their entity id.
  
  sorted_entity_types <- split(names(inferred_entity_types), 
                               inferred_entity_types) 
  # Sorts the entity identifiers according to which entity type they belong to.
  sorted_entity_responses <- purrr::map(sorted_entity_types, 
                                        ~ entity_id_schema_names[which(names(entity_id_schema_names) 
                                                                       %in% .)]) 
  # Matches each entity identifier listed underneath an entity type with their 
  # respective entity schema name.
  sorted_entity_responses <- purrr::map(sorted_entity_responses, 
                                        ~ split(names(.), .))
  # Further organizes sorted_entity_responses by grouping all the entity 
  # identifiers listed under a certain entity type by their entity schema name.
  
  res <- lapply(sorted_entity_responses, function(x) 
    purrr::map(x, ~ tibble::as_tibble(as.data.frame(do.call(rbind, 
                                                            entity_responses[which(names(entity_responses) %in% .)])))))
  # Matches each entity identifier listed for a specific schema name with their 
  # API response data and re-organizes it all into a single tibble-formatted 
  # data table where each row shows the API response data for a specific entity 
  # identifier matched to a specific schema name for a certain entity type. 
  
  return(res)
}

# get_entity_by_id.R

#' Calls each element's Single-Get API endpoints in Benchling and extracts the response.
#' 
#' get_entity_by_id.R takes a character vector with one or more elements and for 
#' each element calls its Single-Get API endpoints in Benchling and extracts the 
#' response contents and stores it in a list.
#' 
#' @include infer_entity_type.R
#' @param entity_id A character vector with 1 or more elements.
#' @param api_key Benchling API key. 
#' @return A named list where the names are entity identifiers and each element is a list that contains
#' the response contents from the call made to that identifier's Single-Get API endpoint in Benchling.
#' @examples \dontrun{
#' entity_id <- c("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", "bfi_Ks908uWV", "ent_Ec76qX9f", 
#' "ent_sPrxBNOh", "box_K9950IQ8", "box_923aklum", "ver_asd89230", "ysq_983mnK4e")
#' res <- get_entity_by_id(entity_id=entity_id, api_key=Sys.getenv("BENCHLING_API_KEY"))
#' }
#' @export

get_entity_by_id <-  function(entity_id, api_key=Sys.getenv("BENCHLING_API_KEY")) {
  inferred_entity_ids <- infer_entity_type(entity_id=entity_id, entity_list=NULL) 
    # Infers the entity schemas for each element.
  invalid_entity_ids <- names(inferred_entity_ids[which(is.na(inferred_entity_ids))]) 
    # Finds all the invalid entity identifiers whose values are NA.
  if (length(invalid_entity_ids) == 1 &
      length(invalid_entity_ids) < length(inferred_entity_ids)) { 
    # Checks if only a single entity identifier is invalid while making sure that
    # not every entity identifier is invalid.
    warning(glue::glue("The following entity identifier cannot be matched to an entity type and has therefore been removed: {toString(invalid_entity_ids)}."))
    # Generates a warning for any invalid entity identifier found.
    inferred_entity_ids <- inferred_entity_ids[! names(inferred_entity_ids) 
                                               %in% invalid_entity_ids]
    # Removes the invalid entity identifier from inferred_entity_ids.
  } else if (length(invalid_entity_ids) > 1 & 
             length(invalid_entity_ids) < length(inferred_entity_ids)) { 
    # Checks if multiple entity identifiers are invalid while making sure that not 
    # every entity identifier is invalid.
    warning(glue::glue("The following entity identifiers cannot be matched to an entity type and have therefore been removed: {toString(invalid_entity_ids)}."))
    # Generates a warning for any invalid entity identifiers found.
    inferred_entity_ids <- inferred_entity_ids[! names(inferred_entity_ids) 
                                               %in% invalid_entity_ids]
    # Removes the invalid entity identifiers from inferred_entity_ids.
  } else if (length(invalid_entity_ids) == length(inferred_entity_ids)) {
    # Checks if all the entity identifiers are invalid.
    stop("None of the entity identifiers can be matched to an entity type and are therefore all invalid.")
    # Stops the function.
  }
  entity_single_get_endpoints  <- .get_api_endpoints(entity_id=inferred_entity_ids, 
                                                     entity_list=NULL) 
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
  return(response_list)
}

