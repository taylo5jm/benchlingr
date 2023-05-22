# bundle_api_results.R

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
#' "seq_Gju61mCm", "bfi_Q13AlXkf", "bfi_Ks908uWV", "ent_Ec76qX9f", 
#' "ent_sPrxBNOh", "box_K9950IQ8", "box_923aklum", "bfi_smi97554l"), 
#' api_key=Sys.getenv("BENCHLING_API_KEY"))
#' res <- .bundle_api_results(entity_responses=entity_responses)
#' }
#' @keywords internal

.bundle_api_results <- function(entity_responses) {
  entity_ids <- names(entity_responses) 
  # Extracts a vector containing the entity identifiers in the input.
  
  inferred_entity_types <- infer_entity_type(entity_id=entity_ids,entity_list=NULL) 
  # Infers the entity types for the entity identifiers. 
  inferred_entity_types[which(purrr::map(names(inferred_entity_types), 
                              ~ !is.null(entity_responses[[.]]$error)) == TRUE)] <- "error" 
  # Re-labels the entity types for entity identifiers with an error in their 
  # response as "error".
  
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
  entity_id_schema_names[which(purrr::map(entity_responses, 
                               ~ !is.null(.$error)) == TRUE)] <- "error" 
  # Generates a value of error for each entity identifier's schema name that have 
  # an error value in their response.
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
                      ~ .[names(.) != "id" & names(.) != "error_invalidId" &
                          names(.) != "schema_id" & names(.) != "schema_name"])
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
  
  res <- lapply(sorted_entity_responses, 
                function(x) purrr::map(x, ~ tibble::as_tibble(as.data.frame(do.call(rbind, 
                entity_responses[which(names(entity_responses) %in% .)])))))
  # Matches each entity identifier listed for a specific schema name with their 
  # API response data and re-organizes it all into a single tibble-formatted 
  # data table where each row shows the API response data for a specific entity 
  # identifier matched to a specific schema name for a certain entity type. 
  
  return(res)
}
