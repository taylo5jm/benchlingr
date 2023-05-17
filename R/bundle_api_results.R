# bundle_api_results.R

#' Generate a named list of tibble-formatted data tables using the response contents from Benchling
#' for specific entity identifiers.
#' 
#' .bundle_api_results.R takes a named list where the names are entity identifiers and each element
#' is a list containing that identifier's Single-Get API endpoint response contents from Benchling 
#' and sorts the data into a named list where the names are the collective entity types for all of 
#' the entity identifiers in the input and each element is another named list where the names are 
#' the entity schema names and the elements are tibble-formatted data tables containing Single-Get API
#' endpoint response contents data for each entity identifier associated with both said schema name 
#' and entity type.
#' 
#' @include infer_entity_type.R
#' @param entity_responses A named list where the names are entity identifiers and each element is a 
#' list that contains the response contents from the call made to that identifier's Single-Get API 
#' endpoint in Benchling.
#' @return A named list where the names are entity schemas and each element is a tibble-formatted
#' data table displaying the Single-Get API endpoint response contents data extracted Benchling for 
#' each entity identifier corresponding to a specific entity schema.
#' @examples \dontrun{
#' entity_responses <- get_entity(entity_id = c("con_m1dmbdV8", "bfi_Ur5DfvGJ", "seq_Gju61mCm", 
#' "bfi_Q13AlXkf", "bfi_Ks908uWV", "ent_Ec76qX9f","ent_sPrxBNOh", "box_K9950IQ8", "box_923aklum", 
#' "bfi_smi97554l"), api_key=Sys.getenv("BENCHLING_API_KEY"))
#' res <- .bundle_api_results(entity_responses=entity_responses)
#' }
#' @keywords internal

.bundle_api_results <- function(entity_responses) {
  entity_ids <- names(entity_responses)
  
  inferred_entity_types <- infer_entity_type(entity_id=entity_ids,entity_list=NULL) # Infers the entity schemas for the entity identifiers whose responses are displayed
  
  entity_id_schema_ids <- purrr::map(entity_responses, ~ .$schema$id)
  entity_id_schema_ids[which(purrr::map(entity_id_schema_ids, ~ is.null(.)) == TRUE)] <- "none"
  entity_id_schema_ids <- unlist(entity_id_schema_ids)
  
  entity_id_schema_names <- purrr::map(entity_responses, ~ .$schema$name)
  entity_id_schema_names[which(purrr::map(entity_responses, ~ !is.null(.$error)) == TRUE)] <- "error"
  entity_id_schema_names[which(purrr::map(entity_id_schema_names, ~ is.null(.)) == TRUE)] <- inferred_entity_types[which(purrr::map(entity_id_schema_names, ~ is.null(.)) == TRUE)]
  entity_id_schema_names <- unlist(entity_id_schema_names)
  
  .api_response_sort <- function(entity_id_response) {
    entity_id_response <- unlist(entity_id_response)
    entity_id_response <- entity_id_response[! (substr(names(entity_id_response), 1, 7) == "fields." & 
                                                  substr(names(entity_id_response), nchar(names(entity_id_response))-5, 
                                                         nchar(names(entity_id_response))) != ".value" & 
                                                  substr(names(entity_id_response), nchar(names(entity_id_response))-4,
                                                         nchar(names(entity_id_response))) != ".type")]
    names(entity_id_response) <- gsub("fields.", "", names(entity_id_response))
    names(entity_id_response) <- gsub("registrationOrigin.", "", names(entity_id_response))
    names(entity_id_response) <- gsub("\\.", "_", names(entity_id_response))
    entity_id_response[entity_id_response == "" | entity_id_response == " "] <- NA 
    return(entity_id_response)
  }
  
  entity_responses <- purrr::map(entity_responses, ~ .api_response_sort(.))
  entity_responses <- purrr::map(entity_responses, 
                                 ~ .[names(.) != "id" & names(.) != "error_invalidId" &
                                       names(.) != "schema_id" & names(.) != "schema_name"])
  entity_responses <- purrr::map(entity_ids, 
                                 ~ c("id" = ., "entityType" = inferred_entity_types[[.]],
                                     "schema_id" = entity_id_schema_ids[[.]],
                                     "schema_name" = entity_id_schema_names[[.]],
                                     entity_responses[[.]]))
  names(entity_responses) <- entity_ids
  
  
  sorted_entity_types <- split(names(inferred_entity_types), inferred_entity_types)
  sorted_entity_responses <- purrr::map(sorted_entity_types, 
                                        ~ entity_id_schema_names[which(names(entity_id_schema_names) %in% .)])
  sorted_entity_responses <- purrr::map(sorted_entity_responses,
                                        ~ split(names(.), .))
  
  # entity_id_response_attributes <- purrr::map(entity_responses, ~ names(.))
  # sorted_entity_response_attributes <- lapply(sorted_entity_responses,
  #                                             function(x) purrr::map(x, ~ unique(unlist(unname(entity_id_response_attributes[which(names(entity_id_response_attributes) %in% .)])))))
  # entity_id_response_attributes <- purrr::map(entity_responses, ~ sorted_entity_response_attributes[[.[['entityType']]]][[.[['schema_name']]]])
  # entity_responses <- purrr::map(entity_ids, ~ entity_responses[[.]][entity_id_response_attributes[[.]]])
  # names(entity_responses) <- entity_ids
  # for (i in 1:length(entity_responses)) {
  #   names(entity_responses[[i]]) <- entity_id_response_attributes[[names(entity_responses)[i]]]
  # }
  
  res <- lapply(sorted_entity_responses,
                function(x) purrr::map(x, ~ tibble::as_tibble(as.data.frame(do.call(rbind, entity_responses[which(names(entity_responses) %in% .)])))))
  return(res)
}
