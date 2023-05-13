# bundle_api_results.R

#' Generate a named list of tibble-formatted data tables using the response contents from Benchling
#' for specific entity identifiers.
#' 
#' .bundle_api_results.R takes a named list where the names are entity identifiers and each element
#' is a list containing that identifier's Single-Get API endpoint response contents from Benchling 
#' and sorts the data into a named list of tibble-formatted data tables where the names are the collective
#' entity schemas each entity identifier corresponds to and the tables feature the Single-Get API 
#' endpoint response contents data for each entity identifier associated with a specific schema.
#' 
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
#' res <- .bundle_api_results(entity_responses=entity_responses1)
#' }

.bundle_api_results <- function(entity_responses) {
  inferred_entity_types <- infer_entity_type(entity_id=names(entity_responses),entity_list=NULL) # Infers the entity schemas for the entity identifiers whose responses are displayed
  entity_response_attributes <- purrr::map(entity_responses, ~ names(unlist(.))) # Extracts all the attributes in each entity identifier's response and displays them so that they can all be seen as a list of vectors
  sorted_entity_types <- split(names(inferred_entity_types), inferred_entity_types) # Sorts the entity schemas so that each schema shows every entity identifier associated with it as a list of vectors
  entity_responses <- purrr::map(entity_responses, ~ unlist(.)) # Unlists all the data in each entity identifier's response so that it can all be seen and matched with the unlisted attributes
  sorted_entity_responses <- purrr::map(sorted_entity_types, 
                                        ~ entity_responses[which(names(entity_responses) %in% .)]) # Organizes all the unlisted responses for each entity identifier by sorting them under the respective entity schema each entity identifier is associated with
  sorted_response_attributes <- purrr::map(sorted_entity_responses,
                                           ~ unique(names(unlist(unname(.))))) # Organizes all the response attributes for all entity identifier responses associated with the same entity schema so that each schema shows the collective, unique and nonrepeating 
                                                                               # attributes associated with all the entity identifiers corresponding to a specific entity schema
  for (i in 1:length(sorted_entity_responses)) { # Runs each entity identifier and extracts its response output from all the response attributes listed for its respective schema in 'sorted_response_attributes' and leaving a value of NA if an output cannot be found for a specific response attribute
    entity_type <- names(sorted_entity_responses)[i]
    for (j in 1:length(sorted_entity_responses[[i]])) {
      entity_id <- names(sorted_entity_responses[[i]])[j]
      sorted_entity_responses[[i]][[j]] <- unlist(purrr::map(sorted_response_attributes[[i]],
                                                             ~ sorted_entity_responses[[i]][[j]][.]))
      names(sorted_entity_responses[[i]][[j]]) <- sorted_response_attributes[[i]]
      sorted_entity_responses[[i]][[j]] <- sorted_entity_responses[[i]][[j]][names(sorted_entity_responses[[i]][[j]]) != "id"]
      sorted_entity_responses[[i]][[j]] <- sorted_entity_responses[[i]][[j]][names(sorted_entity_responses[[i]][[j]]) != "error.invalidId"]
      sorted_entity_responses[[i]][[j]] <- c("entity.Id"=entity_id, "entity.type"=entity_type, sorted_entity_responses[[i]][[j]])
      names(sorted_entity_responses[[i]])[j] <- entity_id
    }
    names(sorted_entity_responses)[i] <- entity_type
  }
  
  res <- purrr::map(sorted_entity_responses, ~ tibble::as_tibble(as.data.frame(do.call(rbind, .)))) # Re-organizes the response contents data for all entity identifiers corresponding to a specific entity schema into a tibble-formatted data table where each row 
                                                                                                    # displays an entity identifier's response contents data and the columns are named after all the collective and unique response attributes listed for its respective 
                                                                                                    # schema
  return(res)
}
