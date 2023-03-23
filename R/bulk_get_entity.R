# bulk_get_entity.R

#' Call the bulk-get API endpoints or single API endpoint if no set of bulk-get 
#' API endpoints is provided for an arbitrary set of entity identifiers.
#' 
#' bulk_get_entity.R will take a character vector or list of character strings and for each element 
#' send a request to that element's set of bulk-get API endpoints or single API endpoint if the 
#' bulk-get API endpoints are written as NA and return a named list containing the responses for 
#' each request in the form of character strings.
#' 
#' @include infer_entity_type.R
#' @param entity_id Either a character vector or list that contains the entities. All entities 
#' in the list or character vector must be character strings.
#' @param api_key Benchling API key.
#' @return A named list, where the names are the identifiers or elements from entity_id and the values
#' are the responses from the calls made to each identifier's set of bulk-get API endpoints or single
#' API endpoint in the form of character strings.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0omCm", "bfi_9fKcrORv")
#' entities1 <- bulk_get_entity(entity_id=entity_id1, api_key=Sys.getenv("BENCHLING_API_KEY"))
#' entity_id2 <- c("etr_IWLMFYhR","etr_T3WZTyAe", "bfi_9fKcrORv", "bfi_KsLU5uWV", "bfi_VVamxrKQ")
#' entities2 <- bulk_get_entity(entity_id=entity_id2, api_key=Sys.getenv("BENCHLING_API_KEY"))
#' entity_id3 <- c("ent_sPrxBNOh")
#' entities3 <- bulk_get_entity(entity_id=entity_id3, api_key=Sys.getenv("BENCHLING_API_KEY"))

#' }
#' @export

bulk_get_entity <- function(entity_id, benchling_api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (benchling_api_key == "") {
    stop("Benchling API key is missing or empty.")
  }
  inferred_types <- infer_entity_type(entity_id)
  if (all(sapply(inferred_types, function(inferred_types) all(is.na(inferred_types))) == TRUE)) {
    stop("All elements are unknown identifiers and only read out as NA.")
  }
  entities <- list()
  .get_entity_from_id <- function(entity, api_key) {
    if (!is.na(entity[2])) {
      if (httr::status_code(httr::GET(as.character(entity[2]), httr::authenticate(api_key, ''))) == 200) {
        single_get_response_content <- httr::content(httr::GET(as.character(entity[2]), httr::authenticate(api_key, '')))
        single_get_response_check <- 1
      } else {
        single_get_response_content <- NA
        single_get_response_check <- 0
      }
    } else {
      single_get_response_content <- NA
      single_get_response_check <- 0
    }
    if (!is.na(entity[3])) {
      if (httr::status_code(httr::GET(as.character(entity[3]), httr::authenticate(api_key, ''))) == 200) {
        bulk_get_response_content <- httr::content(httr::GET(as.character(entity[3]), httr::authenticate(api_key, '')))
        bulk_get_response_check <- 1
      } else {
        bulk_get_response_content <- NA
        bulk_get_response_check <- 0
      }
    } else {
      bulk_get_response_content <- NA
      bulk_get_response_check <- 0
    }
    if (single_get_response_check == 1 | bulk_get_response_check == 1) { 
      if (single_get_response_check == 1 & bulk_get_response_check == 0) {
        response_list <- list(entity[1], single_get_response_content, single_get_response_content[["name"]])
        names(response_list)[1] <- "Entity Schema"
        names(response_list)[2] <- "Single-Get API Endpoint Response Content"
        names(response_list)[3] <- "Entity Name"
      }
      if ((single_get_response_check == 0 & bulk_get_response_check == 1) | 
          (single_get_response_check == 1 & bulk_get_response_check == 1)) {
        response_list <- list(entity[1], bulk_get_response_content, bulk_get_response_content[[1]][[1]][["name"]])
        names(response_list)[1] <- "Entity Schema"
        names(response_list)[2] <- "Bulk-Get API Endpoint Response Content"
        names(response_list)[3] <- "Entity Name"
      }
    } else {
      response_list <- list(entity[1], NA, NA)
      names(response_list)[1] <- "Entity Schema"
      names(response_list)[2] <- "Single-Get API Endpoint/Bulk-Get API Endpoints Response Content"
      names(response_list)[3] <- "Entity Name"
    }
    return(response_list)
  }
  for (i in 1:length(inferred_types)) {
    if (is.na(inferred_types[[i]][1]) | is.na(inferred_types[[i]][2]) | is.na(inferred_types[[i]][3])) {
      if (is.na(inferred_types[[i]][1])) {
        message1 <- paste0("Entity schema for ", as.character(names(inferred_types)[[i]]), " is unknown and labeled as NA. ")
      } else {
        message1 <- ""
      }
      if (is.na(inferred_types[[i]][2])) {
        message2 <- paste0("Single-Get API endpoint for ", as.character(names(inferred_types)[[i]]), " is unknown and labeled as NA. ")
      } else {
        message2 <- ""
      }
      if (is.na(inferred_types[[i]][3])) {
        message3 <- paste0("BulkGet API endpoints for ", as.character(names(inferred_types)[[i]]), " are unknown and labeled as NA.")
      } else {
        message3 <- ""
      }
      warning(paste0(message1, message2, message3))
    }
    if (is.na(inferred_types[[i]][1]) & is.na(inferred_types[[i]][2]) & is.na(inferred_types[[i]][3])) {
      entities[[i]] <- list(NA,NA,NA)
      names(entities[[i]])[1] <- "Entity Schema"
      names(entities[[i]])[2] <- "Single-Get API Endpoint/Bulk-Get API Endpoints Response Content"
      names(entities[[i]])[3] <- "Entity Name"
      names(entities)[i] <- names(inferred_types)[i]
    } else {
      entities[[i]] <- .get_entity_from_id(entity=inferred_types[[i]], api_key=benchling_api_key)
      names(entities)[i] <- names(inferred_types)[i]
    }
  }
  return(entities) # list of lists
}
