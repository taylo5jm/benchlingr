# get_entity.R

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
#' entity_id <- c("seq_Cuf0omCm", "bfi_9fKcrORv")
#' entities <- get_entity(entity_id=entity_id, api_key=Sys.getenv("BENCHLING_API_KEY"))
#' entity_id <- c("etr_IWLMFYhR","etr_T3WZTyAe", "bfi_9fKcrORv", "bfi_KsLU5uWV", "bfi_VVamxrKQ")
#' entity_id <- c("ent_sPrxBNOh")
#' }
#' @export

get_entity <- function(entity_id, api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (api_key == "") {
    stop("Benchling API key is missing or empty.")
  }
  inferred_types <- .infer_entity_type(entity_id)
  if (all(sapply(inferred_types, function(inferred_types) all(is.na(inferred_types))) == TRUE)) {
    stop("All elements are unknown identifiers and only read out as NA.")
  }
  entities <- list()

  # Invert the inferred types list so that we can try the bulk-get endpoints for
  # each endpoint. 
  unique_inferred_types <- unlist(unique(inferred_types))
  inferred_types_inverted <- purrr::map(
    unique_inferred_types, 
    ~ names(inferred_types[which(inferred_types == .)]))
  names(inferred_types_inverted) <- unique_inferred_types
  
  # Load the bulk-get API endpoints
  bulk_endpoints <- .list_api_get_endpoints(endpoint="bulk")
  # See if any of the bulk-get endpoints aren't available for one or more types
  bulk_not_available <- purrr::map_lgl(names(inferred_types_inverted), 
                                       ~ is.na(bulk_endpoints[[.]]))
  
  # If at least one bulk-get endpoint is reasonable to grab, then construct
  # those API calls. We don't need to construct the API calls to the single get
  # endpoints yet, because some of the bulk-get calls may fail. In those cases,
  # we'll need to fall back on the single-get to see if any exist. 
  if (!all(bulk_not_available)) {
    if (length(bulk_not_available) > 0) {
      bulk_types <- inferred_types_inverted[-which(bulk_not_available)]
    } else {
      bulk_types <- inferred_types_inverted
    }
    .make_calls_to_bulk_get_api_endpoints <- function(bulk_types, bulk_endpoints) {
      # Construct API calls for each endpoint relevant to our input entity ids
      bulk_api_calls <- purrr::map2(
        names(bulk_types),
        bulk_types,
        ~ paste0(bulk_endpoints[[.x]], paste0(.y, collapse=',')))
      names(bulk_api_calls) <- names(bulk_types)
      # Make the API calls to the bulk-get endpoints.
      bulk_api_results <- vector("list", length(bulk_api_calls))
      for (i in 1:length(bulk_api_calls)) {
        bulk_api_results[[i]] <- httr::GET(bulk_api_calls[[i]], httr::authenticate(api_key, ''))
        # sleep(1)
      }
      return(bulk_api_results)
    }
    bulk_api_results <- .make_calls_to_bulk_get_api_endpoints(
      bulk_types, bulk_endpoints)
    # Get status codes for the bulk-get endpoints
    responses <- purrr::map_int(bulk_api_results, ~ .$status_code)
    # responses
    if (!all(responses == 200)) {
      inferred_types_inverted
    }
  }

  single_endpoints <- .list_api_get_endpoints(endpoint="single")
  # JT edit end 
  .get_entity_from_id <- function(entity, api_key) {
    if (!is.na(entity[2])) {
      if (httr::status_code() == 200) {
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
