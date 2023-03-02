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
#' @return A named list, where the names are the identifiers or elements from entity_id and the values
#' are the responses from the calls made to each identifier's set of bulk-get API endpoints or single
#' API endpoint in the form of character strings.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0omCm", "bfi_Q1PMlXkf", "loc_43OIpas")
#' entities <- bulk_get_entity(entity_id)
#' }
#' @export

bulk_get_entity <- function(entity_id) {
  inferred_types <- infer_entity_type(entity_id)
  .get_entity_from_id <- function(x) {
    if (all(sapply(x, function(x) is.na(x[1])) == TRUE)) {
      stop("All elements are unknown identifiers and only read out as NA.")
    }
    entity_list <- list()
    for (i in 1:length(x)) {
      if (is.na(x[[i]][1]) | is.na(x[[i]][2]) | is.na(x[[i]][3])) {
        if (is.na(x[[i]][1])) {
          message1 <- paste0("Entity schema for ", as.character(names(x)[i]), " is unknown and labeled as NA. ")
        } else {
          message1 <- ""
        }
        if (is.na(x[[i]][2])) {
          message2 <- paste0("API endpoint for ", as.character(names(x)[i]), " is unknown and labeled as NA. ")
        } else {
          message2 <- ""
        }
        if (is.na(x[[i]][3])) {
          message3 <- paste0("BulkGet API endpoints for ", as.character(names(x)[i]), " are unknown and labeled as NA.")
        } else {
          message3 <- ""
        }
        warning(paste0(message1, message2, message3))
      }
      if (is.na(x[[i]][1]) | (is.na(x[[i]][2]) & is.na(x[[i]][3]))) {
        entity_list[[i]] <- NA
        names(entity_list)[[i]] <- names(x)[i]
      } else {
        if (!is.na(x[[i]][1]) & !is.na(x[[i]][3])) {
          data <- httr::GET(x[[i]][3])
          if (data$status_code != 200) {
            # stop(paste0("Request sent to ", as.character(x[[i]][3]), " was not successful.")
            entity_list[[i]] <- NA
            names(entity_list)[[i]] <- names(x)[i]
          } else {
            entity_list[[i]] <- httr::content(data, "text")
            names(entity_list)[[i]] <- names(x)[i]
          }
        } 
        if (!is.na(x[[i]][1]) & !is.na(x[[i]][2]) & is.na(x[[i]][3])) {
          warning(paste0("BulkGet API endpoints are not available for ",
                         as.character(names(x)[i]), " as none can be found for ",
                         as.character(x[[i]][1]), "-type entities. Instead, we will use the single API endpoint listed for ",
                         as.character(names(x)[i])))
          data <- httr::GET(x[[i]][3])
          if (data$status_code != 200) {
            # stop(paste0("Request sent to ", as.character(x[[i]][3]), " was not successful.")
            entity_list[[i]] <- NA
            names(entity_list)[[i]] <- names(x)[i]
          } else {
            entity_list[[i]] <- httr::content(data, "text")
            names(entity_list)[[i]] <- names(x)[i]
          }
        }
      }
    }
    return(entity_list)
  }
  entities <- .get_entity_from_id(inferred_types)
  return(entities) # list of lists
}
