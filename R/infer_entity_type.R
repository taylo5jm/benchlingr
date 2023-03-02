# infer_entity_type.R

#' Infer the entity types of elements contained in a vector or list
#' 
#' infer_entity_type.R takes either a character vector or list of character strings and 
#' for each element tries to infer the schema type, API endpoint and bulk-get API endpoints 
#' using string-matching techniques.
#' 
#' @param entity_id Either a character vector or list that contains the entities. 
#' All entities in the list or character vector must be character strings.
#' @return A named list, where the names are the identifiers or elements from entity_id
#' and the values are the schema types, single API endpoint and bulk-get API endpoints.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf")
#' res <- infer_entity_type(entity_id)
#' }
#' @export

infer_entity_type <- function(entity_id) {
  entity_lookup <- list("prtn" = c("aa_sequences", "https://benchling.com/api/reference#/AA%20Sequences/getAASequence",
                                   "https://benchling.com/api/reference#/AA%20Sequences/bulkGetAASequences"),
                        "bat" = c("batches", "https://benchling.com/api/reference#/Batches/getBatch", 
                                  "https://benchling.com/api/reference#/Batches/bulkGetBatches"),
                        "plt" = c("plate", "https://benchling.com/api/reference#/Plates/getPlate", 
                                  "https://benchling.com/api/reference#/Plates/bulkGetPlates"),
                        "box" = c("box", "https://benchling.com/api/reference#/Boxes/getBox", 
                                  "https://benchling.com/api/reference#/Boxes/bulkGetBoxes"),
                        "con" = c("container", "https://benchling.com/api/reference#/Containers/getContainer", 
                                  "https://benchling.com/api/reference#/Containers/bulkGetContainers"),
                        "loc" = c("location", "https://benchling.com/api/reference#/Locations/getLocation", 
                                  "https://benchling.com/api/reference#/Locations/bulkGetLocations"),
                        "etr" = c("entry", "https://benchling.com/api/reference#/Entries/getEntry", 
                                  "https://benchling.com/api/reference#/Entries/bulkGetEntries"),
                        "bfi" = c("custom_entity", "https://benchling.com/api/reference#/Custom%20Entities/getCustomEntity",
                                  "https://benchling.com/api/reference#/Custom%20Entities/bulkGetCustomEntities"),
                        "ent" = c("user", "https://benchling.com/api/reference#/Users/getUser", NA),
                        "sfs" = c("dropdown", "https://benchling.com/api/reference#/Dropdowns/getDropdown", NA),
                        "sfso" = c("dropdown_option", "https://benchling.com/api/reference#/Dropdowns/getDropdown", NA), # the dropdown options are available from the `dropdown` endpoint, as well as the `dropdown_option` warehouse table. 
                        "seq" = c("dna_sequence", "https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                  "https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences"), 
                        "mxt"= c("mixture", "https://benchling.com/api/reference#/Mixtures/getMixture", NA),
                        "container_batch" = c("container_content", "https://benchling.com/api/reference#/Containers/getContainerContent", NA))
  
  if (missing(entity_id)) {
    stop("'entity_id' input is missing.")
  }
  
  if (is.null(entity_id) | length(entity_id) == 0) {
    stop("'entity_id' input is invalid.")
  } 
  
  res <- list()
  
  if (is.list(entity_id)) {
    for (i in 1:length(entity_id)) {
      if (length(entity_id[[i]]) != 1) {
        stop("'entity_id' contains an invalid identifier.")
      } else {
        if (!is.character(entity_id[[i]]) | is.na(entity_id[[i]])) {
          stop("'entity_id' contains an invalid identifier.")
        } else {
          if (unlist(gregexpr('_', entity_id[[i]][1]))[1] < 4) {
            # stop(glue::glue("'entity_id' contains an unknown identifier. {entity_id[[i]]} cannot be matched with any of the identifiers listed in 'entity_lookup.'"))
            warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id[[i]]} cannot be matched with any of the identifiers listed in 'entity_lookup.'"))
            res[[i]] <- c(NA,NA,NA)
            names(res)[i] <- entity_id[i]
            names(res[[i]])[1] <- "Entity Schema"
            names(res[[i]])[2] <- "API Endpoint"
            names(res[[i]])[3] <- "BulkGet API Endpoint"
          } else {
            if (identical(grep(substr(entity_id[[i]],1,unlist(gregexpr('_', entity_id[[i]]))[1]-1),names(entity_lookup)),integer(0))) {
              # stop(glue::glue("'entity_id' contains an unknown identifier. {entity_id[[i]]} cannot be matched with any of the identifiers listed in 'entity_lookup.'"))
              warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id[[i]]} cannot be matched with any of the identifiers listed in 'entity_lookup.'"))
              res[[i]] <- c(NA,NA,NA)
              names(res)[i] <- entity_id[i]
              names(res[[i]])[1] <- "Entity Schema"
              names(res[[i]])[2] <- "API Endpoint"
              names(res[[i]])[3] <- "BulkGet API Endpoint"
            } else {
              res[[i]] <- entity_lookup[[grep(substr(entity_id[[i]],1,unlist(gregexpr('_', entity_id[[i]]))[1]-1),names(entity_lookup))]]
              names(res)[i] <- entity_id[i]
              names(res[[i]])[1] <- "Entity Schema"
              names(res[[i]])[2] <- "API Endpoint"
              names(res[[i]])[3] <- "BulkGet API Endpoint"
            }
          }
        }
      }
    }
  } else {
    for (i in 1:length(entity_id)){
      if (!is.character(entity_id[i]) | is.na(entity_id[i])) {
        stop("'entity_id' contains an invalid identifier.")
      } else {
        if (unlist(gregexpr('_', entity_id[i]))[1] < 4) {
          # stop(glue::glue("'entity_id' contains an unknown identifier. {entity_id[i]} cannot be matched with any of the identifiers listed in 'entity_lookup.'"))
          warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id[i]} cannot be matched with any of the identifiers listed in 'entity_lookup.'"))
          res[[i]] <- c(NA,NA,NA)
          names(res)[i] <- entity_id[i]
          names(res[[i]])[1] <- "Entity Schema"
          names(res[[i]])[2] <- "API Endpoint"
          names(res[[i]])[3] <- "BulkGet API Endpoint"
        } else {
          if (identical(grep(substr(entity_id[i],1,unlist(gregexpr('_', entity_id[i]))[1]-1),names(entity_lookup)),integer(0))) {
            # stop(glue::glue("'entity_id' contains an unknown identifier. {entity_id[i]} cannot be matched with any of the identifiers listed in 'entity_lookup.'"))
            warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id[i]} cannot be matched with any of the identifiers listed in 'entity_lookup.'"))
            res[[i]] <- c(NA,NA,NA)
            names(res)[i] <- entity_id[i]
            names(res[[i]])[1] <- "Entity Schema"
            names(res[[i]])[2] <- "API Endpoint"
            names(res[[i]])[3] <- "BulkGet API Endpoint"
          } else {
            res[[i]] <- entity_lookup[[grep(substr(entity_id[i],1,unlist(gregexpr('_', entity_id[i]))[1]-1),names(entity_lookup))]]
            names(res)[i] <- entity_id[i]
            names(res[[i]])[1] <- "Entity Schema"
            names(res[[i]])[2] <- "API Endpoint"
            names(res[[i]])[3] <- "BulkGet API Endpoint"
          }
        }
      }
    }
  }
  return(res)
}


