# infer_entity_type.R

list_api_endpoints <- function() {
  list("bat" = c("batch", "https://hemoshear-dev.benchling.com/api/v2/batches/",
                                "https://hemoshear-dev.benchling.com/api/v2/batches:bulk-get?batchIds="), 
                      "bfi" = c("custom_entity", "https://hemoshear-dev.benchling.com/api/v2/custom-entities/",
                                "https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds="),
                      "box" = c("box", "https://hemoshear-dev.benchling.com/api/v2/boxes/",
                                "https://hemoshear-dev.benchling.com/api/v2/boxes:bulk-get?boxIds="),
                      "con" = c("container", "https://hemoshear-dev.benchling.com/api/v2/containers/",
                                "https://hemoshear-dev.benchling.com/api/v2/containers:bulk-get?containerIds="),
                      "container_batch" = c("container_content", "https://hemoshear-dev.benchling.com/api/v2/containers//contents/", NA),
                      "ent" = c("user", "https://hemoshear-dev.benchling.com/api/v2/users/", NA),
                      "etr" = c("entry", "https://hemoshear-dev.benchling.com/api/v2/entries/",
                                "https://hemoshear-dev.benchling.com/api/v2/entries:bulk-get?entryIds="),
                      "loc" = c("location", "https://hemoshear-dev.benchling.com/api/v2/locations/",
                                "https://hemoshear-dev.benchling.com/api/v2/locations:bulk-get?locationIds="),
                      "mxt"= c("mixture", "https://hemoshear-dev.benchling.com/api/v2/mixtures/", NA),
                      "plt" = c("plate", "https://hemoshear-dev.benchling.com/api/v2/plates/",
                                "https://hemoshear-dev.benchling.com/api/v2/plates:bulk-get?plateIds="),
                      "prtn" = c("aa_sequence", "https://hemoshear-dev.benchling.com/api/v2/aa-sequences/",
                                 "https://hemoshear-dev.benchling.com/api/v2/aa-sequences:bulk-get?aaSequenceIds="),
                      "sfs" = c("dropdown", "https://hemoshear-dev.benchling.com/api/v2/dropdowns/", NA),
                      "sfso" = c("dropdown_option", "https://hemoshear-dev.benchling.com/api/v2/dropdowns/", NA), 
                      "seq" = c("dna_sequence", "https://hemoshear-dev.benchling.com/api/v2/dna-sequences/",
                                "https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds="))
}
# Get the single get API endpoint
# gsub("ENTITY_ID_VARIABLE", entity_id, entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][2])
# Get the bulk get API endpoint
# gsub("ENTITY_ID_VARIABLE", entity_id, entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][3])

# "Single-Get API Endpoint Request URL",
# "Bulk-Get API Endpoints Request URL"


#' Infer the entity types of elements contained in a vector or list
#' 
#' infer_entity_type.R takes either a character vector or list of character strings and 
#' for each element tries to infer the schema type, single-get API Endpoint URL format and bulk-get 
#' API endpoints URL format using string-matching techniques.
#' 
#' @param entity_id Either a character vector or list that contains the entities. 
#' All entities in the list or character vector must be character strings.
#' @param verbose Messages
#' @return A named list, where the names are the identifiers or elements from entity_id
#' and the values are the schema types, single API endpoint and bulk-get API endpoints.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf")
#' res <- .infer_entity_type(entity_id)
#' }
#' @export
#' @keywords internal

.infer_entity_type <- function(entity_id, verbose=F) {
  if (missing(entity_id)) {
    stop("'entity_id' input is missing.")
  }
  if (is.null(entity_id) | length(entity_id) == 0) {
    stop("'entity_id' input is invalid. Must be a 1-D list or character vector")
  } 
  if ((any(purrr::map(entity_id, ~ length(.) != 1) == TRUE)) | 
      (any(purrr::map(entity_id, ~ !is.character(.)) == TRUE)) |
      (any(purrr::map(entity_id, ~ is.na(.)) == TRUE)) |
      ("" %in% entity_id)) {
    stop("'entity_id' contains an invalid identifier. Must be a 1-D list or character vector.")
  }
  res <- list()

  .entity_lookup <- function(entity_id, entity_list) {
    if (substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1) %in% names(entity_list)) {
      entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][1]
    } else {
      NA
      # warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id} cannot be matched with any listed identifier."))
    }
  }
  entity_list <- list_api_endpoints()
  for (i in 1:length(entity_id)){
    if (unlist(gregexpr('_', entity_id[[i]]))[1] < 4) {
      if (verbose) {
        warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id[i]} cannot be matched with any listed identifier."))
      }
      res[[i]] <- c(NA)
    } else {
      res[[i]] <- .entity_lookup(entity_id=entity_id[[i]], entity_list=entity_list)
    }
    names(res)[[i]] <- entity_id[[i]]
    
  }
  return(res)
}
