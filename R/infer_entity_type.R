# infer_entity_type.R

#' Infer the entity types of elements contained in a vector or list
#' 
#' infer_entity_type.R takes either a character vector or list of character strings and 
#' for each element tries to infer the schema type, single-get API Endpoint URL format and bulk-get 
#' API endpoints URL format using string-matching techniques.
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
  entity_list <- list("bat" = c("batch", "https://hemoshear-dev.benchling.com/api/v2/batches/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/batches:bulk-get?batchIds=ENTITY_ID_VARIABLE"), 
                      "bfi" = c("custom_entity", "https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=ENTITY_ID_VARIABLE"),
                      "box" = c("box", "https://hemoshear-dev.benchling.com/api/v2/boxes/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/boxes:bulk-get?boxIds=ENTITY_ID_VARIABLE"),
                      "con" = c("container", "https://hemoshear-dev.benchling.com/api/v2/containers/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/containers:bulk-get?containerIds=ENTITY_ID_VARIABLE"),
                      "container_batch" = c("container_content", "https://hemoshear-dev.benchling.com/api/v2/containers/ENTITY_ID_VARIABLE/contents/ENTITY_ID_VARIABLE", NA),
                      "ent" = c("user", "https://hemoshear-dev.benchling.com/api/v2/users/ENTITY_ID_VARIABLE", NA),
                      "etr" = c("entry", "https://hemoshear-dev.benchling.com/api/v2/entries/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/entries:bulk-get?entryIds=ENTITY_ID_VARIABLE"),
                      "loc" = c("location", "https://hemoshear-dev.benchling.com/api/v2/locations/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/locations:bulk-get?locationIds=ENTITY_ID_VARIABLE"),
                      "mxt"= c("mixture", "https://hemoshear-dev.benchling.com/api/v2/mixtures/ENTITY_ID_VARIABLE", NA),
                      "plt" = c("plate", "https://hemoshear-dev.benchling.com/api/v2/plates/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/plates:bulk-get?plateIds=ENTITY_ID_VARIABLE"),
                      "prtn" = c("aa_sequence", "https://hemoshear-dev.benchling.com/api/v2/aa-sequences/ENTITY_ID_VARIABLE",
                                 "https://hemoshear-dev.benchling.com/api/v2/aa-sequences:bulk-get?aaSequenceIds=ENTITY_ID_VARIABLE"),
                      "sfs" = c("dropdown", "https://hemoshear-dev.benchling.com/api/v2/dropdowns/ENTITY_ID_VARIABLE", NA),
                      "sfso" = c("dropdown_option", "https://hemoshear-dev.benchling.com/api/v2/dropdowns/ENTITY_ID_VARIABLE", NA), 
                      "seq" = c("dna_sequence", "https://hemoshear-dev.benchling.com/api/v2/dna-sequences/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=ENTITY_ID_VARIABLE"))
  
  .entity_lookup <- function(entity_id, entity_list) {
    if (substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1) %in% names(entity_list)) {
      output <- c(entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][1],
                  gsub("ENTITY_ID_VARIABLE", entity_id, entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][2]),
                  gsub("ENTITY_ID_VARIABLE", entity_id, entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][3]))
    } else {
      output <- c(NA,NA,NA)
      warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id} cannot be matched with any listed identifier."))
    }
    return(output)
  }  
  if (missing(entity_id)) {
    stop("'entity_id' input is missing.")
  }
  if (is.null(entity_id) | length(entity_id) == 0) {
    stop("'entity_id' input is invalid.")
  } 
  if ((any(purrr::map(entity_id, ~ length(.) != 1) == TRUE)) | 
      (any(purrr::map(entity_id, ~ !is.character(.)) == TRUE)) |
      (any(purrr::map(entity_id, ~ is.na(.)) == TRUE)) |
      ("" %in% entity_id)) {
    stop("'entity_id' contains an invalid identifier.")
  }
  res <- list()
  for (i in 1:length(entity_id)) {
    if (unlist(gregexpr('_', entity_id[[i]]))[1] < 4) {
      warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id[[i]]} cannot be matched with any listed identifier."))
      res[[i]] <- c(NA,NA,NA)
      names(res[[i]])[1] <- "Entity Schema"
      names(res[[i]])[2] <- "Single-Get API Endpoint Request URL"
      names(res[[i]])[3] <- "Bulk-Get API Endpoints Request URL"
      names(res)[[i]] <- entity_id[[i]]
    } else {
      res[[i]] <- .entity_lookup(entity_id=entity_id[[i]], entity_list=entity_list)
      names(res[[i]])[1] <- "Entity Schema"
      names(res[[i]])[2] <- "Single-Get API Endpoint Request URL"
      names(res[[i]])[3] <- "Bulk-Get API Endpoints Request URL"
      names(res)[[i]] <- entity_id[[i]]
    }
  }
  return(res)
}
