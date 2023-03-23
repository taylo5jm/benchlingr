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
  entity_list <- list("bat" = c("batch", 
                                "https://hemoshear-dev.benchling.com/api/v2/batches?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/batches/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/batches:bulk-get?batchIds=ENTITY_ID_VARIABLE"), 
                      "bfi" = c("custom_entity", 
                                "https://hemoshear-dev.benchling.com/api/v2/custom-entities?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=ENTITY_ID_VARIABLE"),
                      "box" = c("box", 
                                "https://hemoshear-dev.benchling.com/api/v2/boxes?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/boxes/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/boxes:bulk-get?boxIds=ENTITY_ID_VARIABLE"),
                      "con" = c("container", 
                                "https://hemoshear-dev.benchling.com/api/v2/containers?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/containers/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/containers:bulk-get?containerIds=ENTITY_ID_VARIABLE"),
                      "ent" = c("user", 
                                "https://hemoshear-dev.benchling.com/api/v2/users?ids=ENTITY_ID_VARIABLE&pageSize=50&sort=name",
                                "https://hemoshear-dev.benchling.com/api/v2/users/ENTITY_ID_VARIABLE", 
                                NA),
                      "etr" = c("entry", 
                                "https://hemoshear-dev.benchling.com/api/v2/entries?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/entries/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/entries:bulk-get?entryIds=ENTITY_ID_VARIABLE"),
                      "loc" = c("location", 
                                "https://hemoshear-dev.benchling.com/api/v2/locations?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/locations/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/locations:bulk-get?locationIds=ENTITY_ID_VARIABLE"),
                      "mxt"= c("mixture", 
                               "https://hemoshear-dev.benchling.com/api/v2/mixtures?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                               "https://hemoshear-dev.benchling.com/api/v2/mixtures/ENTITY_ID_VARIABLE", 
                               NA),
                      "plt" = c("plate", 
                                "https://hemoshear-dev.benchling.com/api/v2/plates?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/plates/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/plates:bulk-get?plateIds=ENTITY_ID_VARIABLE"),
                      "prtn" = c("aa_sequence", 
                                 "https://hemoshear-dev.benchling.com/api/v2/aa-sequences?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                 "https://hemoshear-dev.benchling.com/api/v2/aa-sequences/ENTITY_ID_VARIABLE",
                                 "https://hemoshear-dev.benchling.com/api/v2/aa-sequences:bulk-get?aaSequenceIds=ENTITY_ID_VARIABLE"),
                      "sfs" = c("dropdown", 
                                NA,
                                "https://hemoshear-dev.benchling.com/api/v2/dropdowns/ENTITY_ID_VARIABLE", 
                                NA),
                      "sfso" = c("dropdown_option", 
                                 NA,
                                 "https://hemoshear-dev.benchling.com/api/v2/dropdowns/ENTITY_ID_VARIABLE", 
                                 NA), 
                      "seq" = c("dna_sequence", 
                                "https://hemoshear-dev.benchling.com/api/v2/dna-sequences?pageSize=50&sort=name&ids=ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/dna-sequences/ENTITY_ID_VARIABLE",
                                "https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=ENTITY_ID_VARIABLE"))
  
  .entity_lookup <- function(entity_id, entity_list) {
    if (substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1) %in% names(entity_list)) {
      output <- c(entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][1])
    } else {
      output <- "unknown"
      warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id} cannot be matched with any listed identifier."))
    }
    return(output)
  }  
  
  .entity_match <- function(listed_ids, entity_list) {
    schema_labels <- unique(names(listed_ids))
    organized_listed_ids <- purrr::map(unique(names(listed_ids)), ~ list(unlist(listed_ids[which(names(listed_ids) == .)], use.names = FALSE)))
    names(organized_listed_ids) <- schema_labels
    
    for (i in 1:length(organized_listed_ids)) {
      schema_label <- names(organized_listed_ids)[i]
      if (names(organized_listed_ids)[i] != "unknown" & length(organized_listed_ids[[i]][[1]])/50 > 1) {
        organized_listed_ids[[i]] <- lapply(seq(1:ceiling(length(organized_listed_ids[[i]][[1]])/50)), 
                                            function(y)  
                                              unlist(purrr::map(organized_listed_ids[[i]][[1]][seq((1+((y-1)*50)),(y*50))], ~ .[!is.na(.)])))
      } 
      for (j in 1:length(organized_listed_ids[[i]])) {
        organized_listed_ids[[i]][[j]] <- list(organized_listed_ids[[i]][[j]])
        if (length(organized_listed_ids[[i]]) > 1) {
          names(organized_listed_ids[[i]])[j] <- paste0("set", as.character(j))
        } else {
          names(organized_listed_ids[[i]])[j] <- "set"
        }
        for (k in 1:length(organized_listed_ids[[i]][[j]])) {
          names(organized_listed_ids[[i]][[j]])[k] <- "items"
        }
      }
      names(organized_listed_ids)[i] <- schema_label
    }
    for (l in 1:length(organized_listed_ids)) {
      for (m in 1:length(organized_listed_ids[[l]])) {
        if (names(organized_listed_ids)[l] == "dropdown" |
            names(organized_listed_ids)[l] == "dropdown_option" | 
            names(organized_listed_ids)[l] == "unknown") {
          organized_listed_ids[[l]][[m]][[2]] <- NA
        } else {
          organized_listed_ids[[l]][[m]][[2]] <- gsub("ENTITY_ID_VARIABLE",
                                                      paste(organized_listed_ids[[l]][[m]][[1]], collapse = "%2C"),
                                                      entity_list[which(lapply(entity_list, 
                                                                               function(x) x[1]) == names(organized_listed_ids)[l])][[1]][2])
        }
        names(organized_listed_ids[[l]][[m]])[2] <- "entity_list_URL"
      }
    }
    return(organized_listed_ids)
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
      (any(purrr::map(entity_id, ~ grepl("_", ., fixed = TRUE)) == FALSE)) |
      ("" %in% entity_id)) {
    stop("'entity_id' contains an invalid identifier.")
  }
  res <- list()
  for (i in 1:length(entity_id)) {
    if (unlist(gregexpr('_', entity_id[[i]]))[1] < 4) {
      warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id[[i]]} cannot be matched with any listed identifier."))
      res[[i]] <- entity_id[[i]]
      names(res)[[i]] <- "unknown"
    } else {
      res[[i]] <- entity_id[[i]]
      names(res)[[i]] <- .entity_lookup(entity_id=entity_id[[i]], entity_list=entity_list)
    }
  }
  organized_res <- .entity_match(listed_ids = res, entity_list = entity_list)
  return(organized_res)
}
