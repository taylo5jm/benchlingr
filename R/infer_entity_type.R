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
#' res <- .infer_entity_type(entity_id)
#' }
#' @export
#' @keywords internal

infer_entity_type <- function(entity_id, entity_list=NULL, verbose=FALSE, 
                              benchling_api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (benchling_api_key == "") {
    stop("Benchling API key is missing or empty.")
  }
  if (missing(entity_id)) {
    stop("'entity_id' input is missing. Must be a 1-D list or character vector")
  }
  if (is.null(entity_id) | length(entity_id) == 0) {
    stop("'entity_id' input is invalid. Must be a 1-D list or character vector")
  } 
  if ((any(purrr::map(entity_id, ~ length(.) != 1) == TRUE)) | 
      (any(purrr::map(entity_id, ~ !is.character(.)) == TRUE)) |
      ("" %in% entity_id)) {
    stop("'entity_id' contains an invalid identifier. Must be a 1-D list or character vector.")
  }
  if ((any(purrr::map(entity_id, ~ is.na(.)) == TRUE)) |
      ("" %in% entity_id)) {
    stop("'entity_id' contains an invalid identifier. Must be a 1-D list or character vector.")
  }
  if (is.null(entity_list)) {
    entity_list <- list_api_contents(contents="all")
  }
  res <- list()
  .entity_lookup <- function(entity_id, entity_list, verbose) {
    if (substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1) %in% names(entity_list)) {
      output <- entity_list[[substr(entity_id, 1, unlist(gregexpr('_', entity_id))[1]-1)]][1]
    } else {
      output <- "invalid_entities"
      if (verbose) {
        warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id} cannot be matched with any listed identifier."))
      }
    }
    return(output)
  }
  
  .entity_match(sorted_ids, entity_list, api_key) {
    entity_types <- unique(names(sorted_ids))
    valid_entity_types <- entity_types[which(entity_types != "invalid_entities")]
    listed_entity_types <- valid_entity_types[which(unlist(lapply(valid_entity_types, function(x)
      !is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][2]))))]
    testable_entity_types <- listed_entity_types[which(unlist(lapply(listed_entity_types, function(x)
      (!is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][3]) |
         !is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][4])))))]
    sorted_ids1 <- list()
    sorted_ids2 <- list()
    sorted_ids_counter <- 0
    for (i in 1:length(testable_entity_types)) {
      ids <- unlist(sorted_ids[which(names(sorted_ids) == testable_entity_types[i])], use.names = FALSE)
      api_urls <- entity_list[which(purrr::map(entity_list, ~ .[1]) == testable_entity_types[[i]])][[1]][c(2,3,4)]
      valid_ids <- list()
      valid_ids_counter <- 0
      for (j in 1:ceiling(length(ids)/50)) {
        ids_set <- ids[seq((1+((j-1)*50)),(j*50))][which(unlist(purrr::map(ids[seq((1+((j-1)*50)), (j*50))],
                                                                           ~ !is.na(.))))]
        lookup_url <- gsub("ENTITY_IDS", paste(ids_set, collapse = "%2C"),
                           api_urls[1])
        lookup_content <- httr::content(httr::GET(lookup_url, httr::authenticate(api_key, '')))
        if ("error" %in% names(lookup_content)) {
          if ("invalidId" %in% names(lookup_content$error)) {
            invalid_ids <- lookup_content$error$invalidId
          }
          if ("invalidIds" %in% names(lookup_content$error)) {
            invalid_ids <- lookup_content$error$invalidIds
          }
          valid_ids_set <- setdiff(ids_set, unlist(invalid_ids))
          sorted_ids2 <- unlist(append(sorted_ids2, invalid_ids))
        } else {
          valid_ids_set <- ids_set
        }
        if (length(valid_ids_set) != 0) {
          valid_ids_counter <- valid_ids_counter + 1
          valid_ids[[valid_ids_counter]] <- valid_ids_set
        }
      }
      valid_ids <- unlist(valid_ids)
      if (!is.null(valid_ids)) {
        sorted_ids_counter <- sorted_ids_counter + 1
        sorted_ids1[[sorted_ids_counter]] <- list()
        names(sorted_ids1)[sorted_ids_counter] <- entity_types1[i]
        if (!is.na(api_urls[3])) {
          for (l in 1:ceiling(length(valid_ids)/50)) {
            ids_set <- valid_ids[seq((1+((l-1)*50)), (l*50))][which(unlist(purrr::map(valid_ids[seq((1+((l-1)*50)), (l*50))],
                                                                                      ~ !is.na(.))))]
            lookup_url <- c("Bulk-Get API Endpoints URL", gsub("ENTITY_IDS", paste(ids_set, collapse = "%2C"),
                                                               api_urls[3]))
            sorted_ids1[[sorted_ids_counter]][[l]] <- list(ids_set, api_urls, lookup_url, "TESTED")
            if (length(ids_set) > 1) {
              names(sorted_ids1[[sorted_ids_counter]][[l]])[1] <- "entity_identifiers"
            } else {
              names(sorted_ids1[[sorted_ids_counter]][[l]])[1] <- "entity_identifier"
            }
            names(sorted_ids1[[sorted_ids_counter]][[l]])[2] <- "api_urls"
            names(sorted_ids1[[sorted_ids_counter]][[l]])[3] <- "lookup_url"
            names(sorted_ids1[[sorted_ids_counter]][[l]])[4] <- "status"
          }
        } else {
          if (!is.na(api_urls[2])) {
            for (m in 1:length(valid_ids)) {
              lookup_url <- c("Single-Get API Endpoint URL", gsub("ENTITY_ID",
                                                                  as.character(valid_ids[m]),
                                                                  api_urls[2]))
              sorted_ids1[[sorted_ids_counter]][[m]] <- list(valid_ids[m], api_urls, lookup_url, "TESTED")
              names(sorted_ids1[[sorted_ids_counter]][[m]])[1] <- "entity_identifier"
              names(sorted_ids1[[sorted_ids_counter]][[m]])[2] <- "api_urls"
              names(sorted_ids1[[sorted_ids_counter]][[m]])[3] <- "lookup_url"
              names(sorted_ids1[[sorted_ids_counter]][[m]])[4] <- "status"
            }
          }
        }
        if (length(sorted_ids1[[sorted_ids_counter]]) == 1) {
          names(sorted_ids1[[sorted_ids_counter]]) <- "set"
        }
        if (length(sorted_ids1[[sorted_ids_counter]]) > 1) {
          names(sorted_ids1[[sorted_ids_counter]]) <- unlist(purrr::map(seq(1,length(sorted_ids1[[sorted_ids_counter]])),
                                                                        ~ paste0("set",as.character(.))))
        }
      }
    }
    if (length(sorted_ids2) >= 1) {
      sorted_ids2 <- list("invalid_entities" = list("set" = list("entity_identifiers" = sorted_ids2, 
                                                                 "api_urls" = "INVALID",
                                                                 "lookup_url_info" = "INVALID",
                                                                 "status" = "INVALID")))
    }
    if ("invalid_entities" %in% names(sorted_ids)) {
      if ("invalid_entities" %in% names(sorted_ids2)) {
        sorted_ids2$invalid_entities$set$entity_identifiers <- append(sorted_ids2$invalid_entities$set$entity_identifiers,
                                                                      unlist(sorted_ids[which(names(sorted_ids) == "invalid_entities")], use.names = FALSE))
      } else {
        sorted_ids2 <- list("invalid_entities" = list("set" = list("entity_identifiers" = unlist(sorted_ids[which(names(sorted_ids) == "invalid_entities")], use.names = FALSE), 
                                                                   "api_urls" = "INVALID",
                                                                   "lookup_url_info" = "INVALID",
                                                                   "status" = "INVALID")))
      }
    } 
    if (length(setdiff(entity_types[which(entity_types != "invalid_entities")], 
                       entity_types1)) != 0) {
      untested_ids <- list()
      for (n in 1:length(setdiff(entity_types[which(entity_types != "invalid_entities")], 
                                 entity_types1))) {
        ids <- unlist(sorted_ids[which(names(sorted_ids) == setdiff(entity_types[which(entity_types != "invalid_entities")], 
                                                                    entity_types1)[n])], 
                      use.names = FALSE)
        api_urls <- entity_list[which(purrr::map(entity_list, ~ .[1]) == 
                                        setdiff(entity_types[which(entity_types != "invalid_entities")], 
                                                entity_types1)[n])][[1]][c(2,3,4)]
        untested_ids[[n]] <- list()
        if (!is.na(api_urls[3])) {
          for (o in 1:ceiling(length(ids)/50)) {
            ids_set <- ids[seq((1+((o-1)*50)), (o*50))][which(unlist(purrr::map(ids[seq((1+((o-1)*50)), (o*50))],
                                                                                ~ !is.na(.))))]
            lookup_url_info <- c("Bulk-Get API Endpoints URL", gsub("ENTITY_IDS", paste(ids_set, collapse = "%2C"),
                                                                    api_urls[3]))
            untested_ids[[n]][[o]] <- list(ids_set, api_urls, lookup_url_info, "UNTESTED")
            if (length(ids_set) > 1) {
              names(untested_ids[[n]][[o]])[1] <- "entity_identifiers"
            } else {
              names(untested_ids[[n]][[o]])[1] <- "entity_identifier"
            }
            names(untested_ids[[n]][[o]])[2] <- "api_urls"
            names(untested_ids[[n]][[o]])[3] <- "lookup_url_info"
            names(untested_ids[[n]][[o]])[4] <- "status"
          }
          if (length(untested_ids[[n]]) == 1) {
            names(untested_ids[[n]]) <- "set"
          }
          if (length(untested_ids[[n]]) > 1) {
            names(untested_ids[[n]]) <- unlist(purrr::map(seq(1,length(untested_ids[[n]])), 
                                                          ~ paste0("set", as.character(.)))) 
          }
          names(untested_ids)[n] <- setdiff(entity_types[which(entity_types != "invalid_entities")], 
                                            entity_types1)[n]
        } else {
          if (!is.na(api_urls[2])) {
            for (p in 1:length(ids)) {
              lookup_url_info <- c("Single-Get API Endpoint URL", gsub("ENTITY_ID", 
                                                                       as.character(ids[p]), 
                                                                       api_urls[2]))
              untested_ids[[n]][[p]] <- list(ids[p], api_urls, lookup_url_info, "UNTESTED")
              names(untested_ids[[n]][[p]])[1] <- "entity_identifier"
              names(untested_ids[[n]][[p]])[2] <- "api_urls"
              names(untested_ids[[n]][[p]])[3] <- "lookup_url_info"
              names(untested_ids[[n]][[p]])[4] <- "status"
            }
            if (length(untested_ids[[n]]) == 1) {
              names(untested_ids[[n]]) <- "set"
            }
            if (length(untested_ids[[n]]) > 1) {
              names(untested_ids[[n]]) <- unlist(purrr::map(seq(1,length(untested_ids[[n]])), 
                                                            ~ paste0("set", as.character(.)))) 
            }
            names(untested_ids)[n] <- setdiff(entity_types[which(entity_types != "invalid_entities")], 
                                              entity_types1)[n]
          }
          if (is.na(api_urls[2])) {
            if ("invalid_entities" %in% names(sorted_ids2)) {
              sorted_ids2$invalid_entities$set$entity_identifiers <- append(sorted_ids2$invalid_entities$set$entity_identifiers,
                                                                            ids)
            } else {
              sorted_ids2 <- list("invalid_entities" = list("set" = list("entity_identifiers" = ids, 
                                                                         "api_urls" = "INVALID",
                                                                         "lookup_url_info" = "INVALID",
                                                                         "status" = "INVALID")))
            }
          }
        }
      }
      sorted_ids2 <- append(sorted_ids2, untested_ids)
    } 
    return(append(sorted_ids1, sorted_ids2))
  }
  
  for (i in 1:length(entity_id)){
    if ((grepl("_", entity_id[[i]], fixed = TRUE) == FALSE) | 
        (stringr::str_count(entity_id[[i]], "_") != 1)) {
      stop(glue::glue("'entity_id' contains an invalid identifier. {entity_id[i]} cannot be matched with any listed identifier."))
    }
    res[[i]] <- entity_id[[i]]
    if (unlist(gregexpr('_', entity_id[[i]]))[1] < 4) {
      if (verbose) {
        warning(glue::glue("'entity_id' contains an unknown identifier. {entity_id[i]} cannot be matched with any listed identifier."))
      }
      names(res)[[i]] <- "invalid_entities"
    } else {
      names(res)[[i]] <- .entity_lookup(entity_id=entity_id[[i]], entity_list=entity_list, verbose=verbose)
    }
  }
  output_ids <- .entity_match(sorted_ids=res, entity_list=entity_list, api_key=benchling_api_key)
  return(output_ids)
}