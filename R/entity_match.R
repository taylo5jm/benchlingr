entity_match(entity_ids_list, entity_list, benchling_api_key) {
  entity_types <- unique(names(entity_ids_list))
  entity_types1 <- entity_types[which(entity_types != 
                                        "invalid_entities")][which(unlist(lapply(entity_types[which(entity_types != "invalid_entities")], function(x)
                                          !is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][2]))))]
  entity_types1 <- entity_types1[which(unlist(lapply(entity_types1, function(x)
    (!is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][3]) |
       !is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][4])))))]
  
  entity_types2 <- c(entity_types[which(entity_types == "invalid_entities")],
                     entity_types[which(entity_types != "invalid_entities")][which(unlist(lapply(entity_types[which(entity_types != "invalid_entities")], function(x)
                       (is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][3]) &
                          is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][4])))))])
  
  entity_types3 <- entity_types[which(entity_types != 
                                        "invalid_entities")][which(unlist(lapply(entity_types[which(entity_types != "invalid_entities")], function(x)
                                          is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][2]))))]
  entity_types3 <- entity_types3[which(unlist(lapply(entity_types3, function(x)
    (!is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][3]) |
       !is.na(entity_list[names(which(purrr::map(entity_list, ~ .[1]) == x))][[1]][4])))))]
  
  tested_ids_list <- list()
  invalid_ids_list <- list()
  untested_ids_list <- list()
  if (length(entity_types1) > 0) {
    for (a in 1:length(entity_types1)) {
      ids <- unlist(entity_ids_list[which(names(entity_ids_list) == entity_types1[a])], use.names = FALSE)
      api_urls <- entity_list[which(purrr::map(entity_list, ~ .[1]) == entity_types1[[a]])][[1]][c(2,3,4)]
      valid_ids <- list()
      tested_ids <- list()
      for (b in 1:ceiling(length(ids)/50)) {
        ids_set <- ids[seq((1+((b-1)*50)),(b*50))][which(unlist(purrr::map(ids[seq((1+((b-1)*50)), (b*50))],
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
          invalid_ids_list <- unlist(append(invalid_ids_list, invalid_ids))
        } else {
          valid_ids_set <- ids_set
        }
        if (length(valid_ids_set) > 0) {
          valid_ids <- unlist(append(valid_ids, valid_ids_set))
        }
      }
      if (length(valid_ids) > 0) {
        tested_ids[[1]] <- list()
        if (!is.na(api_urls[3])) {
          for (c in 1:ceiling(length(valid_ids)/50)) {
            ids_set <- valid_ids[seq((1+((c-1)*50)), (c*50))][which(unlist(purrr::map(valid_ids[seq((1+((c-1)*50)), (c*50))],
                                                                                      ~ !is.na(.))))]
            lookup_url <- c("Bulk-Get API Endpoints URL", gsub("ENTITY_IDS", paste(ids_set, collapse = "%2C"),
                                                               api_urls[3]))
            tested_ids[[1]][[c]] <- list(ids_set, api_urls, lookup_url, "TESTED")
            if (ceiling(length(valid_ids)/50) > 1) {
              names(tested_ids[[1]])[c] <- paste0("set", as.character(l))
            } else {
              names(tested_ids[[1]])[1] <- "set"
            }
            if (length(ids_set) > 1) {
              names(tested_ids[[1]][[c]])[1] <- "entity_identifiers"
            } else {
              names(tested_ids[[1]][[c]])[1] <- "entity_identifier"
            }
            names(tested_ids[[1]][[c]])[2] <- "api_urls"
            names(tested_ids[[1]][[c]])[3] <- "lookup_url"
            names(tested_ids[[1]][[c]])[4] <- "status"
          }
        } else {
          if (!is.na(api_urls[2])) {
            for (d in 1:length(valid_ids)) {
              lookup_url <- c("Single-Get API Endpoint URL", gsub("ENTITY_ID",
                                                                  as.character(valid_ids[d]),
                                                                  api_urls[2]))
              tested_ids[[1]][[d]] <- list(valid_ids[d], api_urls, lookup_url, "TESTED")
              if (length(valid_ids) > 1) {
                names(tested_ids[[1]])[d] <- paste0("set", as.character(d))
              } else {
                names(tested_ids[[1]])[1] <- "set"
              }
              names(tested_ids[[1]][[d]])[1] <- "entity_identifier"
              names(tested_ids[[1]][[d]])[2] <- "api_urls"
              names(tested_ids[[1]][[d]])[3] <- "lookup_url"
              names(tested_ids[[1]][[d]])[4] <- "status"
            }
          }
        }
        names(tested_ids)[1] <- entity_types1[a]
        tested_ids_list <- append(tested_ids_list, tested_ids)
      }
    }
  }
  
  if (length(entity_types2) > 0) {
    invalid_ids_list <- append(invalid_ids_list, 
                               unlist(purrr::map(entity_types2, ~ entity_ids_list[which(names(entity_ids_list) == .)]), 
                                      use.names = FALSE))
    invalid_ids_list <- list("invalid_entities" = list("set" = list("entity_identifiers" = invalid_ids_list,
                                                                    "api_urls" = "INVALID",
                                                                    "lookup_url_info" = "INVALID",
                                                                    "status" = "INVALID")))
  }
  
  if (length(entity_types3) > 0) {
    for (e in 1:length(entity_types3)) {
      ids <- unlist(entity_ids_list[which(names(entity_ids_list) == entity_types3[e])],
                    use.names = FALSE)
      api_urls <- entity_list[which(purrr::map(entity_list, ~ .[1]) == entity_types3[[e]])][[1]][c(2,3,4)]
      untested_ids_list[[e]] <- list()
      if (!is.na(api_urls[3])) {
        for (f in 1:ceiling(length(ids)/50)) {
          ids_set <- ids[seq((1+((f-1)*50)), (f*50))][which(unlist(purrr::map(ids[seq((1+((f-1)*50)), (f*50))],
                                                                              ~ !is.na(.))))]
          lookup_url_info <- c("Bulk-Get API Endpoints URL", gsub("ENTITY_IDS", paste(ids_set, collapse = "%2C"),
                                                                  api_urls[3]))
          untested_ids_list[[e]][[f]] <- list(ids_set, api_urls, lookup_url_info, "UNTESTED")
          if (ceiling(length(ids)/50) > 1) {
            names(untested_ids_list[[e]])[f] <- paste0("set", as.character(f))
          } else {
            names(untested_ids_list[[e]])[1] <- "set"
          }
          if (length(ids_set) > 1) {
            names(untested_ids_list[[e]][[f]])[1] <- "entity_identifiers"
          } else {
            names(untested_ids_list[[e]][[f]])[1] <- "entity_identifier"
          }
          names(untested_ids_list[[e]][[f]])[2] <- "api_urls"
          names(untested_ids_list[[e]][[f]])[3] <- "lookup_url_info"
          names(untested_ids_list[[e]][[f]])[4] <- "status"
        }
        names(untested_ids_list)[e] <- entity_types3[e]
      } else {
        if (!is.na(api_urls[2])) {
          for (g in 1:length(ids)) {
            lookup_url_info <- c("Single-Get API Endpoint URL", gsub("ENTITY_ID",
                                                                     as.character(ids[g]),
                                                                     api_urls[2]))
            untested_ids_list[[e]][[g]] <- list(ids[g], api_urls, lookup_url_info, "UNTESTED")
            if (length(ids) > 1) {
              names(untested_ids_list[[e]])[g] <- paste0("set", as.character(g))
            } else {
              names(untested_ids_list[[e]])[1] <- "set"
            }
            names(untested_ids_list[[e]][[g]])[1] <- "entity_identifier"
            names(untested_ids_list[[e]][[g]])[2] <- "api_urls"
            names(untested_ids_list[[e]][[g]])[3] <- "lookup_url_info"
            names(untested_ids_list[[e]][[g]])[4] <- "status"
          }
        }
      }
      names(untested_ids_list)[e] <- entity_types3[e]
    }
  }
  
  if (length(c(tested_ids_list, invalid_ids_list, untested_ids_list)) > 0) {
    return(c(tested_ids_list, invalid_ids_list, untested_ids_list))
  } else {
    return(NULL)
  }
}