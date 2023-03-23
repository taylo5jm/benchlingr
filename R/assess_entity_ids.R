assess_entity_ids <- function(entity_id_list, benchling_api_key=Sys.getenv("BENCHLING_API_KEY")) {
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
  
  if (benchling_api_key == "") {
    stop("Benchling API key is missing or empty.")
  }
  inferred_types <- infer_entity_type(entity_id=entity_id)
  invalid_ids <- list()
  for (i in 1:length(inferred_types)) {
    invalid_ids_set <- list()
    for (j in 1:length(inferred_types[[1]])) {
      if (!is.na(inferred_types[[i]][[j]][[2]])) {
        content <- httr::content(httr::GET(inferred_types[[i]][[j]][[2]], 
                                           httr::authenticate(benchling_api_key, '')))
        if (!is.null(content$error)) {
          invalid_ids_set <- unlist(content$error$invalidIds)
        }
      }

    }
  }
  x <- httr::content(httr::GET(inferred_types[[2]][[1]][[2]], httr::authenticate(benchling_api_key, '')))
}
