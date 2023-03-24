# list_api_contents.R

list_api_contents <- function(contents="all", entity_list=NULL) {
  if (is.null(entity_list)) {
    entity_list <- list("bat" = c("batch", 
                                  "https://hemoshear-dev.benchling.com/api/v2/batches?pageSize=50&sort=name&ids=ENTITY_IDS",
                                  "https://hemoshear-dev.benchling.com/api/v2/batches/ENTITY_ID",
                                  "https://hemoshear-dev.benchling.com/api/v2/batches:bulk-get?batchIds=ENTITY_IDS"), 
                        "bfi" = c("custom_entity", 
                                  "https://hemoshear-dev.benchling.com/api/v2/custom-entities?pageSize=50&sort=name&ids=ENTITY_IDS",
                                  "https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID",
                                  "https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=ENTITY_IDS"),
                        "box" = c("box", 
                                  "https://hemoshear-dev.benchling.com/api/v2/boxes?pageSize=50&sort=name&ids=ENTITY_IDS",
                                  "https://hemoshear-dev.benchling.com/api/v2/boxes/ENTITY_ID",
                                  "https://hemoshear-dev.benchling.com/api/v2/boxes:bulk-get?boxIds=ENTITY_IDS"),
                        "con" = c("container", 
                                  "https://hemoshear-dev.benchling.com/api/v2/containers?pageSize=50&sort=name&ids=ENTITY_IDS",
                                  "https://hemoshear-dev.benchling.com/api/v2/containers/ENTITY_ID",
                                  "https://hemoshear-dev.benchling.com/api/v2/containers:bulk-get?containerIds=ENTITY_IDS"),
                        "ent" = c("user", 
                                  "https://hemoshear-dev.benchling.com/api/v2/users?ids=ENTITY_IDS&pageSize=50&sort=name",
                                  "https://hemoshear-dev.benchling.com/api/v2/users/ENTITY_ID", 
                                  NA),
                        "etr" = c("entry", 
                                  "https://hemoshear-dev.benchling.com/api/v2/entries?pageSize=50&sort=name&ids=ENTITY_IDS",
                                  "https://hemoshear-dev.benchling.com/api/v2/entries/ENTITY_ID",
                                  "https://hemoshear-dev.benchling.com/api/v2/entries:bulk-get?entryIds=ENTITY_IDS"),
                        "loc" = c("location", 
                                  "https://hemoshear-dev.benchling.com/api/v2/locations?pageSize=50&sort=name&ids=ENTITY_IDS",
                                  "https://hemoshear-dev.benchling.com/api/v2/locations/ENTITY_ID",
                                  "https://hemoshear-dev.benchling.com/api/v2/locations:bulk-get?locationIds=ENTITY_IDS"),
                        "mxt"= c("mixture", 
                                 "https://hemoshear-dev.benchling.com/api/v2/mixtures?pageSize=50&sort=name&ids=ENTITY_IDS",
                                 "https://hemoshear-dev.benchling.com/api/v2/mixtures/ENTITY_ID", 
                                 NA),
                        "plt" = c("plate", 
                                  "https://hemoshear-dev.benchling.com/api/v2/plates?pageSize=50&sort=name&ids=ENTITY_IDS",
                                  "https://hemoshear-dev.benchling.com/api/v2/plates/ENTITY_ID",
                                  "https://hemoshear-dev.benchling.com/api/v2/plates:bulk-get?plateIds=ENTITY_IDS"),
                        "prtn" = c("aa_sequence", 
                                   "https://hemoshear-dev.benchling.com/api/v2/aa-sequences?pageSize=50&sort=name&ids=ENTITY_IDS",
                                   "https://hemoshear-dev.benchling.com/api/v2/aa-sequences/ENTITY_ID",
                                   "https://hemoshear-dev.benchling.com/api/v2/aa-sequences:bulk-get?aaSequenceIds=ENTITY_IDS"),
                        "sfs" = c("dropdown", 
                                  NA,
                                  "https://hemoshear-dev.benchling.com/api/v2/dropdowns/ENTITY_ID", 
                                  NA),
                        "sfso" = c("dropdown_option", 
                                   NA,
                                   "https://hemoshear-dev.benchling.com/api/v2/dropdowns/ENTITY_ID", 
                                   NA), 
                        "seq" = c("dna_sequence", 
                                  "https://hemoshear-dev.benchling.com/api/v2/dna-sequences?pageSize=50&sort=name&ids=ENTITY_IDS",
                                  "https://hemoshear-dev.benchling.com/api/v2/dna-sequences/ENTITY_ID",
                                  "https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=ENTITY_IDS"))
  }
  if (contents == "all") {
    return(entity_list)
  } else if (contents == "bulk-get endpoints") {
    new_entity_list <- purrr::map(entity_list, ~ .[c(1,4)])
    names(new_entity_list) <- names(entity_list)
    return(new_entity_list)
  } else if (contents == "single-get endpoint") {
    new_entity_list <- purrr::map(entity_list, ~ .[c(1,3)])
    names(new_entity_list) <- names(entity_list)
    return(new_entity_list)
  } else if (contents == "list contents") {
    new_entity_list <- purrr::map(entity_list, ~ .[c(1,2)])
    names(new_entity_list) <- names(entity_list)
    return(new_entity_list)
  } else {
    stop("Invalid argument for contents.")
  }
}
