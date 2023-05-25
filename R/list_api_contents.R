# list_api_contents.R

#' Generate a named list where the names are the first characters in an identifier's name for a specific
#' entity and each element is a vector with the first element in the vector being the entity schema, the 
#' second vector is the list API contents, the third is the Single-Get API endpoint option and the fourth
#' is the Bulk-Get API endpoints option.
#' 
#' @param contents A character string to specify which type of information to return for the entities 
#' listed: `"bulk-get endpoints"`, `"single-get endpoint"`,  `"list contents"`, `"entity schema"`,
#' `"entity types"`, `"all"` (the default).
#' @param entity_list A list of vectors where each vector designates the schema type and API options for 
#' the entities we are interested in and the names are the first characters seen in the identifiers for
#' each entity. If NULL (the default), the function will use a default list.
#' 
#' Alternatively, a custom or more specified list can be used based on the purpose and intent of the list
#' and overall application of the function.
#' 
#' @return A named list where the names are the first characters in an identifier's name for a corresponding
#' entity and the elements are vectors that can show either the entity's schema, list API contents, 
#' Single-Get API endpoint option, Bulk-Get API endpoints option or all of them respectively.
#' @examples \dontrun{
#' entity_list <- .list_api_contents()
#' 
#' entity_schemas <- .list_api_contents(contents = "entity schema")
#' entity_list_contents <- .list_api_contents(contents = "list contents")
#' entity_single_get_contents <- .list_api_contents(contents = "single-get endpoint")
#' entity_bulk_get_contents <- .list_api_contents(contents = "bulk-get endpoints")
#' }
#' @keywords internal

.list_api_contents <- function(contents="all", entity_list=NULL) {
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
    new_entity_list <- purrr::map(entity_list, ~ .[4])
    names(new_entity_list) <- names(entity_list)
    return(new_entity_list)
    
  } else if (contents == "single-get endpoint") {
    new_entity_list <- purrr::map(entity_list, ~ .[3])
    names(new_entity_list) <- names(entity_list)
    return(new_entity_list)
    
  } else if (contents == "list contents") {
    new_entity_list <- purrr::map(entity_list, ~ .[2])
    names(new_entity_list) <- names(entity_list)
    return(new_entity_list)
    
  } else if (contents == "entity schema" | contents == "entity types") {
    new_entity_list <- purrr::map(entity_list, ~ .[1])
    names(new_entity_list) <- names(entity_list)
    return(new_entity_list)
    
  } else {
    stop("Invalid argument for contents. Input for contents should either be defined as 'all', 
         'bulk-get endpoints', 'single-get endpoint', 'list contents', 'entity schema', 
         'entity types' or left as NULL.")
  }
}
