# list_api_contents.R

#' Generate a named list where the names are the first characters in an 
#' identifier's name for a specific entity and each element is a vector with the 
#' first element in the vector being the entity schema, the second vector is the 
#' list API contents, the third is the Single-Get API endpoint option and the 
#' fourth is the Bulk-Get API endpoints option.
#' 
#' @param tenant A character string containing the name of the Benchling tenant 
#' for which we want to use to extract information for at least one entity or 
#' element.
#' @param contents A character string to specify which type of information to 
#' return for the entities listed: `"bulk-get endpoints"`, 
#' `"single-get endpoint"`,  `"list contents"`, `"entity schema"`, 
#' `"entity type"`, `"all"` (the default).
#' @return A named list where the names are the first characters in an 
#' identifier's name for a corresponding entity and the elements are vectors 
#' that can show either the entity's schema, list API contents, Single-Get API 
#' endpoint option, Bulk-Get API endpoints option or all of them respectively.
#' @examples \dontrun{
#' entity_list <- .list_api_contents(tenant="hemoshear-dev")
#' 
#' entity_schemas <- .list_api_contents(tenant="hemoshear-dev", 
#' contents="entity schema")
#' 
#' entity_list_contents <- .list_api_contents(tenant="hemoshear-dev", 
#' contents="list contents")
#' 
#' entity_single_get_contents <- .list_api_contents(tenant="hemoshear-dev", 
#' contents="single-get endpoint")
#' 
#' entity_bulk_get_contents <- .list_api_contents(tenant="hemoshear-dev", 
#' contents="bulk-get endpoints")
#' }
#' @keywords internal

.list_api_contents <- function(tenant, contents="all") {
  if (!is.character(tenant) | length(tenant) != 1) {
    # Checks if tenant is not a character string with a length of 1.
    stop(paste0("'tenant' must be a character string with a length of 1 when ",
                "'entity_list' is defined as NULL or left blank."))
    # Stops the function if tenant is not a character string with a length of 1.
  }

  if (grepl("^\\s*$", tenant)) {
    # Checks if tenant is an empty character string.
    stop(paste0("'tenant' cannot be a blank space when 'entity_list' is ",
                "defined as NULL or left blank. 'tenant' must be a character ",
                "string with a length of 1."))
    # Stops the function if tenant is an empty character string.
  }
  
  entity_list <- list("bat" = c("batch", 
                                paste0("https://", as.character(tenant),
                                       ".benchling.com/api/v2/batches?",
                                       "pageSize=50&sort=name&ids=ENTITY_IDS"),
                                paste0("https://", as.character(tenant),
                                       ".benchling.com/api/v2/batches/",
                                       "ENTITY_ID"),
                                paste0("https://", as.character(tenant),
                                       ".benchling.com/api/v2/batches:bulk-get?",
                                       "batchIds=ENTITY_IDS")),
                      "bfi" = c("custom_entity", 
                                paste0("https://", as.character(tenant),
                                       ".benchling.com/api/v2/=",
                                       "custom-entities?pageSize=50&sort=name&",
                                       "ids=ENTITY_IDS"),
                                paste0("https://", as.character(tenant),
                                       ".benchling.com/api/v2/",
                                       "custom-entities/ENTITY_ID"),
                                paste0("https://", as.character(tenant),
                                       ".benchling.com/api/v2/",
                                       "custom-entities:bulk-get?",
                                       "customEntityIds=ENTITY_IDS")),
                       "box" = c("box", 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/boxes?pageSize",
                                        "=50&sort=name&ids=ENTITY_IDS"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/boxes/",
                                        "ENTITY_ID"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/boxes:bulk-get?",
                                        "boxIds=ENTITY_IDS")),
                       "con" = c("container", 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/containers?",
                                        "pageSize=50&sort=name&ids=ENTITY_IDS"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/containers/",
                                        "ENTITY_ID"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/containers:",
                                        "bulk-get?containerIds=ENTITY_IDS")),
                       "ent" = c("user", 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/users?ids=",
                                        "ENTITY_IDS&pageSize=50&sort=name"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/users/",
                                        "ENTITY_ID"), 
                                 NA),
                       "etr" = c("entry", 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/entries?",
                                        "pageSize=50&sort=name&ids=ENTITY_IDS"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/entries/",
                                        "ENTITY_ID"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/entries:",
                                        "bulk-get?entryIds=ENTITY_IDS")),
                       "loc" = c("location", 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/locations?",
                                        "pageSize=50&sort=name&ids=",
                                        "ENTITY_IDS"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/locations/",
                                        "ENTITY_ID"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/locations:",
                                        "bulk-get?locationIds=ENTITY_IDS")),
                       "mxt" = c("mixture", 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/mixtures?",
                                        "pageSize=50&sort=name&ids=ENTITY_IDS"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/mixtures/",
                                        "ENTITY_ID"), 
                                 NA),
                       "plt" = c("plate", 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/plates?",
                                        "pageSize=50&sort=name&ids=ENTITY_IDS"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/plates/",
                                        "ENTITY_ID"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/plates:",
                                        "bulk-get?plateIds=ENTITY_IDS")),
                       "prtn" = c("aa_sequence", 
                                  paste0("https://", as.character(tenant),
                                         ".benchling.com/api/v2/aa-sequences?",
                                         "pageSize=50&sort=name&ids=",
                                         "ENTITY_IDS"),
                                  paste0("https://", as.character(tenant),
                                         ".benchling.com/api/v2/aa-sequences/",
                                         "ENTITY_ID"),
                                  paste0("https://", as.character(tenant),
                                         ".benchling.com/api/v2/aa-sequences:",
                                         "bulk-get?aaSequenceIds=ENTITY_IDS")),
                       "sfs" = c("dropdown", 
                                 NA,
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/dropdowns/",
                                        "ENTITY_ID"), 
                                 NA),
                       "sfso" = c("dropdown_option", 
                                  NA,
                                  paste0("https://", as.character(tenant),
                                         ".benchling.com/api/v2/dropdowns/",
                                         "ENTITY_ID"), 
                                  NA), 
                       "seq" = c("dna_sequence", 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/dna-sequences?",
                                        "pageSize=50&sort=name&ids=ENTITY_IDS"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/dna-sequences/",
                                        "ENTITY_ID"),
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/dna-sequences:",
                                         "bulk-get?dnaSequenceIds=ENTITY_IDS")))
  # Generates a named list called entity_list that contains information for each 
  # entity possible where the names are the first characters seen in an entity 
  # identifier's name for a specific entity type and the elements are the entity 
  # types and the URL formats for each corresponding entity type's 
  # list contents, single-get endpoint contents and bulk-get endpoints contents 
  # in the form of character strings with each format dependent on the tenant 
  # being accessed.
  
  if (contents == "all") {
    # Checks if contents is left as default or defined as "all."
    return(entity_list)
    # Returns all the elements for each specified entity type in entity_list.
    
  } else if (contents == "bulk-get endpoints") {
    # Checks if contents is defined as "bulk-get endpoints."
    new_entity_list <- purrr::map(entity_list, ~ .[4])
    # Extracts only the bulk-get endpoints contents element for each specified
    # entity type in entity_list and stores it in a new list called 
    # new_entity_list.
    names(new_entity_list) <- names(entity_list)
    # Names each entity type's corresponding bulk-get endpoints contents element 
    # after the name assigned to the entity type in entity_list.
    return(new_entity_list)
    # Returns only the bulk-get endpoints contents element for each entity type 
    # in entity_list.
    
  } else if (contents == "single-get endpoint") {
    # Checks if contents is defined as "single-get endpoint."
    new_entity_list <- purrr::map(entity_list, ~ .[3])
    # Extracts only the single-get endpoint contents element for each specified
    # entity type in entity_list and stores it in a new list called
    # new_entity_list.
    names(new_entity_list) <- names(entity_list)
    # Names each entity type's corresponding single-get endpoint contents 
    # element after the name assigned to the entity type in entity_list.
    return(new_entity_list)
    # Returns only the single-get endpoint contents element for each entity type 
    # in entity_list.
    
  } else if (contents == "list contents") {
    # Checks if contents is defined as "list contents."
    new_entity_list <- purrr::map(entity_list, ~ .[2])
    # Extracts only the list contents element for each specified entity type in 
    # entity_list and stores it in a new list called new_entity_list.
    names(new_entity_list) <- names(entity_list)
    # Names each entity type's corresponding list contents element after the 
    # name assigned to the entity type in entity_list.
    return(new_entity_list)
    # Returns only the list contents element for each entity type in 
    # entity_list.
    
  } else if (contents == "entity schema" | contents == "entity type") {
    # Checks if contents is defined as either "entity schema" or "entity type."
    new_entity_list <- purrr::map(entity_list, ~ .[1])
    # Extracts for each entity type the element containing the name for their
    # type in entity_list and stores it in a new list called new_entity_list.
    names(new_entity_list) <- names(entity_list)
    # Names each entity type after the name associated with it and its 
    # corresponding list contents element, single-get endpoint contents element
    # and bulk-get endpoints contents element in entity_list.
    return(new_entity_list)
    # Returns only the entity types from entity_list.
    
  } else {
    stop(paste0("Invalid argument for contents. Input for contents should ",
                "either be defined as 'all', 'bulk-get endpoints', ", 
                "'single-get endpoint', 'list contents', 'entity schema', ",
                "'entity type' or left as NULL."))
    # Stops the function when contents is not defined as either "all", 
    # "bulk-get endpoints", "single-get endpoint", "list contents", "entity 
    # schema", "entity type" or NULL.
  }
}
