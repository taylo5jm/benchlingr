# .get_api_contents.R

#' Attaches API URLs for an entity's list contents, single-get API endpoint contents and bulk-get API 
#' endpoints contents to input.
#' 
#' .get_api_contents.R takes a named list where the names are the entity schemas and the elements are vectors
#' that contain the entity identifiers associated with those schemas and generates a named list of lists.
#' 
#' @importFrom magrittr %in%
#' @param entity_id_list A named list where the names are entity schemas and the elements are vectors containing the entity 
#' identifiers corresponding to those schemas
#' @param entity_list A list of vectors where each vector designates the schema type and API URLs for 
#' the entities we are interested in and the names are the first characters seen in the identifiers for
#' each entity. 
#' @return A named list of lists where the names are the entity schemas and the elements are two lists in which one list 
#' contains the entity identifiers matched with that specific entity and the other contains that entity's list contents 
#' option, single-get API endpoint option and bulk-get API endpoints option formatted in the form of URLs.
#' @examples \dontrun{
#' entity_list <- .list_api_contents.R(contents="all", entity_list=NULL)
#' entity_id <- list("dna_sequence"=c("seq_Mh090ews"), "custom_entity"=c("bfi_klod8Xkf","bfi_Q9m0ulkf",
#' "bfi_Pokd7uWV"),"invalid_entity"=c("ve0k998720u","dpd_as0d0Oi"),"user"=c("ent_EuhqX69f"))
#' entity_id_info <- .get_api_contents(entity_id=entity_id, entity_list=entity_list)
#' }
#' @export
#' @keywords internal

.get_api_contents <- function(entity_id_list, entity_list) {
  API_URLs <- purrr::map(entity_list, ~ .[c(2,3,4)]) # Extracts metadata information for each entity 
                                                     # schema's list API contents, single-get API 
                                                     # endpoint contents and bulk-get endpoints 
                                                     # contents from Benchlingr
  names(API_URLs) <- purrr::map_chr(entity_list, ~ .[1]) # Assigns names using entity schemas in entity_list
  entity_id_types <- names(entity_id_list)
  entity_id_info <- list()
  for (i in 1:length(entity_id_types)) {
    if (entity_id_types[i] %in% names(API_URLs)) { # Checks if the input's name is an entity schema from entity_list
      entity_id_info[[i]] <- list("entity_identifiers"=entity_id_list[[entity_id_types[i]]], # Attaches an entity's metadata 
                                                                                             # information to its respective 
                                                                                             # entity schema under the vector 
                                                                                             # of identifiers associated with 
                                                                                             # it 
                                  "api_urls"=API_URLs[[entity_id_types[i]]])
    } else {
      warning(glue::glue("{entity_id_types[i]} is not a valid entity schema.")) # Generates a warning if the name is not a defined 
                                                                                # entity schema
      entity_id_info[[i]] <- list("entity_identifiers"=entity_id_list[[entity_id_types[i]]], # Attaches to it a vector containing 3 NA 
                                                                                             # values for its list API contents information,
                                                                                             # single-get API endpoint contents information,
                                                                                             # and bulk-get API endpoints contents 
                                                                                             # information
                                  "api_urls"=c(NA,NA,NA))
    }
    names(entity_id_info)[i] <- entity_id_types[i] # Makes the name the same as the input's name
  }
  return(entity_id_info)
}
