# infer_entity_type.R

#' Infer the entity types of elements contained in a vector or list
#' 
#' infer_entity_type.R takes either a character vector or list of character strings and 
#' for each element tries to infer the schema type using string-matching techniques.
#' 
#' @param entity_id Either a character vector or list that contains the entities that we 
#' are trying to find the schema types for. All entities in the list or character vector 
#' must be character strings.
#' @return A named character vector, where the names are the identifiers or elements from entity_id
#' and the values are the schema types.
#' @examples \dontrun{
#' entity_id <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf")
#' res <- infer_entity_type(entity_id)
#' }
#' @export



infer_entity_type <- function(entity_id) {
  entity_lookup <- list("plt_" = c("plate", "https://benchling.com/api/reference#/Plates/getPlate"),
                        "box_" = c("box", "https://benchling.com/api/reference#/Boxes/getBox"),
                        "con_" = c("container", "https://benchling.com/api/reference#/Containers/getContainer"),
                        "loc_" = c("location", "https://benchling.com/api/reference#/Locations/getLocation"),
                        "etr_" = c("entry", "https://benchling.com/api/reference#/Entries/getEntry"),
                        "bfi_" = c("custom_entity", "https://benchling.com/api/reference#/Custom%20Entities/getCustomEntity"),
                        "ent_" = c("user", "https://benchling.com/api/reference#/Users/getUser"),
                        "sfs_" = c("dropdown", "https://benchling.com/api/reference#/Dropdowns/getDropdown"),
                        "sfso_" = c("dropdown_option", "https://benchling.com/api/reference#/Dropdowns/getDropdown"), # the dropdown options are available from the `dropdown` endpoint, as well as the `dropdown_option` warehouse table. 
                        "seq" = c("dna_sequence", NA), # both dna_oligo and dna_sequence types start with seq, so there isn't one endpoint. find these in the database in the `entity` table instead.
                        "mxt"= c("mixture", "https://benchling.com/api/reference#/Mixtures/getMixture"),
                        "container_batch" = c("container_content", "https://benchling.com/api/reference#/Containers/getContainerContent"))
  
  if (missing(entity_id)) {
    stop("'entity_id' input is missing.")
  }
  
  if (is.null(entity_id) | length(entity_id) == 0) {
    stop("'entity_id' input is invalid.")
  } 
  
  if (anyNA(entity_id, recursive = TRUE)) {
    stop("'entity_id' input is invalid.")
  }
  
  if (is.list(entity_id)) {
    for (i in 1:length(entity_id)) {
      if (length(entity_id[[i]]) > 1) {
        stop("'entity_id' input contains an invalid identifier.")
      } 
      if (length(entity_id[[i]]) == 1 & !is.character(entity_id[[i]][1])) {
        stop("'entity_id' input contains an invalid identifier.")
      }
      if (length(entity_id[[i]]) == 1 & is.character(entity_id[[i]][1])) {
        next
      }
    }
    entity_id <- c(unlist(entity_id))
  }
  
  res <- list()
  
  for (i in 1:length(entity_id)){
    if (!is.character(entity_id[i])) {
      stop("'entity_id' contains an invalid identifier.")
    } else {
      if (unlist(gregexpr('_', entity_id[i]))[1] < 4) {
        res[i] <- NA
        names(res)[i] <- entity_id[i]
      }
      if (unlist(gregexpr('_', entity_id[i]))[1] >= 4) {
        if (!identical(grep(substr(entity_id[i],1,unlist(gregexpr('_', entity_id[i]))[1]-1),names(entity_lookup)),integer(0))) {
          res[i] <- entity_lookup[[grep(substr(entity_id[i],1,unlist(gregexpr('_', entity_id[i]))[1]-1),names(entity_lookup))]][1]
          names(res)[i] <- entity_id[i]
        } else {
          res[i] <- NA
          names(res)[i] <- entity_id[i]
        }
      } 
    }
  }
  res <- unlist(res, use.names = TRUE)
  res
}


