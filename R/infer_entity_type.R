# infer_entity_type.R

#' Infer the entity types of elements contained in a vector or list
#' 
#' infer_entity_type.R takes either a character vector or list of character strings and 
#' for each element tries to infer the schema type.
#' 
#' @param entity_id Either a character vector or list that contains the entities. 
#' All entities in the list or character vector must be character strings.
#' @param entity_list A list of vectors where each vector designates the schema type and API URLs for 
#' the entities we are interested in and the names are the first characters seen in the identifiers for
#' each entity. 
#' @return A named list, where the names are the identifiers or elements from entity_id
#' and the values are the schema types.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf", "ver_io98720u")
#' res1 <- infer_entity_type(entity_id1)
#' 
#' entity_id2 <- c("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", "bfi_Ks908uWV",
#' "ent_Ec76qX9f", "ent_sPrxBNOh", "box_K9950IQ8", "dis_89mkooip", "bxo_923aklum")
#' res2 <- infer_entity_type(entity_id2)
#' }
#' @export
#' @keywords internal

infer_entity_type <- function(entity_id, entity_list=NULL) {
  if (!is.character(entity_id) | length(entity_id) == 0) { # Checks if entity_id is a character vector with a length greater than 0.
    stop("'entity_id' input is invalid. Must be a character vector with a length greater than 0.")
  } 
  
  if (any(purrr::map(entity_id, ~ length(.) != 1) == TRUE)) { # Checks if entity_id contains only single elements.
    stop("'entity_id' input is invalid. Must be a character vector with single elements.")
  }
  
  if (any(is.na(entity_id))) { # Checks if entity_id contains any values that are NA.
    stop("'entity_id' input is invalid. Cannot contain values that are NA.")
  }
  
  if (any(grepl("^\\s*$", entity_id))) { # Checks if entity_id contains any empty or blank strings.
    stop("'entity_id' input is invalid. Cannot contain empty or blank strings.")
  }

  if (is.null(entity_list)) { # Checks if entity_list has not been defined
    entity_list <- list_api_contents(contents="all") # Defines entity_list if left as NULL using list_api_contents.R
  }
  
  entity_prefix <- purrr::map_chr(entity_id, ~ gsub("^([[:alnum:]]+)_.+","\\1",.)) # Extracts the entity prefix
  entity_sublist <- entity_list[entity_prefix] 
  entity_sublist[sapply(entity_sublist, is.null)] <- NA # Assigns NA to all invalid identifiers in entity sublist
  
  entity_types <- purrr::map_chr(entity_sublist, ~ .[1]) # Extracts entity types from entity sublist
  names(entity_types) <- entity_id # Assigns names using entity_id
  
  return(entity_types)
}

 