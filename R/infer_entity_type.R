# infer_entity_type.R

#' Infer the entity types of elements contained in a vector
#' 
#' infer_entity_type.R takes a character vector with one or more elements and 
#' for each element tries to infer the schema type.
#' 
#' @include list_api_contents.R
#' @param entity_id A character vector with 1 or more elements.
#' @param tenant A character string containing the name of the Benchling tenant 
#' for which we want to use to extract information for at least one entity or 
#' element. 
#' @return A named character vector, where the names are the identifiers or 
#' elements from entity_id and the values are the schema types.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm", "bfi_Q1PMlXkf", "ver_io98720u")
#' res1 <- infer_entity_type(entity_id=entity_id1, entity_list=NULL)
#' 
#' entity_id2 <- c("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", 
#' "bfi_Ks908uWV",
#' "ent_Ec76qX9f", "ent_sPrxBNOh", "box_K9950IQ8", "dis_89mkooip", 
#' "bxo_923aklum")
#' res2 <- infer_entity_type(entity_id=entity_id2, entity_list=NULL)
#' }
#' @export

infer_entity_type <- function(entity_id, tenant) {
  if (!is.character(entity_id) | length(entity_id) == 0) { 
    # Checks if entity_id is a character vector with a length greater than 0.
    stop(paste0("'entity_id' input is invalid. Must be a character vector with",
                "a length greater than 0."))
    # Stops the function if entity_id is not a character vector or the length is
    # equal to 0.
  } 

  if (any(is.na(entity_id))) { 
    # Checks if entity_id contains any values that are NA.
    entity_id <- as.character(stats::na.omit(entity_id)) 
    # Removes NA values.
    warning("'entity_id' contains NA values. Removing them.")
    # Optional: stop("'entity_id' input is invalid. Must not contain NA 
    # values.")
  }
  
  if (any(grepl("^\\s*$", entity_id))) { 
    # Check if entity_id contains blank spaces.
    warning("'entity_id' contains blank elements.")
    # Optional: stop("'entity_id' input is invalid. Must not contain blank or 
    # empty strings.")
  }

  if (is.null(tenant)) {
    stop("'tenant' cannot be blank or left as NULL. Must be a character 
         string.")
  }
  
  if (is.null(tenant)) { 
    # Checks if entity_list has not been defined.
    entity_list <- .list_api_contents(contents="all", entity_list=NULL) 
    # Defines entity_list if left as NULL using .list_api_contents.R.
  }
  
  entity_prefix <- purrr::map_chr(entity_id, ~ gsub("^([[:alnum:]]+)_.+","\\1",.)) # Extracts the entity prefix.
  
  entity_sublist <- entity_list[entity_prefix]  # Extracts the entities from entity_list associated with the elements in entity_id.
  entity_sublist[sapply(entity_sublist, is.null)] <- NA # Assigns NA to all invalid identifiers in entity sublist.
  
  entity_types <- purrr::map_chr(entity_sublist, ~ .[1]) # Extracts entity types from entity sublist.
  names(entity_types) <- entity_id # Assigns names using entity_id.
  
  return(entity_types)
}
 