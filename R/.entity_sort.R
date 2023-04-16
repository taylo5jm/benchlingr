# .entity_sort.R

#' Organize and group entity identifiers according to their respective entity schemas
#' 
#' .entity_sort.R takes a named character vector where the names are entity identifiers and the elements 
#' are the entity schemas for those identifiers or NA if an identifier cannot be matched with any other 
#' entity schema and organizes it into a named list where the names are the entity schemas and the 
#' elements are vectors containing the entity identifiers that correspond with those entity schemas. 
#' Entity identifiers whose values were NA will be placed in another vector in the list under the name 
#' "invalid_entity."
#' 
#' @param entity_id A named character vector where the names are entity identifiers and the elements are 
#' the entity schemas.
#' @return A named list, where the names are the entity schemas for entity identifiers associated with
#' a specific entity or invalid_entity for entity identifiers that could not be matched with any entity 
#' and the elements are vectors that contain the entity identifiers associated with those schemas and 
#' therefore placed under their respective names and the entity identifiers not associated with any 
#' entity at all and placed under invalid_entity.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm"="dna_sequence", "bfi_Q1PMlXkf"="custom_entity", "ver_io98720u"=NA)
#' res1 <- .entity_sort(entity_id1)
#' 
#' entity_id2 <- c("bfi_Ur5DfvGJ"="custom_entity", "seq_Gju61mCm"="dna_sequence", 
#' "bfi_Q13AlXkf"="custom_entity", "bfi_Ks908uWV"="custom_entity", "ent_Ec76qX9f"="user", 
#' "ent_sPrxBNOh"="user", "box_K9950IQ8"="box", "dis_89mkooip"=NA, "bxo_923aklum"=NA)
#' res2 <- .entity_sort(entity_id2)
#' }
#' @export
#' @keywords internal

.entity_sort <- function(entity_id) {
  entity_id[which(is.na(entity_id))] <- "invalid_entity"
  entity_id_types <- unique(entity_id)
  entity_id_list <- purrr::map(entity_id_types, ~ names(entity_id[which(entity_id == .)]))
  names(entity_id_list) <- entity_id_types
  if 
  return(entity_id_list)
}
