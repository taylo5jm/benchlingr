# as_name.R

#' Extract the 'name' attribute for all entities through the responses made to their bulk-get API
#' endpoints or single API endpoint if bulk-get API endpoints register as NA for that entity.
#' 
#' This function will take a character vector or list of character strings and for each element 
#' send a request to that element's set of bulk-get API endpoints or single API endpoint if the 
#' bulk-get API endpoints are written as NA and analyze the response from that request and extract 
#' the 'name' attribute.
#' 
#' @include bulk_get_entity.R
#' @param entity_id Either a character vector or list that contains the entities. All entities 
#' in the list or character vector must be character strings.
#' @param return_df If return_df is TRUE, then the function will return a two column data frame
#' where the first column is named 'id' and represents the names of the identifiers or elements from 
#' entity_id and the second column is named 'name' and represents that element's corresponding 'name' 
#' attribute extracted from the request made to its set of bulk-get API endpoints or single API 
#' endpoint. If FALSE, the function will return a named list where the names are the elements from 
#' entity_id and the values are each element's corresponding 'name' attribute.
#' @returns
#' * If return_df is TRUE, 'as_name.R' returns a two column data frame where the first column is 
#' named 'id' and represents the names of the identifiers or elements from entity_id and the
#' second column is named 'name' and represents that element's corresponding 'name' attribute.
#' 
#' * If return_df is FALSE, 'as_name.R' returns a named list where the names are the identifiers 
#' or elements from entity_id and the values are each element's corresponding 'name' attribute.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm", "box_oiU2Xkf")
#' entities1 <- as.name(entity_id1, return_df=FALSE)
#' 
#' entity_id2 <- c("seq_Cuf0omCm", "bfi_Q1PMlXkf", "etr_n123p", "loc_43OIpas")
#' entities2 <- as_name(entity_id2, return_df=TRUE)
#' }
#' @export

as_name <- function(entity_id, return_df=FALSE) {
  entities <- bulk_get_entity(entity_id)
  # extract the 'name' attribute for all entities
  
  return(entities)
}
