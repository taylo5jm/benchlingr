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
#' endpoint. If FALSE, the function will return a named vector where the names are the elements from 
#' entity_id and the values are each element's corresponding 'name' attribute.
#' @returns
#' * If return_df is TRUE, 'as_name.R' returns a two column data frame where the first column is 
#' named 'id' and represents the names of the identifiers or elements from entity_id and the
#' second column is named 'name' and represents that element's corresponding 'name' attribute.
#' 
#' * If return_df is FALSE, 'as_name.R' returns a named vector where the names are the identifiers 
#' or elements from entity_id and the values are each element's corresponding 'name' attribute.
#' @examples \dontrun{
#' entity_id1 <- c("seq_Cuf0bmCm", "bfi_9fKcrORv")
#' entities1 <- as_name(entity_id=entity_id1, benchling_api_key = Sys.getenv("BENCHLING_API_KEY"), return_df=FALSE)
#' 
#' entity_id2 <- c("seq_Cuf0bmCm", "seq_Cuf0umCm", "bfi_9fKcrORv", "etr_n123p")
#' entities2 <- as_name(entity_id=entity_id2, benchling_api_key = Sys.getenv("BENCHLING_API_KEY"), return_df=TRUE)
#' }
#' @export

as_name <- function(entity_id, benchling_api_key = Sys.getenv("BENCHLING_API_KEY"), return_df=FALSE) {
  entities <- bulk_get_entity(entity_id = entity_id, benchling_api_key = benchling_api_key)
  entities_vector <- list()
  for (i in 1:length(entities)) {
    if ("Entity Name" %in% names(entities[[i]])) {
      entities_vector[[i]] <- entities[[i]]$`Entity Name`
      names(entities_vector)[i] <- names(entities)[i]
    } else {
      stop(glue::glue("Entity name is not listed for {entities[[i]]} or is not labeled as 'Entity Name.'"))
    }
    
    # Note: This code is just in case the vector/data frame should be modified to include the entity schema as well as the entity name for each entity identifier.
    # if ((!("Entity Schema" %in% names(entities[[i]]))) | (!("Entity Name" %in% names(entities[[i]])))) {
    #   if (!("Entity Schema" %in% names(entities[[i]]))) {
    #     message_schema <- glue::glue("Entity schema is not listed for {entities[[i]]} or is not labeled as 'Entity Schema.' ")
    #   } else {
    #     message_schema <- ""
    #   }
    #   if (!("Entity Name" %in% names(entities[[i]]))) {
    #     message_name <- glue::glue("Entity name is not listed for {entities[[i]]} or is not labeled as 'Entity Name.'")
    #   } else {
    #     message_name <- ""
    #   }
    #   stop(paste0(message_schema, message_name))
    # } else {
    #   entities_vector[[i]] <- c(entities[[i]]$`Entity Schema`, entities[[i]]$`Entity Name`)
    #   names(entities_vector)[i] <- names(entities)[i]
    #   names(entities_vector[[i]])[1] <- "Entity Schema"
    #   names(entities_vector[[i]])[2] <- "Entity Name"
    # }

  }
  entities_vector <- c(unlist(entities_vector))
  if (return_df == TRUE) {
    entities_dataframe <- data.frame("id" = names(entities_vector), "names" = entities_vector, row.names = NULL)
    
    # entities_dataframe <- data.frame("id" = names(entities_vector), 
    #                                  "schema" = as.data.frame(data.table::transpose(entities_vector))[,1],
    #                                  "names" = as.data.frame(data.table::transpose(entities_vector))[,2])
    
    if (anyNA(entities_dataframe)) {
      entities_dataframe <- entities_dataframe %>% replace(is.na(.),"NA")
    }
    return(entities_dataframe)
  } else {
    return(entities_vector)
  }
}
 