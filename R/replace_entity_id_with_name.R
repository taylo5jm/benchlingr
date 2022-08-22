# replace_entity_id_with_name.R

#' Replace identifiers in entity columns with the entity names. 
#' 
#' Values in an entity column will appear as Benchling identifiers when
#' pulled from the data warehouse. This function converts the identifiers 
#' into their human-readable names. For example, a custom entity value
#' will have an identifier that looks like "bfi_10dgdgFJx", but the name
#' might be "My cell line". 
#' 
#' @include get_entity_table.R
#' @param conn Database connection opened by `warehouse_connect`.
#' @param df Data frame with entity columns.
#' @return data.frame with the Benchling entity identifiers replaced by the 
#' names of the entities. 
#' @export
replace_entity_id_with_name <- function(conn, df) {
  # Get the rows from the data warehouse tables that correspond
  # to the entities in the data frame.
  res <- get_entity_table(conn, df, return_cols=c('id', 'name$'))
  # Closure to replace the ID with the name. 
  .replace_id <- function(df, column, mapping) {
    this_mapping <- mapping$`name$`
    names(this_mapping) <- mapping$id
    
    df[column] <- this_mapping[df[[column]]]
    return(df)
  }
  # Iterate over the entity columns and replace the IDs with 
  # the names. 
  for (i in 1:length(res)) {
    df[[names(res)[i]]] %<>% as.character()
    df <- tryCatch({
      .replace_id(df, names(res)[i], res[[i]])},
      error = function(e) {
        df
      })
  }
  return(df)
}

