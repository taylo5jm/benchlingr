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
#' @examples 
#' conn <- warehouse_connect("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' 
#' df <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping_with_two_analytes$raw WHERE entry_id$ = 'etr_lnZDpRVI'")
#' res <- replace_entity_id_with_name(conn, df)
#' DBI::dbDisconnect(conn)

replace_entity_id_with_name <- function(conn, df) {
  # Get the rows from the data warehouse tables that correspond
  # to the entities in the data frame.
  entity_cols <- list_entity_columns(conn, df)
  multi_cols <- list_multiselect_columns(conn, df)
  res <- get_entity_table(conn, df, 
                          columns = colnames(df)[setdiff(entity_cols, multi_cols)],
                          return_cols=c('id', 'name$'))
  to_unpack <- which(multi_cols %in% entity_cols)
  if (length(to_unpack > 0)) {
    warning(glue::glue("{paste0(names(multi_cols)[to_unpack], collapse=',')} must be unpacked
                       with `expand_entity_column` before the ID can be replaced."))
  }
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

