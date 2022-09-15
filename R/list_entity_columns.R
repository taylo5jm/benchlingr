# list_entity_columns.R

#' List the names and indices of entity columns in a data frame corresponding
#' to a warehouse table. 
#' 
#' @include util.R
#' @importFrom magrittr %>%
#' @param conn Database connection opened with `warehouse_connect`
#' @param df Data frame with entity columns
#' @return Vector where the names are the names of entity columns in the data frame
#' and the values are the corresponding indices. 
#' @examples 
#' conn <- warehouse_connect('hemoshear')
#' res <- DBI::dbGetQuery(conn, "SELECT * FROM cdd_vault_compound_concentration")
#' list_entity_columns(conn, res)
#' DBI::dbDisconnect(conn)
#' @export 
list_entity_columns <- function(conn, df) {
  is_schema_in_dataframe(df)
  schema_id <- DBI::dbGetQuery(
    conn, glue::glue("SELECT id FROM schema WHERE system_name = {shQuote(unique(df$schema))}")) %>%
    as.character()
  res <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT * FROM schema_field WHERE schema_id = {shQuote(schema_id)}")) %>%
    dplyr::filter(!is.na(target_schema_id))
  entities <- which(colnames(df) %in% res$system_name)
  names(entities) <- colnames(df)[entities]
  entities
}