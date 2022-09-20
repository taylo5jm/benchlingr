#' Map entity field names in one warehouse table to corresponding warehouse
#' table with the relevant entities. 
#' 
#' 
#' A schema field name can differ from the schema name itself, so this function
#' uses the `schema` and `schema_field` tables in the warehouse to link 
#' the field name in one schema to the actual name of the entity.
#' 
#' @include vec2sql_tuple.R
#' @importFrom magrittr %<>% %>%
#' @param conn Database connection opened by `warehouse_connect`
#' @param df data.frame with one or more fields that are entity types. 
#' @return A vector where the names are the warehouse names for the fields 
#' in the schema and the values are the warehouse names for the entities in the 
#' schema 
#' @export
#' @examples 
#' conn <- warehouse_connect(
#' "hemoshear-dev",
#'  username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'  password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#'  
#'  d <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw")
#'  .map_entity_field_names_to_warehouse_tables(conn, d)
.map_entity_field_names_to_warehouse_tables <- function(conn, df) {
  if (!('schema' %in% colnames(df))) {
    stop("'schema' column is missing from the input data.frame.
         Verify that the data.frame is a valid warehouse table.")
  }
  # Get the id of the schema
  schema_id <- DBI::dbGetQuery(conn, 
    glue::glue("SELECT id FROM schema WHERE system_name = {shQuote(unique(df$schema))}")) %>%
    as.character()
  # Get all fields for the schema
  res <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT * FROM schema_field WHERE schema_id = {shQuote(schema_id)}"))
  # Retain only the entity fields in the schema
  res %<>% dplyr::filter(!is.na(target_schema_id))
  # Get the actual names of the entity types in the target schema
  schema <- DBI::dbGetQuery(conn, 
    glue::glue("SELECT * FROM schema WHERE id IN {.vec2sql_tuple(res$target_schema_id)}"))
  # Trim the data frame down to the name and ID we care about. 
  ## The first column here is the name of the schema
  ## The second column is the name of the *field* that is the entity type. 
  schema <- dplyr::select(schema, id, system_name)
  colnames(schema) <- c('target_schema_id', 'entity_system_name')
  # Join the names we need to the original schema
  res %<>% dplyr::inner_join(schema, by='target_schema_id')
  mapping <- res$entity_system_name
  names(mapping) <- res$system_name
  mapping
}
