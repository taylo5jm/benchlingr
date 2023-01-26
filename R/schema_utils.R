#' Get schema fields from the data warehouse.
#' @param conn Database connection opened with benchlingr::warehouse_connect
#' @param schema_id Schema identifier
#' @include vec2sql_tuple.R
#' @importFrom magrittr %<>%
#' @keywords internal
.get_schema_field_metadata <- function(conn, schema_id) {
  schema_def <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT schema_field.archived$,schema.name as schema_name,schema.id,
        schema_field.name,schema_field.display_name,schema_field.type,
        schema_field.is_multi,schema_field.is_required,schema_field.system_name,
        schema_field.target_schema_id 
        FROM schema INNER JOIN schema_field ON 
        schema.id = schema_field.schema_id WHERE schema.id = '{`schema_id`}'"))
  
  if (nrow(schema_def) == 0) {
    stop('Schema ID not found. Run `DBI::dbGetQuery(conn, "SELECT * FROM schema")`
             to see all')
  }
  
  # Create a lookup for the column types.
  type_map <- as.character(schema_def$type)
  names(type_map) <- as.character(schema_def$system_name)
  
  # Create a lookup for multi-select.
  multi_select_map <- as.character(schema_def$is_multi)
  names(multi_select_map) <- as.character(schema_def$system_name)
  
  # Find the warehouse table names for the entity_link and storage_link columns. 
  connected_tables <- DBI::dbGetQuery(
    conn, 
    glue::glue(
      "SELECT id as target_schema_id,schema_type,
        system_name as warehouse_table_name FROM schema WHERE id IN 
        {.vec2sql_tuple(unique(schema_def$target_schema_id))}"))
  schema_def %<>% dplyr::left_join(connected_tables)
  
  # Create a lookup for the warehouse tables
  target_schema_map <- as.character(schema_def$warehouse_table_name)
  names(target_schema_map) <- as.character(schema_def$system_name)
  
  return(
    list(
      type_map = type_map,
      target_schema_map = target_schema_map,
      multi_select_map = multi_select_map
    )
  )
}

#' Makes a direct API Call to Benchling Schema endpoints without benchling-sdk client.
#' 
#' @include error.R
#' @param schema_id provided schema id
#' @param schema_type schema type name as it shown on benhcling API endpoints 
#' with or without `-schemas` ending. See \url{https://benchling.com/api/reference#/Schemas}.
#' @param tenant is tenant name in the form "https://your-organization.benchling.com".
#' Default value is the `BENCHLING_TENANT` environment variable. 
#' @param api_key API key. Default value is the `BENCHLING_API_KEY` environment
#' variable. 
#' @examples \dontrun{
#' schema_id <- "assaysch_nIw4yAq8"
#' schema_type <- "assay-result"
#' }
#' @keywords internal
get_schema_fields <- function(schema_id, schema_type, 
                              tenant=Sys.getenv("BENCHLING_TENANT"),
                              api_key=Sys.getenv("BENCHLING_API_KEY")) {
  
  if (api_key == "") {
    .missing_api_key_error()
  }
  
  if (tenant == "") {
    .missing_tenant_error()
  }

  if ('-schemas' %in% schema_type) {
    schema_type <- gsub("-schemas", "", schema_type)
  }

  base_url <- glue::glue('{tenant}/api/v2/{schema_type}-schemas/{schema_id}')

  schema_fields_raw <- httr::GET(url = base_url, httr::authenticate(api_key, ''))
  schema_fields <- httr::content(schema_fields_raw)
  if (length(schema_fields$error) > 0) {
    stop(schema_fields$error)
  }
  field_definitions <- schema_fields$fieldDefinitions
  return(field_definitions)
}

#'
#' compares schema fields with dataframe colnames and verifies
#'
#' @param schema_id provided schema id
#' @param schema_type schema type name as it shown on benhcling API endpoints with or without `-schemas` ending.
#' @param df provided dataframe
#' @param strict_check is an optional arguement. if set TRUE, the function looks for every schema field names in colnames.
#' @param tenant is tenant name. If missing, it will be reading from sys.env
#' @param api_key API key. Default value is the `BENCHLING_API_KEY` environment
#' variable. 
#' @examples \dontrun{
#' schema_id <- "assaysch_nIw4yAq8"
#' schema_type <- "assay-result",
#' df <- data.frame(
#'  "plate" = c('davut'),
#'  "analytes" = c('0.2'),
#'  'File1' = c('aaaa'),
#'  check.names = FALSE
#' }
#' @return boolean field
#' @keywords internal

verify_schema_fields <- function(schema_id, schema_type, df, strict_check = FALSE, 
                                 tenant=Sys.getenv("BENCHLING_TENANT"),
                                 api_key=Sys.getenv("BENCHLING_API_KEY")) {

  if (tenant == "") {
    .missing_tenant_error()
  }

  if (is.null(df) || nrow(df) == 0) {
    stop('Dataframe is empty or none.')
    return(FALSE)
  }

  column_names <- names(df)
  field_definitions <- get_schema_fields(schema_id, schema_type, tenant = tenant,
                                         api_key=api_key)

  if (length(field_definitions) == 0) {
    stop("This schema has no fields.")
    return(FALSE)
  }

  for (field in field_definitions) {
    if (field$isRequired == TRUE) {
      if (!(field$name %in% column_names)) {
        stop(glue::glue('\`{field$name}\` is a required field of this schema.
        Please verify your dataframe fields.'))
        return(FALSE)
      }
    }else if (strict_check) {
      if (!(field$name %in% column_names)) {
        stop(glue::glue('\`{field$name}\` does not match with any column of this df.
         Please either provide the required field or call this method with strict_check=FALSE'))
        return(FALSE)
      }
    }
  }

  # todo Check to see if the types of the columns in the data frame match the types of the fields in the schema.

  return(TRUE)
}

.verify_schema_field_types <- function(schema_fields, df, strict_check) {


}