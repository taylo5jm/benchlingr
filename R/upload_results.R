

#' Upload assay results to Benchling from a data frame
#' 
#' @importFrom magrittr %<>%
#' @include schema_utils.R
#' @param conn Database connection. 
#' @param client Benchling API client. 
#' @param df Data frame / tibble of results to be uploaded to Benchling. 
#' @param project_id Benchling project identifier.
#' @param schema_id Results schema ID (starts with "assaysch_"). 
#' @export
#' @examples \dontrun{
#' schemaId <- "assaysch_BoT5QoQc"
#' conn <- warehouse_connect("hemoshear")
#' }

upload_results <- function(conn, df, project_id, schema_id, tenant=Sys.getenv("BENCHLING_TENANT"),
                           id_or_name='name') {
  
  if (tenant == "") {
    .missing_tenant_error()
  }
  # 1. Check to see if all columns are present for all required fields in the results schema.
  df_is_valid <- verify_schema_fields(
    schema_id, schema_type='assay-result',
    df=df, strict_check = TRUE, tenant=tenant)

  # Stop if not
  # 3. Check to see if the types of the columns in the data frame match the types of the fields in the schema.
  schema_def <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT schema_field.archived$,schema.name as schema_name,schema.id,
    schema_field.name,schema_field.display_name,schema_field.type,
    schema_field.is_multi,schema_field.is_required,schema_field.system_name,
    schema_field.target_schema_id 
    FROM schema INNER JOIN schema_field ON 
    schema.id = schema_field.schema_id WHERE schema.id = '{`schemaId`}'"))
  
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
  names(target_schema_map) <- as.character(connected_tables$system_name)
  
  to_query <- c('entity_link', 'dropdown', 'storage_link')

  errors <- c()
  for (i in 1:length(colnames(df))) {
    errors <- .validate_column_types(
      errors, df[,i], colnames(d)[i],
      type_map=type_map, multi_select_map = multi_select_map)
    errors <- .validate_column_values(
      errors, df[,i], colnames(d)[i],
      type_map=type_map, multi_select_map=multi_select_map)
  }
  # Stop function execution and show all errors to the user. 
  assertthat::assert_that(length(errors) == 0,
                          msg=paste0(errors, collapse='\n'))
      
    # warn user if info is not getting written but notresent in the data frame
    # upload results with API/SDK
    # return a data frame with the submitted results, as well as the IDs. 
  
  
}

.validate_column_types <- function(errors, values, column_name,
                                   type_map, multi_select_map) {
  numeric_type_mapping <- c('integer' = 'numeric', 'float' = 'numeric')
  # Check characters
  if (type_map[column_name] %in% c('text', 'entity_link', 'dropdown',
                                       'long_text', 'storage_link', 'blob_link',
                                       'dna_sequence_link')) {
    is_text <- all(is.character(values))
    if (!is_text) {
      errors <- c(errors, c(glue::glue('{column_name} must be a character type.')))
    }
    #   
  } else if (type_map[column_name] %in% names(numeric_type_mapping)) {
    is_numeric <- all(is.numeric(values))
    if (!is_numeric) {
      errors <- c(errors, glue::glue("{column_name} must be a numeric type."))
    }
  } else if (multi_select_map[column_name]) {
    is_multiselect_list <- is.list(values)
    if (!is_multiselect_list) {
      errors <- c(errors, glue::glue("{column_name} must be a list."))
    }
  } 
  
  return(errors)
}


.validate_column_values <- function(errors, values, column_name) {
  # If the column is an entity_link, storage_link, or  dropdown,
  # then we need to check the values against the ones already registered. 
  if (type_map[column_name] == 'entity_link') {
    registered_values <- DBI::dbGetQuery(conn, 
                                         "SELECT {`id_or_name`} FROM {colnames(df)[i]} WHERE 
      {`id_or_name`} IN {.vec2sql_tuple(unique(df[[i]]))}")
    if (!(all(unique(values) %in% registered_values))) {
      # Which ones?
      errors <- c(errors, 
                  glue::glue(
                    "Not all values in '{column_name}' are registered."))
    }
  } else if (type_map[colnames(df)[i]] == 'dropdown') {
    get_dropdown_schema()
    
  } else if (type_map[colnames(df)[i]] == 'storage_link') {
    
  } else if (type_map[colnames(df)[i]] == 'blob_link') {
    # upload the files
    # get the IDs
    # upload IDs with results. 
    
  } else { # text / float / integer 
    
  }
  return(errors)
}
# Add these convenience functions for getting hte ProjectIds and results schema Ids

get_project_ids <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT id,name FROM project")
}



get_results_schema_ids <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT id,name FROM schema WHERE schema_type = 'assay_result'")
}


get_dropdown_options <- function(conn, dropdown_id) {
  DBI::dbGetQuery(conn, 
  glue::glue("SELECT dropdown_option.name FROM dropdown INNER JOIN 
  dropdown_option ON dropdown.id = dropdown_option.dropdown_id 
             WHERE dropdown.id = '{dropdown_id}'"))
}


get_dropdown <- function(conn, name) {
  DBI::dbGetQuery(conn, glue::glue("SELECT dropdown WHERE name = '{name}'"))
}
