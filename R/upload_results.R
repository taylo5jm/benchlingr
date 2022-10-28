

#' Upload assay results to Benchling from a data frame
#' 
#' @include schema_utils.R
#' @param conn Database connection. 
#' @param client Benchling API client. 
#' @param df Data frame / tibble of results to be uploaded to Benchling. 
#' @param projectId Benchling project identifier.
#' @param schemaId Results schema ID (starts with "assaysch_"). 
#' @export
#' @examples \dontrun{
#' schemaId <- "assaysch_BoT5QoQc"
#' }
#' 
# client <- benchlingr::benchling_api_auth(tenant="https://hemoshear.benchling.com")
upload_results <- function(df, project_id, schema_id, tenant=Sys.getenv("BENCHLING_TENANT"),
                           id_or_name='name') {
  
  if (tenant == "") {
    .missing_tenant_error()
  }
  # 1. Check to see if all columns are present for all required fields in the results schema.
  df_is_valid <- verify_schema_fields(schema_id, schema_type='assay-result',
                       df=df, strict_check = TRUE,
                       tenant=tenant)

  # Stop if not
  # 3. Check to see if the types of the columns in the data frame match the types of the fields in the schema.
  conn <- warehouse_connect("hemoshear")
  schema_def <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT schema_field.archived$,schema.name,schema.id,
    schema_field.name,schema_field.display_name,schema_field.type,
    schema_field.is_multi,schema_field.is_required,schema_field.system_name  
    FROM schema INNER JOIN schema_field ON 
    schema.id = schema_field.schema_id WHERE schema.id = '{`schemaId`}'"))
  type_map <- as.character(schema_def$type)
  names(type_map) <- as.character(schema_def$system_name)
  
  multi_select_map <- as.character(schema_def$is_multi)
  names(multi_select_map) <- as.character(schema_def$system_name)
  
  #table_name <- as.character(schema_def$is_multi)
  #names(multi_select_map) <- as.character(schema_def$system_name)
  
  to_query <- c('entity_link', 'dropdown', 'storage_link')
  basic_type_mapping <- c('integer' = 'numeric', 'float' = 'numeric')
  schema_def
  errors <- c('')
  for (i in 1:length(colnames(df))) {
    # type check
    if (type_map[colnames(df)] %in% c('text', to_query)) {
      is_text <- all(is.character(df[,i]))
      if (!is_text) {
        errors <- c(errors, c(glue::glue('{colnames(df)[i]} must be a character type.')))
      }
      
    } else if (type_map[colnames(df)] %in% names(basic_type_mapping)) {
      is_numeric <- all(is.numeric(df[,i]))
      if (!is_numeric) {
        errors <- c(errors, glue::glue("{colnames(df)[i]} must be a numeric type."))
      }
    } else if (multi_select_map[colnames(df)[i]]) {
      is_multiselect_list <- is.list(df[,i])
      if (!is_multiselect_list) {
        errors <- c(errors, glue::glue("{colnames(df)[i]} must be a list."))
      }
      
    }
    
    # If the column is an entity_link, storage_link, or  dropdown,
    # then we need to check the values against the ones already registered. 
    if (type_map[colnames(df)[i]] == 'entity_link') {
      registered_values <- DBI::dbGetQuery(conn, "SELECT {`id_or_name`} FROM {colnames(df)[i]} WHERE 
                      {`id_or_name`} IN {.vec2sql_tuple(unique(df[[i]]))}")
      if (!(all(unique(df[[i]]) %in% registered_values))) {
        errors <- c(errors, glue::glue(("Not all values in '{colnames(df)[i]}' are registered.")))
      }
    } else if (type_map[colnames(df)[i]] == 'dropdown') {
      # get_dropdown_schema()
      
    } else if (type_map[colnames(df)[i]] == 'storage_link') {
      
    } else if (type_map[colnames(df)[i]] == 'blob_link') {
      # upload the files
      # get the IDs
      # upload IDs with results. 
      
    } else { # text / float / integer 
      next()
    }
    
    # warn user if info is not getting written but present in the data frame
    # upload results with API/SDK
    # return a data frame with the submitted results, as well as the IDs. 
    
  }
  
  
}

# Add these convenience functions for getting hte ProjectIds and results schema Ids

get_project_ids <- function() {
  DBI::dbGetQuery(conn, "SELECT id,name FROM project")
}



get_results_schema_ids <- function() {
  DBI::dbGetQuery(conn, "SELECT id,name FROM schema WHERE schema_type = 'assay_result'")
}


get_dropdown_schema <- function(conn, dropdown_id) {
  DBI::dbGetQuery(conn, glue::glue("SELECT dropdown_option.name FROM dropdown INNER JOIN dropdown_option ON 
                  dropdown.id = dropdown_option.dropdown_id 
                  WHERE dropdown.id = '{dropdown_id}'"))
}
