

#' Upload assay results to Benchling from a data frame
#' 
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
upload_results <- function(conn, client, df, projectId, schemaId) {
  # 1. Check to see if all columns are present for all required fields in the results schema.
  schema_def <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT schema_field.archived$,schema.name,schema.id,
    schema_field.name,schema_field.display_name,schema_field.type,
    schema_field.is_multi,schema_field.is_required,schema_field.system_name  
    FROM schema INNER JOIN schema_field ON 
    schema.id = schema_field.schema_id WHERE schema.id = '{`schemaId`}'"))
  # res <- client$schemas$get_assay_result_schema_by_id(schemaId)
  # res$field_definitions[[1]]$is_multi
  # res$field_definitions
  
  
  # 2. Check to see if other columns are present that match optional fields in the results schema.
  # Stop if not
  # 3. Check to see if the types of the columns in the data frame match the types of the fields in the schema.
  # 4. Upload all file attachments and keep the blob identifiers.
  # 5. Fill the original data frame with the blob identifiers.
  # 6. Upload the results (data frame) via the Python SDK in R.
  
}

# Add these convenience functions for getting hte ProjectIds and results schema Ids

get_project_ids <- function() {
  DBI::dbGetQuery(conn, "SELECT id,name FROM project")
}



get_results_schema_ids <- function() {
  DBI::dbGetQuery(conn, "SELECT id,name FROM schema WHERE schema_type = 'assay_result'")
}

