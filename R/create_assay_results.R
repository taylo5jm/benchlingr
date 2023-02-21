#' @keywords internal
#' @importFrom magrittr %>%
.upload_results <- function(client, df, project_id, schema_id) {
  # This function takes a data frame and converts each row into the 
  # nested list that .create_assay_result needs.
  .to_fields <- function(df) {
    res <- vector("list", length=nrow(df))
    for (i in 1:nrow(df)){
      res[[i]] <- df[i,] %>% as.list() %>%
        purrr::map(~ as.list(.) %>%
                     magrittr::set_names(., 'value'))
      names(res[[i]]) <- colnames(df)
    }
    return(res)
  }
  
  created_results <- list()
  created_fields <- .to_fields(df)
  for (i in 1:nrow(df)) {
    created_results[[i]] <- .create_assay_result(
      project_id=project_id,
      schema_id=schema_id,
      fields=created_fields[i])
  }
  
  .upload_results_with_python_sdk <- function(results) {
    reticulate::source_python(
      system.file(
        "python", "upload_results_with_sdk.py", 
        package = "benchlingr"))
    res <- upload_results_with_sdk(client, results)
    return(res)
  }
  # return(created_results)

  .upload_results_with_python_sdk(created_results)

}


#' Upload assay results to Benchling from a data frame
#' 
#' @importFrom magrittr %<>%
#' @include schema_utils.R
#' @include error.R
#' @include upload_files.R
#' @include field_validation.R
#' @param conn Database connection. 
#' @param client Benchling API client. 
#' @param df Data frame / tibble of results to be uploaded to Benchling. 
#' @param project_id Benchling project identifier.
#' @param schema_id Results schema ID (starts with "assaysch_"). 
#' @param fk_type Are the entity links identifiers or names?
#' @param tenant URL for the Benchling tenant.
#' @param api_key API key for the Benchling tenant.
#' @export
#' @examples \dontrun{
#' schema_id <- "assaysch_yKoqVsej"
#' conn <- warehouse_connect("hemoshear-dev")
#' api_key <- Sys.getenv("BENCHLING_DEV_API_KEY")
#' tenant <- "hemoshear-dev"
#' }

create_assay_results <- function(conn, client, df, project_id, schema_id, 
                           fk_type='name', 
                           tenant=Sys.getenv("BENCHLING_TENANT"),
                           api_key=Sys.getenv("BENCHLING_API_KEY")) {
  
  if (tenant == "") {
    .missing_tenant_error()
  }
  # 1. Check to see if all columns are present for all required fields in the results schema.
  df_is_valid <- verify_schema_fields(
    schema_id, schema_type='assay-result',
    df=df, strict_check=FALSE, tenant=tenant,
    api_key=api_key)

  # Stop if not
  # 2. Check to see if the types of the columns in the data frame match the types of the fields in the schema.
  #schema_def <- get_schema_fields(schema_id=schema_id, schema_type='assay-result',
  #                                tenant=tenant, api_key=api_key) %>%
  #  purrr::map_df(~ .)
  mappings <- .get_schema_field_metadata(conn=conn, schema_id=schema_id)
  if (length(fk_type) == 1) {
    fk_type <- rep(fk_type, length(colnames(df)))
    names(fk_type) <- colnames(df)
  }
  
  to_query <- c('entity_link', 'dropdown', 'storage_link')

  errors <- c()
  for (i in 1:length(colnames(df))) {
    this_colname <- colnames(df)[i]
    errors <- .validate_column_types(
      errors, df[,i], this_colname,
      benchling_type=mappings$type_map[this_colname], 
      multi_select = mappings$multi_select_map[this_colname])
    errors <- .validate_column_values(
      client=client, conn=conn, errors=errors, values=df[,i], 
      column_name=this_colname,
      benchling_type=mappings$type_map[this_colname], 
      multi_select=mappings$multi_select_map[this_colname],
      fk_type=fk_type[this_colname],
      target_schema_id=mappings$target_schema_map[this_colname])
  }
  # Stop function execution and show all errors to the user. 
  assertthat::assert_that(
    length(errors) == 0,
    msg=paste0(errors, collapse='\n'))
  if (length(errors) == 0) {
      blob_link_column_names <- names(
        mappings$type_map[which(mappings$type_map %in% 'blob_link')])
    # If all looks good, let's upload the files if we need to
    if (any(colnames(df) %in% blob_link_column_names)) {
      if (any(fk_type[blob_link_column_names] == "name")) {
        df <- upload_files(
          file=df, client=client, 
          blob_link_cols=blob_link_column_names)
      } else {
        # check to see if blobs currently exist. 
        df <- df
      }
      
    }  
  
    
    .upload_results(client, df,
                    project_id=project_id,
                    schema_id=schema_id)
  } else {
    return(errors)
  }
    # warn user if info is not getting written but notresent in the data frame
    # upload results with API/SDK
    # return a data frame with the submitted results, as well as the IDs. 
  
  
}


#' Create an assay result to be uploaded to Benchling
#' 
#' @param project_id Identifier for the Benchling project
#' @param schema Identifier for the result schema
#' @param fields List where the keys are strings corresponding to the field names. The values
#' should be lists with at least one element named `value`, which is the value that should be submitted
#' for that field. 
#' ex. list(study = list(value = "MyFakeStudy"))
#' @param id UUID for result. Optional
#' @param field_validation Field validation for result. Optional. 
#' @return List to be passed to `upload_assay_result`.
#' @keywords internal

.create_assay_result <- function(project_id, schema_id, fields, id=NULL, 
                                 field_validation=NULL) {
  assertthat::assert_that(is.list(fields),
                          msg="fields must be a nested list.")
  assertthat::assert_that(is.character(project_id),
                          msg="project_id must be a character vector.")
  assertthat::assert_that(is.character(schema_id),
                          msg="schema_id must be a character vector.")
  res <- list(project_id = project_id,
              schema_id = schema_id,
              fields = fields)
  if (!is.null(id)) {
    res$id <- id
  } 
  if (!is.null(field_validation)) {
    res$field_validation <- field_validation
  }
  res
}


# Add these convenience functions for getting hte ProjectIds and results schema Ids
#' Get Benchling project metadata
#' 
#' @param conn Database connection opened with `warehouse_connect`.
#' @return data.frame with `id` and `name` attributes for Benchling projects. 
#' @export

get_project_ids <- function(conn) {
  return(DBI::dbGetQuery(conn, "SELECT id,name FROM project"))
}



get_results_schema_ids <- function(conn) {
  return(DBI::dbGetQuery(
    conn, 
    "SELECT id,name FROM schema WHERE schema_type = 'assay_result'"))
}
