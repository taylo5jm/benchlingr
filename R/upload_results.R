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
#' @param conn Database connection. 
#' @param client Benchling API client. 
#' @param df Data frame / tibble of results to be uploaded to Benchling. 
#' @param project_id Benchling project identifier.
#' @param schema_id Results schema ID (starts with "assaysch_"). 
#' @param id_or_name Are the entity links identifiers or names?
#' @export
#' @examples \dontrun{
#' schema_id <- "assaysch_yKoqVsej"
#' conn <- warehouse_connect("hemoshear-dev")
#' api_key <- Sys.getenv("BENCHLING_DEV_API_KEY")
#' tenant <- "hemoshear-dev"
#' }

upload_assay_results <- function(conn, client, df, project_id, schema_id, 
                           tenant=Sys.getenv("BENCHLING_TENANT"),
                           id_or_name='name', api_key=Sys.getenv("BENCHLING_API_KEY")) {
  
  if (tenant == "") {
    .missing_tenant_error()
  }
  # 1. Check to see if all columns are present for all required fields in the results schema.
  df_is_valid <- verify_schema_fields(
    schema_id, schema_type='assay-result',
    df=df, strict_check=FALSE, tenant=tenant,
    api_key=api_key)

  # Stop if not
  # 3. Check to see if the types of the columns in the data frame match the types of the fields in the schema.
  #schema_def <- get_schema_fields(schema_id=schema_id, schema_type='assay-result',
  #                                tenant=tenant, api_key=api_key) %>%
  #  purrr::map_df(~ .)
  
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
  
  to_query <- c('entity_link', 'dropdown', 'storage_link')

  errors <- c()
  for (i in 1:length(colnames(df))) {
    errors <- .validate_column_types(
      errors, df[,i], colnames(df)[i],
      benchling_type=type_map[colnames(df)[i]], 
      multi_select = multi_select_map[colnames(df)[i]])
    errors <- .validate_column_values(
      conn=conn, errors=errors, values=df[,i], 
      column_name=colnames(df)[i],
      benchling_type=type_map[colnames(df)[i]], 
      multi_select=multi_select_map[colnames(df)[i]],
      id_or_name=id_or_name,
      target_schema_id=target_schema_map[colnames(df)[i]])
  }
  # Stop function execution and show all errors to the user. 
  assertthat::assert_that(
    length(errors) == 0,
    msg=paste0(errors, collapse='\n'))
  if (length(errors) == 0) {
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




#' Validate the values in the column of a results table.
#' 
#' If the column refers to an entity link field, the entity IDs in the input
#' data frame are checked against those currently in the registry. If the column
#' is a dropdown, then the values are checked against the dropdown options. 
#' If the column is a storage link, the function checks to see if the IDs in the 
#' input data frame exist in the inventory. If the column refers to a blob
#' link field, then the function will assume the values are file paths and will
#' check to see if the file exists on the local machine. 
#' @param errors
#' @param values
#' @param column_name Name of the column in the data frame to be uploaded to Benchling.
#' @param benchling_type The Benchling "type" that the column corresponds to.
#' @param multi_select Boolean indicating whether or not the column corresponds
#' to a multi-select field.
#' @param id_or_name String "id" or "name$" which indicates whether the column
#' represents the Benchling ID or name of the entities. 
#' @param target_schema_id The name of the warehouse table which corresponds
#' to the column. 
#' @keywords internal

.validate_column_values <- function(conn, errors, values, column_name, benchling_type,
                                    multi_select, id_or_name,
                                    target_schema_id) {
  # If the column is an entity_link, storage_link, or  dropdown,
  # then we need to check the values against the ones already registered. 
  if (benchling_type == 'entity_link') {
    errors <- .validate_entity_column_values(
      conn=conn, errors=errors, values=values, column_name=column_name,
      id_or_name=id_or_name, target_schema_id=target_schema_id)
  } else if (benchling_type == 'dropdown') {
    errors <- .validate_dropdown_column_values(
      conn=conn, errors=errors, values=values, column_name=column_name,
      dropdown_id=dropdown_id)
  } else if (benchling_type == 'storage_link') {
    errors <- .validate_storage_link_column_values(errors, values, column_name)
  } else if (benchling_type == 'blob_link') {
    errors <- .validate_blob_link_column_values(errors, values, column_name,
                                                multi_select)
    # upload the files
    # get the IDs
    # upload IDs with results. 
    
  } else { # text / float / integer 
    
  }
  return(errors)
}


#' Ensure that column types are valid for a data frame that is to be uploaded
#' to Benchling as a results table. 
#' 
#' integer|float --> numeric
#' text|entity_link|dropdown|long_text|storage_link|blob_link|dna_sequence_link --> character
#' @keywords internal
#' 

.validate_column_types <- function(errors, values, column_name,
                                   benchling_type, multi_select) {
  numeric_type_mapping <- c('integer' = 'numeric', 'float' = 'numeric')
  # Check characters
  if (benchling_type %in% 
      c('text', 'entity_link', 'dropdown', 'long_text', 
        'storage_link', 'blob_link', 'dna_sequence_link')) {
    is_text <- all(is.character(values))
    if (!is_text) {
      errors <- c(errors, 
                  c(glue::glue('{column_name} must be a character type.')))
    }
    #   
  } else if (benchling_type %in% names(numeric_type_mapping)) {
    is_numeric <- all(is.numeric(values))
    if (!is_numeric) {
      errors <- c(errors, glue::glue("{column_name} must be a numeric type."))
    }
  } else if (multi_select) {
    is_multiselect_list <- is.list(values)
    if (!is_multiselect_list) {
      errors <- c(errors, glue::glue("{column_name} must be a list."))
    }
  } 
  
  return(errors)
}



#' Verify that the identifiers (or names) for an entity link field match
#' identifiers (or names) that currently exist in the registry. 
#' @keywords internal
.validate_entity_column_values <- function(conn, errors, values, column_name,
                                           target_schema_id,
                                           id_or_name) {
  registered_values <- DBI::dbGetQuery(
    conn, glue::glue("SELECT {`id_or_name`} FROM {`target_schema_id`} WHERE 
      {`id_or_name`} IN {.vec2sql_tuple(values)}"))
  if (!(all(unique(values) %in% registered_values[,1]))) {
    # Which ones?
    errors <- c(errors, glue::glue(
                  "Not all values in '{column_name}' are registered."))
  }
  return(errors)
  
}

#' Verify that values in dropdown column are valid options in the dropdown schema. 
#' @keywords internal
.validate_dropdown_column_values <- function(conn, errors, values, column_name,
                                             dropdown_id) {
  valid_options <- get_dropdown_options(conn, dropdown_id)
  invalid <- setdiff(values, valid_options)
  if (length(invalid) > 0) {
    errors <- c(errors, 
    glue::glue("Not all values in '{column_name}' are valid dropdown options.
               {paste0(invalid, collapse=',')}"))
  }
  return(errors)
}


#' @keywords internal
.validate_storage_link_column_values <- function(errors, values, column_name) {
  return(errors)
}


#' Verify that values in a blob link column refer to valid file paths on the 
#' local machine.
#' 
#' @param errors Errors
#' @param values Values in the column. 
#' @param column_name Name of the column in the input data frame. 
#' @keywords internal
.validate_blob_link_column_values <- function(errors, values, column_name,
                                              multi_select) {
  # If multi-select then the column type will be a list
  if (multi_select) {
    new_errors <- list()
    for (i in 1:length(values)) {
      new_errors <- purrr::map2(values[[i]], values[[i]], 
                            ~ data.frame(exists =  file.exists(.x),
                                         filename = .y))
    }
  }
  else {
    new_errors <- purrr::map2(
      values, values, ~ data.frame(
        exists =  file.exists(.x),
        filename = .y))
  }
  new_errors <- dplyr::bind_rows(new_errors) %>%
    dplyr::filter(!exists)
  if (nrow(new_errors) > 0) {
    new_errors <- paste0(
      new_errors$filename, " does not exist on the local machine.")
    return(c(errors, new_errors))
  } else {
    return(errors)
  }
    
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

#' Get the options in a dropdown menu
#' 
#' 
#' @importFrom magrittr %>%
#' @param conn Database connection opened with `warehouse_connect`.
#' @param dropdown_id Schema ID for the dropdown.
#' @return character vector of dropdown menu options 
#' @export

get_dropdown_options <- function(conn, id) {
  return(DBI::dbGetQuery(
    conn, 
      glue::glue("SELECT dropdown_option.name FROM dropdown INNER JOIN 
      dropdown_option ON dropdown.id = dropdown_option.dropdown_id 
                 WHERE dropdown.id = '{id}'")) %>%
      .[,1] %>%
      as.character())
}


#' Get metadata for a dropdown menu
#' 
#' @param conn Database connection opened with `warehouse_connect`.
#' @param name Schema name for the dropdown
#' @export
#' 
get_dropdown <- function(conn, name) {
  return(DBI::dbGetQuery(
    conn, glue::glue("SELECT dropdown WHERE name = '{name}'")))
}
