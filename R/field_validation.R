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

#' Validate the values in the column of a results table.
#' 
#' If the column refers to an entity link field, the entity IDs in the input
#' data frame are checked against those currently in the registry. If the column
#' is a dropdown, then the values are checked against the dropdown options. 
#' If the column is a storage link, the function checks to see if the IDs in the 
#' input data frame exist in the inventory. If the column refers to a blob
#' link field, then the function will assume the values are file paths and will
#' check to see if the file exists on the local machine. 
#' @param conn Database connection typically opened with `warehouse_connect`.
#' @param client A Benchling SDK object typically created with `benchling_api_auth`.
#' @param errors Character vector of errors.
#' @param values Values in the column. 
#' @param column_name Name of the column in the data frame to be uploaded to Benchling.
#' @param benchling_type The Benchling "type" that the column corresponds to.
#' @param multi_select Boolean indicating whether or not the column corresponds
#' to a multi-select field.
#' @param fk_type String "id" or "name$" which indicates whether the column
#' represents the Benchling ID or name of the entities. 
#' @param target_schema_id The name of the warehouse table which corresponds
#' to the column. 
#' @keywords internal

.validate_column_values <- function(conn, client, errors, values, column_name, benchling_type,
                                    multi_select, fk_type,
                                    target_schema_id) {
  # If the column is an entity_link, storage_link, or  dropdown,
  # then we need to check the values against the ones already registered. 
  if (benchling_type == 'entity_link') {
    errors <- .validate_entity_column_values(
      conn=conn, errors=errors, values=values, column_name=column_name,
      fk_type=fk_type, target_schema_id=target_schema_id)
  } else if (benchling_type == 'dropdown') {
    errors <- .validate_dropdown_column_values(
      conn=conn, errors=errors, values=values, column_name=column_name,
      dropdown_id=target_schema_id)
  } else if (benchling_type == 'storage_link') {
    errors <- .validate_storage_link_column_values(errors, values, column_name)
  } else if (benchling_type == 'blob_link') {
    errors <- .validate_blob_link_column_values(
      client, errors, values, column_name, multi_select, fk_type=fk_type)
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
  } else if (benchling_type == 'float') {
    is_numeric <- all(is.numeric(values))
    if (!is_numeric) {
      errors <- c(errors, glue::glue("{column_name} must be a numeric type."))
    }
  } else if (benchling_type == 'integer') {
    is_integer <- all(is.integer(values))
    if (!is_integer) {
      errors <- c(errors, glue::glue("{column_name} must be an integer type."))
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
                                           fk_type) {
  registered_values <- DBI::dbGetQuery(
    conn, glue::glue("SELECT {`fk_type`} FROM {`target_schema_id`} WHERE 
      {`fk_type`} IN {.vec2sql_tuple(values)}"))
  if (!(all(unique(values) %in% registered_values[,1]))) {
    # Which ones?
    errors <- c(errors, glue::glue(
                  "Not all values in '{column_name}' are registered."))
  }
  return(errors)
  
}

#' Verify that values in dropdown column are valid options in the dropdown schema. 
#' @include dropdown.R
#' @keywords internal
.validate_dropdown_column_values <- function(conn, errors, values, column_name,
                                             dropdown_id) {
  valid_options <- get_dropdown_options(conn, schema_id=dropdown_id)
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
#' local machine or that identifiers already exist. 
#' 
#' @param errors Errors
#' @param values Values in the column. 
#' @param column_name Name of the column in the input data frame. 
#' @param fk_type "id" or "name". Use "id" if the blob already exists on 
#' Benchling and you are submitting the ID for the blob. Use "name" if
#' the file doesn't already exist on Benchling as a blob and you need to upload
#' the file to Benchling.
#' @keywords internal
.validate_blob_link_column_values <- function(client, errors, values, column_name,
                                              multi_select, fk_type) {
  # If multi-select then the column type will be a list
  new_errors <- list()
  if (multi_select) {
    # Check to see if files exist.
    if (fk_type == "name") {
      for (i in 1:length(values)) {
        new_errors[[i]] <- purrr::map2(
          values[[i]], values[[i]], 
          ~ data.frame(exists =  file.exists(.x),
                       filename = .y))
      }
    } else { # Check to see if blob identifiers are valid
      for (i in 1:length(values)) {
        res <- tryCatch({
            client$blobs$bulk_get(values[[i]])},
          error = function(e) {
            "One or more blob identifiers could not be found in Benchling."})
        if (!is.list(res)) {
          new_errors[[i]] <- res
        }
      }
      return(unlist(new_errors))
    }
  }
  else { 
    # Check to see if files exist.
    if (fk_type == "name") {
      new_errors <- purrr::map2(
        values, values, ~ data.frame(
          exists =  file.exists(.x),
          filename = .y))
    } else { # Check to see if blob identifiers are valid
      # id case here
      # need to make `values` a list if it is only one item.
      if (length(values) == 1 & !is.list(values)) {
        values <- list(values)
      }
      res <- tryCatch({
        client$blobs$bulk_get(values)},
        error = function(e) {
          "One or more blob identifiers could not be found in Benchling."})
      if (!is.list(res)) {
        new_errors <- res
        return(c(errors, new_errors))
      } else {
        return(c())
      }
    }
  }
  if (fk_type == 'name') {
    new_errors <- dplyr::bind_rows(new_errors) %>%
      dplyr::filter(!exists)
    if (nrow(new_errors) > 0) {
      new_errors <- paste0(
        new_errors$filename, " does not exist on the local machine.")
      return(c(errors, new_errors))
    } else {
      return(c())
    }
  } 
    
}
  
