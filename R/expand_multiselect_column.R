# expand_multiselect_column.R

#' List the multi-select columns in a data frame retrieved from the data 
#' warehouse
#'
#' @include util.R 
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @param conn Database connection opened with `connect_warehouse`.
#' @param df Data frame retrieved from the Benchling data warehouse with one or 
#' more multi-select columns. The data frame must also have a column called `schema`, 
#' which indicates the schema name of the warehouse table. 
#' One can use `DBI::dbReadTable` or `DBI::dbGetQuery` to retrieve tables 
#' from the data warehouse.
#' @return Character vector where the names are the names of the multi-select
#' columns in the data frame and the values are the positions of the multi-select
#' columns in the data frame. 
#' @examples \dontrun{
#' conn <- connect_warehouse("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' res <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw")
#' list_multiselect_columns(conn, res)
#' }
#' @export
#' 
list_multiselect_columns <- function(conn, df) {
  is_schema_in_dataframe(df)
  schema_id <- DBI::dbGetQuery(
    conn, glue::glue(
      "SELECT id FROM schema WHERE system_name = {shQuote(unique(df$schema))}")) %>% 
    as.character()
  res <- DBI::dbGetQuery(conn, 
    glue::glue("SELECT * FROM schema_field WHERE schema_id = {shQuote(schema_id)}")) %>%
    dplyr::filter(!is.na(.data$target_schema_id), .data$is_multi)
  vec <- purrr::map_int(res$name, ~ which(colnames(df) == .))
  names(vec) <- res$name
  vec
}


#' Unpack the values in a JSON column from a data frame retrieved from
#' the data warehouse. 
#' 
#' When a schema contains a multi-select field, the column in the data frame
#' retrieved from the data warehouse will be a JSON type. This function allows
#' one to unpack the values in the column, creating either new rows or new 
#' columns in the data frame. 
#' 
#' @importFrom rlang .data
#' @param conn Database connection opened by `connect_warehouse`. This is used
#' to ensure that the specified `column` is actually a multi-select field
#' defined in the schema. 
#' @param df Data frame retrieved from the Benchling data warehouse with one or 
#' more multi-select columns. The data frame must also have a column called `schema`, 
#' which indicates the schema name of the warehouse table. 
#' One can use `DBI::dbReadTable` or `DBI::dbGetQuery` to retrieve tables 
#' from the data warehouse.
#' @param column Name of the JSON column that should be expanded.
#' @param shape The `shape` argument determines if the values in the JSON column
#' should be unpacked to create new rows (`long`) or new columns (`wide`).
#' @param column_prefix If the `shape` argument is `wide`, then new columns
#' are created in the data frame. By default, the new column names will
#' start with the name of the original column and end with an integer (1, 2, ..., 
#' number of maximum values in a single row in the JSON field.). One can
#' override this behavior by passing in a character vector to the `column_prefix`
#' argument. Using the `column_prefix` argument is not recommended if the
#' column in an entity type, as one will not be able to use `get_entity_table` 
#' or `replace_entity_id_with_name` with the new columns.
#' @return Data frame with the values in `column` unpacked. 
#' @export
#' @examples \dontrun{
#' conn <- connect_warehouse("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' d <- DBI::dbGetQuery(conn, "SELECT plate,analytes FROM simple_plate_analyte_mapping$raw 
#' WHERE entry_id$ = 'etr_MWQ7M7Pz'")
#' res <- expand_multiselect_column(conn, d, column="analytes", shape="long")
#' res <- expand_multiselect_column(conn, d, column="analytes", shape="wide")
#' DBI::dbDisconnect(conn)
#' }
#' 
expand_multiselect_column <- function(conn, df, column, shape="long",
                                      column_prefix=NULL) {
  assertthat::assert_that(
    shape %in% c('long', 'wide'),
    msg = "'shape' argument must be 'long' to unpack values 
    into new rows or 'wide' to unpack values into new columns")
  if (shape == 'long' & !is.null(column_prefix)) {
    warning("'shape' argument is 'long' and a column prefix was specified.
            Ignoring the 'column_prefix' argument")
  }
  assertthat::assert_that(
    column %in% colnames(df),
    msg = glue::glue('{`column`} is not a column in the data frame.'))
  if (shape == 'long') {
    df <- .unpack_long(df, column)
  } else {
    df <- .unpack_wide(df, column)
  }
  df
}

#' Unpack the values of a JSON column into new rows
#' 
#' @param df Data frame with JSON columns to be unpacked into new rows.
#' @param column Name of the JSON column to be unpacked.
#' @return Data frame with the values in the JSON column unpacked. 
#' @keywords internal
#' 
.unpack_long <- function(df, column) {
  res <- vector("list", length=nrow(df))
  df[[column]] <- purrr::map(
    as.character(df[[column]]), ~RJSONIO::fromJSON(.))
  new_rows <- list()
  for (i in 1:nrow(df)) {
    if (length(df[[i,column]]) == 0) {
      new_rows[[i]] <- df[i,]
      new_rows[[i]][column] <- NA
    } else {
      new_rows[[i]] <- purrr::map_df(
        df[[i,column]], 
        ~ dplyr::mutate(df[i,], {{ column }} := .))
    }
  }
  res <- do.call("rbind", new_rows)
  res[column] %<>% unlist
  return(res)
}

#' Unpack the values of a JSON column into new columns
#' 
#' @importFrom magrittr %>% %<>%
#' @param df Data frame with JSON columns to be unpacked into new columns
#' @param column Name of the JSON column to be unpacked.
#' @return Data frame with the values in the JSON column unpacked. 
#' @keywords internal
#' 
.unpack_wide <- function(df, column) {
  
  unpacked <- purrr::map(as.character(df[[column]]),
                           ~ RJSONIO::fromJSON(.))
  
  # Get the maximum number of values that an element in the list can have.
  max_n_values <- purrr::map_int(unpacked, ~ length(.)) %>%
    max()
  if (max_n_values > 0) {
    # Not totally sure if we want to allow the user to replace entity names
    # with IDs here. 
    # unique_values <- unique(unlist(treatments))
    # treatment_df <- DBI::dbGetQuery(conn, glue::glue(
    #  "SELECT id,name$ FROM treatments$raw WHERE id IN {vec2sql_tuple(unique_treatments)}"))
    #treatment_map <- treatment_df$`name$`
    #names(treatment_map) <- treatment_df$id
    # Create new columns for the treatments. In other words, expand wide.
    wide <- list()
    for (i in 1:max_n_values) {
      wide[[i]] <- purrr::map2_chr(
        unpacked, i, 
        ~ tryCatch({.x[.y]}, error = function(e) {NA})) # some vectors will be < max length, hence NA
    }
    # Merge the new columns into a single data frame. 
    wide %<>% dplyr::bind_cols()
    # New columns are labeled "column__x", where x is a number.
    colnames(wide) <- paste0(column, 1:max_n_values)
    # Merge the new data frame into the original data frame  
    df %<>% dplyr::bind_cols(wide)
    return(df)
  } else {
    warning(glue::glue("All values in {`column`} are empty.
                       Returning the original data frame."))
    return(df)
  }
}


