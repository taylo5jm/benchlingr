# get_entity_table.R

#' Get additional information about entities in a warehouse table (single entity column.)
#' 
#' Given a data frame retrieved from the Benchling warehouse, extract the
#' additional table in the warehouse that correspond to the entities in the 
#' data frame. 
#' 
#' @importFrom methods is
#' @include vec2sql_tuple.R
#' @param conn Database connection opened by `warehouse_connect`
#' @param df Data frame with one or more entity columns. The data frame
#' must also have a column called `schema`, which indicates the schema
#' name of the warehouse table. 
#' @param column Name of the entity column. 
#' @param return_cols Character vector of warehouse column names to include
#'  in results. If `return_cols` is missing, then every column will be returned. 
#' @param key The column in the data warehouse table that should be used
#' for the primary key, which can be `id` or `name$`. The default option, `id`,
#'  is the Benchling identifier. The `name$` option should be used if the table 
#'  has been transformed by `replace_entity_id_with_name`.
#' @return data frame with rows from warehouse table that correspond to 
#' entities found in the input data frame (`df`). 
#' @keywords internal
.get_entity_table <- function(conn, df, column, return_cols=c('id', 'name$'),
                              key='id') {
  # Raise exception if the specified column isn't in the data frame. 
  assertthat::assert_that(
    column %in% colnames(df),
    msg = glue::glue("{`column`} is not a column in the data frame!"))
  # Raise exception if `key` is not a valid option
  assertthat::assert_that(
    key %in% c("id", "name$"),
    msg = glue::glue("key must be 'id' or 'name$'!"))
  # If the field is multi-select, then extract all identifiers
  if (is(df[[column]], 'pq_jsonb')) {
    id_list <- .vec2sql_tuple(
      Filter(function(x) (length(x) > 0),
             purrr::map(as.character(df[[column]]),
                        ~ RJSONIO::fromJSON(.))) %>%
        unlist %>% unique)
    # The code above will return an empty list as a string if 
    # the entity column is completely empty.
    if (id_list == '()') {
      return(NA)
    }
  } else { # Get all identifiers from single value columns
    id_list <- .vec2sql_tuple(
      Filter(function(x) (!is.na(x)), unique(df[[column]])))
  }
  # If user doesn't specify any columns to return,
  # then retrieve all of them. 
  if (missing(return_cols)) {
    return_cols <- '*'
  }
  if (length(return_cols) == 0) {
    return_cols <- '*'
  }
  
  # Pull these columns from the warehouse
  warehouse_cols <- paste0(return_cols, collapse=',')
  
  # In some results schemas, the warehouse name of a column will be different than
  # the warehouse name of the corresponding entity table, so one must construct a 
  # mapping from the former to the latter by traversing some of the schema tables. 
  column_map <- .map_entity_field_names_to_warehouse_tables(conn, df)
  query <- glue::glue("SELECT {`warehouse_cols`} FROM {column_map[column]}
                        WHERE {`key`} IN {`id_list`}")
  mapping <- DBI::dbGetQuery(
    conn, query)
  return(mapping)
}


#' Get additional information about entities in a warehouse table
#' 
#' Given a data frame retrieved from the Benchling warehouse, extract the
#' additional tables in the warehouse that correspond to the entities in the 
#' data frame. 
#' 
#' @include list_entity_columns.R
#' @param conn Database connection opened with `warehouse_connect`
#' @param df Data frame with one or more entity columns. The data frame
#' must also have a column called `schema`, which indicates the schema
#' name of the warehouse table. 
#' @param columns Character vector of column names to expand. If NULL, then all entity columns
#' will be expanded. To see which columns in the data frame correspond to
#' entity fields, use the `list_entity_columns` tables. 
#' @param return_cols Character vector of warehouse columns to return for the entity.
#' The default value (`*`) will return every column. When specifying a different 
#' set of columns to return, it is recommended that one includes the `id` 
#' and `name$` columns. If they are not explicitly included, the function 
#' will add them implicitly. 
#' @param key The column in the data warehouse table that should be used
#' for the primary key, which can be `id` or `name$`. The default option, `id`,
#' is the Benchling identifier. The `name$` option should be used if the table 
#' has been transformed by `replace_entity_id_with_name`.
#' @return List of data frames with rows from warehouse table that correspond to 
#' entities found in the input data frame (`df`). Each element in the list 
#' corresponds to an entity column in the input data frame.
#' @export
#' @examples \dontrun{
#' conn <- warehouse_connect("hemoshear-dev", 
#'    username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'    password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' df <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw")
#' get_entity_table(conn,  df)
#' }
#' 
get_entity_table <- function(conn, df, columns=NULL, return_cols='*',
                             key="id") {
    # Expand all entity columns if the user doesn't specify any.
    if (is.null(columns)) {
      columns <- names(list_entity_columns(conn, df))
    } 
    # Check for columns with all NA values
    na_cols <- apply(df, 2, function(x) (all(is.na(x))))
    if (any(na_cols)) { # Don't expand columns where all values are NA
      columns <- setdiff(columns, names(na_cols)[which(na_cols)])
    }
    # Make sure that `id` and `name$` are included in the set of columns
    # to be returned. 
    if (!('*' %in% return_cols)) {
      return_cols <- union(c('id', 'name$'), return_cols)
    }
    # Get all the rows from the relevant warehouse tables. 
    res <- purrr::map(columns, 
      ~ .get_entity_table(conn, df, ., return_cols = return_cols,
                          key = key))
    names(res) <- columns
    return(res)
}