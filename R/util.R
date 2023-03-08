# util.R

#' Raise exception if the `schema` column is not in the data frame. 
#' 
#' The `schema` column appears in every data warehouse table. This function
#' stops function execution if `schema` is not in the data frame. 
#' @param df data.frame with table from the data warehouse. 
#' @return Used for side effect only.
#' @keywords internal
#' 
is_schema_in_dataframe <- function(df) {
  if (!('schema' %in% colnames(df))) {
    stop("'schema' column is missing from the input data.frame.
           Verify that the data.frame is a valid warehouse table.")
  }
}

#' Raise exception if the `schema` column is not in the data frame.
#'  This function takes a data frame and converts each row into the nested list that .create_assay_result needs.
#' @param df data.frame with table from the data warehouse.
#' @return res
#' @keywords internal
#'
.to_fields <- function(df) {
  res <- vector("list", length = nrow(df))
  for (i in 1:nrow(df)) {
    res[[i]] <- df[i,] %>%
      as.list() %>%
      purrr::map(~as.list(.) %>%
        magrittr::set_names(., 'value'))
    names(res[[i]]) <- colnames(df)
  }
  return(res)
}


#' Raise exception if the `schema` column is not in the data frame.
#'  Dataframe validation
#' @param df data.frame with table from the data warehouse.
#' @return error list
#' @export
#' @keywords internal

validate_data_frame <- function (df, fk_type='name', mappings){
    errors <- c()
  # print(df)
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

  return(errors)
}