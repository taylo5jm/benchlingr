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


