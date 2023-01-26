
#' @export
validate_data_frame <- function(conn, tenant, api_key, df, schema_id) {
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
      conn=conn, errors=errors, values=df[,i], 
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
}