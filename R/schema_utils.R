#' Makes a direct API Call to Benchling Schema endpoints without benchling-sdk client.
#' 
#' @include error.R
#' @param schema_id provided schema id
#' @param schema_type schema type name as it shown on benchling API endpoints 
#' with or without `-schemas` ending. See \url{https://benchling.com/api/reference#/Schemas}.
#' @param tenant is tenant name in the form "https://your-organization.benchling.com".
#' Default value is the `BENCHLING_TENANT` environment variable. 
#' @param api_key API key. Default value is the `BENCHLING_API_KEY` environment
#' variable. 
#' @examples \dontrun{
#' schema_id <- "assaysch_nIw4yAq8"
#' schema_type <- "assay-result"
#' }
#' @keywords internal
get_schema_fields <- function(schema_id, schema_type, 
                              tenant=Sys.getenv("BENCHLING_TENANT"),
                              api_key=Sys.getenv("BENCHLING_API_KEY")) {
  
  if (api_key == "") {
    .missing_api_key_error()
  }
  
  if (tenant == "") {
    .missing_tenant_error()
  }

  if ('-schemas' %in% schema_type) {
    schema_type <- gsub("-schemas", "", schema_type)
  }

  base_url <- glue::glue('{tenant}/api/v2/{schema_type}-schemas/{schema_id}')

  schema_fields_raw <- httr::GET(url = base_url, httr::authenticate(api_key, ''))
  schema_fields <- httr::content(schema_fields_raw)
  if (length(schema_fields$error) > 0) {
    stop(schema_fields$error)
  }
  field_definitions <- schema_fields$fieldDefinitions
  return(field_definitions)
}

#'
#' compares schema fields with dataframe colnames and verifies
#'
#' @param schema_id provided schema id
#' @param schema_type schema type name as it shown on benhcling API endpoints with or without `-schemas` ending.
#' @param df provided dataframe
#' @param strict_check is an optional arguement. if set TRUE, the function looks for every schema field names in colnames.
#' @param tenant is tenant name. If missing, it will be reading from sys.env
#' @param api_key API key. Default value is the `BENCHLING_API_KEY` environment
#' variable. 
#' @examples \dontrun{
#' schema_id <- "assaysch_nIw4yAq8"
#' schema_type <- "assay-result",
#' df <- data.frame(
#'  "plate" = c('davut'),
#'  "analytes" = c('0.2'),
#'  'File1' = c('aaaa'),
#'  check.names = FALSE
#' }
#' @return boolean field
#' @keywords internal

verify_schema_fields <- function(schema_id, schema_type, df, strict_check = FALSE, 
                                 tenant=Sys.getenv("BENCHLING_TENANT"),
                                 api_key=Sys.getenv("BENCHLING_API_KEY")) {

  if (tenant == "") {
    .missing_tenant_error()
  }

  if (is.null(df) || nrow(df) == 0) {
    stop('Dataframe is empty or none.')
    return(FALSE)
  }

  column_names <- names(df)
  field_definitions <- get_schema_fields(schema_id, schema_type, tenant = tenant,
                                         api_key=api_key)

  if (length(field_definitions) == 0) {
    stop("This schema has no fields.")
    return(FALSE)
  }

  for (field in field_definitions) {
    if (field$isRequired == TRUE) {
      if (!(field$name %in% column_names)) {
        stop(glue::glue('\`{field$name}\` is a required field of this schema.
        Please verify your dataframe fields.'))
        return(FALSE)
      }
    }else if (strict_check) {
      if (!(field$name %in% column_names)) {
        stop(glue::glue('\`{field$name}\` does not match with any column of this df.
         Please either provide the required field or call this method with strict_check=FALSE'))
        return(FALSE)
      }
    }
  }

  # todo Check to see if the types of the columns in the data frame match the types of the fields in the schema.

  return(TRUE)
}

.verify_schema_field_types <- function(schema_fields, df, strict_check) {


}