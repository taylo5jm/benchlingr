library(httr)
library(jsonlite)

#' Makes a direct API Call to Benchling Schema endpoints without benchling-sdk client.
#'
#' @param schema_id provided schema id
#' @param schema_type schema type name as it shown on benhcling API endpoints with or without `-schemas` ending.
#' @param tenant is tenant name. If missing, it will be reading from sys.env
#' @examples \dontrun{
#' schema_id <- "assaysch_nIw4yAq8"
#' schema_type <- "assay-result"
#' }
#'

get_schema_fields <- function(schema_id, schema_type, tenant) {

  if (missing(tenant)) {
    tenant <- Sys.getenv("BENCHLING_TENANT")
  }

  if ('-schemas' %in% schema_type) {
    schema_type <- gsub("-schemas", "", schema_type)
  }

  base_url <- glue::glue('https://{tenant}.benchling.com/api/v2/{schema_type}-schemas/{schema_id}')

  schema_fields_raw <- GET(url = base_url, httr::authenticate(Sys.getenv('BENCHLING_DEV_API_KEY'), ''))
  schema_fields <- content(schema_fields_raw)
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

verify_schema_fields <- function(schema_id, schema_type, df, strict_check = FALSE, tenant) {

  if (missing(tenant)) {
    tenant <- Sys.getenv("BENCHLING_TENANT")
  }

  if (is.null(df) || nrow(df) == 0) {
    warning('Dataframe is empty or none.')
    return(FALSE)
  }

  column_names <- names(df)
  field_definitions <- get_schema_fields(schema_id, schema_type, tenant = tenant)

  if (length(field_definitions) == 0) {
    warning("This schema has no fields.")
    return(FALSE)
  }

  for (field in field_definitions) {
    if (field$isRequired == TRUE) {
      if (!(field$name %in% column_names)) {
        warning(glue::glue('\`{field$name}\` is a required field of this schema. Please verify your dataframe fields.'))
        return(FALSE)
      }
    }else if (strict_check) {
      if (!(field$name %in% column_names)) {
        warning(glue::glue('\`{field$name}\` does not match with any column of this df. Please either provide the required field or call this method with strict_check=FALSE'))
        return(FALSE)
      }
    }
  }

  return(TRUE)
}