library(httr)
library(jsonlite)

#' Makes a direct API Call to Benchling Schema endpoints without benchling-sdk client.
#'
#' @param schema_id
#' @param schema_type schema type name as it shown on benhcling API endpoints with or without `-schemas` ending.
#' @export schema fields.
#' @examples \dontrun{
#' schema_id <- "assaysch_nIw4yAq8"
#' schema_type <- "assay-result"
#' }
#'

get_schema_fields <- function(schema_id, schema_type) {

  if('-schemas' %in% schema_type){
    schema_type <- gsub("-schemas", "", schema_type)
  }

  base_url <- glue::glue('https://{Sys.getenv("BENCHLING_TENANT")}.benchling.com/api/v2/{schema_type}-schemas/{schema_id}')

  schema_fields_raw <- GET(url = base_url, httr::authenticate(Sys.getenv('BENCHLING_DEV_API_KEY'), ''))
  schema_fields <- content(schema_fields_raw)
  if(length(schema_fields$error) > 0){
    stop(schema_fields$error)
  }
  field_definitions <- schema_fields$fieldDefinitions

  return(field_definitions)
}

#'
#'
#'

verify_schema_fields <- function(schema_id, schema_type, df, strict_check = FALSE) {

  column_names <- names(df)
  field_definitions <- get_schema_fields(schema_id, schema_type)

  for (field in field_definitions) {
    if (field$isRequired == TRUE) {
      if (!(field$displayName %in% column_names)) {
        stop(glue::glue('\`{field$displayName}\` is a required field of this schema. Please verify your dataframe fields.'))
      }
    }else if (strict_check) {
      if (!(field$displayName %in% column_names)) {
        stop(glue::glue('\`{field$displayName}\` does not match with any column of this df. Please either provide the required field or call this method with strict_check=FALSE'))
      }
    }
  }

  return(TRUE)
}


# keko <- get_schema_fields('ts_aUrdwIoK', 'entity')
# keko <- get_schema_fields('assaysch_nIw4yAq8', 'assay-result')
df <- data.frame(
  "Platse" = c('davut'),
  "Analytes" = c('0.2'),
  'File1' = c('aaaa'),
  check.names = FALSE
)
keko <- verify_schema_fields('assaysch_nIw4yAq8', 'assay-result', df = df, strict_check = TRUE)
print(keko)