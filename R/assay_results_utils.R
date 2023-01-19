#' Makes a direct API Call to Benchling Assay Result Archive endpoint
#' endpoint: https://benchling.com/api/reference#/Assay%20Results/archiveAssayResults
#' @param tenant tenant name
#' @param api_key api key
#' @param assay_result_ids is vector type assay result ids. This function supports archiving multiple
#' items in one function call
#' @param reason the reason of the archiving operation. "Made in error" is default but you can change as you wish
#' based on supported reasons on Benchling
#' @export
#' @examples \dontrun{
#' assay_result_ids <- assay_result_ids=c("908df834-220e-4b59-975b-c21bbbb8191a")
#' reason = "Made in error"
#' }
#'
#' @returns assay_result_ids
#'
archive_assay_results <- function(tenant = Sys.getenv("BENCHLING_TENANT"),
                                  api_key = Sys.getenv("BENCHLING_API_KEY"),
                                  assay_result_ids = NULL,
                                  reason = "Made in error") {
  if (api_key == "") {
    .missing_api_key_error()
  }

  if (tenant == "") {
    .missing_tenant_error()
  }

  if (is.null(assay_result_ids)) {
    stop('Please provide assay result ids in a vector. Example: c("908df834-220e-4b59-975b-c21bbbb8191a,
    908df834-220e-4b59-975b-c21bbbb8191b")')
  }

  assay_result_ids_json <- list(
    assayResultIds = list(assay_result_ids),
    reason = reason
  )
  base_url <- glue::glue('https://{tenant}.benchling.com/api/v2/assay-results:archive')
  result_raw <- httr::POST(url = base_url,
                           body = assay_result_ids_json,
                           encode = "json",
                           httr::authenticate(api_key, ''))
  result <- httr::content(result_raw)

  if (!is.null(result$error)) {
    stop(result$error)
  }

  return(result)

}

#' Makes a direct API Call to Benchling Assay Result Unarchive endpoint
#' endpoint: https://benchling.com/api/reference#/Assay%20Results/unarchiveAssayResults
#' @param tenant tenant name
#' @param api_key api key
#' @param assay_result_ids is vector type assay result ids. This function supports archiving multiple
#' items in one function call
#' @export
#' @examples \dontrun{
#' assay_result_ids <- assay_result_ids=c("908df834-220e-4b59-975b-c21bbbb8191a")
#' }
#' @returns assay_result_ids
#'
unarchive_assay_results <- function(tenant = Sys.getenv("BENCHLING_TENANT"),
                                    api_key = Sys.getenv("BENCHLING_API_KEY"),
                                    assay_result_ids = NULL) {
  if (api_key == "") {
    .missing_api_key_error()
  }

  if (tenant == "") {
    .missing_tenant_error()
  }

  if (is.null(assay_result_ids)) {
    stop('Please provide assay result ids in a vector. Example: c("908df834-220e-4b59-975b-c21bbbb8191a,
    908df834-220e-4b59-975b-c21bbbb8191b")')
  }

  assay_result_ids_json <- list(
    assayResultIds = list(assay_result_ids)
  )

  base_url <- glue::glue('https://{tenant}.benchling.com/api/v2/assay-results:unarchive')
  result_raw <- httr::POST(url = base_url,
                           body = assay_result_ids_json,
                           encode = "json",
                           httr::authenticate(api_key, ''))
  result <- httr::content(result_raw)

  if (!is.null(result$error)) {
    stop(result$error)
  }

  return(result)

}
#' Makes a direct API Call to Benchling Get Assay Result
#' endpoint: https://benchling.com/api/reference#/Assay%20Results/getAssayResult
#' @param tenant tenant name
#' @param api_key api key
#' @param assay_result_id is the id of the assay result.
#' @export
#'@examples \dontrun{
#' assay_result_id <- '908df834-220e-4b59-975b-c21bbbb8191a'
#'}
#'@returns assay result details
#'
get_assay_result <- function(tenant = Sys.getenv("BENCHLING_TENANT"),
                             api_key = Sys.getenv("BENCHLING_API_KEY"),
                             assay_result_id = '') {
  if (api_key == "") {
    .missing_api_key_error()
  }

  if (tenant == "") {
    .missing_tenant_error()
  }

  if (assay_result_id == '') {
    stop("assay_result_id is a required field for this operation. Please provide an assay_result_id.")
    return(FALSE)
  }

  base_url <- glue::glue('https://{tenant}.benchling.com/api/v2/assay-results/{assay_result_id}')

  assay_result_raw <- httr::GET(url = base_url, httr::authenticate(api_key, ''))
  assay_result <- httr::content(assay_result_raw)

  if (!is.null(assay_result$error)) {
    stop(assay_result$error)
  }
  return(assay_result)
}