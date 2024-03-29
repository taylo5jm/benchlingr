# connect_sdk.R
#' Create a Benchling API client that can be used to access the API via R. 
#' 
#' @include error.R
#' @param tenant URL for the Benchling tenant (character).
#' @param api_key API key for the Benchling tenant (character). The default
#' value is the "BENCHLING_API_KEY" environment variable in the `.Renviron`
#' file. 
#' @return Benchling API client object that can be used for accessing the 
#' API via R. 
#' @export
#' @examples \dontrun{
#' client <- connect_sdk(tenant="https://my-company-tenant-name.benchling.com")
#' }

connect_sdk <- function(
  tenant, api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (api_key == "") {
    .missing_api_key_error()
  }
  reticulate::source_python(
    system.file("python", "benchling_sdk_entrypoint.py", package = "benchlingr"))
  return(benchling_sdk_entrypoint(tenant, api_key))
}

