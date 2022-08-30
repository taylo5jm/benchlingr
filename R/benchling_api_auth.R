# benchling_api_client.R
#' Create a Benchling API client that can be used to access the API via R. 
#' 
#' @param tenant URL for the Benchling tenant (character).
#' @param api_key API key for the Benchling tenant (character). The default
#' value is the "BENCHLING_API_KEY" environment variable in the `.Renviron`
#' file. 
#' @return Benchling API client object that can be used for accessing the 
#' API via R. 
#' @export
#' @examples \dontrun{
#' client <- benchling_api_auth(tenant="https://my-company-tenant-name.benchling.com")
#' }

benchling_api_auth <- function(
  tenant, api_key=Sys.getenv("BENCHLING_API_KEY")) {
  if (api_key == "") {
    stop("'api_key' cannot be an empty string. Set the 'BENCHLING_API_KEY' 
         environment variable in your .Renviron file. Edit the .Renviron file
         in R using the following command:
         usethis::edit_r_environ()")
  }
  #if (env_type == 'virtualenv') {
  #  
  #  reticulate::use_virtualenv(virtualenv=env_name)
  #} else {
  #  # Sys.setenv(RETICULATE_PYTHON=env_name)
  #  reticulate::use_condaenv(condaenv = env_name)
  #}
  reticulate::source_python(
    system.file("python", "benchling_api_client.py", package = "benchlingr"))
  return(benchling_api_client(tenant, api_key))
}
#Sys.setenv(RETICULATE_PYTHON='/Users/justintaylor/.virtualenvs/benchling-reticulate/bin/python')
#client <- benchling_api_auth(tenant="https://hemoshear.benchling.com",
#                             env_name="benchling-reticulate", env_type="virtualenv")
#client$entries$get_entry_by_id('etr_FbZuzgv6')
