# config_sdk_env.R

#' Configure environment for accessing API
#' 
#' Helper function to create an Anaconda environment and install
#' the Benchling Python SDK (`benchling-sdk`) on pip. This Anaconda environment
#' is used by the reticulate package in R.
#' @param env_name Name for the new environment to be created. The default name
#' is 'benchling-reticulate'. It is highly recommended that the user keep
#' the default name, as many functions in the package assume the environment 
#' name is 'benchling-reticulate'. If you choose a different environment
#' name, then you will need to set the "BENCHLINGR_RETICULATE_ENV" option in
#' any scripts that invoke the API. 
#' (ex. 'options(BENCHLINGR_RETICULATE_ENV = "my_env_name")')
#' @examples \dontrun{
#' config_sdk_env()
#' }
#' @export
#' 
config_sdk_env <- function(env_name='benchling-reticulate') {
  reticulate::conda_create(env_name)
  reticulate::conda_install(env_name, 'benchling-sdk')
}