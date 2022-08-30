# config_sdk_env.R

#' Configure environment for accessing API
#' 
#' Helper function to create an Anaconda environment and install
#' the Benchling Python SDK (`benchling-sdk`) on pip. This Anaconda environment
#' is used by the reticulate package in R.
#' @param env_name Name for the new environment to be created. The default name
#' is 'benchling-reticulate'.
#' @param env_type 'virtualenv' or 'conda'.
#' (ex. 'options(BENCHLINGR_RETICULATE_ENV = "my_env_name")')
#' @examples \dontrun{
#' config_sdk_env()
#' }
#' @export
#' 
config_sdk_env <- function(env_name='benchling-reticulate',
                           env_type='virtualenv') {
  if (env_type == 'conda') {
    reticulate::conda_create(env_name)
    reticulate::conda_install(env_name, 'benchling-sdk')
  } else {
    reticulate::virtualenv_create(env_name)
    reticulate::virtualenv_install(env_name, packages='benchling-sdk')
  }
  
  
}