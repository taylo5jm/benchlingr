# error.R

#' Stop function execution if API key is an empty string.
#' 
#' @keywords internal
#' @return Nothing.
.missing_api_key_error <- function() {
  stop("'api_key' cannot be an empty string. Set the 'BENCHLING_API_KEY' 
         environment variable in your .Renviron file. Edit the .Renviron file
         in R using the following command:
         usethis::edit_r_environ()")
}

#' Stop function execution if tenant is an empty string.
#' 
#' @keywords internal
#' @return Nothing.
.missing_tenant_error <- function() {
  stop("'tenant' cannot be an empty string. Set the 'BENCHLING_TENANT' 
         environment variable in your .Renviron file. Edit the .Renviron file
         in R using the following command:
         usethis::edit_r_environ()")
}
