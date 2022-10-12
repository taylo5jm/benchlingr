globalVariables(".")
globalVariables("benchling_api_client")
globalVariables("download_files")

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
}