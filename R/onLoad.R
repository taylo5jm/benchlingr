globalVariables(".")
globalVariables("benchling_sdk_entrypoint")
globalVariables("download_files")
globalVariables("upload_results_with_sdk")

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
}