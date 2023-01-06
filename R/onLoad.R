globalVariables(".")
globalVariables("benchling_sdk_entrypoint")
globalVariables("download_files")

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
}