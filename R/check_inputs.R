check_inputs <- function(entity_id, benchling_api_key) {
  if (benchling_api_key == "") { # Check if the Benchling API Key is not defined or is missing from the .Renviron file.
    stop("Benchling API key is missing or empty.") # Stop the function if the Benchling API Key is missing or not defined in the .Renviron file.
  }
}
  