get_schema_fields <- function(schema_id) {
  client <- benchling_api_auth(tenant = "https://hemoshear-dev.benchling.com",
                               api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))
}

verify_schema_fields <- function(schema_id, df) { }