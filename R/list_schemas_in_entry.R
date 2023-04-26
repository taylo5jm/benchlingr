
#' List the schemas for the results and registration tables contained within
#' a notebook entry
#' 
#' @importFrom magrittr %>%
#' @param client Benchling API client created by `connect_sdk`.
#' @param conn Benchling data warehouse connection created with `connect_warehouse`.
#' @param entry Notebook entry retrieved with the `entries$get_entry_by_id` method
#' of the Benchling Python SDK facade object created by `benchlingr::connect_sdk`.
#' @return A data frame that includes the names and identifiers of the results
#' and registration schemas contained within a notebook entry. 
#' @export
#' @examples \dontrun{
#' conn <- connect_warehouse("hemoshear-dev", 
#'                          username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'                          password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' client <- connect_sdk(tenant="https://hemoshear-dev.benchling.com",
#'                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
#' nb_entry <- client$entries$get_entry_by_id("etr_MWQ7M7Pz")
#' list_schemas_in_entry(client, conn, nb_entry)
#' }

list_schemas_in_entry <- function(client, conn, entry) {
    
    class2field <- c(
      "benchling_api_client.v2.stable.models.registration_table_note_part.RegistrationTableNotePart" = "entity_schema_id",
      "benchling_api_client.v2.stable.models.results_table_note_part.ResultsTableNotePart" = "assay_result_schema_id")
    
    .extract_schema_ids <- function(entry_day) { 
    
      classes <- purrr::map_chr(entry_day$notes, ~ class(.) %>% .[1])
      schemas_in_entry <- list()
      for (i in 1:length(class2field)) {
          this_table_set <- which(classes == names(class2field)[i])
          schemas <- purrr::map_chr(entry_day$notes[this_table_set],
                                    ~ .[class2field[i]])
          schemas_in_entry[[i]] <- schemas
      }
      unlist(schemas_in_entry)
    }
    schemas_in_entry <- purrr::map(entry$days, ~ .extract_schema_ids(.)) %>%
      unlist
    if (length(schemas_in_entry) > 0) {
      DBI::dbGetQuery(
        conn, 
        glue::glue("SELECT * FROM schema WHERE id IN {.vec2sql_tuple(unlist(schemas_in_entry))}"))
    } else {
      data.frame()
    }
}

