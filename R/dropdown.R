#' Get the options in a dropdown menu
#' 
#' 
#' @importFrom magrittr %>%
#' @param conn Database connection opened with `connect_warehouse`.
#' @param schema_id Schema ID for the dropdown. This value is stored in the `id`
#' column of the `dropdown` table in the Benchling data warehouse. It will always
#' start with `sfs_`. You can find dropdown menus in the Benchling interface
#' by clicking the "Registries" grid, then "Registry setting" under the cog icon,
#' then "Dropdowns". 
#' @return character vector of dropdown menu options 
#' @export
#' @examples \dontrun{
#' # Connect to the Benchling data warehouse
#' conn <- connect_warehouse("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' # To see all dropdown menus, you can read the entire `dropdown` table. 
#' # This won't pull the dropdown options that correspond to each dropdown,
#' # so it probably won't be that large. 
#' DBI::dbReadTable(conn, "dropdown")
#' }

get_dropdown_options <- function(conn, schema_id) {
  return(DBI::dbGetQuery(
    conn, 
    glue::glue("SELECT dropdown_option.name FROM dropdown INNER JOIN 
      dropdown_option ON dropdown.id = dropdown_option.dropdown_id 
                 WHERE dropdown.id = '{schema_id}'")) %>%
      .[,1] %>%
      as.character())
}


#' Get metadata for a dropdown menu
#' 
#' @param conn Database connection opened with `connect_warehouse`.
#' @param name Schema name for the dropdown. This value is stored in the `name`
#' column of the `dropdown` table in the Benchling data warehouse. 
#' The name of a dropdown is set when it is created, and it doesn't have to follow
#' a specific pattern.
#' @export
#' @examples \dontrun{
#' # Connect to the Benchling data warehouse
#' conn <- connect_warehouse("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' # To see all dropdown menus, you can read the entire `dropdown` table. 
#' # This won't pull the dropdown options that correspond to each dropdown,
#' # so it probably won't be that large. 
#' DBI::dbReadTable(conn, "dropdown")
#' }
get_dropdown <- function(conn, name) {
  return(DBI::dbGetQuery(
    conn, glue::glue("SELECT dropdown WHERE name = '{name}'")))
}
