#' Get the options in a dropdown menu
#' 
#' 
#' @importFrom magrittr %>%
#' @param conn Database connection opened with `connect_warehouse`.
#' @param schema_id Schema ID for the dropdown.
#' @return character vector of dropdown menu options 
#' @export

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
#' @param name Schema name for the dropdown
#' @export
#' 
get_dropdown <- function(conn, name) {
  return(DBI::dbGetQuery(
    conn, glue::glue("SELECT dropdown WHERE name = '{name}'")))
}
