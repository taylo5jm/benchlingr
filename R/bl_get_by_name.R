# bl_get_by_name.R

#' Generic method for getting Benchling entities by name. 
#' 
#' The naming convention of a Benchling entity depend on the naming options 
#' given to Benchling users in the schema definition. The name might be the 
#' same as the registry ID, however, it is also possible to allow Benchling 
#' users to name entities in a free-form manner, or "name templates" can be 
#' created to allow for entity names to be comprised of fields in the entity. 
#' @param bl_class Benchling "class". 
#' @param name Name of the Benchling entity to be retrieved.
#' @param source Component of the Benchling developer platform where
#' the entity should be retrieved from. Entities can be retrieved from
#' the API (`api`) or data warehouse (`db`). 
#' @export
bl_get_by_name <- function(bl_class, name=NULL, source="api") {
    UseMethod("bl_get_by_name")
}
