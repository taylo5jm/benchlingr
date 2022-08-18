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
#' @param connector Benchling API client object or data warehouse
#' connection. See `bl_api_auth` and `bl_warehouse_connect`.
#' If a API client is passed, then the function will attempt to 
#' retrieve the data from the API. If database connection is passed,
#' then the function will attempt to retrieve the data from the warehouse. 
#' @export
bl_get_by_name <- function(bl_class, name=NULL, connector=NULL) {
    if (is.null(connector)) {
      stop("API client or database connection must be passed to
           the `connector` argument.")
    }
    UseMethod("bl_get_by_name")
}
