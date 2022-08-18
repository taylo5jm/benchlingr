# bl_get_by_id.R

#' Generic method for getting Benchling entities by identifier.
#' 
#' Benchling identifiers will typically start with three letters, 
#' followed by an underscore, and end with a series of alphanumeric 
#' characters. Custom entities will always start with `bfi_`, while other 
#' "built-in Benchling types" will start with a specific set of three letters.
#' Some examples of built-in Benchling types include plates, (`plt`), 
#' entries (`etr`), and containers (`con`). 
#' 
#' @param bl_class Benchling "class". 
#' @param id Benchling identifier for the object. 
#' @param connector Benchling API client object or data warehouse
#' connection. See `bl_api_auth` and `bl_warehouse_connect`.
#' If a API client is passed, then the function will attempt to 
#' retrieve the data from the API. If database connection is passed,
#' then the function will attempt to retrieve the data from the warehouse. 
#' @export

bl_get_by_id <- function(bl_class, id=NULL, connector=NULL) {
  if (is.null(connector)) {
    stop("API client or database connection must be passed to
           the `connector` argument.")
  }
  UseMethod("bl_get_by_id")
}