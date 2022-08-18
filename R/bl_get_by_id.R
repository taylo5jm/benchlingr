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
#' @param source Component of the Benchling developer platform where
#' the entity should be retrieved from. Entities can be retrieved from
#' the API (`api`) or data warehouse (`db`). 
#' @export

bl_get_by_id <- function(bl_class, id=NULL, source="api") {
  UseMethod("bl_get_by_id")
}