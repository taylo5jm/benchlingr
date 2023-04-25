# .craft_api_queries.R

#' Format each element using its entity identifier's name to generate proper Single-Get API endpoint URLs for 
#' that identifier
#' 
#' .craft_api_queries.R takes a named character vector with one or more elements where the names are the names 
#' of entity identifiers and the elements are each identifier's respective Single-Get API endpoint URL formats 
#' and rewrites the element using the name of the identifier to generate proper Single-Get API endpoint URLs.
#' 
#' @param entity_single_get_endpoints A named character vector where the names are the names of entity 
#' identifiers and the elements are the Single-Get API endpoint URL formats for each identifier.
#' @return A named character vector where the names are the names of entity identifiers and the elements are 
#' the respective Single-Get API endpoint URLs for each identifier.
#' @examples \dontrun{
#' entity_single_get_endpoints <- c("seq_Cuf0bmCm"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences/ENTITY_ID", 
#' "bfi_Q1PMlXkf"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID", 
#' "box_7YutniM0"="https://hemoshear-dev.benchling.com/api/v2/boxes/ENTITY_ID",
#' "bfi_Ks908uWV"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/ENTITY_ID")
#' res <- .craft_api_queries(entity_single_get_endpoints=entity_single_get_endpoints)
#' }
#' @export
#' @keywords internal

.craft_api_queries <- function(entity_single_get_endpoints) {
  api_queries <- sapply(names(entity_single_get_endpoints),
                        function(x) gsub("ENTITY_ID", x, entity_single_get_endpoints[x])) # Prepares single-get API endpoint 
                                                                                   # URL queries for each entity 
                                                                                   # identifier not assigned with a 
                                                                                   # value of NA.
  names(api_queries) <- names(entity_single_get_endpoints) # Re-assigns entity identifiers as names
                                                    # to each corresponding URL query.
  return(api_queries)
}
  