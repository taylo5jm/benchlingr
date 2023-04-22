.craft_api_queries <- function(entity_get_endpoints) {
  api_queries <- sapply(names(entity_get_endpoints),
                        function(x) gsub("ENTITY_ID", x, entity_get_endpoints[x])) # Prepares single-get API endpoint 
                                                                                   # URL queries for each entity 
                                                                                   # identifier not assigned with a 
                                                                                   # value of NA.
  names(api_queries) <- names(entity_get_endpoints) # Re-assigns entity identifiers as names
                                                    # to each corresponding URL query.
  return(api_queries)
}
  