.construct_schema_sets <- function(entity_id, size_format) {
  if (size_format=="list contents" | size_format=="single-get endpoint" | # Only accepts 3 parameters for size_format since the size of the 
                                                                          # set of entity identifiers depends on whether it will be used in 
                                                                          # conjunction with the list api contents option, the single-get 
                                                                          # endpoint contents option or the bulk-get endpoints contents option
      size_format=="bulk-get endpoints") {
    if (size_format=="list contents" | size_format=="bulk-get endpoints") {
      size = 50
    } 
    if (size_format=="single-get endpoint") {
      size = 1
    }
  } else { # Stops the function if the input for size_format is not either 'list contents', 'single-get endpoint' 
           # or 'bulk-get endpoints'
    stop("size_format input is invalid. Must be defined either as 'list contents', 'single-get endpoint' 
         or 'bulk-get endpoints.'")
  }
  
  entity_id_sets <- split(entity_id, ceiling(seq_along(entity_id)/size)) # Splits a vector containing entity identifiers into a 
                                                                         # number of sets based on the maximum size of each set
  
  if (length(entity_id_sets) > 1) { # Names sets depending on whether or not there is more than one set
    names(entity_id_sets) <- sapply(seq(length(entity_id_sets)), function(x) paste0("set", x)) 
  } else {
    names(entity_id_sets) <- "set"
  }
  
  return(entity_id_sets)
}
