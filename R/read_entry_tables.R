# read_entry_tables.R

#' Read unstructured table in a notebook entry
#' 
#' @importFrom magrittr %>%
#' @param entry Notebook entry in JSON format.
#' @param day Integer for the day in the notebook entry. See `find_entry_tables`.
#' @param table_index Integer for the position of the table in the notebook entry list. 
#' See `find_entry_tables`.
#' @return Data frame representing the unstructured table in the notebook
#' entry.
#' @export
#' @examples \dontrun{
#' entry <- get_entry(id = "etr")
#' table_indices <- find_entry_tables(entry)
#' # This code will read the first table found in the first day of the notebook
#' # entry.
#' a_table <- read_table(entry, day=1, table_index=table_indices[[1]][1])
#' }
read_entry_table <- function(entry, day, table_index) {
  a_table <- entry$days[[day]]$notes[[table_index]]
  a_table$table$rows %>%
    purrr::map(~ .[[1]]) %>%
    do.call('rbind', .) %>% 
    as.data.frame()
}



#' Read all unstructured tables in a notebook entry. 
#' 
#' This function will read all unstructured tables in a notebook entry,
#' returning a list of data frames. 
#' 
#' @include find_entry_tables.R
#' @param entry Notebook entry in JSON format. See `get_entry`.
#' @param verbose If verbose, then the function will alert the user
#' if no tables can be found for some days in the notebook. 
#' @return List of data frames representing the unstructured tables in
#' the notebook entry.
#' @examples \dontrun{
#' entry <- get_entry(id = "etr")
#' tables <- read_tables(entry)
#' }
#' @export

read_entry_tables <- function(entry, verbose=FALSE) {
  table_indices <- find_entry_tables(entry)
  res <- list(); k <- 1;
  for (i in 1:length(table_indices)) {
    if (length(table_indices[[i]]) > 0) {
      for (j in 1:length(table_indices[[i]])) {
        res[[k]] <- read_entry_table(entry, day=i, 
                               table_index=table_indices[[i]][j])
        k <- k + 1
      }
      
    } else {
      if (verbose) {
        cat(glue::glue("No tables found for day {`i`}\n"))
      }
    }
  }
  res 
}