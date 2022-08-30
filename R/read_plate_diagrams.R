# read_plate_diagrams.R

#' Find and parse plate diagrams in a notebook entry
#' 
#' Benchling users can add special unstructured tables to notebook entrys
#' called "plate diagrams" for conveying information about the contents
#' of physical plates. The `read_plate_layouts` function will extract these 
#' plate diagrams into data frames for further processing in R.
#' 
#' @include find_entry_tables.R
#' @param json response from GET /entry/{entry_id}
#' @param min_rows Minimum number of rows on each plate. 
#' @param flatten If flatten is TRUE, then the results are collapsed into a 
#' single data frame, in which the `entry_index` column shows which day the 
#' plate was read from. If flatten is FALSE, then a nested list is returned, 
#' where each element corresponds to a single day. 
#' @param verbose If verbose, then the table names are printed to the console.
#' @return List of data frames representing the plate diagrams present
#' in the notebook entry.
#' @export
read_plate_diagrams <- function(json, min_rows=NULL,
                               flatten=TRUE, verbose=TRUE) {
  tables <- find_entry_tables(json, min_rows=min_rows)
  pls <- vector("list", length(tables))
  k <- 1
  for (d in 1:length(tables)) {
    for (j in 1:length(tables[[d]])) {
      this_d <- json$days[[d]]$notes[tables[[d]][j]][[1]]
      pls[[k]] <- tryCatch({
        .parse_plate_diagram(this_d)},
        error = function(e) {e})
      pls[[k]]$entry_day <- d
      k <- k + 1
    }
  }
  if (flatten) {
    res <- dplyr::bind_rows(pls)
  } else {
    res <- pls
  }
  
  if (verbose) {
    cat(glue::glue("Read {length(unique(res$plate))} plates:\n\n"))
    cat(paste0(unique(res$plate), sep='\n'))
  }
  return(res)
}


#' Parse well layout JSON from a notebook entry
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @param json Well layout JSON. 
#' @return Long form table with the following columns: `row`, `column`, `content`, and `plate`. 
.parse_plate_diagram <- function(json) {
  pl <- suppressMessages(purrr::map_df(json$table$rows, ~ unlist(.[[1]])))
  id <- 'content'
  pl %>%
    setNames(1:ncol(.)) %>%
    dplyr::mutate(row = LETTERS[1:nrow(.)]) %>%
    tidyr::gather(key = "column", value = !!id, -row) %>%
    dplyr::mutate(plate = json$table$name)
  
}