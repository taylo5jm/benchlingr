# read_plate_diagrams.R

#' Find and parse plate diagrams in a notebook entry
#' 
#' Benchling users can add special unstructured tables to notebook entrys
#' called "plate diagrams" for conveying information about the contents
#' of physical plates. The `read_plate_layouts` function will extract these 
#' plate diagrams into data frames for further processing in R.
#' 
#' @param entry response from GET /entry/{entry_id}
#' @return List of data frames representing the plate diagrams present
#' in the notebook entry.
#' @examples 
#' client <- benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
#'                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
#' entry <- client$entries$get_entry_by_id("etr_f1bpDIes")
#' plate_diagrams <- read_plate_diagrams(entry)
#' @export
read_plate_diagrams <- function(entry) {
  is_plate_diagram <- function(df) {
    if (all(rownames(df) %in% 1:32) &
        all(colnames(df) %in% 1:48)) {
      TRUE
    } else {
      FALSE
    }
  }
  parse_plate_diagram <- function(df) {
    id <- 'content'
    df %>%
      setNames(1:ncol(.)) %>%
      dplyr::mutate(row = LETTERS[1:nrow(.)]) %>%
      tidyr::gather(key = "column", value = !!id, -row) 
  }
  res <- read_entry_tables(entry)
  plate_diagram_indices <- purrr::map_lgl(res, is_plate_diagram)
  res <- res[plate_diagram_indices]
  purrr::map(res, ~ parse_plate_diagram(.))
  
}
