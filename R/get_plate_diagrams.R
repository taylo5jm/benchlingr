# get_plate_diagrams.R

#' Find and parse plate diagrams in a notebook entry
#' 
#' Benchling users can add special unstructured tables to notebook entrys
#' called "plate diagrams" for conveying information about the contents
#' of physical plates. The `read_plate_layouts` function will extract these 
#' plate diagrams into data frames for further processing in R.
#' 
#' @include get_entry_tables.R
#' @importFrom stats setNames
#' @param entry  Notebook entry retrieved with the `entries$get_entry_by_id` method
#' of the Benchling Python SDK facade object created by `benchling::connect_sdk`.
#' @param plate_dim A numeric vector of length 1 that describes the number of wells
#' in the plates that should be read. The plate sizes must be one of the following:
#' 6, 12, 24, 48, 96, 384, and 1536. By default, `plate_dim` is `NULL` and the 
#' function will return plates of all sizes supported by Benchling. 
#' @return List of data frames representing the plate diagrams present
#' in the notebook entry.
#' @examples \dontrun{
#' client <- connect_sdk(tenant="https://hemoshear-dev.benchling.com",
#'                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
#' entry <- client$entries$get_entry_by_id("etr_f1bpDIes")
#' plate_diagrams <- get_plate_diagrams(entry)
#' }
#' @export
get_plate_diagrams <- function(entry, plate_dim=NULL) {
  if (!is.null(plate_dim)) {
    assertthat::assert_that(
      plate_dim %in% c(6, 12, 24, 48, 96, 384, 1536),
      msg="Plate dimensions argument ('plate_dim') must be one of the following:
      6, 12, 24, 48, 96, 384, 1536")
    
    plate_dim_map <- list(
      '6' = c(2, 3),
      '12' = c(3, 4),
      '24' = c(4, 6),
      '48' = c(6, 8),
      '96' = c(8, 12),
      '384' = c(16, 24),
      '1536' = c(32, 48))
    plate_dim <- plate_dim_map[[as.character(plate_dim)]]
  }
  is_plate_diagram <- function(df, plate_dim=NULL) {
    # If user selects a plate size, verify it is one of the plate sizes
    # offered by Benchling. 
    if (!is.null(plate_dim)) {
      valid_rows <- c(2, 3, 4, 6, 8, 16, 32)
      valid_cols <- c(3, 4, 6, 8, 12, 24, 48)
      assertthat::assert_that(
        plate_dim[1] %in% valid_rows,
        msg=glue::glue("Number of rows must be in (paste0(valid_rows, collapse=','))"))
      assertthat::assert_that(
        plate_dim[2] %in% valid_cols,
        msg=glue::glue("Number of columns must be in (paste0(valid_columns, collapse=','))"))
      if (setequal(rownames(df), 1:plate_dim[1]) &
          setequal(colnames(df), 1:plate_dim[2])) {
        TRUE
      } else {
        FALSE
      }
    } else { # If user doesn't provide plate dimensions, grab every plate
        row_index <- 32
        col_index <- 48
        if (all(rownames(df) %in% 1:row_index) &
            all(colnames(df) %in% 1:col_index)) {
          TRUE
        } else {
          FALSE
        }
    }
  }

  parse_plate_diagram <- function(df) {
    id <- 'content'
    df %>%
      setNames(1:ncol(.)) %>%
      dplyr::mutate(row = LETTERS[1:nrow(.)]) %>%
      tidyr::gather(key = "column", value = !!id, -row) 
  }
  res <- get_entry_tables(entry)
  plate_diagram_indices <- purrr::map_lgl(res, ~ is_plate_diagram(., plate_dim))
  res <- res[plate_diagram_indices]
  purrr::map(res, ~ parse_plate_diagram(.))
  
}
