# download_files.R

#' Download file attachments from Benchling
#' 
#' Download "blobs" attached to Benchling entities or results.
#' 
#' @param client Benchling client object created by `benchling_api_auth()`.
#' @param file_map List where names are blob (file) identifiers
#' and values are file names.
#' @param outdir Directory in which to save output files. 
#' @param condaenv Name of the conda environment to use. Defaults to 
#' `benchling-reticulate`.
#' @export
#' @examples \dontrun{
#' conn <- warehouse_connect("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' res <- DBI::dbGetQuery(conn, "SELECT * FROM 
#' non_barcode_plate_file_results$raw WHERE study_codes::text LIKE 
#' '%bfi_jUgN7fPL%'")
#' file_col <- purrr::map(as.character(res$excel_file),
#'                       ~ RJSONIO::fromJSON(.) %>% .[[1]])
#' file_map <- purrr::map_chr(file_col, ~ .['name'])
#' names(file_map) <- purrr::map_chr(file_col, ~ .['id'])
#' file_map <- as.list(file_map)
#' download_blobs(file_map, outdir='data')
#' }
#' 
download_blobs <- function(client, file_map, outdir,
                           condaenv='benchling-reticulate') {
    # reticulate::use_condaenv(condaenv = condaenv)
    reticulate::source_python(
        system.file("python", "download_files.py", package = "benchlingr"))
    if (is.data.frame(file_map)) {
        fmap <- file_map$file_name
        names(fmap) <- file_map$blob_id
        file_map <- as.list(fmap)
    }
    # Add on the output directory to the file name. 
    prev_names <- names(file_map)
    new_files <- file.path(outdir, file_map)
    file_map <- new_files
    names(file_map) <- prev_names
    file_map <- as.list(file_map)
    if (!dir.exists(outdir)) {
        dir.create(outdir)
    }
    download_files(client, file_map)
}

#' Download blobs contained within a warehouse table. 
#' 
#' @include util.R
#' @importFrom magrittr %>%
#' @param conn A warehouse connection opened with `warehouse_connect`.
#' @param df Data frame retrieved from the Benchling data warehouse.
#' Must have the `schema` column included.
#' @param columns Character vector of column names in the table to download 
#' blobs from. 
#' @param outdir Directory where the files should be saved on the local machine.
#' Subdirectories are created within this directory for each blob link column
#' in the data frame. 
#' @export
#' @examples \dontrun{
#' library(magrittr)
#' client <- benchling_api_auth("https://hemoshear.benchling.com")
#' conn <- warehouse_connect("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' res <- DBI::dbGetQuery(conn, "SELECT * FROM 
#'  non_barcode_plate_file_results$raw WHERE study_codes::text LIKE 
#'  '%bfi_jUgN7fPL%'")
#' download_blobs_in_warehouse_table(conn, res, outdir='temp_data_dir/')
#' }

download_blobs_in_warehouse_table <- function(conn, df, columns=NULL, outdir='.') {
  is_schema_in_dataframe(df)
  blob_link_columns <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT schema_field.name FROM schema 
    INNER JOIN schema_field ON schema.id = schema_field.schema_id 
    WHERE schema.system_name = {shQuote(unique(df$schema))} AND 
    schema_field.type = 'blob_link'")) %>%
  .[,1]
  assertthat::assert_that(length(blob_link_columns) > 1,
                          msg="There are no blob link fields in this schema.")
  # Download all files in the data frame if columns is missing
  if (is.null(columns)) {
    columns <- blob_link_columns
  }
  assertthat::assert_that(all(columns %in% blob_link_columns),
                          msg="The columns argument ({paste0(columns, collapse=',')})
                             contains columns that do not exist in the schema. 
                            Only the following columns are blob links in this schema ({unique(df$schema)}):
                            paste0('-', blob_link_columns, '\n')")

  .download_blobs <- function(df, column, outdir) {
    file_col <- purrr::map(as.character(res[[column]]),
                           ~ RJSONIO::fromJSON(.) %>% .[[1]])
    file_map <- purrr::map_chr(file_col, ~ .['name'])
    names(file_map) <- purrr::map_chr(file_col, ~ .['id'])
    file_map <- as.list(file_map)
    download_files(client, file_map, outdir=outdir)
  }
  purrr::walk(columns, ~ .download_blobs(df, ., outdir=file.path(outdir, .)))
}
