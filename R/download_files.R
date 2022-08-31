# download_files.R

#' Download file attachments from Benchling
#' @param file_map List where names are blob (file) identifiers
#' and values are file names.
#' @param outdir Directory in which to save output files. 
#' @param api_key Benchling API key. Defaults to the `BENCHLING_API_KEY` 
#' environment variable in `.Renviron`.
#' @param condaenv Name of the conda environment to use. Defaults to 
#' `benchling-reticulate`.
#' @export
#' @examples \dontrun{
#' res <- DBI::dbGetQuery(conn, "SELECT * FROM 
#' non_barcode_plate_file_results$raw WHERE study_codes::text LIKE 
#' '%bfi_jUgN7fPL%'")
#' file_col <- purrr::map(as.character(res$excel_file),
#'                       ~ RJSONIO::fromJSON(.) %>% .[[1]])
#' file_map <- purrr::map_chr(file_col, ~ .['name'])
#' names(file_map) <- purrr::map_chr(file_col, ~ .['id'])
#' file_map <- as.list(file_map)
#' download_files(file_map, outdir='data')
#' }
#' 
download_files <- function(file_map, outdir, 
                           api_key=Sys.getenv("BENCHLING_API_KEY"),
                           condaenv='benchling-reticulate') {
    reticulate::use_condaenv(condaenv = condaenv)
    reticulate::source_python(
        system.file("python", "download_files.py", package = "benchling"))
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
    download_files(file_map, api_key=api_key)
}

