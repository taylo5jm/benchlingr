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
#' @param client A Benchling API client object. 
#' @param conn A warehouse connection opened with `warehouse_connect`.
#' @param df Data frame retrieved from the Benchling data warehouse.
#' Must have the `schema` column included.
#' @param columns Character vector of column names in the table to download 
#' blobs from. 
#' @param outdir Directory where the files should be saved on the local machine.
#' Subdirectories are created within this directory for each blob link column
#' in the data frame. If `outdir_column` is provided, then this argument is 
#' ignored.
#' @param outdir_column Column in the data frame that represents the directories
#' in which the files should be saved. This argument overrides `outdir`.
#' 
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

download_blobs_in_warehouse_table <- function(client, conn, df, columns=NULL, 
                                              outdir=NULL, outdir_column=NULL) {
  is_schema_in_dataframe(df)
  blob_link_columns <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT schema_field.name FROM schema 
    INNER JOIN schema_field ON schema.id = schema_field.schema_id 
    WHERE schema.system_name = {shQuote(unique(df$schema))} AND 
    schema_field.type = 'blob_link'")) %>%
  .[,1]
  assertthat::assert_that(length(blob_link_columns) > 0,
                          msg="There are no blob link fields in this schema.")
  if (is.null(outdir) & is.null(outdir_column)) {
    warning("Both 'outdir' and 'outdir_column' are missing. Saving files to
            the current directory")
    outdir <- '.'
  }
  # Download all files in the data frame if columns is missing
  if (is.null(columns)) {
    columns <- blob_link_columns
  }
  assertthat::assert_that(
    all(columns %in% blob_link_columns),
    msg="The columns argument ({paste0(columns, collapse=',')})
         contains columns that do not exist in the schema. 
         Only the following columns are blob links in this schema ({unique(df$schema)}):
        paste0('-', blob_link_columns, '\n')")
  if (!is.null(outdir_column)) {
    assertthat::assert_that(
      length(outdir_column) == 1,
      msg=glue::glue("The 'outdir_column' argument ({paste0(outdir_column, collapse=',')})
      must be a character vector of length 1.")
    )
    assertthat::assert_that(
      outdir_column %in% colnames(df),
      msg=glue::glue("The 'outdir_column' argument ({paste0(outdir_column, collapse=',')})
      must be the name of a column in the input data frame.")
    )
    for (i in 1:length(unique(df[[outdir_column]]))) {
      if (!dir.exists(df[[outdir_column]][i])) {
        dir.create(df[[outdir_column]][i], recursive = T)
      }
    }

    if (!is.null(outdir)) {
      warning("Both `outdir` and `outdir_column` were passed to the function.
            Using `outdir_column`.")
    }
    
  } else {
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = T)
    }
  }

  .download_blobs <- function(client, df, column, outdir, outdir_column) {
    reticulate::source_python(
      system.file("python", "download_files.py", package = "benchlingr"))
    file_col <- purrr::map(as.character(df[[column]]),
                           ~ RJSONIO::fromJSON(.))
    # If user wants to save results in child directories named by result ID,
    # then carry that information with the rest of the file metadata.
    for (i in 1:length(file_col)) {
      # Single value
      if (is.character(file_col[[i]])) {
        if (!is.null(outdir_column)) {
          file_col[[i]]['outdir'] <- df[[outdir_column]][i]
        } else {
          file_col[[i]]['outdir'] <- outdir
        }
      } else { # multi-select
        for (j in 1:length(file_col[[i]])) {
          if (!is.null(outdir_column)) {
            file_col[[i]][[j]]['outdir'] <- df[[outdir_column]][i]
          } else {
            file_col[[i]][[j]]['outdir'] <- outdir
            }
          }
      }
    }
    unlisted_file_col <- unlist(file_col, recursive = FALSE)
  
    # multi-select
    if (is.list(unlisted_file_col)) {
      file_col <- unlisted_file_col
    }
    
    file_map <- purrr::map_chr(
        file_col, ~ file.path(.['outdir'], .['name']))

    names(file_map) <- purrr::map_chr(file_col, ~ .['id'])
    file_map <- as.list(file_map)
    download_files(client, file_map)
  }

  purrr::walk(columns, ~ .download_blobs(client, df, ., outdir,
                                         outdir_column))
  return(df)
}
