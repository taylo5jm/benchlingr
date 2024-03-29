# test-upload_files.R

client <- benchlingr::connect_sdk(
  tenant="https://hemoshear-dev.benchling.com",
  api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))


# upload_files.character ===============================================
test_that("upload_files.character works for one file", {
  res <- benchlingr:::.upload_files.character(
    file="test-upload_files.R", 
    client=client)
  testthat::expect_length(res, 1)
})

test_that("upload_files.character works for two files", {
  res <- benchlingr:::.upload_files.character(
    file=c("test-upload_files.R", "test-list_entry_tables.R"), 
    client=client)
  testthat::expect_length(res, 2)
})

test_that("upload_files.character gives informative error when file is missing", 
          {
    testthat::expect_error(
      benchlingr:::.upload_files.character(
      "file_that_does_not_exist.R", 
      client=client))
})

# upload_files.list ===============================================
test_that("upload_files.character works for one file", {
  res <- benchlingr:::.upload_files.list(
    file=list("test-upload_files.R"), 
    client=client)
  testthat::expect_length(res, 1)
})

test_that("upload_files.character works for two files", {
  res <- benchlingr:::.upload_files.list(
    file=list(c("test-upload_files.R","test-list_schemas_in_entry.R"), "test-list_entry_tables.R"), 
    client=client)
  testthat::expect_length(res, 2)
})

# upload_files.data.frame ========================================
test_that("upload_files.data.frame works for a data frame with one blob link
          column", {
          df <- data.frame(file = "test-upload_files.R")
          res <- benchlingr:::.upload_files.data.frame(file=df, client=client, blob_link_cols="file")
          testthat::expect_equal(nrow(res), 1)
})


# test_that("upload_files.character gives informative error when file is missing", 
#           {
#             testthat::expect_error(
#               benchlingr:::upload_files.character(
#                 "file_that_does_not_exist.R", 
#                 client=client))
#           })
