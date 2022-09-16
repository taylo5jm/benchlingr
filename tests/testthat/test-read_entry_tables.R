# test-read_entry_tables.R

library(benchlingr)
client <- benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
tables <- read_entry_tables(entry)

test_that("read_entry_tables works", {
  expect_equal(length(tables), 1)
})

test_that("read_entry_tables returns column names", {
  expect_setequal(colnames(tables[[1]]),c("sample_id", "treatment"))
})

test_that("read_entry_tables returns correct number of rows", {
  expect_equal(nrow(tables[[1]]),6)
})