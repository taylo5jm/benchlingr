# test-expand_multiselect_column.R

conn <- connect_warehouse("hemoshear-dev", 
     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))

d <- DBI::dbGetQuery(conn, "SELECT plate,analytes FROM simple_plate_analyte_mapping$raw WHERE entry_id$ = 'etr_MWQ7M7Pz'")

test_that("expand_multiselect_column works when shape is long", {
  res <- expand_multiselect_column(conn, d, column="analytes", shape="long")
  expect_equal(nrow(res), 4)
})

test_that("expand_multiselect_column works when shape is wide.", {
  res <- expand_multiselect_column(conn, d, column="analytes", shape="wide")
  expect_equal(nrow(res), 2)
  expect_setequal(c("plate", "analytes", "analytes1", "analytes2"), colnames(res))
})

test_that("expand_multiselect_column gives informative error when user tries
          to expand column that doesn't exist in the data frame.", {
  this_d <- d
  this_d$analytes <- NULL
  expect_error(expand_multiselect_column(conn, this_d, column="analytes", shape="wide"),
               regexp = "analytes is not a column")
})




DBI::dbDisconnect(conn)
