# test-list_entity_columns.R

conn <- warehouse_connect("hemoshear-dev", 
     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
res <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw")

test_that("list_entity_columns works", {
  expect_equal(list_entity_columns(conn, res), 
               c("analytes"= (which(colnames(res) == 'plate') + 1)))
})
