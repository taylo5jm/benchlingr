conn <- warehouse_connect("hemoshear-dev", 
    username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
    password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))



test_that("get_entity_table works for tables with one entity column", {
  df <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw WHERE entry_id$ = 'etr_MWQ7M7Pz'")
  res <- get_entity_table(conn,  df)
  expect_equal(nrow(res$analytes), 4)
})

test_that("get_entity_table works for tables with one entity column after
           table has been transformed by replace_entity_id_with_name", {
  df <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw WHERE entry_id$ = 'etr_MWQ7M7Pz'")
  df <- df %>% replace_entity_id_with_name(conn, .)
  res <- get_entity_table(conn,  df, key = "name$")
  expect_equal(nrow(res$analytes), 4)
})



test_that("get_entity_table works for tables with more than one entity column", {
  df <- DBI::dbGetQuery(
    conn, 
    "SELECT * FROM simple_plate_analyte_mapping_with_two_analytes$raw WHERE entry_id$ = 'etr_lnZDpRVI'")
  res <- get_entity_table(conn,  df)
  expect_equal(nrow(res$analyte_set_1), 1)
  expect_equal(nrow(res$analyte_set_2), 2)
  
})


DBI::dbDisconnect(conn)