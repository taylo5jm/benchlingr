conn <- warehouse_connect("hemoshear-dev", 
                          username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                          password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))



test_that("replace_entity_id_with_name works for tables with one entity column", {
  df <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping_with_two_analytes$raw WHERE entry_id$ = 'etr_lnZDpRVI'")
  res <- replace_entity_id_with_name(conn, df)
  expect_equal(res$analyte_set_1, "12C-Uric Acid")
})


test_that("replace_entity_id_with_name gives warning when user tries to use function
          on multi-select, entity field", {
  df <- DBI::dbGetQuery(
    conn, 
    "SELECT * FROM simple_plate_analyte_mapping_with_two_analytes$raw WHERE entry_id$ = 'etr_lnZDpRVI'")
  expect_warning(replace_entity_id_with_name(conn,  df),
                 regexp="before the ID can be")

})