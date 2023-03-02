# test-bulk_get_entity.R

# test case 1
test_that("Verify that bulk_get_entity.R works in providing the correct response when entity_id is missing or invalid", {
  expect_error(bulk_get_entity(), "'entity_id' input is missing.")
  expect_error(bulk_get_entity(NULL), "'entity_id' input is invalid.")
  expect_error(bulk_get_entity(NA), "'entity_id' contains an invalid identifier.")
  expect_error(bulk_get_entity(list(124,"seq_Cuf0bmCm", "bfi_Q13AlXkf")), "'entity_id' contains an invalid identifier.")
  expect_error(bulk_get_entity(list("seq_Cuf0bmCm", NA, "bfi_Q13AlXkf")), "'entity_id' contains an invalid identifier.")
  expect_error(bulk_get_entity(list("seq0bmCm","notr", "zreimas")), "All elements are unknown identifiers and only read out as NA.")
  expect_error(bulk_get_entity(c("sewqz", "zreimas")), "All elements are unknown identifiers and only read out as NA.")
  expect_error(bulk_get_entity(c("s1eseq_qz", "z3124.ls")), "All elements are unknown identifiers and only read out as NA.")
})

# test case 2
test_that("Verify that bulk_get_entity.R works in generating a proper list of entities while at the same time generates the right warnings for any entity identifiers that cannot be matched", {
  expect_equal(length(bulk_get_entity(list("etr_Q13AlXkf", "bfi_VVamxrKQ"))),2)
  expect_true(!is.na(bulk_get_entity(c("etr_Q13AlXkf", "bfi_VVamxrKQ"))[[1]][1]))
  expect_true(!is.na(bulk_get_entity(c("etr_Q13AlXkf", "bfi_VVamxrKQ"))[[2]][1]))
  expect_equal(length(bulk_get_entity(list("et3AlXkf", "OPias.asd", "bfi_VVamxrKQ"))),3)
  expect_warning(bulk_get_entity(c("et3AlXkf", "OPias.asd", "bfi_VVamxrKQ")), 
                 "Entity schema for et3AlXkf is unknown and labeled as NA. API endpoint for et3AlXkf is unknown and labeled as NA. BulkGet API endpoints for et3AlXkf are unknown and labeled as NA.")
  expect_warning(bulk_get_entity(c("et3AlXkf", "OPias.asd", "bfi_VVamxrKQ")), 
                 "Entity schema for OPias.asd is unknown and labeled as NA. API endpoint for OPias.asd is unknown and labeled as NA. BulkGet API endpoints for OPias.asd are unknown and labeled as NA.")
  expect_true(is.na(bulk_get_entity(c("et3AlXkf", "OPias.asd", "bfi_VVamxrKQ"))[[1]][1]))
  expect_true(is.na(bulk_get_entity(c("et3AlXkf", "OPias.asd", "bfi_VVamxrKQ"))[[2]][1]))
  expect_true(!is.na(bulk_get_entity(c("et3AlXkf", "OPias.asd", "bfi_VVamxrKQ"))[[3]][1]))
})
