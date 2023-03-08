# test-bulk_get_entity.R

api_key <- Sys.getenv("BENCHLING_DEV_API_KEY")

# test case 1
test_that("Verify that bulk_get_entity.R works in providing the correct response when entity_id is missing or invalid", {
  expect_error(bulk_get_entity(), "'entity_id' input is missing.")
  expect_error(bulk_get_entity(entity_id=NULL, benchling_api_key=api_key), "'entity_id' input is invalid.")
  expect_error(bulk_get_entity(entity_id=integer(0), benchling_api_key=api_key), "'entity_id' input is invalid.")
  expect_error(bulk_get_entity(entity_id=NA, benchling_api_key=api_key), "'entity_id' contains an invalid identifier.")
  expect_error(bulk_get_entity(entity_id=list(124,"seq_Cuf0bmCm", "bfi_Q13AlXkf"), benchling_api_key=api_key), "'entity_id' contains an invalid identifier.")
  expect_error(bulk_get_entity(entity_id=list("seq_Cuf0bmCm", NA, "bfi_Q13AlXkf"), benchling_api_key=api_key), "'entity_id' contains an invalid identifier.")
  expect_error(bulk_get_entity(entity_id=list("seq0bmCm","notr", "zreimas"), benchling_api_key=api_key), "All elements are unknown identifiers and only read out as NA.")
  expect_error(bulk_get_entity(entity_id=c("sewqz", "zreimas"), benchling_api_key=api_key), "All elements are unknown identifiers and only read out as NA.")
  expect_error(bulk_get_entity(entity_id=c("s1eseq_qz", "z3124.ls"), benchling_api_key=api_key), "All elements are unknown identifiers and only read out as NA.")
  })

# test case 2
test_that("Verify that bulk_get_entity.R works in generating a proper list of entities while at the same time generates the right warnings for any entity identifiers that cannot be matched", {
  expect_length(bulk_get_entity(entity_id=c("seq_Cuf0omCm", "bfi_9fKcrORv"), benchling_api_key=api_key), 2)
  expect_length((bulk_get_entity(entity_id=list("seq_Cuf0omCm", "bfi_9fKcrORv"), benchling_api_key=api_key))[[1]], 3)
  expect_length((bulk_get_entity(entity_id=c("seq_Cuf0omCm", "bfi_9fKcrORv"), benchling_api_key=api_key))[[2]], 3)
  expect_match(as.character(class(bulk_get_entity(entity_id=list("seq_Cuf0omCm", "bfi_9fKcrORv"), benchling_api_key=api_key))), "list")
  expect_match(as.character(class(bulk_get_entity(entity_id=c("seq_Cuf0omCm", "bfi_9fKcrORv"), benchling_api_key=api_key)[[1]])), "list")
  expect_match(as.character(class(bulk_get_entity(entity_id=list("seq_Cuf0omCm", "bfi_9fKcrORv"), benchling_api_key=api_key)[[2]])), "list")
  expect_equal(as.character((bulk_get_entity(entity_id=c("seq_Cuf0omCm", "bfi_9fKcrORv"), benchling_api_key=api_key))[[2]][[2]][[1]][[1]][["name"]]), "12C-Methylmalonic Acid")
  expect_equal(as.character((bulk_get_entity(entity_id=c("seq_Cuf0omCm", "bfi_9fKcrORv"), benchling_api_key=api_key))[[2]][["Entity Name"]]), "12C-Methylmalonic Acid")
  expect_warning(bulk_get_entity(entity_id=list("seq_Cuf0omCm", "test_02erf%6", "bfi_9fKcrORv"), benchling_api_key=api_key), 
                 "Entity schema for test_02erf%6 is unknown and labeled as NA. Single-Get API endpoint for test_02erf%6 is unknown and labeled as NA. BulkGet API endpoints for test_02erf%6 are unknown and labeled as NA.")
  expect_warning(bulk_get_entity(entity_id=list("seq_Cuf0bmCm", "test_02erf%6", "bfi_9fKcrORv"), benchling_api_key=api_key), 
                 "'entity_id' contains an unknown identifier. test_02erf%6 cannot be matched with any listed identifier.")
  expect_length((bulk_get_entity(entity_id=c("seq_Cuf0bmCm", "bfi_9fKcrORv", "etr_191Oksd", "ent_lo4wksd"), benchling_api_key=api_key)), 4)
  expect_equal(as.character((bulk_get_entity(entity_id=c("seq_Cuf0bmCm", "bfi_9fKcrORv", "etr_191Oksd", "ent_lo4wksd"), benchling_api_key=api_key))[[1]][[2]][[1]][[1]][["name"]]), "CYP1A1")
  expect_equal(as.character((bulk_get_entity(entity_id=c("seq_Cuf0bmCm", "bfi_9fKcrORv", "etr_191Oksd", "ent_lo4wksd"), benchling_api_key=api_key))[[1]][["Entity Name"]]), "CYP1A1")
  })
