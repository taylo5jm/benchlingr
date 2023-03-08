# test-as_name.R

api_key <- Sys.getenv("BENCHLING_DEV_API_KEY")

# test case 1
test_that("Verify that as_name.R works in providing the correct response when entity_id is missing or invalid", {
  expect_error(as_name(), "'entity_id' input is missing.")
  expect_error(as_name(entity_id=NULL, benchling_api_key=api_key, return_df=FALSE), "'entity_id' input is invalid.")
  expect_error(as_name(entity_id=integer(0), benchling_api_key=api_key, return_df=FALSE), "'entity_id' input is invalid.")
  expect_error(as_name(entity_id=NA, benchling_api_key=api_key, return_df=FALSE), "'entity_id' contains an invalid identifier.")
  expect_error(as_name(entity_id=list(124,"sebmCm", NA, "bfi_Q13AlXkf"), benchling_api_key=api_key, return_df=FALSE), "'entity_id' contains an invalid identifier.")
  expect_error(as_name(entity_id=list("etr6hmoi99Cm","seb32r", "entfzreimas"), benchling_api_key=api_key, return_df=FALSE), "All elements are unknown identifiers and only read out as NA.")
  expect_error(as_name(entity_id=NULL, benchling_api_key=api_key, return_df=TRUE), "'entity_id' input is invalid.")
  expect_error(as_name(entity_id=integer(0), benchling_api_key=api_key, return_df=TRUE), "'entity_id' input is invalid.")
  expect_error(as_name(entity_id=NA, benchling_api_key=api_key, return_df=TRUE), "'entity_id' contains an invalid identifier.")
  expect_error(as_name(entity_id=list(900,"s3", 10993, "bfi_Q13AlXkf"), benchling_api_key=api_key, return_df=TRUE), "'entity_id' contains an invalid identifier.")
  expect_error(as_name(entity_id=c("et699Cm","seq32r", "entfzreimas"), benchling_api_key=api_key, return_df=TRUE), "All elements are unknown identifiers and only read out as NA.")
  })

# test case 2
entity_id1 <- c("seq_Cuf0bmCm", "bfi_9fKcrORv")
entity_id2 <- c("seq_Cuf0bmCm", "seq_Cuf0umCm", "bfi_9fKcrORv", "etn123p")
entity_vector1 <- c("seq_Cuf0bmCm" = "CYP1A1", "bfi_9fKcrORv" = "12C-Methylmalonic Acid")
entity_vector2 <- c("seq_Cuf0bmCm" = "CYP1A1", "seq_Cuf0umCm" = NA, 
                    "bfi_9fKcrORv" = "12C-Methylmalonic Acid", "etn123p" = NA)
entity_dataframe1 <- data.frame("id" = c("seq_Cuf0bmCm", "bfi_9fKcrORv"), 
                                "names" = c("CYP1A1", "12C-Methylmalonic Acid"))
entity_dataframe2 <- data.frame("id" = c("seq_Cuf0bmCm", "seq_Cuf0umCm", "bfi_9fKcrORv", "etn123p"),
                                "names" = c("CYP1A1", "NA", "12C-Methylmalonic Acid", "NA"))

test_that("Check that as_name.R works in either creating a labeled vector of entity names or a 
          two-column data frame where the first column is 'id' and the second column is 'name' 
          while at the same time generating the right warnings for any entity identifiers 
          that cannot be matched", {
  expect_equal(as_name(entity_id=entity_id1, benchling_api_key=api_key, return_df=FALSE), entity_vector1)
  expect_equal(as_name(entity_id=entity_id2, benchling_api_key=api_key, return_df=FALSE), entity_vector2)
  expect_warning(as_name(entity_id=entity_id2, benchling_api_key=api_key, return_df=FALSE), "'entity_id' contains an unknown identifier. etn123p cannot be matched with any listed identifier.")
  expect_warning(as_name(entity_id=entity_id2, benchling_api_key=api_key, return_df=FALSE), "Entity schema for etn123p is unknown and labeled as NA. Single-Get API endpoint for etn123p is unknown and labeled as NA. BulkGet API endpoints for etn123p are unknown and labeled as NA.")
  expect_equal(as_name(entity_id=entity_id1, benchling_api_key=api_key, return_df=TRUE), entity_dataframe1)
  expect_equal(as_name(entity_id=entity_id2, benchling_api_key=api_key, return_df=TRUE), entity_dataframe2)
  expect_warning(as_name(entity_id=entity_id2, benchling_api_key=api_key, return_df=TRUE), "'entity_id' contains an unknown identifier. etn123p cannot be matched with any listed identifier.")
  expect_warning(as_name(entity_id=entity_id2, benchling_api_key=api_key, return_df=TRUE), "Entity schema for etn123p is unknown and labeled as NA. Single-Get API endpoint for etn123p is unknown and labeled as NA. BulkGet API endpoints for etn123p are unknown and labeled as NA.")
  })
  
