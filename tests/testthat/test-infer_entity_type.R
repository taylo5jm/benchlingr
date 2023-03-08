# test-infer_entity_type.R

# test case 1
test_that("Verify that infer_entity_type.R provides the correct response when entity_id is missing or invalid", {
  expect_error(infer_entity_type(), "'entity_id' input is missing.")
  expect_error(infer_entity_type(NULL), "'entity_id' input is invalid.")
  expect_error(infer_entity_type(NA), "'entity_id' contains an invalid identifier.")
  expect_error(infer_entity_type(list(124,"seq_Cuf0bmCm", "bfi_Q13AlXkf")), "'entity_id' contains an invalid identifier.")
  expect_error(infer_entity_type(list("seq_Cuf0bmCm", NA, "bfi_Q13AlXkf")), "'entity_id' contains an invalid identifier.")
  })

# test case 2
test_that("Verify that infer_entity_type.R works in creating a proper character vector while providing warnings for any elements listed in 'entity_id' that cannot be matched", {
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "bfi_Q13AlXkf")), list("seq_Cuf0bmCm" = c("Entity Schema"="dna_sequence", "Single-Get API Endpoint Request URL"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences/seq_Cuf0bmCm",
                                                                                             "Bulk-Get API Endpoints Request URL"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=seq_Cuf0bmCm"),
                                                                          "bfi_Q13AlXkf" = c("Entity Schema"="custom_entity", "Single-Get API Endpoint Request URL"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/bfi_Q13AlXkf",
                                                                                             "Bulk-Get API Endpoints Request URL"="https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=bfi_Q13AlXkf")))
  expect_equal(infer_entity_type(c("seq_CufCm")), list("seq_CufCm" = c("Entity Schema"="dna_sequence", "Single-Get API Endpoint Request URL"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences/seq_CufCm",
                                                                       "Bulk-Get API Endpoints Request URL"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=seq_CufCm")))
  expect_equal(infer_entity_type(list("etr_Q13AlXkf", "bfi_VVamxrKQ")), list("etr_Q13AlXkf" = c("Entity Schema"="entry", "Single-Get API Endpoint Request URL"="https://hemoshear-dev.benchling.com/api/v2/entries/etr_Q13AlXkf",
                                                                                                "Bulk-Get API Endpoints Request URL"="https://hemoshear-dev.benchling.com/api/v2/entries:bulk-get?entryIds=etr_Q13AlXkf"),
                                                                             "bfi_VVamxrKQ" = c("Entity Schema"="custom_entity", "Single-Get API Endpoint Request URL"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/bfi_VVamxrKQ",
                                                                                                "Bulk-Get API Endpoints Request URL"="https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=bfi_VVamxrKQ")))
  expect_equal(infer_entity_type(list("seq_Cuf0AAAA")), list("seq_Cuf0AAAA" = c("Entity Schema"="dna_sequence", "Single-Get API Endpoint Request URL"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences/seq_Cuf0AAAA",
                                                                                "Bulk-Get API Endpoints Request URL"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=seq_Cuf0AAAA")))
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")), list("seq_Cuf0bmCm" = c("Entity Schema"="dna_sequence", "Single-Get API Endpoint Request URL"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences/seq_Cuf0bmCm",
                                                                                               "Bulk-Get API Endpoints Request URL"="https://hemoshear-dev.benchling.com/api/v2/dna-sequences:bulk-get?dnaSequenceIds=seq_Cuf0bmCm"),
                                                                            "not_a_real_key" = c("Entity Schema"=NA, "Single-Get API Endpoint Request URL"=NA,"Bulk-Get API Endpoints Request URL"=NA)))
  expect_warning(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")), "'entity_id' contains an unknown identifier. not_a_real_key cannot be matched with any listed identifier.")
  expect_equal(infer_entity_type(list("bfi_VKYYKQ","1234")), list("bfi_VKYYKQ" = c("Entity Schema"="custom_entity", "Single-Get API Endpoint Request URL"="https://hemoshear-dev.benchling.com/api/v2/custom-entities/bfi_VKYYKQ",
                                                                                   "Bulk-Get API Endpoints Request URL"="https://hemoshear-dev.benchling.com/api/v2/custom-entities:bulk-get?customEntityIds=bfi_VKYYKQ"),
                                                                  "1234" = c("Entity Schema"=NA, "Single-Get API Endpoint Request URL"=NA,"Bulk-Get API Endpoints Request URL"=NA)))
  expect_warning(infer_entity_type(list("bfi_VKYYKQ","1234")), "'entity_id' contains an unknown identifier. 1234 cannot be matched with any listed identifier.")
  expect_equal(infer_entity_type("RQDFLKJ"), list("RQDFLKJ" = c("Entity Schema"=NA, "Single-Get API Endpoint Request URL"=NA,"Bulk-Get API Endpoints Request URL"=NA)))
  expect_warning(infer_entity_type("RQDFLKJ"), "'entity_id' contains an unknown identifier. RQDFLKJ cannot be matched with any listed identifier.")
  })
