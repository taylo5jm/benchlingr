# test-infer_entity_type.R

# entity_lookup <- list("prtn" = c("aa_sequences", "https://benchling.com/api/reference#/AA%20Sequences/getAASequence",
#                                  "https://benchling.com/api/reference#/AA%20Sequences/bulkGetAASequences"),
#                       "bat" = c("batches", "https://benchling.com/api/reference#/Batches/getBatch", 
#                                 "https://benchling.com/api/reference#/Batches/bulkGetBatches"),
#                       "plt" = c("plate", "https://benchling.com/api/reference#/Plates/getPlate", 
#                                 "https://benchling.com/api/reference#/Plates/bulkGetPlates"),
#                       "box" = c("box", "https://benchling.com/api/reference#/Boxes/getBox", 
#                                 "https://benchling.com/api/reference#/Boxes/bulkGetBoxes"),
#                       "con" = c("container", "https://benchling.com/api/reference#/Containers/getContainer", 
#                                 "https://benchling.com/api/reference#/Containers/bulkGetContainers"),
#                       "loc" = c("location", "https://benchling.com/api/reference#/Locations/getLocation", 
#                                 "https://benchling.com/api/reference#/Locations/bulkGetLocations"),
#                       "etr" = c("entry", "https://benchling.com/api/reference#/Entries/getEntry", 
#                                 "https://benchling.com/api/reference#/Entries/bulkGetEntries"),
#                       "bfi" = c("custom_entity", "https://benchling.com/api/reference#/Custom%20Entities/getCustomEntity",
#                                 "https://benchling.com/api/reference#/Custom%20Entities/bulkGetCustomEntities"),
#                       "ent" = c("user", "https://benchling.com/api/reference#/Users/getUser", NA),
#                       "sfs" = c("dropdown", "https://benchling.com/api/reference#/Dropdowns/getDropdown", NA),
#                       "sfso" = c("dropdown_option", "https://benchling.com/api/reference#/Dropdowns/getDropdown", NA), # the dropdown options are available from the `dropdown` endpoint, as well as the `dropdown_option` warehouse table. 
#                       "seq" = c("dna_sequence", "https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
#                                 "https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences"), 
#                       "mxt"= c("mixture", "https://benchling.com/api/reference#/Mixtures/getMixture", NA),
#                       "container_batch" = c("container_content", "https://benchling.com/api/reference#/Containers/getContainerContent", NA))

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
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "bfi_Q13AlXkf")), list("seq_Cuf0bmCm" = c("Entity Schema"="dna_sequence", "API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                                                                             "BulkGet API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences"), 
                                                                          "bfi_Q13AlXkf" = c("Entity Schema"="custom_entity", "API Endpoint"="https://benchling.com/api/reference#/Custom%20Entities/getCustomEntity",
                                                                                             "BulkGet API Endpoint"="https://benchling.com/api/reference#/Custom%20Entities/bulkGetCustomEntities")))
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "seq_ZZZZZZZZ")), list("seq_Cuf0bmCm" = c("Entity Schema"="dna_sequence", "API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                                                                            "BulkGet API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences"), 
                                                                         "seq_ZZZZZZZZ" = c("Entity Schema"="dna_sequence", "API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                                                                            "BulkGet API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences")))
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm")), list("seq_Cuf0bmCm" = c("Entity Schema"="dna_sequence", "API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                                                             "BulkGet API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences")))
  expect_equal(infer_entity_type(c("seq_Cuf0AAAA")), list("seq_Cuf0AAAA" = c("Entity Schema"="dna_sequence", "API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                                                             "BulkGet API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences")))
  expect_equal(infer_entity_type(list("etr_Q13AlXkf", "bfi_VVamxrKQ")), list("etr_Q13AlXkf" = c("Entity Schema"="entry", "API Endpoint"="https://benchling.com/api/reference#/Entries/getEntry", 
                                                                                                "BulkGet API Endpoint"="https://benchling.com/api/reference#/Entries/bulkGetEntries"), 
                                                                             "bfi_VVamxrKQ" = c("Entity Schema"="custom_entity", "API Endpoint"="https://benchling.com/api/reference#/Custom%20Entities/getCustomEntity",
                                                                                                "BulkGet API Endpoint"="https://benchling.com/api/reference#/Custom%20Entities/bulkGetCustomEntities")))
  expect_equal(infer_entity_type(c("bfi_Q13AlXkf", "bfi_VVamxrKQ", "seq_Cuf0AAAA")), list("bfi_Q13AlXkf" = c("Entity Schema"="custom_entity", "API Endpoint"="https://benchling.com/api/reference#/Custom%20Entities/getCustomEntity",
                                                                                                             "BulkGet API Endpoint"="https://benchling.com/api/reference#/Custom%20Entities/bulkGetCustomEntities"), 
                                                                                          "bfi_VVamxrKQ" = c("Entity Schema"="custom_entity", "API Endpoint"="https://benchling.com/api/reference#/Custom%20Entities/getCustomEntity",
                                                                                                             "BulkGet API Endpoint"="https://benchling.com/api/reference#/Custom%20Entities/bulkGetCustomEntities"), 
                                                                                          "seq_Cuf0AAAA" = c("Entity Schema"="dna_sequence", "API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                                                                                             "BulkGet API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences")))
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")), list("seq_Cuf0bmCm" = c("Entity Schema"="dna_sequence", "API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                                                                               "BulkGet API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences"), 
                                                                            "not_a_real_key" = c("Entity Schema"=NA, "API Endpoint"=NA,"BulkGet API Endpoint"=NA)))
  expect_warning(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")), "'entity_id' contains an unknown identifier. not_a_real_key cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_equal(infer_entity_type(list("seq_Cuf0bmCm","1234")), list("seq_Cuf0bmCm" = c("Entity Schema"="dna_sequence", "API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/getDNASequence", 
                                                                                       "BulkGet API Endpoint"="https://benchling.com/api/reference#/DNA%20Sequences/bulkGetDNASequences"), 
                                                                    "1234" = c("Entity Schema"=NA, "API Endpoint"=NA,"BulkGet API Endpoint"=NA)))
  expect_warning(infer_entity_type(list("seq_Cuf0bmCm","1234")), "'entity_id' contains an unknown identifier. 1234 cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_equal(infer_entity_type("RQDFLKJ"), list("RQDFLKJ" = c("Entity Schema"=NA, "API Endpoint"=NA,"BulkGet API Endpoint"=NA)))
  expect_warning(infer_entity_type("RQDFLKJ"), "'entity_id' contains an unknown identifier. RQDFLKJ cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_equal(infer_entity_type(list("RQDFLK_J")), list("RQDFLK_J" = c("Entity Schema"=NA, "API Endpoint"=NA,"BulkGet API Endpoint"=NA)))
  expect_warning(infer_entity_type("RQDFLK_J"), "'entity_id' contains an unknown identifier. RQDFLK_J cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  })

# test_that("Verify that infer_entity_type.R provides the correct response when entity_id is missing or invalid", {
#   expect_error(infer_entity_type(entity_id=, entity_lookup=entity_lookup), "'entity_id' input is missing.")
#   expect_error(infer_entity_type(entity_id=NULL, entity_lookup=entity_lookup), "'entity_id' input is invalid.")
#   expect_error(infer_entity_type(entity_id=NA, entity_lookup=entity_lookup), "'entity_id' contains an invalid identifier.")
#   expect_error(infer_entity_type(entity_id=list(124,"seq_Cuf0bmCm", "bfi_Q13AlXkf"), entity_lookup=entity_lookup), "'entity_id' contains an invalid identifier.")
#   expect_error(infer_entity_type(entity_id=list("seq_Cuf0bmCm", NA, "bfi_Q13AlXkf"), entity_lookup=entity_lookup), "'entity_id' contains an invalid identifier.")
#   expect_error(infer_entity_type(entity_id=list("seq_Cuf0bmCm","1234"), entity_lookup=entity_lookup),"'entity_id' contains an unknown identifier. 1234 cannot be matched with any of the identifiers listed in 'entity_lookup.'")
#   expect_error(infer_entity_type(entity_id=c("seq_Cuf0bmCm", "not_a_real_key"), entity_lookup=entity_lookup),"'entity_id' contains an unknown identifier. not_a_real_key cannot be matched with any of the identifiers listed in 'entity_lookup.'")
#   expect_error(infer_entity_type(entity_id="RQDFLKJ", entity_lookup=entity_lookup),"'entity_id' contains an unknown identifier. RQDFLKJ cannot be matched with any of the identifiers listed in 'entity_lookup.'")
#   expect_error(infer_entity_type(entity_id="RQDFLK_J", entity_lookup=entity_lookup),"'entity_id' contains an unknown identifier. RQDFLK_J cannot be matched with any of the identifiers listed in 'entity_lookup.'")
#   })

# test_that("Verify that infer_entity_type.R works in creating a proper character vector while providing warnings for any elements listed in 'entity_id' that cannot be matched", {
#   expect_equal(infer_entity_type(entity_id=c("seq_Cuf0bmCm", "bfi_Q13AlXkf"), entity_lookup=entity_lookup), c("seq_Cuf0bmCm" = "dna_sequence", "bfi_Q13AlXkf" = "custom_entity"))
#   expect_equal(infer_entity_type(entity_id=c("seq_Cuf0bmCm", "seq_ZZZZZZZZ"), entity_lookup=entity_lookup), c("seq_Cuf0bmCm" = "dna_sequence", "seq_ZZZZZZZZ" = "dna_sequence"))
#   expect_equal(infer_entity_type(entity_id=c("seq_Cuf0bmCm"), entity_lookup=entity_lookup), c("seq_Cuf0bmCm" = "dna_sequence"))
#   expect_equal(infer_entity_type(entity_id=c("seq_Cuf0AAAA"), entity_lookup=entity_lookup), c("seq_Cuf0AAAA" = "dna_sequence"))
#   expect_equal(infer_entity_type(entity_id=list("etr_Q13AlXkf", "bfi_VVamxrKQ"), entity_lookup=entity_lookup), c("etr_Q13AlXkf" = "entry", "bfi_VVamxrKQ" = "custom_entity"))
#   expect_equal(infer_entity_type(entity_id=c("bfi_Q13AlXkf", "bfi_VVamxrKQ", "seq_Cuf0AAAA"), entity_lookup=entity_lookup), c("bfi_Q13AlXkf" = "custom_entity", "bfi_VVamxrKQ" = "custom_entity", "seq_Cuf0AAAA" = "dna_sequence"))
#   })

