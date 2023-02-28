# test-infer_entity_type.R

entity_lookup <- list("plt_" = c("plate", "https://benchling.com/api/reference#/Plates/getPlate"),
                      "box_" = c("box", "https://benchling.com/api/reference#/Boxes/getBox"),
                      "con_" = c("container", "https://benchling.com/api/reference#/Containers/getContainer"),
                      "loc_" = c("location", "https://benchling.com/api/reference#/Locations/getLocation"),
                      "etr_" = c("entry", "https://benchling.com/api/reference#/Entries/getEntry"),
                      "bfi_" = c("custom_entity", "https://benchling.com/api/reference#/Custom%20Entities/getCustomEntity"),
                      "ent_" = c("user", "https://benchling.com/api/reference#/Users/getUser"),
                      "sfs_" = c("dropdown", "https://benchling.com/api/reference#/Dropdowns/getDropdown"),
                      "sfso_" = c("dropdown_option", "https://benchling.com/api/reference#/Dropdowns/getDropdown"), # the dropdown options are available from the `dropdown` endpoint, as well as the `dropdown_option` warehouse table.
                      "seq_" = c("dna_sequence", NA), # both dna_oligo and dna_sequence types start with seq, so there isn't one endpoint. find these in the database in the `entity` table instead.
                      "mxt_"= c("mixture", "https://benchling.com/api/reference#/Mixtures/getMixture"),
                      "container_batch" = c("container_content", "https://benchling.com/api/reference#/Containers/getContainerContent"))

# test_that("Verify that infer_entity_type.R provides the correct response when entity_id is missing or invalid", {
#   expect_error(infer_entity_type(), "'entity_id' input is missing.")
#   expect_error(infer_entity_type(NULL), "'entity_id' input is invalid.")
#   expect_error(infer_entity_type(NA), "'entity_id' contains an invalid identifier.")
#   expect_error(infer_entity_type(list(124,"seq_Cuf0bmCm", "bfi_Q13AlXkf")), "'entity_id' contains an invalid identifier.")
#   expect_error(infer_entity_type(list("seq_Cuf0bmCm", NA, "bfi_Q13AlXkf")), "'entity_id' contains an invalid identifier.")
#   })
# 
# test_that("Verify that infer_entity_type.R works in creating a proper character vector while providing warnings for any elements listed in 'entity_id' that cannot be matched", {
#   expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "bfi_Q13AlXkf")), c("seq_Cuf0bmCm" = "dna_sequence", "bfi_Q13AlXkf" = "custom_entity"))
#   expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "seq_ZZZZZZZZ")), c("seq_Cuf0bmCm" = "dna_sequence", "seq_ZZZZZZZZ" = "dna_sequence"))
#   expect_equal(infer_entity_type(c("seq_Cuf0bmCm")), c("seq_Cuf0bmCm" = "dna_sequence"))
#   expect_equal(infer_entity_type(c("seq_Cuf0AAAA")), c("seq_Cuf0AAAA" = "dna_sequence"))
#   expect_equal(infer_entity_type(list("etr_Q13AlXkf", "bfi_VVamxrKQ")), c("etr_Q13AlXkf" = "entry", "bfi_VVamxrKQ" = "custom_entity"))
#   expect_equal(infer_entity_type(c("bfi_Q13AlXkf", "bfi_VVamxrKQ", "seq_Cuf0AAAA")), c("bfi_Q13AlXkf" = "custom_entity", "bfi_VVamxrKQ" = "custom_entity", "seq_Cuf0AAAA" = "dna_sequence"))
#   expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")), c("seq_Cuf0bmCm" = "dna_sequence", "not_a_real_key" = NA))
#   expect_warning(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")),"'entity_id' contains an unknown identifier. not_a_real_key cannot be matched with any of the identifiers listed in 'entity_lookup.'")
#   expect_equal(infer_entity_type(list("seq_Cuf0bmCm","1234")), c("seq_Cuf0bmCm" = "dna_sequence", "1234" = NA))
#   expect_warning(infer_entity_type(list("seq_Cuf0bmCm","1234")),"'entity_id' contains an unknown identifier. 1234 cannot be matched with any of the identifiers listed in 'entity_lookup.'")
#   expect_equal(infer_entity_type("RQDFLKJ"), c("RQDFLKJ" = NA))
#   expect_warning(infer_entity_type("RQDFLKJ"),"'entity_id' contains an unknown identifier. RQDFLKJ cannot be matched with any of the identifiers listed in 'entity_lookup.'")
#   expect_equal(infer_entity_type(list("RQDFLK_J")), c("RQDFLK_J" = NA))
#   expect_warning(infer_entity_type("RQDFLK_J"),"'entity_id' contains an unknown identifier. RQDFLK_J cannot be matched with any of the identifiers listed in 'entity_lookup.'")
#   })

test_that("Verify that infer_entity_type.R provides the correct response when entity_id is missing or invalid", {
  expect_error(infer_entity_type(entity_id=, entity_lookup=entity_lookup), "'entity_id' input is missing.")
  expect_error(infer_entity_type(entity_id=NULL, entity_lookup=entity_lookup), "'entity_id' input is invalid.")
  expect_error(infer_entity_type(entity_id=NA, entity_lookup=entity_lookup), "'entity_id' contains an invalid identifier.")
  expect_error(infer_entity_type(entity_id=list(124,"seq_Cuf0bmCm", "bfi_Q13AlXkf"), entity_lookup=entity_lookup), "'entity_id' contains an invalid identifier.")
  expect_error(infer_entity_type(entity_id=list("seq_Cuf0bmCm", NA, "bfi_Q13AlXkf"), entity_lookup=entity_lookup), "'entity_id' contains an invalid identifier.")
  expect_error(infer_entity_type(entity_id=list("seq_Cuf0bmCm","1234"), entity_lookup=entity_lookup),"'entity_id' contains an unknown identifier. 1234 cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_error(infer_entity_type(entity_id=c("seq_Cuf0bmCm", "not_a_real_key"), entity_lookup=entity_lookup),"'entity_id' contains an unknown identifier. not_a_real_key cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_error(infer_entity_type(entity_id="RQDFLKJ", entity_lookup=entity_lookup),"'entity_id' contains an unknown identifier. RQDFLKJ cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_error(infer_entity_type(entity_id="RQDFLK_J", entity_lookup=entity_lookup),"'entity_id' contains an unknown identifier. RQDFLK_J cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  })

test_that("Verify that infer_entity_type.R works in creating a proper character vector while providing warnings for any elements listed in 'entity_id' that cannot be matched", {
  expect_equal(infer_entity_type(entity_id=c("seq_Cuf0bmCm", "bfi_Q13AlXkf"), entity_lookup=entity_lookup), c("seq_Cuf0bmCm" = "dna_sequence", "bfi_Q13AlXkf" = "custom_entity"))
  expect_equal(infer_entity_type(entity_id=c("seq_Cuf0bmCm", "seq_ZZZZZZZZ"), entity_lookup=entity_lookup), c("seq_Cuf0bmCm" = "dna_sequence", "seq_ZZZZZZZZ" = "dna_sequence"))
  expect_equal(infer_entity_type(entity_id=c("seq_Cuf0bmCm"), entity_lookup=entity_lookup), c("seq_Cuf0bmCm" = "dna_sequence"))
  expect_equal(infer_entity_type(entity_id=c("seq_Cuf0AAAA"), entity_lookup=entity_lookup), c("seq_Cuf0AAAA" = "dna_sequence"))
  expect_equal(infer_entity_type(entity_id=list("etr_Q13AlXkf", "bfi_VVamxrKQ"), entity_lookup=entity_lookup), c("etr_Q13AlXkf" = "entry", "bfi_VVamxrKQ" = "custom_entity"))
  expect_equal(infer_entity_type(entity_id=c("bfi_Q13AlXkf", "bfi_VVamxrKQ", "seq_Cuf0AAAA"), entity_lookup=entity_lookup), c("bfi_Q13AlXkf" = "custom_entity", "bfi_VVamxrKQ" = "custom_entity", "seq_Cuf0AAAA" = "dna_sequence"))
  })

