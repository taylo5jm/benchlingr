# test-infer_entity_type.R

test_that("Verify that infer_entity_type.R provides the correct response when entity_id is missing or invalid", {
  expect_error(infer_entity_type(), "'entity_id' input is missing.")
  expect_error(infer_entity_type(NULL), "'entity_id' input is invalid.")
  expect_error(infer_entity_type(NA), "'entity_id' contains an invalid identifier.")
  expect_error(infer_entity_type(list(124,"seq_Cuf0bmCm", "bfi_Q13AlXkf")), "'entity_id' contains an invalid identifier.")
  expect_error(infer_entity_type(list("seq_Cuf0bmCm", NA, "bfi_Q13AlXkf")), "'entity_id' contains an invalid identifier.")
})

test_that("Verify that infer_entity_type.R works in creating a proper character vector while providing warnings for any elements listed in 'entity_id' that cannot be matched", {
  expect_equal(infer_entity_type(list("seq_Cuf0bmCm","1234")), c("seq_Cuf0bmCm" = "dna_sequence", "1234" = NA))
  expect_warning(infer_entity_type(list("seq_Cuf0bmCm","1234")),"'entity_id' contains an unknown identifier. 1234 cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "bfi_Q13AlXkf")), c("seq_Cuf0bmCm" = "dna_sequence", "bfi_Q13AlXkf" = "custom_entity"))
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")), c("seq_Cuf0bmCm" = "dna_sequence", "not_a_real_key" = NA))
  expect_warning(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")),"'entity_id' contains an unknown identifier. not_a_real_key cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "seq_ZZZZZZZZ")), c("seq_Cuf0bmCm" = "dna_sequence", "seq_ZZZZZZZZ" = "dna_sequence"))
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm")), c("seq_Cuf0bmCm" = "dna_sequence"))
  expect_equal(infer_entity_type(c("seq_Cuf0AAAA")), c("seq_Cuf0AAAA" = "dna_sequence"))
  expect_equal(infer_entity_type(list("etr_Q13AlXkf", "bfi_VVamxrKQ")), c("etr_Q13AlXkf" = "entry", "bfi_VVamxrKQ" = "custom_entity"))
  expect_equal(infer_entity_type(c("bfi_Q13AlXkf", "bfi_VVamxrKQ", "seq_Cuf0AAAA")), c("bfi_Q13AlXkf" = "custom_entity", "bfi_VVamxrKQ" = "custom_entity", "seq_Cuf0AAAA" = "dna_sequence"))
  expect_equal(infer_entity_type("RQDFLKJ"), c("RQDFLKJ" = NA))
  expect_warning(infer_entity_type("RQDFLKJ"),"'entity_id' contains an unknown identifier. RQDFLKJ cannot be matched with any of the identifiers listed in 'entity_lookup.'")
  expect_equal(infer_entity_type(list("RQDFLK_J")), c("RQDFLK_J" = NA))
  expect_warning(infer_entity_type("RQDFLK_J"),"'entity_id' contains an unknown identifier. RQDFLK_J cannot be matched with any of the identifiers listed in 'entity_lookup.'")
})
