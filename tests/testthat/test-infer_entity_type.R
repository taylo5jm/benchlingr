# test-infer_entity_type.R

# test case 1
test_that("Verify that infer_entity_type.R works in creating a proper named character vector when provided a character vector or 1D list", {
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "bfi_Q13AlXkf")), c("seq_Cuf0bmCm" = "dna_sequence", 
                                                                       "bfi_Q13AlXkf" = "custom_entity"))
  expect_equal(infer_entity_type(list("bfi_Ur5DfvGJ", "seq_Gju61mCm", "bfi_Q13AlXkf", "bfi_Ks908uWV",
                                      "eut_Ec76_X9f", "ent_sPrxBN.h", "box_K9950IQ8", "dis_89mkooip",
                                      "bxo_923aklum")),
               c("bfi_Ur5DfvGJ" = "custom_entity", "seq_Gju61mCm" = "dna_sequence", "bfi_Q13AlXkf" = "custom_entity", 
                 "bfi_Ks908uWV" = "custom_entity", "eut_Ec76_X9f" = NA, "ent_sPrxBN.h" = "user", "box_K9950IQ8" = "box", 
                 "dis_89mkooip" = NA, "bxo_923aklum" = NA))
  expect_equal(infer_entity_type(c("seq_Cuf0bmCm", "not_a_real_key")), c("seq_Cuf0bmCm" = "dna_sequence", 
                                                                         "not_a_real_key" = NA))
  expect_equal(infer_entity_type(list("seq_Cuf0AAAA")), c("seq_Cuf0AAAA" = "dna_sequence"))
  expect_equal(infer_entity_type(c("bfi_Q13AlXkf", "bfi_VVamxrKQ")), c("bfi_Q13AlXkf" = "custom_entity", 
                                                                      "bfi_VVamxrKQ" = "custom_entity"))
  expect_equal(infer_entity_type(list("bfi_Q13AlXkf", "bfi_VVamxrKQ", "seq_Cuf0AAAA")), c("bfi_Q13AlXkf" = "custom_entity", 
                                                                                          "bfi_VVamxrKQ" = "custom_entity", 
                                                                                          "seq_Cuf0AAAA" = "dna_sequence"))
  expect_equal(infer_entity_type(c("RQDFLKJ")), c("RQDFLKJ" = NA))
  })
