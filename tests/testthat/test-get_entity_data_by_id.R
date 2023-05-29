# test-get_entity_data_by_id.R

api_key = Sys.getenv("BENCHLING_API_KEY")

# test case 1
test_that("Verify that get_entity_data_by_id.R generates warnings when encountering
          invalid entity identifiers", {
  expect_warning(get_entity_data_by_id(entity_id=c("bfi_ztXInwdh", "box_t99a7IQ8",
                                                   "byi_97Yp9bb1", "tpi_TyH556Ger"), 
                                       api_key = api_key), 
                 "The following entity identifiers cannot be matched to an entity type and have therefore been removed: byi_97Yp9bb1, tpi_TyH556Ger.")
  expect_warning(get_entity_data_by_id(entity_id=c("ent_a0SApq3z", "bfi_Y7ORWDSz",
                                                   "seq_Cuf0bmCm", "rev_W387Jkh1"), 
                                                 api_key = api_key), 
                 "The following entity identifier cannot be matched to an entity type and has therefore been removed: rev_W387Jkh1.")
  
  expect_warning(get_entity_data_by_id(entity_id=c("con_m1dmbdV8", "mis_88BuyojM",
                                                   "seq_Gyum90j7", "bfi_ru401kLi",
                                                   "jhi_poT429cD"), 
                                       api_key = api_key), 
                 "The following entity identifiers cannot be matched to an entity type and have therefore been removed: mis_88BuyojM, jhi_poT429cD.")
  })

# test case 2
test_that("Verify that get_entity_data_by_id.R works in creating a proper named list
          of Single-Get API endpoint response contents for entity identifiers matched
          to a known entity type", {
  expect_equal(length(suppressWarnings(get_entity_data_by_id(entity_id=c("box_t99a7IQ8", "bfi_8ujY7lkI",
                                                                         "spi_R498kjLo", "wqe_tY50laks",
                                                                         "bfi_Y7ORWDSz"),
                                                             api_key = api_key))), 3)
  
  expect_equal(length(suppressWarnings(get_entity_data_by_id(entity_id=c("seq_9oUjkiG6", "bfi_O9iP8yMn",
                                                                         "lbu_As7jkExO9", "cvr_iO8u2q09",
                                                                         "con_oIur9Klr"),
                                                             api_key = api_key))), 3)
  
  expect_equal(length(suppressWarnings(get_entity_data_by_id(entity_id=c("etr_lu8wTLfL", "vxh_9IumaEwp", "pji_iOi90JmN"),
                                                             api_key = api_key))), 1)
  })

# test case 3
test_that("Verify that get_entity_data_by_id.R generates errors when provided an input
          of invalid entity identifiers", {
  expect_error(get_entity_data_by_id(entity_id = c("bif_98IupkJ", "cta_Oi9s1055",
                                                   "mni_as2d5567", "uht_pYlas844"),
                                     api_key = api_key),
               "None of the entity identifiers can be matched to an entity type and are therefore all invalid.")
  
  expect_error(get_entity_data_by_id(entity_id = c("dx9_2l1oxd", "bun_oi644fft",
                                                   "car_ayyrvLan", "Iur_iu82844"),
                                     api_key = api_key),
               "None of the entity identifiers can be matched to an entity type and are therefore all invalid.")
  
  expect_error(get_entity_data_by_id(entity_id = c("Pas_past3112", "bun_oi644fft",
                                                   "CAR_ayyrvLan", "IUUU_iu82844"),
                                     api_key = api_key),
               "None of the entity identifiers can be matched to an entity type and are therefore all invalid.")
})
