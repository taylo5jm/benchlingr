# test-get_entity_by_id.R

api_key = Sys.getenv("BENCHLING_DEV_API_KEY")
tenant = "hemoshear-dev"

# test case 1
test_that("Verify that get_entity_by_id.R generates warnings when 
          encountering invalid entity identifiers", {
  expect_warning(get_entity_by_id(entity_id=c("bfi_ztXInwdh", "box_t99a7IQ8",
                                              "byi_97Yp9bb1", "tpi_TyH556Ger"), 
                                  api_key = api_key, tenant=tenant,
                                  bundle=TRUE), 
                 paste0("The following entity identifiers cannot be matched ",
                        "to an entity type and have therefore been removed: ",
                        "byi_97Yp9bb1, tpi_TyH556Ger."))
            
  expect_warning(get_entity_by_id(entity_id=c("ent_a0SApq3z", "bfi_Y7ORWDSz",
                                              "seq_Cuf0bmCm", "rev_W387Jkh1"), 
                                  api_key = api_key, tenant=tenant,
                                  bundle=FALSE), 
                 paste0("The following entity identifier cannot be matched to ",
                        "an entity type and has therefore been removed: ",
                        "rev_W387Jkh1."))
  
  expect_warning(get_entity_by_id(entity_id=c("ent_a0SApq3z", "bfi_Y7ORWDSz",
                                              "seq_Cuf0bmCm", "rev_W387Jkh1"), 
                                  api_key = api_key, tenant=tenant,
                                  bundle=FALSE), 
                 paste0("Errors were found in the API response output for ",
                        "ent_a0SApq3z with the following error message: Item ",
                        "not found or you may not have permissions to view ",
                        "the item. Therefore, the entity identifier along ",
                        "with its respective API response output was removed."))
  
  expect_warning(get_entity_by_id(entity_id=c("con_m1dmbdV8", "mis_88BuyojM",
                                              "seq_Gyum90j7", "bfi_ru401kLi",
                                              "jhi_poT429cD"), 
                                  api_key = api_key, tenant=tenant,
                                  bundle=TRUE), 
                 paste0("The following entity identifiers cannot be matched ",
                        "to an entity type and have therefore been removed: ",
                        "mis_88BuyojM, jhi_poT429cD."))
  
  expect_warning(get_entity_by_id(entity_id=c("con_m1dmbdV8", "mis_88BuyojM",
                                              "seq_Gyum90j7", "bfi_ru401kLi",
                                              "jhi_poT429cD"),
                                  api_key = api_key, tenant=tenant,
                                  bundle=TRUE),
                 paste0("Errors were found in the API response outputs for ",
                        "seq_Gyum90j7, bfi_ru401kLi with the following error ",
                        "message: Item not found or you may not have ",
                        "permissions to view the item. Therefore, the entity ",
                        "identifiers along with their respective API response ",
                        "outputs were removed."))
  
  expect_warning(get_entity_by_id(entity_id=c("cse_12kasd01", "bfi_ztXInwdh",
                                              "bfi_Y7ORWDSz", "ent_Ec76qX9f",
                                              "seq_91lkjuup","box_12459222"),
                                  api_key = api_key, tenant=tenant,
                                  bundle=FALSE),
                 paste0("The following entity identifier cannot be matched ",
                        "to an entity type and has therefore been removed: ",
                        "cse_12kasd01."))

  expect_warning(get_entity_by_id(entity_id=c("cse_12kasd01", "bfi_ztXInwdh",
                                              "bfi_Y7ORWDSz", "ent_Ec76qX9f",
                                              "seq_91lkjuup","box_12459222"),
                                  api_key = api_key, tenant=tenant,
                                  bundle=FALSE),
                 paste0("Errors were found in the API response outputs for ",
                        "seq_91lkjuup, box_12459222 with the following error ",
                        "message: Item not found or you may not have ",
                        "permissions to view the item. Therefore, the entity ",
                        "identifiers along with their respective API response ",
                        "outputs were removed."))
})

# test case 2
test_that("Verify that get_entity_by_id.R works in creating a proper named list
          of Single-Get API endpoint response contents for entity identifiers
          matched to a known entity type", {
  entity_response_results1 <- suppressWarnings(get_entity_by_id(entity_id=c(
    "box_t99a7IQ8", "bfi_80007lkI", "spi_R498kjLo", "wqe_tY50laks",
    "bfi_Y7ORWDSz"), api_key=api_key, tenant=tenant, bundle=TRUE))
  expect_equal(class(entity_response_results1), "list")
  expect_equal(class(entity_response_results1[[1]][[1]]), 
               c("tbl_df","tbl","data.frame"))
  expect_equal(class(entity_response_results1[[2]][[1]]), 
               c("tbl_df","tbl","data.frame"))
  
  entity_response_results2 <- suppressWarnings(get_entity_by_id(entity_id=c(
    "seq_9oUjkiG6", "etr_lu8wTLfL", "cvr_iO8u2q09", "bfi_IXHyv1M0"), 
    api_key=api_key, tenant=tenant, bundle=FALSE))
  expect_equal(class(entity_response_results2), "list")
  expect_equal(names(entity_response_results2), c("etr_lu8wTLfL", 
                                                  "bfi_IXHyv1M0"))
  
  entity_response_results3 <- suppressWarnings(get_entity_by_id(entity_id=c(
    "seq_Cuf0bmCm", "eos_i9182331", "bfi_bVdXMOom", "bfi_SxrjOAdP"), 
    api_key=api_key, tenant=tenant, bundle=TRUE))
  expect_equal(class(entity_response_results3), "list")
  expect_equal(class(entity_response_results3[[1]][[1]]),
               c("tbl_df","tbl","data.frame"))
  expect_equal(class(entity_response_results3[[1]][[2]]),
               c("tbl_df","tbl","data.frame"))
  expect_equal(class(entity_response_results3[[2]][[1]]),
               c("tbl_df","tbl","data.frame"))
  
  entity_response_results4 <- suppressWarnings(get_entity_by_id(entity_id=c(
    "bat_UOIr8IjL", "bfi_cgIIb43v", "jut_12sad131wd", "bfi_jdiasE45"),
    api_key=api_key, tenant=tenant, bundle=FALSE))
  expect_equal(class(entity_response_results4), "list")
  expect_equal(names(entity_response_results4), c("bfi_cgIIb43v"))
})

# test case 3
test_that("Verify that get_entity_by_id.R generates errors when provided an input
          of invalid entity identifiers", {
  expect_error(get_entity_by_id(entity_id = c("bif_98IupkJ", "cta_Oi9s1055",
                                              "mni_as2d5567", "uht_pYlas844"),
                                api_key = api_key, tenant=tenant, bundle=TRUE),
               paste0("None of the entity identifiers can be matched to an ",
                      "entity type and are therefore all invalid."))

  expect_error(get_entity_by_id(entity_id = c("dx9_2l1oxd", "bun_oi644fft",
                                              "car_ayyrvLan", "IuR_iu82844"),
                                api_key = api_key, tenant=tenant, bundle=FALSE),
               paste0("None of the entity identifiers can be matched to an ",
                      "entity type and are therefore all invalid."))
})

