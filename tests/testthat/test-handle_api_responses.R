# test-.handle_api_responses.R

api_key = Sys.getenv("BENCHLING_DEV_API_KEY")

# test case 1
test_that("Verify that .handle_api_responses.R generates warnings when noticing
          entity identifiers with errors in their responses", {
  entity_id_responses1 <- suppressWarnings(get_entity_data_by_id(entity_id=c("bfi_ztXInwdh", "bfi_Y7ORWDSz",
                                                                             "seq_GyT5omCm", "tae_T9iU9Gpr",
                                                                             "ent_a0SApq3z"), 
                                                                 api_key=api_key))
  expect_warning(.handle_api_responses(entity_id_responses=entity_id_responses1),
                 paste0("Errors were found in the API response outputs for ",
                        "seq_GyT5omCm, ent_a0SApq3z with the following error message: ", 
                        "Item not found or you may not have permissions to view the item. ", 
                        "Therefore, the entity identifiers along with their respective API ", 
                        "response outputs were removed."))
  
  entity_id_responses2 <- suppressWarnings(get_entity_data_by_id(entity_id=c("bfi_Y7ORWDSz", "plt_NgX9UPRa",
                                                                             "loc_Uy6trE3p", "con_m1dmbdV8"), 
                                                                 api_key=api_key))
  expect_warning(.handle_api_responses(entity_id_responses=entity_id_responses2),
                 paste0("Errors were found in the API response output for ",
                        "loc_Uy6trE3p with the following error message: ", 
                        "Item not found or you may not have permissions to view the item. ", 
                        "Therefore, the entity identifier along with its respective API ", 
                        "response output was removed."))
  
  entity_id_responses3 <- suppressWarnings(get_entity_data_by_id(entity_id=c("ent_Ec76qX9f", "seq_uYt7jmI9",
                                                                             "box_98IujkP0", "bfi_7UyoI80P"), 
                                                                 api_key=api_key))
  expect_warning(.handle_api_responses(entity_id_responses=entity_id_responses3),
                 paste0("Errors were found in the API response outputs for ",
                        "seq_uYt7jmI9, box_98IujkP0, bfi_7UyoI80P with the following error ",
                        "message: Item not found or you may not have permissions to view the ",
                        "item. Therefore, the entity identifiers along with their respective API ", 
                        "response outputs were removed."))
  })

# test case 2
test_that("Verify that .handle_api_responses.R works in returning a named list of 
          Single-Get API endpoint response contents after analyzing the input and 
          removing any invalid entity identifiers", {
  entity_id_responses4 <- suppressWarnings(get_entity_data_by_id(entity_id=c("box_t99a7IQ8", "ent_Ec76qX9f",
                                                                             "seq_Muy6oipm", "gtu_Urw021or",
                                                                             "etr_I90lKas0"), 
                                                                 api_key=api_key))
  expect_equal(length(suppressWarnings(.handle_api_responses(entity_id_responses=entity_id_responses4))),
               2)
  
  entity_id_responses5 <- suppressWarnings(get_entity_data_by_id(entity_id=c("con_m1dmbdV8", "plt_9Iujh72as",
                                                                             "seq_Dug3021a", "bfi_IXHyv1M0",
                                                                             "bfi_bVdXMOom"), 
                                                                 api_key=api_key))
  expect_equal(length(suppressWarnings(.handle_api_responses(entity_id_responses=entity_id_responses5))),
               3)
  
  entity_id_responses6 <- suppressWarnings(get_entity_data_by_id(entity_id=c("bfi_SxrjOAdP", "bfi_I90klPo5",
                                                                             "ent_9Oik901m"), 
                                                                 api_key=api_key))
  expect_equal(length(suppressWarnings(.handle_api_responses(entity_id_responses=entity_id_responses6))),
               1)
})

# test case 3
test_that("Verify that .handle_api_responses.R generates an error when all the
          entity identifiers in the input contain errors in their Single-Get
          API endpoint response contents", {
  entity_id_responses7 <- suppressWarnings(get_entity_data_by_id(entity_id=c("bfi_UjI0123m", "bfi_ouU01319",
                                                                             "seq_U90lO0ja", "sre_Sr0lLjam",
                                                                             "etr_R4o012as"),
                                                                 api_key=api_key))
  expect_error(.handle_api_responses(entity_id_responses=entity_id_responses7),
               cat("Errors were found in all API response outputs.", 
                   paste0("Errors were found in the API response outputs for ",
                          "bfi_UjI0123m, bfi_ouU01319, seq_U90lO0ja, etr_R4o012as ",
                          "with the following error message: Item not found or you ",
                          "may not have permissions to view the item."), sep = "\n"))
  
  entity_id_responses8 <- suppressWarnings(get_entity_data_by_id(entity_id=c("seq_dNu129m2", "etn_9012loIa",
                                                                             "qse_Oi0qwMnl", "bft_912P12lk",
                                                                             "zry_o0LOop4s"),
                                                                 api_key=api_key))
  expect_error(.handle_api_responses(entity_id_responses=entity_id_responses8),
               cat("Errors were found in all API response outputs.", 
                   paste0("Errors were found in the API response output for ",
                          "seq_dNu129m2 with the following error message: ", 
                          "Item not found or you may not have ",
                          "permissions to view the item."), sep = "\n"))
  
  entity_id_responses9 <- suppressWarnings(get_entity_data_by_id(entity_id=c("seq_ZzRt9212", "ent_ia09dPas",
                                                                             "sqe_Ko012mfs", "ati_0123kaLa",
                                                                             "ztr_oasd124s", "mpa_0124fdqa"),
                                                                 api_key=api_key))
  expect_error(.handle_api_responses(entity_id_responses=entity_id_responses9),
               cat("Errors were found in all API response outputs.", 
                   paste0("Errors were found in the API response outputs for ",
                          "seq_ZzRt9212, ent_ia09dPas with the following error ",
                          "message: Item not found or you may not have ",
                          "permissions to view the item."), sep = "\n"))
  })
