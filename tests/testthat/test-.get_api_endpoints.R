# test-.get_api_endpoints.R

tenant = "hemoshear-dev"

# test case 1
test_that("Verify that .get_api_endpoints.R extracts the URL formats for each
          element's Single-Get API endpoint", {
  entity_id1 <- c("seq_12kl23sa", "bfi_Q1PMlXkf", "box_7YutniM0")
  inferred_id1 <- infer_entity_type(entity_id=entity_id1, tenant=tenant)
  api_endpoints1 <- .get_api_endpoints(entity_id=inferred_id1, 
                                       tenant=tenant)
  expect_equal(api_endpoints1, c("seq_12kl23sa"= 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/dna-sequences/",
                                        "ENTITY_ID"),
                                 "bfi_Q1PMlXkf"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/",
                                        "custom-entities/ENTITY_ID"), 
                                 "box_7YutniM0"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/boxes/",
                                        "ENTITY_ID")))
  
  entity_id2 <- c("con_1mus3Ree", "bfi_VVamxrKQ", "seq_Cuf0bmCm",
                  "plt_NgX9UPRa")
  inferred_id2 <- infer_entity_type(entity_id=entity_id2, tenant=tenant)
  api_endpoints2 <- .get_api_endpoints(entity_id=inferred_id2, 
                                       tenant=tenant)
  expect_equal(api_endpoints2, c("con_1mus3Ree"= 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/containers/",
                                        "ENTITY_ID"),
                                 "bfi_VVamxrKQ"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/",
                                        "custom-entities/ENTITY_ID"), 
                                 "seq_Cuf0bmCm"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/dna-sequences/",
                                        "ENTITY_ID"),
                                 "plt_NgX9UPRa"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/plates/",
                                        "ENTITY_ID")))
  
  entity_id3 <- c("bat_UOIr8IjL", "con_ZBL9QQWD", "loc_QT5BJ1PZ",
                  "ent_Ec76qX9f", "etr_192kl322")
                  
  inferred_id3 <- infer_entity_type(entity_id=entity_id3, tenant=tenant)
  api_endpoints3 <- .get_api_endpoints(entity_id=inferred_id3, 
                                       tenant=tenant)
  expect_equal(api_endpoints3, c("bat_UOIr8IjL"= 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/batches/",
                                        "ENTITY_ID"),
                                 "con_ZBL9QQWD"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/containers/",
                                        "ENTITY_ID"), 
                                 "loc_QT5BJ1PZ"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/locations/",
                                        "ENTITY_ID"),
                                 "ent_Ec76qX9f"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/users/",
                                        "ENTITY_ID"),
                                 "etr_192kl322"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/entries/",
                                        "ENTITY_ID")))
  })


