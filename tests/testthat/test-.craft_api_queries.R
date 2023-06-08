# test-.craft_api_queries.R

tenant = "hemoshear-dev"

# test case 1
test_that("Verify that .craft_api_queries.R generates properly formatted 
          Single-Get API endpoint request URLS for each element", {
  entity_id1 <- c("seq_12kl23sa", "bfi_Q1PMlXkf", "box_7YutniM0")
  inferred_id1 <- infer_entity_type(entity_id=entity_id1, tenant=tenant)
  api_endpoints1 <- .get_api_endpoints(entity_id=inferred_id1, 
                                       tenant=tenant)
  api_queries1 <- .craft_api_queries(entity_single_get_endpoints=
                                     api_endpoints1)
  expect_equal(api_queries1, c("seq_12kl23sa"= 
                               paste0("https://", as.character(tenant),
                                      ".benchling.com/api/v2/dna-sequences/",
                                      "seq_12kl23sa"),
                               "bfi_Q1PMlXkf"=
                               paste0("https://", as.character(tenant),
                                      ".benchling.com/api/v2/",
                                      "custom-entities/bfi_Q1PMlXkf"), 
                               "box_7YutniM0"=
                               paste0("https://", as.character(tenant),
                                      ".benchling.com/api/v2/boxes/",
                                      "box_7YutniM0")))
            
  entity_id2 <- c("con_1mus3Ree", "bfi_VVamxrKQ", "seq_Cuf0bmCm",
                  "plt_NgX9UPRa")
  inferred_id2 <- infer_entity_type(entity_id=entity_id2, tenant=tenant)
  api_endpoints2 <- .get_api_endpoints(entity_id=inferred_id2, 
                                       tenant=tenant)
  api_queries2 <- .craft_api_queries(entity_single_get_endpoints=
                                     api_endpoints2)        
  expect_equal(api_queries2, c("con_1mus3Ree"= 
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/containers/",
                                        "con_1mus3Ree"),
                                 "bfi_VVamxrKQ"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/",
                                        "custom-entities/bfi_VVamxrKQ"), 
                                 "seq_Cuf0bmCm"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/dna-sequences/",
                                        "seq_Cuf0bmCm"),
                                 "plt_NgX9UPRa"=
                                 paste0("https://", as.character(tenant),
                                        ".benchling.com/api/v2/plates/",
                                        "plt_NgX9UPRa")))
            
  entity_id3 <- c("bat_UOIr8IjL", "con_ZBL9QQWD", "loc_QT5BJ1PZ",
                  "ent_Ec76qX9f", "etr_192kl322")
  inferred_id3 <- infer_entity_type(entity_id=entity_id3, tenant=tenant)
  api_endpoints3 <- .get_api_endpoints(entity_id=inferred_id3, 
                                       tenant=tenant)
  api_queries3 <- .craft_api_queries(entity_single_get_endpoints=
                                     api_endpoints3) 
  expect_equal(api_queries3, c("bat_UOIr8IjL"= 
                               paste0("https://", as.character(tenant),
                                      ".benchling.com/api/v2/batches/",
                                      "bat_UOIr8IjL"),
                               "con_ZBL9QQWD"=
                               paste0("https://", as.character(tenant),
                                      ".benchling.com/api/v2/containers/",
                                      "con_ZBL9QQWD"), 
                               "loc_QT5BJ1PZ"=
                               paste0("https://", as.character(tenant),
                                      ".benchling.com/api/v2/locations/",
                                      "loc_QT5BJ1PZ"),
                               "ent_Ec76qX9f"=
                               paste0("https://", as.character(tenant),
                                      ".benchling.com/api/v2/users/",
                                      "ent_Ec76qX9f"),
                               "etr_192kl322"=
                               paste0("https://", as.character(tenant),
                                      ".benchling.com/api/v2/entries/",
                                      "etr_192kl322")))
})
