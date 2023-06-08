# test-.submit_queries.R

api_key = Sys.getenv("BENCHLING_DEV_API_KEY")
tenant = "hemoshear-dev"

# test case 1
test_that("Verify that .submit_queries.R works in calling each element's 
          Single-Get API endpoint in Benchling and extracting the response 
          contents", {
  entity_id1 <- c("seq_12kl23sa", "bfi_Q1PMlXkf", "box_7YutniM0")
  inferred_id1 <- infer_entity_type(entity_id=entity_id1, tenant=tenant)
  api_endpoints1 <- .get_api_endpoints(entity_id=inferred_id1, 
                                       tenant=tenant)
  api_queries1 <- .craft_api_queries(entity_single_get_endpoints=
                                       api_endpoints1)
  entity_responses1 <- .submit_queries(api_queries=api_queries1, 
                                       api_key=api_key)
  expect_equal(class(entity_responses1), "list")
  expect_equal(names(entity_responses1), c("seq_12kl23sa", "bfi_Q1PMlXkf", 
                                           "box_7YutniM0"))
  
  entity_id2 <- c("con_1mus3Ree", "bfi_VVamxrKQ", "seq_Cuf0bmCm",
                  "plt_NgX9UPRa")
  inferred_id2 <- infer_entity_type(entity_id=entity_id2, tenant=tenant)
  api_endpoints2 <- .get_api_endpoints(entity_id=inferred_id2, 
                                       tenant=tenant)
  api_queries2 <- .craft_api_queries(entity_single_get_endpoints=
                                       api_endpoints2)        
  entity_responses2 <- .submit_queries(api_queries=api_queries2, 
                                       api_key=api_key)
  expect_equal(class(entity_responses2), "list")
  expect_equal(names(entity_responses2), c("con_1mus3Ree", "bfi_VVamxrKQ", 
                                           "seq_Cuf0bmCm", "plt_NgX9UPRa"))
  
  entity_id3 <- c("bat_UOIr8IjL", "con_ZBL9QQWD", "loc_QT5BJ1PZ",
                  "ent_Ec76qX9f", "etr_192kl322")
  inferred_id3 <- infer_entity_type(entity_id=entity_id3, tenant=tenant)
  api_endpoints3 <- .get_api_endpoints(entity_id=inferred_id3, 
                                      tenant=tenant)
  api_queries3 <- .craft_api_queries(entity_single_get_endpoints=
                                       api_endpoints3) 
  entity_responses3 <- .submit_queries(api_queries=api_queries3, 
                                       api_key=api_key)
  expect_equal(class(entity_responses3), "list")
  expect_equal(names(entity_responses3), c("bat_UOIr8IjL", "con_ZBL9QQWD", 
                                           "loc_QT5BJ1PZ", "ent_Ec76qX9f", 
                                           "etr_192kl322"))
})
