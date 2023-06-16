# test-bundle_api_results.R

api_key = Sys.getenv("BENCHLING_DEV_API_KEY")
tenant = "hemoshear-dev"

# test case 1
test_that("Verify that .bundle_api_results.R generates a named list of 
          tibble-formatted data tables organized by schema name", {
          entity_id1 <- c("bat_UOIr8IjL", "con_ZBL9QQWD", "seq_ObbdtGhC",
                          "bfi_9fKcrORv")
          inferred_id1 <- infer_entity_type(entity_id=entity_id1, tenant=tenant)
          entity_single_get_endpoints1 <- .get_api_endpoints(
            entity_id=inferred_id1, tenant=tenant)
          api_queries1 <- .craft_api_queries(entity_single_get_endpoints=
          entity_single_get_endpoints1)
          entity_responses1 <- .submit_queries(api_queries=api_queries1,
          api_key=api_key)
          bundled_api_results1 <- .bundle_api_results(entity_responses=
            entity_responses1, tenant=tenant)
          expect_equal(class(bundled_api_results1), "list")
          expect_equal(length(bundled_api_results1), 4)
          expect_equal(class(bundled_api_results1[[1]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results1[[2]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results1[[3]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results1[[4]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          
          
          entity_id2 <- c("bfi_KsLU5uWV", "bfi_Io0d22lf", "seq_12345667", 
                          "ent_Ec76qX9f")
          inferred_id2 <- infer_entity_type(entity_id=entity_id2, tenant=tenant)
          entity_single_get_endpoints2 <- .get_api_endpoints(
            entity_id=inferred_id2, tenant=tenant)
          api_queries2 <- .craft_api_queries(entity_single_get_endpoints=
                                             entity_single_get_endpoints2)
          entity_responses2 <- .submit_queries(api_queries=api_queries2,
                                               api_key=api_key)
          bundled_api_results2 <- .bundle_api_results(entity_responses=
            entity_responses2, tenant=tenant)
          expect_equal(class(bundled_api_results2), "list")
          expect_equal(length(bundled_api_results2), 3)
          expect_equal(class(bundled_api_results2[[1]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results2[[1]][[2]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results2[[2]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results2[[3]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          
          
          entity_id3 <- c("bfi_VVamxrKQ", "seq_Cuf0bmCm", "plt_NgX9UPRa", 
                          "con_m02222V8", "bfi_23552344")
          inferred_id3 <- infer_entity_type(entity_id=entity_id3, tenant=tenant)
          entity_single_get_endpoints3 <- .get_api_endpoints(
            entity_id=inferred_id3, tenant=tenant)
          api_queries3 <- .craft_api_queries(entity_single_get_endpoints=
                                             entity_single_get_endpoints3)
          entity_responses3 <- .submit_queries(api_queries=api_queries3,
                                               api_key=api_key)
          bundled_api_results3 <- .bundle_api_results(entity_responses=
            entity_responses3, tenant=tenant)
          expect_equal(class(bundled_api_results3), "list")
          expect_equal(length(bundled_api_results3), 4)
          expect_equal(class(bundled_api_results3[[1]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results3[[2]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results3[[2]][[2]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results3[[3]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
          expect_equal(class(bundled_api_results3[[4]][[1]]), 
                       c("tbl_df","tbl","data.frame"))
})
