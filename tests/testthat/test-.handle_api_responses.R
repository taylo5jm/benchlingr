# test-.handle_api_responses.R

api_key = Sys.getenv("BENCHLING_DEV_API_KEY")

# test case 1
test_that("Verify that .handle_api_responses.R generates warnings when noticing
          entity identifiers with errors in their responses", {
          entity_id1 <- c("bfi_ztXInwdh", "bfi_Y7ORWDSz", "seq_GyT5omCm", 
            "ent_a0SApq3z")
          inferred_id1 <- infer_entity_type(entity_id=entity_id1, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints1 <- .get_api_endpoints(
            entity_id=inferred_id1, tenant="hemoshear-dev")
          api_queries1 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints1)
          entity_id_responses1 <- .submit_queries(api_queries=api_queries1, 
            api_key=api_key)
          expect_warning(.handle_api_responses(entity_id_responses=
                                                 entity_id_responses1),
                         paste0("Errors were found in the API response ",
                                "outputs for seq_GyT5omCm, ent_a0SApq3z ",
                                "with the following error message: Item not ",
                                "found or you may not have permissions to ",
                                "view the item. Therefore, the entity ",
                                "identifiers along with their respective ",
                                "API response outputs were removed."))
          
          entity_id2 <- c("bfi_Y7ORWDSz", "plt_NgX9UPRa", "loc_Uy6trE3p", 
            "con_m1dmbdV8")
          inferred_id2 <- infer_entity_type(entity_id=entity_id2, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints2 <- .get_api_endpoints(
            entity_id=inferred_id2, tenant="hemoshear-dev")
          api_queries2 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints2)
          entity_id_responses2 <- .submit_queries(api_queries=api_queries2, 
            api_key=api_key)
          expect_warning(.handle_api_responses(entity_id_responses=
                                               entity_id_responses2),
                         paste0("Errors were found in the API response ",
                                "output for loc_Uy6trE3p with the following ",
                                "error message: Item not found or you may ",
                                "not have permissions to view the item. ", 
                                "Therefore, the entity identifier along ",
                                "with its respective API response output ",
                                "was removed."))
          
          entity_id3 <- c("ent_Ec76qX9f", "seq_uYt7jmI9", "box_98IujkP0", 
            "bfi_7UyoI80P")
          inferred_id3 <- infer_entity_type(entity_id=entity_id3, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints3 <- .get_api_endpoints(
            entity_id=inferred_id3, tenant="hemoshear-dev")
          api_queries3 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints3)
          entity_id_responses3 <- .submit_queries(api_queries=api_queries3, 
            api_key=api_key)
          expect_warning(.handle_api_responses(entity_id_responses=
                                               entity_id_responses3),
                         paste0("Errors were found in the API response ",
                                "outputs for seq_uYt7jmI9, box_98IujkP0, ",
                                "bfi_7UyoI80P with the following error ",
                                "message: Item not found or you may not ",
                                "have permissions to view the item. ",
                                "Therefore, the entity identifiers along ",
                                "with their respective API response outputs ",
                                "were removed."))
          })

# test case 2
test_that("Verify that .handle_api_responses.R works in returning a named list 
          of Single-Get API endpoint response contents after analyzing the input 
          and removing any invalid entity identifiers", {
          entity_id4 <- c("box_t99a7IQ8", "ent_Ec76qX9f", "seq_Muy6oipm", 
            "etr_I90lKas0")
          inferred_id4 <- infer_entity_type(entity_id=entity_id4, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints4 <- .get_api_endpoints(
            entity_id=inferred_id4, tenant="hemoshear-dev")
          api_queries4 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints4)
          entity_id_responses4<- .submit_queries(api_queries=api_queries4, 
            api_key=api_key)
          expect_equal(length(suppressWarnings(
            .handle_api_responses(entity_id_responses=entity_id_responses4))),
            2)
          
          entity_id5 <- c("con_m1dmbdV8", "plt_9Iujh72as", "seq_Dug3021a", 
            "bfi_IXHyv1M0", "bfi_bVdXMOom")
          inferred_id5 <- infer_entity_type(entity_id=entity_id5, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints5 <- .get_api_endpoints(
            entity_id=inferred_id5, tenant="hemoshear-dev")
          api_queries5 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints5)
          entity_id_responses5 <- .submit_queries(api_queries=api_queries5, 
            api_key=api_key)
          expect_equal(length(suppressWarnings(
            .handle_api_responses(entity_id_responses=entity_id_responses5))),
            3)
          
          entity_id6 <- c("bfi_SxrjOAdP", "bfi_I90klPo5", "ent_9Oik901m")
          inferred_id6 <- infer_entity_type(entity_id=entity_id6, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints6 <- .get_api_endpoints(
            entity_id=inferred_id6, tenant="hemoshear-dev")
          api_queries6 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints6)
          entity_id_responses6 <- .submit_queries(api_queries=api_queries6, 
            api_key=api_key)
          expect_equal(length(suppressWarnings(
            .handle_api_responses(entity_id_responses=entity_id_responses6))),
            1)
          })

# test case 3
test_that("Verify that .handle_api_responses.R generates an error when all the
          entity identifiers in the input contain errors in their Single-Get
          API endpoint response contents", {
          entity_id7 <- c("bfi_jjI0123m", "bfi_ouU01319", "seq_U90lO0ja", 
            "con_u78kkkjm", "etr_R4o012as")
          inferred_id7 <- infer_entity_type(entity_id=entity_id7, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints7 <- .get_api_endpoints(
            entity_id=inferred_id7, tenant="hemoshear-dev")
          api_queries7 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints7)
          entity_id_responses7 <- .submit_queries(api_queries=api_queries7, 
            api_key=api_key)
          expect_error(.handle_api_responses(entity_id_responses=entity_id_responses7),
                       cat("Errors were found in all API response outputs.",
                           paste0("Errors were found in the API response ",
                                  "outputs for bfi_jjI0123m, bfi_ouU01319, ",
                                  "seq_U90lO0ja, con_u78kkkjm, etr_R4o012as ",
                                  "with the following error message: Item not ",
                                  "found or you may not have permissions to ",
                                  "view the item."), sep = "\n"))

          entity_id8 <- c("seq_dNu129m2", "ent_9012loIa", "box_Oi0qwMnl", 
            "bfi_912P12lk")
          inferred_id8 <- infer_entity_type(entity_id=entity_id8, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints8 <- .get_api_endpoints(
            entity_id=inferred_id8, tenant="hemoshear-dev")
          api_queries8 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints8)
          entity_id_responses8 <- .submit_queries(api_queries=api_queries8, 
            api_key=api_key)
          expect_error(.handle_api_responses(entity_id_responses=entity_id_responses8),
                       cat("Errors were found in all API response outputs.",
                           paste0("Errors were found in the API response ",
                                  "output for seq_dNu129m2, ent_9012loIa, ",
                                  "box_Oi0qwMnl, bfi_912P12lk with the ",
                                  "following error message: Item not found or ",
                                  "you may not have permissions to view the ",
                                  "item."), sep = "\n"))
          
          entity_id9 <- c("seq_ZzRt9212", "ent_ia09dPas", "seq_Ko012mfs", 
            "box_0123kaLa", "box_oasd124s", "bfi_0124fdqa")
          inferred_id9 <- infer_entity_type(entity_id=entity_id9, 
            tenant="hemoshear-dev")
          entity_single_get_endpoints9 <- .get_api_endpoints(
            entity_id=inferred_id9, tenant="hemoshear-dev")
          api_queries9 <- .craft_api_queries(entity_single_get_endpoints=
            entity_single_get_endpoints9)
          entity_id_responses9 <- .submit_queries(api_queries=api_queries9, 
            api_key=api_key)
          expect_error(.handle_api_responses(entity_id_responses=entity_id_responses9),
                       cat("Errors were found in all API response outputs.",
                           paste0("Errors were found in the API response ",
                                  "outputs for seq_ZzRt9212, ent_ia09dPas, ",
                                  "seq_Ko012mfs, box_0123kaLa, box_oasd124s, ",
                                  "bfi_0124fdqa with the following error ",
                                  "message: Item not found or you may not ",
                                  "have permissions to view the item."), 
                           sep = "\n"))
          })
