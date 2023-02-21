# test-read_entry_table.R 

library(benchlingr)
library(magrittr)
client <- benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")

test_that("read_entry_table works", {
  expect_equal(nrow(read_entry_table(entry, day = 1, table_position = 2, return_table_name = TRUE)), 6)
  expect_equal(ncol(read_entry_table(entry, day = 1, table_position = 2, return_table_name = TRUE)), 3)
  expect_equal(ncol(read_entry_table(entry, day = 1, table_position = 2, return_table_name = FALSE)), 2)
  expect_equal(read_entry_table(entry, day = 1, table_position = 2, return_table_name = TRUE),
                   data.frame(sample_id = c("1", "2", "3", "4", "5", "6"), 
                              treatment = c("Drug A", "Drug A", "Drug A", "Control", "Control","Control"),
                              return_table_name = c("Experimental Conditions", "Experimental Conditions", 
                                             "Experimental Conditions", "Experimental Conditions", 
                                             "Experimental Conditions", "Experimental Conditions")))
  expect_equal(read_entry_table(entry, day = 1, table_position = 2, return_table_name = FALSE),
                   data.frame(sample_id = c("1", "2", "3", "4", "5", "6"), 
                              treatment = c("Drug A", "Drug A", "Drug A", "Control", "Control","Control")))
})