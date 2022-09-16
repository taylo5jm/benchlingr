# test-read_entry_table.R 

library(benchlingr)
client <- benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
a_table <- read_entry_table(entry, day = 1, table_index = 2)


test_that("read_entry_table works", {
  expect_equal(nrow(a_table), 6)
})
