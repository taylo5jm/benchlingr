# test-read_entry_table.R 

library(benchlingr)
client <- benchlingr:::benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
a_table <- read_entry_table(entry, day = 1, table_position = 2)


test_that("", {
  expect_identical(read_entry_table(entry,"","","Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))
  expect_identical(read_entry_table(entry,NA,NA,"Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))
  expect_identical(read_entry_table(entry,NULL,NULL,"Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))
  expect_identical(read_entry_table(entry,"","","Table1",FALSE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","","")))
  expect_identical(read_entry_table(entry,NA,NA,"Table1",FALSE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","","")))
  expect_identical(read_entry_table(entry,NULL,NULL,"Table1",FALSE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","","")))
})

test_that("", {
  expect_identical(read_entry_table(entry,"","","Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))
  expect_identical(read_entry_table(entry,NA,NA,"Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))
  expect_identical(read_entry_table(entry,NULL,NULL,"Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))
  expect_identical(read_entry_table(entry,"","","Table1",FALSE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","","")))
  expect_identical(read_entry_table(entry,NA,NA,"Table1",FALSE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","","")))
  expect_identical(read_entry_table(entry,NULL,NULL,"Table1",FALSE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","","")))
})




