# test-read_entry_table.R 

library(magrittr)

client <- benchlingr::benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                                         api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_IWLMFYhR")

test_that("", {
  
  expect_identical(read_entry_table(entry,"","","Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))
  expect_identical(read_entry_table(entry,NA,NA,"Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))
  expect_identical(read_entry_table(entry,NULL,NULL,"Table1",TRUE), data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""), table_name = c("Table1","Table1","Table1")))

  expect_identical(read_entry_table(entry,"","","Table2",FALSE), data.frame(V1 = c("B","",""), V2 = c("","",""), V3 = c("","",""), V4 = c("","","")))
  expect_identical(read_entry_table(entry,NA,NA,"Table2",FALSE), data.frame(V1 = c("B","",""), V2 = c("","",""), V3 = c("","",""), V4 = c("","","")))
  expect_identical(read_entry_table(entry,NULL,NULL,"Table2",FALSE), data.frame(V1 = c("B","",""), V2 = c("","",""), V3 = c("","",""), V4 = c("","","")))

})

entry_table_name_list <- list()
counter <- 0
for (i in 1:length(entry$days)) {
  for (j in 1:length(entry$days[[i]]$notes)) {
    if (entry$days[[i]]$notes[[j]]$type$value == "table") {
      counter <- counter + 1
      entry_table_name_list[[counter]] <- entry$days[[i]]$notes[[j]]$table$name
    } 
    else {
      next
    }
  }
}

test_that("", {
  
  expect_warning(read_entry_table(entry, 1, 2,"Table2", FALSE),"'day' and 'table_position' arguments will be ignored in favor of 'table_name.'")
  expect_warning(read_entry_table(entry, 1, NULL,"Table2", FALSE),"'day' argument will be ignored in favor of 'table_name.' Also 'table_position' should either be written as an integer as it designates the location of the unstructured table in the notebook entry or kept blank along with 'day.'")
  expect_warning(read_entry_table(entry, NULL, 2,"Table2", FALSE),"'table_position' argument will be ignored in favor of 'table_name.' Also 'day' should either be written as an integer as it designates the day of the unstructured table in the notebook entry or kept blank along with 'day.'")
  
  expect_warning(read_entry_table(entry, 1, "Invalid","Table2", FALSE),"'day' argument will be ignored in favor of 'table_name.' Also 'table_position' should either be written as an integer as it designates the location of the unstructured table in the notebook entry or kept blank along with 'day.'")
  expect_warning(read_entry_table(entry, "Invalid", 2,"Table2", FALSE),"'table_position' argument will be ignored in favor of 'table_name.' Also 'day' should either be written as an integer as it designates the day of the unstructured table in the notebook entry or kept blank along with 'day.'")
  
  expect_warning(read_entry_table(entry, 1, NULL,"Table", FALSE),paste0(paste(c("Name for table could not be found. If possible try to find actual table name from the following list of tables found in the entry:", entry_table_name_list), collapse=" "),"."))
  
})

# test_that("", {
#   
#   expect_warning(read_entry_table(entry, NA, NA, NA, FALSE),)
#   expect_warning(read_entry_table(entry, NULL, NULL, NULL, FALSE),)
#   expect_warning(read_entry_table(entry, "", "", "", FALSE),)

#   expect_warning(read_entry_table(NA, 1, "Invalid", NA, FALSE),)
#   expect_warning(read_entry_table(NULL, "Invalid", NULL, NULL, FALSE),)
#   expect_warning(read_entry_table("", 3, "", "Table2", FALSE),)

# })




