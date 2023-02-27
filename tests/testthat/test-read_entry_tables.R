# test-read_entry_tables.R

client1 <- benchlingr::benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                                          api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry1 <- client1$entries$get_entry_by_id("etr_T3WZTyAe")

table1A <- data.frame(sample_id = c("1","2","3","4","5","6"),
                      treatment = c("Drug A","Drug A","Drug A","Control","Control","Control"),
                      return_table_name = c("Experimental Conditions","Experimental Conditions","Experimental Conditions",
                                            "Experimental Conditions","Experimental Conditions","Experimental Conditions"))
table1B <- data.frame(sample_id = c("1","2","3","4","5","6"),
                      treatment = c("Drug A","Drug A","Drug A","Control","Control","Control"))
table1C <- list(table1B)
table1D <- table1C
names(table1D) <- "Experimental Conditions"

client2 <- benchlingr::benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                                          api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))
entry2 <- client2$entries$get_entry_by_id("etr_IWLMFYhR")
table2A <- data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""),
                      return_table_name = c("Table1","Table1","Table1"))
table2B <- data.frame(V1 = c("A","",""), V2 = c("","",""), V3 = c("","",""))
table2C <- data.frame(V1 = c("B","",""), V2 = c("","",""), V3 = c("","",""), V4 = c("","",""),
                      return_table_name = c("Table2","Table2","Table2"))
table2D <- data.frame(V1 = c("B","",""), V2 = c("","",""), V3 = c("","",""), V4 = c("","",""))
table2E <- list(table2B,table2D)
table2F <- table2E
names(table2F) <- c("Table1","Table2")

# test case 1
test_that("check that read_entry_tables works when setting 'day' and 'table_position'
          as NULL and defining 'table_name'", {
  expect_equal(read_entry_tables(entry=entry1,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name="Experimental Conditions",
                                 return_table_name=TRUE,
                                 verbose=FALSE), table1A)
  expect_equal(read_entry_tables(entry=entry1,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name="Experimental Conditions",
                                 return_table_name=FALSE,
                                 verbose=FALSE), table1B)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name="Table1",
                                 return_table_name=TRUE,
                                 verbose=FALSE), table2A)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name="Table1",
                                 return_table_name=FALSE,
                                 verbose=FALSE), table2B)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name="Table2",
                                 return_table_name=TRUE,
                                 verbose=FALSE), table2C)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name="Table2",
                                 return_table_name=FALSE,
                                 verbose=FALSE), table2D)
})

# test case 2
test_that("check that read_entry_tables works when setting 'table_name'
          as NULL and defining 'day' and 'table_position'", {
  expect_equal(read_entry_tables(entry=entry1,
                                 day=1,
                                 table_position=2,
                                 table_name=NULL,
                                 return_table_name=TRUE,
                                 verbose=FALSE), table1A)
  expect_equal(read_entry_tables(entry=entry1,
                                 day=1,
                                 table_position=2,
                                 table_name=NULL,
                                 return_table_name=FALSE,
                                 verbose=FALSE), table1B)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=1,
                                 table_position=2,
                                 table_name=NULL,
                                 return_table_name=TRUE,
                                 verbose=FALSE), table2A)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=1,
                                 table_position=2,
                                 table_name=NULL,
                                 return_table_name=FALSE,
                                 verbose=FALSE), table2B)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=1,
                                 table_position=4,
                                 table_name=NULL,
                                 return_table_name=TRUE,
                                 verbose=FALSE), table2C)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=1,
                                 table_position=4,
                                 table_name=NULL,
                                 return_table_name=FALSE,
                                 verbose=FALSE), table2D)
          })

# test case 3
test_that("check that read_entry_tables works when setting 'table_name','day' and 'table_position' as NULL", {
  expect_equal(read_entry_tables(entry=entry1,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name=NULL,
                                 return_table_name=TRUE,
                                 verbose=FALSE), table1D)
  expect_equal(read_entry_tables(entry=entry1,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name=NULL,
                                 return_table_name=FALSE,
                                 verbose=FALSE), table1C)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name=NULL,
                                 return_table_name=TRUE,
                                 verbose=FALSE), table2F)
  expect_equal(read_entry_tables(entry=entry2,
                                 day=NULL,
                                 table_position=NULL,
                                 table_name=NULL,
                                 return_table_name=FALSE,
                                 verbose=FALSE), table2E)
  })

# test case 4
test_that("check that read_entry_tables makes sure to send an error whenever it senses that the input for 'entry' is missing or invalid", {
#   expect_error(read_entry_tables(entry=,
#                                  day=1,
#                                  table_position=2,
#                                  table_name="Experimental Conditions",
#                                  return_table_name=TRUE,
#                                  verbose=FALSE), "'entry' input is missing. See ?benchlingr::get_entry.")
#   expect_error(read_entry_tables(entry="entry1",
#                                  day=NULL,
#                                  table_position=NULL,
#                                  table_name="Experimental Conditions",
#                                  return_table_name=TRUE,
#                                  verbose=FALSE), "'entry' input is invalid. See ?benchlingr::get_entry.")
#   expect_error(read_entry_tables(entry="entry1",
#                                  day=1,
#                                  table_position=2,
#                                  table_name=NULL,
#                                  return_table_name=TRUE,
#                                  verbose=FALSE), "'entry' input is invalid. See ?benchlingr::get_entry.")
#   expect_error(read_entry_tables(entry="entry2",
#                                  day=NULL,
#                                  table_position=NULL,
#                                  table_name="Table1",
#                                  return_table_name=TRUE,
#                                  verbose=FALSE), "'entry' input is invalid. See ?benchlingr::get_entry.")
  expect_error(read_entry_tables(entry="entry2",
                                 day=1,
                                 table_position=2,
                                 table_name=NULL,
                                 return_table_name=TRUE,
                                 verbose=FALSE), "'entry' input is invalid. See ?benchlingr::get_entry.")
  })

# # test case 5
# test_that("check that read_entry_tables sends warnings should inputs be provided for 'day' and 'table_position' when 'table_name'
#           is already defined", {
#   expect_warning(read_entry_tables(entry=entry1,
#                                    day=1,
#                                    table_position=2,
#                                    table_name="Experimental Conditions",
#                                    return_table_name=TRUE,
#                                    verbose=FALSE), "'day' and 'table_position' will be ignored in favor of 'table_name'.")
#   })
# 
# # test case 6
# test_that("check that read_entry_tables still outputs a table even when inputs
#           cause a warning to be produced", {
#   expect_equal((read_entry_tables(entry=entry2,
#                                   day=1,
#                                   table_position=NULL,
#                                   table_name="Table2",
#                                   return_table_name=TRUE,
#                                   verbose=FALSE)), table2C)
#   expect_equal((read_entry_tables(entry=entry2,
#                                    day=1,
#                                    table_position=NULL,
#                                    table_name="Table2",
#                                    return_table_name=FALSE,
#                                    verbose=FALSE)), table2D)
#   })
# 
