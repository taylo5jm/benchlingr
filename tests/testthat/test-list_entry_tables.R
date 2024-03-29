# test-list_entry_tables.R
client <- connect_sdk(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
tables <- benchlingr:::list_entry_tables(entry)

test_that("list_entry_tables works", {
  expect_equal(length(tables), 2)
  expect_equal(tables[[1]][1], c("Experimental Conditions" = 2))
  expect_equal(length(tables[[2]]), 0) # second day should be empty. 
})

# empty notebook entry
entry <- client$entries$get_entry_by_id("etr_CaN3D2BF")

test_that("check if list_entry_tables gives a warning when no unstructured tables
          are found in the notebook entry",{
            expect_warning(list_entry_tables(entry),"No tables were found") # function should give a warning.
})
