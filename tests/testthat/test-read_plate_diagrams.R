library(benchlingr)
client <- connect_sdk(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
entry <- client$entries$get_entry_by_id("etr_f1bpDIes")
plate_diagrams <- get_plate_diagrams(entry)


test_that("get_plate_diagrams works", {
  testthat::expect_type(plate_diagrams, "list")
})

test_that("get_plate_diagrams returns correct number of 
          data frames when plate_dim is NULL.", {
  testthat::expect_equal(length(plate_diagrams), 2)
})

test_that("get_plate_diagrams returns table names as names of the list", {
            testthat::expect_setequal(c("Well1", "Well2"), names(plate_diagrams))
          })

test_that("get_plate_diagrams returns correct number of 
          data frames when plate_dim is c(16, 24)", {
            plate_diagrams <- get_plate_diagrams(entry, plate_dim = c(16, 24))
            testthat::expect_equal(length(plate_diagrams), 1)
            testthat::expect_equal(nrow(plate_diagrams[[1]]), 384)
})


test_that("get_plate_diagrams returns correct number of 
          data frames when plate_dim is c(8, 12)", {
            plate_diagrams <- get_plate_diagrams(entry, plate_dim = c(8, 12))
            testthat::expect_equal(length(plate_diagrams), 1)
            testthat::expect_equal(nrow(plate_diagrams[[1]]), 96)
            
})

test_that("get_plate_diagrams raises exception when invalid plate size is 
          passed to plate_dim", {
            
            testthat::expect_error(get_plate_diagrams(entry, plate_dim = c(10, 10)),
                                   regexp = "Number of")
})
