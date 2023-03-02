# test-warehouse_disconnect.R

# test case 1 - verifying that a disconnect was successful
conn1 <- warehouse_connect(tenant = "hemoshear-dev", 
                           username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                           password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
test_that("Verify that a disconnect was successful when the user 
          provides a valid PqConnection-class object.", {
            expect_true(warehouse_disconnect(conn1))
})

# test case 2 - checking if the function errors out when provided with an input
# that is a database connection object but it is not PqConnection-class
conn4 <- DBI::dbConnect(RSQLite::SQLite(), "memory")
test_that("Check if the function errors out with an informative message
          when the user provides an input that is a database connection 
          object but it is not PqConnection-class.", {
            expect_error(warehouse_disconnect(conn4),
                         "Input is not a PqConnection class object.")
          })
DBI::dbDisconnect(conn4)

# test case 3 - checking if the function errors with an informative message 
# when the user provides an input that is a database connection object and 
# PqConnection-class but it is no longer valid or has already been 
# disconnected
conn5 <- warehouse_connect(tenant = "hemoshear-dev",
                           username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                           password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
DBI::dbDisconnect(conn5)
test_that("Check if the function errors out with an informative message
          when the user provides a database connection input that is 
          PqConnection-class but it is no longer valid or has already been
          disconnected.", {
            expect_warning(warehouse_disconnect(conn5),
                         "The input is no longer valid or has already been disconnected.")
          })
