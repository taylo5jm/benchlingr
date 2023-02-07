# test-warehouse_disconnect.R

conn1 <- benchlingr::warehouse_connect(tenant = "hemoshear-dev", 
                                       username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                                       password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))

test_that("Verify that a disconnect was successful when the user 
          provides a valid PqConnection-type object.", {
            expect_true(warehouse_disconnect(conn1))
})

# conn2 <- benchlingr::warehouse_connect(tenant = "...",
#                                        username = "...",
#                                        password = "...")
# 
# test_that("Check if the function errors out with an informative message 
#           when the user provides an input that is not a PqConnection-type 
#           object.", {
#             expect_error(warehouse_disconnect(conn2),
#                          "Input was not a PqConnection class object.")
#           })
 
conn3 <- benchlingr::warehouse_connect(tenant = "hemoshear-dev",
                                       username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                                       password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
DBI::dbDisconnect(conn3)

test_that("Check if the function errors out with an informative message
          when the user provides an input that has already been
          disconnected.", {
            expect_error(warehouse_disconnect(conn3),
                         "The input is no longer valid or has already been disconnected.")
          })
 
# conn4 <- benchlingr::warehouse_connect(tenant = "...",
#                                        username = "...",
#                                        password = "...")
# 
# test_that("Check if the function errors out with an informative message 
#           when the user provides an input that is invalid.", {
#             expect_error(warehouse_disconnect(conn4),
#                          "The input is no longer valid or has already been disconnected.")
#           })
