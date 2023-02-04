# test-warehouse_disconnect.R
test_that("Verify that a disconnect was successful when the user 
          provides a valid PqConnection-type object.", {
            expect_true(warehouse_disconnect(...))
})

test_that("Check if the function errors out with an informative message 
          when the user provides an input that is not a PqConnection-type 
          object.", {
            expect_error(warehouse_disconnect(...),
                         "Input was not a PqConnection class object.")
          })

