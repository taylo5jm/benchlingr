test_that("This first test is designed to see if a disconnect is successful 
          when the user provides a valid PqConnection-type object.", {
            expect_true(warehouse_disconnect(...))
})

test_that("This second test is designed to see if the function errors 
          out with an informative message when the user provides an 
          input that is not a PqConnection-type object.", {
            expect_error(warehouse_disconnect(...),
                         "Input was not a PqConnection class object.")
          })

