# test-list_api_contents.R

# test case 1
test_that("Check that .list_api_contents generates an error when given an 
          invalid argument for contents", {
  expect_error(.list_api_contents(tenant = "hemoshear-dev", 
                                  contents = "Invalid"), 
               "Invalid argument for contents.")
  expect_error(.list_api_contents(tenant = "hemoshear-dev",
                                  contents = "endpoint"), 
               "Invalid argument for contents.")
})

# test case 2
test_that("Check that .list_api_contents generates an appropriate list of 
          entities when given a valid input for contents", {
  expect_equal(class(.list_api_contents(tenant = "hemoshear-dev", 
                                        contents = "all")),
              "list")
  expect_equal(class(.list_api_contents(tenant = "hemoshear-dev", 
                                        contents = "entity schema")),
              "list")
  expect_equal(class(.list_api_contents(tenant = "hemoshear-dev", 
                                        contents = "list contents")),
              "list")
  expect_equal(class(.list_api_contents(tenant = "hemoshear-dev",
                                        contents = "single-get endpoint")),
              "list")
  expect_equal(class(.list_api_contents(tenant = "hemoshear-dev",
                                        contents = "bulk-get endpoints")),
              "list")
})