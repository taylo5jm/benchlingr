# test-connect_sdk.R

test_that("connect_sdk gives informative error when api_key is missing", {
  expect_error(
    connect_sdk(
      tenant="https://hemoshear-dev.benchling.com",api_key = ""), 
    regexp="cannot be an empty string")
})
