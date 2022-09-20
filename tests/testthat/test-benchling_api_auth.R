# test-benchling_api_auth.R

test_that("benchling_api_auth gives informative error when api_key is missing", {
  expect_error(
    benchling_api_auth(
      tenant="https://hemoshear-dev.benchling.com",api_key = ""), 
    regexp="cannot be an empty string")
})
