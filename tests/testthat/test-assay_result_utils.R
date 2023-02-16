#'
#' assay result = '908df834-220e-4b59-975b-c21bbbb8191a'
#' this assay result is created by api. so, this test cases assume this assay result is not archived when the
#' tests perform. Thus, test cases can get, archive and unarchive the result in order.
#' we can utilize create assay result function (once it finalized) to have more robust unit tests in the future.
#'
#' 01.23.2023 Davut Ucar



test_that('get assay result',{
  assay_result_id <- '908df834-220e-4b59-975b-c21bbbb8191a'
  assay_result_id_list <- list(c(assay_result_id))
  result <- benchlingr::get_assay_result(
    assay_result_id = assay_result_id,
    tenant="hemoshear-dev",
    api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
  expect_equal(result[[7]], assay_result_id)
})


test_that('archive_assay_results success', {
  assay_result_ids <- c('908df834-220e-4b59-975b-c21bbbb8191a')
  archive_result <- benchlingr::archive_assay_results(
    assay_result_ids = assay_result_ids,
    tenant="hemoshear-dev",
    api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
  assay_result_ids_list <- list(assay_result_ids)
  testthat::expect_equal(archive_result[[1]], assay_result_ids_list, 
                         label = "archive assay result success result.")
})

test_that('archive_assay_results fail', {
  assay_result_ids <- c('908df834-220e-4b59-975b-c21bbbb8191a')
  assay_result_ids_list <- list(assay_result_ids)
  expect_error(benchlingr::archive_assay_results(
    assay_result_ids = assay_result_ids,
    tenant="hemoshear-dev",
    api_key=Sys.getenv("BENCHLING_DEV_API_KEY")), 
    regexp = '^.*has already been archived.*$')
})

test_that('unarchive_assay_results success', {
  assay_result_ids <- c('908df834-220e-4b59-975b-c21bbbb8191a')
  archive_result <- benchlingr::unarchive_assay_results(
    assay_result_ids = assay_result_ids,
    tenant="hemoshear-dev",
    api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
  assay_result_ids_list <- list(assay_result_ids)
  testthat::expect_equal(archive_result[[1]], assay_result_ids_list, 
                         label = "archive assay result success result.")
})

test_that('unarchive_assay_results fail', {
  assay_result_ids <- c('908df834-220e-4b59-975b-c21bbbb8191a')
  assay_result_ids_list <- list(assay_result_ids)
  expect_error(benchlingr::unarchive_assay_results(
    assay_result_ids = assay_result_ids,
    tenant="hemoshear-dev",
    api_key=Sys.getenv("BENCHLING_DEV_API_KEY")),
    regexp = '^.*has already been unarchived.*$')
})

