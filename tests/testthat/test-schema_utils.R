library(benchlingr)

test_that('get_schema_fields successful', {
  schema_id <- 'assaysch_nIw4yAq8'
  schema_type <- 'assay-result'
  field_definition <- get_schema_fields(schema_id, schema_type)
  testthat::expect_equal(3, length(field_definition))
})

test_that('Strict and loose verification of dataframe colnames. ', {
  schema_id <- 'assaysch_nIw4yAq8'
  schema_type <- 'assay-result'
  df <- data.frame(
    "plate" = c('davut'),
    "analytes" = c('0.2'),
    'File1' = c('aaaa'),
    check.names = FALSE
  )
  testthat::expect_true(verify_schema_fields(schema_id, schema_type, df))
  testthat::expect_warning(verify_schema_fields(schema_id, schema_type, df, strict_check = TRUE))
})

test_that('Strict and loose verification of dataframe colnames', {
  schema_id <- 'assaysch_nIw4yAq8'
  schema_type <- 'assay-result'
  df <- data.frame(
    "plate" = c('davut'),
    "analytes" = c('0.2'),
    'file' = c('aaaa'),
    check.names = FALSE
  )
  testthat::expect_true(verify_schema_fields(schema_id, schema_type, df))
  testthat::expect_true(verify_schema_fields(schema_id, schema_type, df, strict_check = TRUE))
})

test_that('Empty schema/df tests.', {
  schema_id <- 'assaysch_IoMpKwiX' # Empty Schema
  schema_type <- 'assay-result'

  empty_df <- data.frame(

  )
  testthat::expect_equal(0, length(get_schema_fields(schema_id = schema_id, schema_type = schema_type)))
  testthat::expect_warning(verify_schema_fields(schema_id = schema_id, schema_type = schema_type, df = empty_df))

  df <- data.frame(
    "plate" = c('davut'),
    "analytes" = c('0.2'),
    'file' = c('aaaa'),
    check.names = FALSE
  )

  testthat::expect_warning(verify_schema_fields(schema_id = schema_id, schema_type = schema_type, df = df))
})