library(benchlingr)

conn <- benchlingr::connect_warehouse(
  "hemoshear-dev",
  Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
  Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))

client <- benchlingr::connect_sdk(
  tenant = "https://hemoshear-dev.benchling.com",
  api_key = Sys.getenv("BENCHLING_DEV_API_KEY"))

res <- data.frame(
  study_name = "Honeycomb",
  plate = as.integer(1),
  date = as.character(Sys.Date()),
  datetime = as.character(Sys.time()),
  json = RJSONIO::toJSON(list(algorithm = "sgd")),
  dna_sequence = "seq_Cuf0bmCm",
  file = 'test-create_assay_results.R',
  analyte = "bfi_KsLU5uWV"
)


test_that("create_assay_results will stop if a file in a blob link column
          does not exist on the local machine.", {
  res$file <- 'fakefile'
  testthat::expect_error(
    benchlingr::create_assay_results(
      conn, client, df = res, project_id = "src_ZRvTYOgM",
      schema_id = "assaysch_eBsoKyRO",
      tenant = "https://hemoshear-dev.benchling.com",
      api_key = Sys.getenv("BENCHLING_DEV_API_KEY"),
      fk_type = "id")
  )
}
)


test_that("create_assay_results will succeed with valid set of minimal input.", {
  res <- data.frame(
    study_name = "Honeycomb",
    plate = as.integer(1),
    date = as.character(Sys.Date()),
    datetime = as.character(Sys.time()),
    json = RJSONIO::toJSON(list(algorithm = "sgd")),
    dna_sequence = "seq_Cuf0bmCm",
    analyte = "bfi_KsLU5uWV"
  )
  res <- benchlingr::create_assay_results(
    conn, client, df = res, project_id = "src_ZRvTYOgM",
    schema_id = "assaysch_eBsoKyRO",
    tenant = "https://hemoshear-dev.benchling.com",
    api_key = Sys.getenv("BENCHLING_DEV_API_KEY"),
    fk_type = "id")
  testthat::expect_equal(length(res), 1)
}
)

test_that("create_assay_results will fail when an integer field
          is represented as a numeric in R.", {
  res <- data.frame(
    study_name = "Honeycomb",
    plate = 1,
    date = as.character(Sys.Date()),
    datetime = as.character(Sys.time()),
    json = RJSONIO::toJSON(list(algorithm = "sgd")),
    dna_sequence = "seq_Cuf0bmCm",
    analyte = "bfi_KsLU5uWV"
  )
  testthat::expect_error(
    benchlingr::create_assay_results(
      conn, client, df = res, project_id = "src_ZRvTYOgM",
      schema_id = "assaysch_eBsoKyRO",
      tenant = "https://hemoshear-dev.benchling.com",
      api_key = Sys.getenv("BENCHLING_DEV_API_KEY"),
      fk_type = "id"),
    regexp = "integer type"
  )
}
)

test_that("create_assay_results will work when a file needs to be uploaded.", {
  res <- data.frame(
    file = 'test-create_assay_results.R'
  )
  testthat::expect_equal(
    length(benchlingr::create_assay_results(
      conn, client, df = res, project_id = "src_ZRvTYOgM",
      schema_id = "assaysch_eBsoKyRO",
      tenant = "https://hemoshear-dev.benchling.com",
      api_key = Sys.getenv("BENCHLING_DEV_API_KEY"),
      fk_type = "name")),
    1
  )
}
)

test_that("create_assay_results will work when a file
          doesn't need to be uploaded.", {
  res <- data.frame(
    file = '49176d96-42a2-44f2-ae33-d97589601b62'
  )

  created_results <- create_assay_results(
    conn, client, df = res, project_id = "src_ZRvTYOgM",
    schema_id = "assaysch_eBsoKyRO",
    tenant = "https://hemoshear-dev.benchling.com",
    api_key = Sys.getenv("BENCHLING_DEV_API_KEY"),
    fk_type = "id")

  testthat::expect_equal(
    length(created_results), 1)
})

test_that("create_assay_results with more than 100 results.", {
  res <- data.frame(plate = seq.int(1, 110))

  created_results <- create_assay_results(
    conn, client, df=res, project_id="src_ZRvTYOgM",
    schema_id="assaysch_eBsoKyRO",
    tenant="https://hemoshear-dev.benchling.com",
    api_key=Sys.getenv("BENCHLING_DEV_API_KEY"),
    fk_type = "id")

  testthat::expect_equal(
    length(created_results), 110)
})

test_that("create_assay_results works with schema that has multi-select field and
          when fk_type has multiple values.", {
  df <- tibble::tibble(
    plate = c(as.integer(1), 
              as.integer(2)),
    analytes = list(list("bfi_9fKcrORv", "bfi_VVamxrKQ"), list( "bfi_VVamxrKQ")),
    file = c("test-create_assay_results.R", "test-download_blobs.R"))
  res <- create_assay_results(
    conn=conn, client=client, df=df,
    project_id="src_ZRvTYOgM",
    schema_id="assaysch_nIw4yAq8",
    fk_type=c(analytes = "id", file = "name"),
    tenant="hemoshear-dev.benchling.com",
    api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))

  expect_equal(length(res), 2)

})
DBI::dbDisconnect(conn)
