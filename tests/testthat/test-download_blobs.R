conn <- warehouse_connect("hemoshear-dev", 
                          username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                          password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
d <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw WHERE entry_id$ = 'etr_JYUlMiIs'")

file_map <- list("ff0cca5f-b400-4e42-9df6-9f1badc4b7e2" = "Plate1-Data.csv",
                 "a2ed3ec8-59b9-451a-81eb-c4fb5322858b" = "Plate2-Data.csv")
client <- benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
data_dirs <- c('download_blobs_data', 'download_blobs_in_warehouse_table_data',
               'download_blobs_in_warehouse_table_single_data')

for (i in data_dirs) {
  for (j in unlist(file_map)) {
    this_file <- file.path(i, j)
    if (file.exists(this_file)) {
      file.remove(this_file)
    }
  }
}

test_that("download_blobs works", {
  download_blobs(client, file_map=file_map, outdir='download_blobs_data')
  expect_equal(length(dir('data')), 2)
})

test_that("download_blobs_in_warehouse_table works when file column is single-select.", {
            d <- DBI::dbGetQuery(
              conn, 
              "SELECT * FROM simple_plate_analyte_mapping$raw WHERE entry_id$ = 'etr_JYUlMiIs'")
            download_blobs_in_warehouse_table(
              client, conn, d, outdir='download_blobs_in_warehouse_table_single_data')
            expect_equal(length(dir('download_blobs_in_warehouse_table_single_data')), 2)
          })


test_that("download_blobs_in_warehouse_table works when file column is
          multi-select.", {
  d <- DBI::dbGetQuery(
    conn, 
    "SELECT * FROM simple_plate_analyte_mapping_multi_file$raw WHERE entry_id$ = 'etr_JYUlMiIs'")
  download_blobs_in_warehouse_table(
    client, conn, d, outdir='download_blobs_in_warehouse_table_data')
  expect_equal(length(dir('download_blobs_in_warehouse_table_data')), 4)
})


DBI::dbDisconnect(conn)