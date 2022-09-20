conn <- warehouse_connect("hemoshear-dev", 
                          username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                          password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
d <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw WHERE entry_id$ = 'etr_JYUlMiIs'")

file_map <- list("ff0cca5f-b400-4e42-9df6-9f1badc4b7e2" = "Plate1-Data.csv",
                 "a2ed3ec8-59b9-451a-81eb-c4fb5322858b" = "Plate2-Data.csv")
client <- benchling_api_auth(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))

test_that("download_blobs works", {
  download_blobs(client, file_map=file_map, outdir='data')
  expect_equal(length(dir('data')), 2)
})

DBI::dbDisconnect(conn)