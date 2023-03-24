conn <- connect_warehouse("hemoshear-dev", 
                          username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                         password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
client <- connect_sdk(tenant="https://hemoshear-dev.benchling.com",
                             api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))


test_that("list_schemas_in_entry works for notebook entry
          with results and registration tables.", {
    nb_entry <- client$entries$get_entry_by_id("etr_MWQ7M7Pz")
    res <- list_schemas_in_entry(client, conn, nb_entry)
    testthat::expect_equal(nrow(res), 2)
})


test_that("list_schemas_in_entry returns empty data frame
           when the notebook entry. ", {
    nb_entry <- client$entries$get_entry_by_id("etr_T3WZTyAe")
    res <- list_schemas_in_entry(client, conn, nb_entry)
    testthat::expect_equal(nrow(res), 0)
})
