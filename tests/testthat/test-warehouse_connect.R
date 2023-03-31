#test-connect_warehouse.R

tenant1 = "hemoshear-dev"
tenant2 = "hemoshear2"

test_that("Verify that the connect_warehouse function returns a
          PqConnection-type object when the user provides a legitimate
          tenant, username, and password.", {
            expect_match(class(connect_warehouse(tenant = tenant1,
                                                 username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                                                 password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))),
                         "PqConnection")
            })

test_that("Verify that the function gives an error when the user provides a 
          legitimate tenant and password but the username is empty.", {
            expect_error(connect_warehouse(tenant = tenant1,
                                           username = "",
                                           password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD")),
                         'Warehouse username cannot be an empty string. Set the "BENCHLING_WAREHOUSE_USERNAME" environment variable in your .Renviron file. Open the file for editing in R with the following command: usethis::edit_r_environ().')
          })

test_that("Verify that the function gives an error when the user provides a 
          legitimate tenant and username but the password is empty.", {
            expect_error(connect_warehouse(tenant = tenant1,
                                           username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                                           password = ""),
                         'Warehouse password cannot be an empty string. Set the "BENCHLING_WAREHOUSE_PASSWORD" environment variable in your .Renviron file. Open the file for editing in R with the following command: usethis::edit_r_environ().')
          })

test_that("Verify that the function gives an error when the user provides a 
          legitimate tenant but both the username and password are empty.", {
            expect_error(connect_warehouse(tenant = tenant1,
                                           username = "",
                                           password = ""),
                         'Both warehouse username and password cannot be empty strings. Set the "BENCHLING_WAREHOUSE_USERNAME" environment variable and "BENCHLING_WAREHOUSE_PASSWORD" environment variable in your .Renviron file. Open the file for editing in R with the following command: usethis::edit_r_environ().')
          })

test_that("Verify that the function gives an error when the user provides a 
          legitimate username and password but the database cannot be found due 
          to the user passing in the name of a Benchling tenant that doesn't 
          actually exist.", {
            expect_error(connect_warehouse(tenant = tenant2,
                                           username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                                           password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD")),
                         paste0('Could not translate host name "postgres-warehouse.',
                                tenant2,'.benchling.com" to address: nodename nor servname provided, or not known.'))
          })
