#test-warehouse_connect.R

test_that("Verify that the warehouse_connect function returns a 
          PqConnection-type object when the user provides a legitimate 
          tenant, username, and password.", {
            expect_match(class(warehouse_connect("hemoshear-dev", 
                                                 username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                                                 password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))),
                         "PqConnection")
            })

test_that("Verify that the function gives an error when the username is
          empty.", {
            expect_error(warehouse_connect("hemoshear-dev", 
                                           username = "",
                                           password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD")),
                         "Warehouse username cannot be an empty string. 
         Set the 'BENCHLING_DEV_WAREHOUSE_USERNAME' environment variable in your
         .Renviron file. Open the file for editing in R with the following command:
         usethis::edit_r_environ()")
          })

test_that("Verify that the function gives an error when the password is
          empty.", {            
            expect_error(warehouse_connect("hemoshear-dev", 
                                           username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                                           password = ""),
                         "Warehouse password cannot be an empty string.
         Set the 'BENCHLING_DEV_WAREHOUSE_PASSWORD' environment variable in your
         .Renviron file. Open the file for editing in R with the following command:
         usethis::edit_r_environ()")
          })

test_that("Verify that the function gives an error when the both the 
          username and password are empty.", { 
            expect_error(warehouse_connect("hemoshear-dev", 
                                           username = "",
                                           password = ""),
                         "Both warehouse username and password cannot be empty strings.
         Set the 'BENCHLING_DEV_WAREHOUSE_USERNAME' environment variable and
         'BENCHLING_DEV_WAREHOUSE_PASSWORD' environment variable in your
         .Renviron file. Open the file for editing in R with the following command:
         usethis::edit_r_environ()")
          })

test_that("Verify that the function gives an error when the database
          cannot be found due to the user passing in the name of a
          Benchling tenant that doesn't actually exist", {
            expect_error(warehouse_connect("hemoshear2",username,password),
                         "could not translate host name 'postgres-warehouse.hemoshear2.benchling.com' to address: nodename nor servname provided, or not known")
          })