#test-warehouse_connect.R
# tenant <- ...
# username <- ...
# password <- ...

test_that("Verify that the warehouse_connect function returns a 
          PqConnection-type object when the user provides a legitimate 
          tenant, username, and password.", {
            expect_match(class(warehouse_connect(tenant,username,password)),
            "PqConnection")
})

test_that("Verify that the function gives an error when the username 
          and/or password are empty.", {
            expect_error(warehouse_connect(tenant,"",password),
                         "Warehouse username cannot be an empty string. 
         Set the 'BENCHLING_WAREHOUSE_USERNAME' environment variable in your
         .Renviron file. Open the file for editing in R with the following command:
         usethis::edit_r_environ()")
            expect_error(warehouse_connect(tenant,username,""),
                         "Warehouse password cannot be an empty string.
         Set the 'BENCHLING_WAREHOUSE_PASSWORD' environment variable in your
         .Renviron file. Open the file for editing in R with the following command:
         usethis::edit_r_environ()")
            expect_error(warehouse_connect(tenant,"",""),
                         "Both warehouse username and password cannot be empty strings.
         Set the 'BENCHLING_WAREHOUSE_USERNAME' environment variable and 
         'BENCHLING_WAREHOUSE_PASSWORD' environment variable in your
         .Renviron file. Open the file for editing in R with the following command:
         usethis::edit_r_environ()")
          })

test_that("Verify that the function gives an error when the database 
          cannot be found due to the user passing in the name of a 
          Benchling tenant that doesn't actually exist", {
            expect_error(warehouse_connect("hemoshear2",username,password),
                         "could not translate host name 'postgres-warehouse.hemoshear2.benchling.com' to address: nodename nor servname provided, or not known")
          })