# warehouse_connect.R

#' Connect to a Benchling data warehouse
#' 
#' Open a connection to the data warehouse for a particular Benchling tenant
#' using the `DBI` and `RPostgres` packages. 
#' @param tenant Name of the Benchling tenant for which a warehouse connection
#' should be opened (character). 
#' @param username Username for the Benchling tenant (character). The default
#' argument is the 'BENCHLING_WAREHOUSE_USERNAME' environment variable in the
#' `.Renviron` file.
#' @param password Password for the Benchling tenant (character). The default
#' argument is the 'BENCHLING_WAREHOUSE_PASSWORD' environment variable in the 
#' `.Renviron` file.
#' @export warehouse_connect
#' @return A 'database_connection***' object that can be used by the `DBI` package
#' to make queries against the Postgres database for the specified tenant.
#' @examples \dontrun{
#' conn <- warehouse_connect("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_WAREHOUSE_PASSWORD"))
#' # It is good practice to close the connection after finishing your queries. 
#' DBI::dbDisconnect(conn)
#' }

warehouse_connect <- function(tenant, 
  username=Sys.getenv("BENCHLING_WAREHOUSE_USERNAME"),
  password=Sys.getenv("BENCHLING_WAREHOUSE_PASSWORD")) {
  if (DBI::dbCanConnect(RPostgres::Postgres(), dbname="warehouse",
                        host=paste0("postgres-warehouse.", tenant, ".benchling.com"),
                        port=5432, user=username, password=password) == TRUE) {
    conn <- DBI::dbConnect(RPostgres::Postgres(), dbname="warehouse",
                           host=paste0("postgres-warehouse.", tenant, ".benchling.com"),
                           port=5432, user=username, password=password)
    return(conn)
  } else {
    if (username != "" & password != "") {
      stop(paste0('Could not translate host name "postgres-warehouse.',
                  tenant,'.benchling.com" to address: nodename nor servname provided, or not known.'))
    }
    if (username == "" & password != "") {
      stop('Warehouse username cannot be an empty string. Set the "BENCHLING_WAREHOUSE_USERNAME" environment variable in your .Renviron file. Open the file for editing in R with the following command: usethis::edit_r_environ().')
    }
    if (username != "" & password == "") {
      stop('Warehouse password cannot be an empty string. Set the "BENCHLING_WAREHOUSE_PASSWORD" environment variable in your .Renviron file. Open the file for editing in R with the following command: usethis::edit_r_environ().')
    }
    if (username == "" & password == "") {
      stop('Both warehouse username and password cannot be empty strings. Set the "BENCHLING_WAREHOUSE_USERNAME" environment variable and "BENCHLING_WAREHOUSE_PASSWORD" environment variable in your .Renviron file. Open the file for editing in R with the following command: usethis::edit_r_environ().')
    }
  }
}
