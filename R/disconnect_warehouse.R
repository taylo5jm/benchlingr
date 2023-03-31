# disconnect_warehouse.R

#' Disconnect from a Benchling data warehouse
#' 
#' Close an already opened connection to the data warehouse.
#' @param conn A warehouse connection object of class PqConnection usually 
#' returned by connect_warehouse()
#' @export disconnect_warehouse
#' @return TRUE, invisibly
#' @examples 
#' \dontrun{
#' conn <- connect_warehouse(tenant = "hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_WAREHOUSE_PASSWORD"))
#' disconnect_warehouse(conn)    
#' }

disconnect_warehouse <- function(conn) {
    if (!is(conn, "PqConnection")) { # Checks to see if the input's class is PqConnection
      stop("Input is not a PqConnection class object.")
    } 
    if (!DBI::dbIsValid(conn)) { # Checks to see if the input is still valid
        warning("The input is no longer valid or has already been disconnected.")
      } else {
        DBI::dbDisconnect(conn) # returns true invisibly
    }
}



