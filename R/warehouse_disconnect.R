# warehouse_disconnect.R

#' Disconnect from a Benchling data warehouse
#' 
#' Close an already opened connection to the data warehouse.
#' @param conn A warehouse connection object of class PqConnection usually 
#' returned by warehouse_connect()
#' @export warehouse_disconnect
#' @return TRUE, invisibly
#' @examples 
#' \dontrun{
#' conn <- warehouse_connect(tenant = "hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_WAREHOUSE_PASSWORD"))
#' warehouse_disconnect(conn)    
#' }

warehouse_disconnect <- function(conn) {
    if (class(conn) != "PqConnection") { # Checks to see if the input's class is PqConnection
      stop("Input is not a PqConnection class object.")
    } 
    if (!DBI::dbIsValid(conn)) { # Checks to see if the input is still valid
        warning("The input is no longer valid or has already been disconnected.")
      } else {
        DBI::dbDisconnect(conn) # returns true invisibly
    }
}



