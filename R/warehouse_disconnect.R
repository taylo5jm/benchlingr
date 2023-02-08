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
#' conn <- warehouse_connect("hemoshear-dev", 
#'     username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
#'     password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
#' warehouse_disconnect(conn)    
#' }

warehouse_disconnect <- function(conn){
  if (DBI::dbIsValid(conn) == TRUE) {
    if (class(conn) == "PqConnection") {
      DBI::dbDisconnect(conn) # returns true invisibly
    } else {
      stop("Input was not a PqConnection class object.")
    }
  } else {
    stop("The input is no longer valid.")
  }
}

