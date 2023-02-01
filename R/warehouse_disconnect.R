warehouse_disconnect <- function(conn){
  if (class(conn) == "PqConnection") {
    if (DBI::dbDisconnect(conn) == TRUE) {
      return(TRUE)
    } else {
      return("Disconnect was not successful.")
    }
  } else {
    stop("Input was not a PqConnection class object.")
  }
}
