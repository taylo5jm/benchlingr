% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_entity_columns.R
\name{list_entity_columns}
\alias{list_entity_columns}
\title{List the names and indices of entity columns in a data frame corresponding
to a warehouse table.}
\usage{
list_entity_columns(conn, df)
}
\arguments{
\item{conn}{Database connection opened with `connect_warehouse`}

\item{df}{Data frame with entity columns}
}
\value{
Vector where the names are the names of entity columns in the data frame
and the values are the corresponding indices.
}
\description{
List the names and indices of entity columns in a data frame corresponding
to a warehouse table.
}
\examples{
\dontrun{
conn <- connect_warehouse("hemoshear-dev", 
    username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
    password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
res <- DBI::dbGetQuery(conn, "SELECT * FROM simple_plate_analyte_mapping$raw")
list_entity_columns(conn, res)
DBI::dbDisconnect(conn)
}
}
