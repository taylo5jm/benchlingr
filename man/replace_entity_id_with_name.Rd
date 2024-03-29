% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_entity_id_with_name.R
\name{replace_entity_id_with_name}
\alias{replace_entity_id_with_name}
\title{Replace identifiers in entity columns with the entity names.}
\usage{
replace_entity_id_with_name(conn, df)
}
\arguments{
\item{conn}{Database connection opened by `connect_warehouse`.}

\item{df}{Data frame retrieved from the Benchling data warehouse with one or 
more entity link columns. The data frame must also have a column called `schema`, 
which indicates the schema name of the warehouse table. 
One can use `DBI::dbReadTable` or `DBI::dbGetQuery` to retrieve tables 
from the data warehouse.}
}
\value{
data.frame with the Benchling entity identifiers replaced by the 
names of the entities.
}
\description{
Values in an entity column will appear as Benchling identifiers when
pulled from the data warehouse. This function converts the identifiers 
into their human-readable names. For example, a custom entity value
will have an identifier that looks like "bfi_10dgdgFJx", but the name
might be "My cell line".
}
\examples{
\dontrun{
conn <- connect_warehouse("hemoshear-dev", 
    username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
    password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))

df <- DBI::dbGetQuery(conn, "SELECT * FROM 
simple_plate_analyte_mapping_with_two_analytes$raw WHERE 
entry_id$ = 'etr_lnZDpRVI'")
res <- replace_entity_id_with_name(conn, df)
DBI::dbDisconnect(conn)
}
}
