% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_schemas_in_entry.R
\name{list_schemas_in_entry}
\alias{list_schemas_in_entry}
\title{List the schemas for the results and registration tables contained within
a notebook entry}
\usage{
list_schemas_in_entry(client, conn, entry)
}
\arguments{
\item{client}{Benchling API client created by `connect_sdk`.}

\item{conn}{Benchling data warehouse connection created with `connect_warehouse`.}

\item{entry}{Notebook entry retrieved with the `entries$get_entry_by_id` method
of the Benchling Python SDK facade object created by `benchlingr::connect_sdk`.}
}
\value{
A data frame that includes the names and identifiers of the results
and registration schemas contained within a notebook entry.
}
\description{
List the schemas for the results and registration tables contained within
a notebook entry
}
\examples{
\dontrun{
conn <- connect_warehouse("hemoshear-dev", 
                         username = Sys.getenv("BENCHLING_DEV_WAREHOUSE_USERNAME"),
                         password = Sys.getenv("BENCHLING_DEV_WAREHOUSE_PASSWORD"))
client <- connect_sdk(tenant="https://hemoshear-dev.benchling.com",
                            api_key=Sys.getenv("BENCHLING_DEV_API_KEY"))
nb_entry <- client$entries$get_entry_by_id("etr_MWQ7M7Pz")
list_schemas_in_entry(client, conn, nb_entry)
}
}
