% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_entity_id_with_name.R
\name{replace_entity_id_with_name}
\alias{replace_entity_id_with_name}
\title{Replace identifiers in entity columns with the entity names.}
\usage{
replace_entity_id_with_name(conn, df)
}
\arguments{
\item{conn}{Database connection opened by `warehouse_connect`.}

\item{df}{Data frame with entity columns.}
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