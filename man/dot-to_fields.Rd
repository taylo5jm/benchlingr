% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{.to_fields}
\alias{.to_fields}
\title{Raise exception if the `schema` column is not in the data frame.
 This function takes a data frame and converts each row into the nested list that .create_assay_result needs.}
\usage{
.to_fields(df)
}
\arguments{
\item{df}{data.frame with table from the data warehouse.}
}
\value{
res
}
\description{
Raise exception if the `schema` column is not in the data frame.
 This function takes a data frame and converts each row into the nested list that .create_assay_result needs.
}
\keyword{internal}
