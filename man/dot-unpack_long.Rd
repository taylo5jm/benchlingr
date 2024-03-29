% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expand_multiselect_column.R
\name{.unpack_long}
\alias{.unpack_long}
\title{Unpack the values of a JSON column into new rows}
\usage{
.unpack_long(df, column)
}
\arguments{
\item{df}{Data frame with JSON columns to be unpacked into new rows.}

\item{column}{Name of the JSON column to be unpacked.}
}
\value{
Data frame with the values in the JSON column unpacked.
}
\description{
Unpack the values of a JSON column into new rows
}
\keyword{internal}
