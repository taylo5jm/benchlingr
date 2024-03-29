% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assay_results_utils.R
\name{unarchive_assay_results}
\alias{unarchive_assay_results}
\title{Makes a direct API Call to Benchling Assay Result Unarchive endpoint}
\usage{
unarchive_assay_results(
  tenant = Sys.getenv("BENCHLING_TENANT"),
  api_key = Sys.getenv("BENCHLING_API_KEY"),
  assay_result_ids = NULL
)
}
\arguments{
\item{tenant}{tenant name}

\item{api_key}{api key}

\item{assay_result_ids}{is vector type assay result ids. This function supports archiving multiple
items in one function call}
}
\value{
assay_result_ids
}
\description{
https://benchling.com/api/reference#/Assay%20Results/unarchiveAssayResults
}
\examples{
\dontrun{
assay_result_ids <- assay_result_ids=c("908df834-220e-4b59-975b-c21bbbb8191a")
}
}
