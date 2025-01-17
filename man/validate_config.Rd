% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_handling.R
\name{validate_config}
\alias{validate_config}
\title{Validate configuration}
\usage{
validate_config(cnfg, expected_parameters)
}
\arguments{
\item{cnfg}{Configuration paramter list generated by \code{batchreporter::load_config()}}

\item{expected_parameters}{Character object. Vector of expected parameter names
in the cnfg list}
}
\value{
NULL. If missing parameters, error is thrown with descriptive message
}
\description{
Validate that configuration contains expected parameters and those parameters
are non-missing
}
