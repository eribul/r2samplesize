% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subsamples.R
\name{subsamples}
\alias{subsamples}
\title{Take subsamples from data.frame}
\usage{
subsamples(d, n.max = 500, N = 10)
}
\arguments{
\item{d}{object of class \code{\link{sim_data}}}

\item{n.max}{maximum subsample size}

\item{N}{number of subsamples}
}
\value{
list of length \code{N} with objects of
class \code{\link{sim_data}}, each with \code{n.max} rows
}
\description{
Take subsamples from data.frame
}

