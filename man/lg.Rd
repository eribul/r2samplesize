% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{lg}
\alias{lg}
\title{Log messages from other functions within the package}
\usage{
lg(..., fun = log4r::info)
}
\arguments{
\item{...}{arguments pased to \code{\link{paste}} as log message.
If no arguments given, \code{\link{sys.call}} will be printed to log.}

\item{fun}{one of the functions "debug", "info", "warn", "error" or "fatal"
from the "log4r" package}
}
\value{
Nothing. The function is called for its side effects.
}
\description{
This function is for internal use only
}

