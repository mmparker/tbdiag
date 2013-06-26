\name{trim.output}
\alias{trim.output}
\title{Truncate Quantiferon and TSPOT qualitative results to desired verbosity.}
\usage{
  trim.output(res, verbosity = "terse")
}
\arguments{
  \item{res}{A vector of qualitative results}

  \item{verbosity}{The desired level of verbosity; defaults
  to \code{terse}}
}
\value{
  A vector of appropriately-verbose results.
}
\description{
  This function truncates the qualitative results of the
  \code{\link{qft.interp}} and \code{\link{tspot.interp}}
  functions to the users desired level of verbosity:
  \itemize{ \item a single character (\code{onechar}),
  \item a terse, one-word result (\code{terse}), or \item a
  more verbose answer indicating the specific reason for
  invalid and indeterminate tests (\code{verbose}). }
}
\details{
  Intended for internal use only.
}

