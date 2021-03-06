% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/qft.cens.r
\name{qft.cens}
\alias{qft.cens}
\title{Censor Quantiferon quantitative results to 10 IU/mL.}
\usage{
qft.cens(x)
}
\arguments{
\item{x}{A vector of numeric results from the Quantiferon}
}
\value{
The input vector with all results greater than 10 censored to 10.
}
\description{
This functions censors a vector of quantitative results of the
Quantiferon (nil, antigen, mitogen) from values greater than 10 IU/mL to
10 IU/mL, following the Cellestis test insert instructions.
Intended for internal use only.
}
\seealso{
\code{\link{tspot.cens}}
}

