\name{tspot.cens}
\alias{tspot.cens}
\title{Censor TSPOT quantitative results to 20 spots.}
\usage{
  tspot.cens(x)
}
\arguments{
  \item{x}{A vector of numeric results from the TSPOT}
}
\value{
  The input vector with all results greater than 20
  censored to 20.
}
\description{
  This functions censors a vector of quantitative results
  of the TSPOT (nil, Panel A, Panel B, mitogen) from counts
  greater than 20 spots to 20 spots, following the Oxford
  test insert instructions. Intended for internal use only.
}
\seealso{
  \code{\link{qft.cens}}
}

