\name{equal.lengths}
\alias{equal.lengths}
\title{Confirm that all input vectors are the same length.}
\usage{
  equal.lengths(...)
}
\arguments{
  \item{...}{Two or more vectors to be compared.}
}
\value{
  Returns an error if the vectors are of unequal length,
  returns a warning if only one vector is supplied, and
  returns nothing if the vectors are of equal length.
}
\description{
  Confirm that all input vectors are the same length.
}
\examples{
\dontrun{equal.lengths(rnorm(10), rnorm(10), rnorm(9))}
\dontrun{equal.lengths(rnorm(10))}
\dontrun{equal.lengths(rnorm(10), rnorm(10), rnorm(10))}
}

