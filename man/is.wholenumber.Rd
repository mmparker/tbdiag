\name{is.wholenumber}
\alias{is.wholenumber}
\title{Confirm that the input values are integers.}
\usage{
  is.wholenumber(x, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{x}{A vector of numeric values to be tested.}

  \item{tol}{The maximum allowable difference between an
  input number and the nearest whole number.}
}
\value{
  Returns a TRUE if all of the values are integers, FALSE
  if one or more are not. Drawn directly from the examples
  in the \code{\link{is.integer}} documentation.
}
\description{
  This function validates whether the input values are
  whole integers or not. It does this by rounding the input
  values to their nearest whole integer and then evaluating
  the difference. If the difference is very, very small
  (the default is \code{.Machine$double.eps^0.5}), the
  numbers will be accepted as whole.
}
\details{
  All that nonsense is required to avoid floating point
  comparison errors.
}
\seealso{
  \code{\link{is.integer}}
}

