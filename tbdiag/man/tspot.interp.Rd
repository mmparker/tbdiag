\name{tspot.interp}
\alias{tspot.interp}
\title{Interpret the results of the Oxford Immunotec TSPOT.TB assay for latent tuberculosis infection.}
\usage{
  tspot.interp(nil, panel_a, panel_b, mito,
    criteria = "oxford.usa", verbosity = "terse", ...)
}
\arguments{
  \item{nil}{A vector of nil results (in spots)}

  \item{panel_a}{A vector of Panel A TB antigen (ESAT-6)
  results (in spots)}

  \item{panel_b}{A vector of Panel B TB antigen (CFP10)
  results (in spots)}

  \item{mito}{A vector of mitogen results (in spots)}

  \item{criteria}{The name of the desired result criteria
  (defaults to the Oxford Immunotec criteria for North
  America).}

  \item{verbosity}{The level of verbosity ("onechar",
  "terse", "verbose") of the output.}

  \item{...}{Other arguments passed to the crtieria
  evaluation function chosen by the "criteria" argument.}
}
\value{
  The function returns a vector of qualitative results.
  The verbosity of results depends on the argument passed
  to "verbosity": \item{onechar }{Returns a single
  character indicating the result (N for Negative, B for
  Borderline, P for Positive, I for Indeterminate).}
  \item{terse }{Returns a single word indicating the result
  (Negative, Borderline, Positive, Indeterminate).}
  \item{verbose }{Returns the same results as "terse", with
  the addition of a short comment indicating the reason for
  an "Indeterminate" result.}
}
\description{
  Given vectors of nil, TB antigen (panels A and B), and
  mitogen results in spots, this function computes TSPOT
  qualitative interpretations. The function uses the Oxford
  Immunotec North America criterion by default; alternative
  criteria sets can be created as methods for the
  tspots.criteria function
}
\note{
  This function is provided purely as a convenience and is
  not a replacement for manual interpretation,
  manufacturer-provided software, or common sense.
  Absolutely not for clinical use.
}
\references{
  Oxford Immunotec <http://www.oxfordimmunotec.com/>
}
\seealso{
  \code{\link{qft.interp}} for Quantiferon interpretation.
}

