\name{qft.interp}
\alias{qft.interp}
\title{Interpret the results of the Cellestis Quantiferon Gold In-Tube assay for latent tuberculosis infection.}
\usage{
  qft.interp(nil, tb, mito, criteria = "cellestis.usa",
    verbosity = "terse", ...)
}
\arguments{
  \item{nil}{A vector of nil results (in IU/mL)}

  \item{tb}{A vector of TB antigen results (in IU/mL)}

  \item{mito}{A vector of mitogen results (in IU/mL)}

  \item{criteria}{The name of the desired result criteria
  (defaults to the Cellestis criteria for the United
  States).}

  \item{verbosity}{The level of verbosity ("onechar",
  "terse", "verbose") of the output.}

  \item{...}{Other arguments passed to the crtieria
  evaluation function chosen by the "criteria" argument.}
}
\value{
  The function returns a vector of qualitative assay
  results according to the criteria chosen using the
  "criteria" argument.  The verbosity of the results
  depends on the value passed to the "verbosity" argument:
  \item{onechar }{Returns a single character indicating the
  result (N for Negative, P for Positive, I for
  Indeterminate).} \item{terse }{Returns a single word
  indicating the result (Negative, Positive,
  Indeterminate).} \item{verbose }{Returns the same results
  as "terse", with the addition of a short comment
  indicating the reason for an "Indeterminate" result.}
}
\description{
  Given vectors of nil, TB antigen, and mitogen results in
  IU/mL, this function computes QFT qualitative
  interpretations.  The function uses the Cellestis USA
  criterion by default; other criteria sets can be created
  as methods for the qft.criteria function.
}
\note{
  This function is provided purely as a convenience and is
  not a replacement for manual interpretation,
  manufacturer-provided software, or common sense.
  Absolutely not for clinical use.
}
\examples{
# Calculate results
test.qfts$result.check <- with(test.qfts,
                               qft.interp(nil = nil,
                                          tb = tb,
                                          mito = mito))

# Compare lab and calculated results
with(test.qfts, table(lab.result, result.check, exclude = NULL))

# Compare different levels of verbosity
test.qfts$verbose.check <- with(test.qfts,
                                qft.interp(nil = nil,
                                           tb = tb,
                                           mito = mito,
                                           verbosity = "verbose"))

test.qfts$onechar.check <- with(test.qfts,
                                qft.interp(nil = nil,
                                           tb = tb,
                                           mito = mito,
                                           verbosity = "onechar"))

unique(test.qfts[ , c("lab.result", "result.check",
                      "verbose.check", "onechar.check")])
}
\references{
  Cellestis <http://www.cellestis.com/>
}
\seealso{
  \code{\link{tspot.interp}} for TSPOT.TB interpretation.
}

