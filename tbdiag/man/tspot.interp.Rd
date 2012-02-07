\name{tspot.interp}
\alias{tspot.interp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpret the results of the Oxford Immunotec TSPOT.TB.
}
\description{
This function takes quantitative results of the TSPOT (nil, panel A, panel B, and mitogen) and returns qualitative interpretations based on the selected criteria.
}
\usage{
tspot.interp(nil, panel_a, panel_b, mito, criteria = "oxford.usa", verbosity = "terse", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nil}{
A vector of nil/negative control results (in spots)
}
  \item{panel_a}{
A vector of Panel A (ESAT-6) results (in spots)
}
  \item{panel_b}{
A vector of Panel B (CFP10) results (in spots)
}
  \item{mito}{
A vector of mitogen/positive control results (in spots)
}
  \item{criteria}{
The name of the desired result criteria (defaults to the Oxford Immunotec criteria for the United States).
}
  \item{verbosity}{
The level of verbosity ("onechar", "terse", "verbose") of the output
}
  \item{...}{
Other arguments passed to the criteria-evaluation function chosen by the "criteria" argument.
}
}
\details{
}
\value{
The function returns a vector of qualitative results.  The verbosity of results depends on the argument passed to "verbosity":
\item{onechar }{Returns a single character indicating the result (N for Negative, B for Borderline, P for Positive, I for Indeterminate).}
\item{terse }{Returns a single word indicating the result (Negative, Borderline, Positive, Indeterminate).}
\item{verbose }{Returns the same results as "terse", with the addition of a short comment indicating the reason for an "Indeterminate" result.
}
\references{
Oxford Immunotec <http://www.oxfordimmunotec.com/>
}
\author{
Matt Parker <mattmparker@gmail.com>
}
\note{
This function is provided purely as a convenience and is not a replacement for manual interpretation, manufacturer-provided software, or common sense.  Absolutely not for clinical use.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{qft.interp}} for Quantiferon interpretation.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
