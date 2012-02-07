\name{qft.interp}
\alias{qft.interp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpret the results of the Cellestis Quantiferon Gold In-Tube.
}
\description{
This function takes quantitative results of the Quantiferon (nil, TB antigen, and mitogen in IU/mL) and returns qualitative interpretations based on the selected criteria.
}
\usage{
qft.interp(nil, tb, mito, criteria = "cellestis.usa", verbosity = "verbose", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nil}{
A vector of nil results (in IU/mL)
}
  \item{tb}{
A vector of TB antigen results (in IU/mL)
}
  \item{mito}{
A vector of mitogen results (in IU/mL)
}
  \item{criteria}{
The name of the desired result criteria (defaults to the Cellestis criteria for the United States).
}
  \item{verbosity}{
The level of verbosity ("onechar", "terse", "verbose") of the output
}
  \item{...}{
Other arguments passed to the criteria-evaluation function chosen by the "criteria" argument.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a vector of qualitative results.  The verbosity of results depends on the argument passed to "verbosity":
\item{onechar }{Returns a single character indicating the result (N for Negative, P for Positive, I for Indeterminate).}
\item{terse }{Returns a single word indicating the result (Negative, Positive, Indeterminate).}
\item{verbose }{Returns the same results as "terse", with the addition of a short comment indicating the reason for an "Indeterminate" result.}
}
\references{
Cellestis <http://www.cellestis.com/>
}
\author{
Matt Parker <mattmparker@gmail.com>
}
\note{
This function is provided purely as a convenience and is not a replacement for manual interpretation, manufacturer-provided software, or common sense.  Absolutely not for clinical use.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{tspot.interp}} for TSPOT.TB interpretation.
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
