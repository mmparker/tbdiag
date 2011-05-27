\name{tspot.interp}
\alias{tspot.interp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpret the results of the Oxford Immunotec TSPOT.TB.
}
\description{
This function takes quantitative results of the TSPOT (nil, panel A, panel B, and mitogen) and returns the manufacturer-specified qualitative interpretation.
}
\usage{
tspot.interp(nil, panel_a, panel_b, mitogen, output = "terse")
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
  \item{mitogen}{
A vector of mitogen/positive control results (in spots)
}
  \item{output}{
The level of verbosity ("onechar", "terse", "verbose") of the output
}
}
\details{
}
\value{
The function returns a vector of qualitative results.  The verbosity of results depends on the argument passed to "output":
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

## The function is currently defined as
function(nil, panel_a, panel_b, mitogen, output = "terse"){
# Given vectors of nil, TB antigen (panels A and B), and mitogen results
# in spots, this function computes TSPOT qualitative interpretations.
# The function uses the Oxford Immunotec North America criterion by default;
# alternative TB - nil cutoffs can be specified using the tbnil.cutoff
# argument.  Further changes to the criteria require modification of the
# function.

    # Set up the interpretation table with terse and verbose messages
    interp.table <- data.frame(
                    onechar = c("I", "B", "P", "N", "I"),
                    terse = c("Invalid",
                              "Borderline", 
                              "Positive", 
                              "Negative", 
                              "Invalid"),
                    verbose = c("Invalid - high nil", 
                                "Borderline", 
                                "Positive", 
                                "Negative", 
                                "Invalid - low mitogen"),
                    stringsAsFactors = FALSE
    )

    # Check for valid output argument
    if(!(output \%in\% names(interp.table))){"An invalid output option was specified."}


    # Check for equal vector lengths
    if(!isTRUE(all.equal(length(nil), length(panel_a)) |
               all.equal(length(nil), length(panel_b)) |
               all.equal(length(nil), length(mitogen)))){stop(
               "The vectors of nil, Panel A, Panel B, and mitogen values must all be the same length.")}
    
    # Check for numeric results
    if(!isTRUE(is.numeric(nil) &
               is.numeric(panel_a) &
               is.numeric(panel_b) &
               is.numeric(mitogen))){stop(
               "The vectors of nil, TB - Panel A, TB - Panel B, and mitogen values must all be numeric.")}

    
    # Set up the results vector
    result <- rep(NA, times = length(nil)) 

    # Identify the maximum of Panel A - Nil and Panel B - Nil
    panel.max <- ifelse((panel_a - nil) > (panel_b - nil),
                        yes = (panel_a - nil), no = (panel_b - nil)
    )

    # Compute the results
    result[is.na(result) &
           nil > 10] <- interp.table[1, output]

    result[is.na(result) &
           panel.max \%in\% c(5, 6, 7)] <- interp.table[2, output]

    result[is.na(result) &
           panel.max >= 8] <- interp.table[3, output]

    result[is.na(result) &
           panel.max <= 4 &
           mitogen >= 20] <- interp.table[4, output]

    result[is.na(result) &
           panel.max <= 4 &
           mitogen < 20] <- interp.table[5, output]

    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
