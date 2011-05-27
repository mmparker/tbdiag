\name{qft.interp}
\alias{qft.interp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interpret the results of the Cellestis Quantiferon Gold In-Tube.
}
\description{
This function takes quantitative results of the Quantiferon (nil, TB antigen, and mitogen in IU/mL) and returns the manufacturer-specified qualitative interpretation.
}
\usage{
qft.interp(nil, tb, mitogen, tbnil.cutoff = 0.35, output = "terse")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nil}{
A vector of nil results (in IU/mL)
}
  \item{tb}{
A vector of TB antigen results (in IU/mL)
}
  \item{mitogen}{
A vector of mitogen results (in IU/mL)
}
  \item{tbnil.cutoff}{
The value of TB antigen - nil at which the test will interpreted as a positive
}
  \item{output}{
The level of verbosity ("onechar", "terse", "verbose") of the output
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a vector of qualitative results.  The verbosity of results depends on the argument passed to "output":
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

## The function is currently defined as
function(nil, tb, mitogen,
                       tbnil.cutoff = 0.35,
                       output = "terse"){

    # Given vectors of nil, TB antigen, and mitogen results in IU/mL,
    # this function computes QFT qualitative interpretations.  The function
    # uses the Cellestis North America criterion by default;
    # alternative TB - nil cutoffs can be specified using the tbnil.cutoff
    # argument.  Further changes to the criteria require modification of the
    # function.

    # Set up the interpretation table with terse and verbose messages
    interp.table <- data.frame(
               onechar = c("I", "P", "N", "I"),
               terse = c("Indeterminate", 
                         "Positive", 
                         "Negative", 
                         "Indeterminate"),
               verbose = c("Indeterminate - high nil",
                           "Positive", 
                           "Negative",
                           "Indeterminate - mitogen too close to nil"),
               stringsAsFactors = FALSE
    )

    # Check for valid output argument
    if(!(output \%in\% names(interp.table))){
        "An invalid output option was specified."}

    # Check for equal vector lengths
    if(!isTRUE(all.equal(length(nil), length(tb))) |
       !isTRUE(all.equal(length(nil), length(mitogen)))){stop(
           "The vectors of TB, nil, and mitogen values must all be the same length.")}
    
    # Check for numeric results
    if(!isTRUE(is.numeric(nil) &
               is.numeric(tb) &
               is.numeric(mitogen))){stop(
               "The vectors of TB, nil, and mitogen values must all be numeric.")}
               
    # Floating point comparisons can be a problem here.
    # Instead of >=, define a small value and add it to the number 
    # being compared.
    # In essence, convert any left-hand value that's truly equal
    # to the right-hand # value into one that is *greater* 
    # than the right-hand value.
    # This is the tolerance value used by all.equal().
    tol <- .Machine$double.eps ^ 0.5

    
    # Set up the results vector
    result <- rep(NA, times = length(nil)) 

    # Compute the results
    result[is.na(result) &
           nil + tol > 8.0] <- interp.table[1, output]

    result[is.na(result) &
           (tb - nil + tol > tbnil.cutoff) & 
           (tb - nil + tol > .25 * nil)] <- interp.table[2, output]

    result[is.na(result) & 
           (tb - nil + tol < tbnil.cutoff | tb - nil + tol < .25 * nil) &
           !(mitogen - nil + tol < 0.5)] <- interp.table[3, output]

    result[is.na(result) &
           ((tb - nil + tol < tbnil.cutoff | tb - nil + tol < .25 * nil) &
            mitogen - nil + tol < 0.5)] <- interp.table[4, output]
  
    return(result)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
