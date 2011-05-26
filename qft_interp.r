
qft.interp <- function(nil, tb, mitogen, tbnil.cutoff = 0.35){
# Given vectors of nil, TB antigen, and mitogen results in IU/mL,
# this function computes QFT qualitative interpretations.  The function
# uses the Cellestis North America criterion by default;
# alternative TB - nil cutoffs can be specified using the tbnil.cutoff
# argument.  Further changes to the criteria require modification of the
# function.

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
           nil + tol > 8.0] <- "Indeterminate" 

    result[is.na(result) &
           (tb - nil + tol > tbnil.cutoff) & 
           (tb - nil + tol > .25 * nil)] <- "Positive"

    result[is.na(result) & 
           (tb - nil + tol < tbnil.cutoff | tb - nil + tol < .25 * nil) &
           !(mitogen - nil + tol < 0.5)] <- "Negative"

    result[is.na(result) &
           ((tb - nil + tol < tbnil.cutoff | tb - nil + tol < .25 * nil) &
            mitogen - nil + tol < 0.5)] <- "Indeterminate"
  
    return(result)

}

# Development notes: This algorithm is based on the flow chart L05995008A Apr 2007 from Cellestis.
