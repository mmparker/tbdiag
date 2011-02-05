
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
               
    # Floating point comparisons will bite you in the ass for some of these.
    # Instead of >=, define a small value and add it to the number being compared
    # In essence, convert any left-hand value that's truly equal to the right-hand
    # value into one that is *greater* than the right-hand value.
    # This is the tolerance value used by all.equal().
    tol <- .Machine$double.eps ^ 0.5
               
    # Check for positive results
    if(!isTRUE((na.omit(nil) > (0 - tol)) &
               (na.omit(tb) > (0 - tol)) &
               (na.omit(mitogen) > (0 - tol)))){stop(
               "The vectors of TB, nil, and mitogen values must all be positive.")}
    
    # Set up the results vector
    result <- rep(NA, times = length(nil)) 

    # Iterate through each test.
    for(i in seq_along(result)){
        # If any test value is NA, no interpretation - skip
        if(is.na(tb[i]) | is.na(nil[i]) | is.na(mitogen[i])) {next}

        if(nil[i] + tol > 8.0) {result[i] <- "Indeterminate"} else {
            if(tb[i] - nil[i] + tol > tbnil.cutoff & tb[i] - nil[i] + tol > .25 * nil[i])
                {result[i] <- "Positive"} else {
                    if((tb[i] - nil[i] + tol < tbnil.cutoff | tb[i] - nil[i] + tol < .25 * nil[i]) &
                        !(mitogen[i] - nil[i] + tol < 0.5))
                        {result[i] <- "Negative"} else {
                            if((tb[i] - nil[i] + tol < tbnil.cutoff | tb[i] - nil[i] + tol < .25 * nil[i]) &
                                mitogen[i] - nil[i] + tol < 0.5)
                                {result[i] <- "Indeterminate"}
                }
            }
        }
    }
    return(result)
}




# Development notes: This algorithm is based on the flow chart L05995008A Apr 2007 from Cellestis.