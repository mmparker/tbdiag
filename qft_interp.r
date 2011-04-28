
qft.interp <- function(nil, tb, mitogen,
                       tbnil.cutoff = 0.35,
                       output = "verbose"){

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
                           "Indeterminate - mitogen too close to nil")
                               )

    # Check for valid output argument
    if(!(output %in% names(interp.table))){
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
               
    # Floating point comparisons will bite you in the ass for some of these.
    # Instead of >=, define a small value and add it to the number being compared
    # In essence, convert any left-hand value that's truly equal to the right-hand
    # value into one that is *greater* than the right-hand value.
    # This is the tolerance value used by all.equal().
    tol <- .Machine$double.eps ^ 0.5

    
    # Set up the results vector
    result <- rep(NA, times = length(nil)) 

    # Iterate through each test.
    for(i in seq_along(result)){
        # If any test value is NA, no interpretation - skip
        if(is.na(tb[i]) | is.na(nil[i]) | is.na(mitogen[i])) {next}

        if(nil[i] + tol > 8.0) {result[i] <- interp.table[1, output]} else {
            if(tb[i] - nil[i] + tol > tbnil.cutoff & tb[i] - nil[i] + tol > .25 * nil[i])
                {result[i] <- interp.table[2, output]} else {
                    if((tb[i] - nil[i] + tol < tbnil.cutoff | tb[i] - nil[i] + tol < .25 * nil[i]) &
                        !(mitogen[i] - nil[i] + tol < 0.5))
                        {result[i] <- interp.table[3, output]} else {
                            if((tb[i] - nil[i] + tol < tbnil.cutoff | tb[i] - nil[i] + tol < .25 * nil[i]) &
                                mitogen[i] - nil[i] + tol < 0.5)
                                {result[i] <- interp.table[4, output]}
                }
            }
        }
    }
    return(result)
}




# Development notes: This algorithm is based on the flow chart L05995008A Apr 2007 from Cellestis.
