


# Set up a coordinating script to check data and call the generic
# criteria function




qft.interp <- function(nil, tb, mito,
                       criteria = "cellestis.usa"){

    # Given vectors of nil, TB antigen, and mitogen results in IU/mL,
    # this function computes QFT qualitative interpretations.  The function
    # uses the Cellestis USA criterion by default; other criteria
    # sets can be created as methods for the qft.criteria function.



#     # Check for valid output argument
#     if(!(output %in% names(interp.table))){
#         "An invalid output option was specified."}

    # Check for equal vector lengths
    if(any(!isTRUE(all.equal(length(nil), length(tb))),
           !isTRUE(all.equal(length(nil), length(mito)))
       )){stop(
    "The vectors of TB, nil, and mitogen values must all be the same length.")}


    # Check for numeric results
    if(any(!is.numeric(nil),
           !is.numeric(tb),
           !is.numeric(mito))){stop(
           "The vectors of TB, nil, and mitogen values must all be numeric.")}


    # Set up the interpretation object
    interp.this <- data.frame(nil = nil,
                              tb = tb,
                              mito = mito
    )                                

    # Set the class so that the generic function knows which method to call
    class(interp.this) <- c("data.frame", criteria)

    # Call the generic function to apply the appropriate criteria
    qft.criteria(interp.this)

}


# Define the generic funciton
qft.criteria <- function(interp.this){
    UseMethod("qft.criteria", interp.this)
}


qft.criteria.cellestis.usa <- function(qft.obj, output){
    # This method calculates the QFT interpretation based on
    # Cellestis' American criteria

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
    
    output <- "verbose"

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
    # Indeterminate due to high nil
    result[is.na(result) &
           nil + tol > 8.0] <- interp.table[1, output]

    # Positive
    result[is.na(result) &
           (tb - nil + tol > 0.35) & 
           (tb - nil + tol > .25 * nil)] <- interp.table[2, output]

    # Negative
    result[is.na(result) & 
           (tb - nil + tol < 0.35 | tb - nil + tol < .25 * nil) &
           !(mito - nil + tol < 0.5)] <- interp.table[3, output]

    # Indeterminate due to nil ~ mitogen
    result[is.na(result) &
           ((tb - nil + tol < 0.35 | tb - nil + tol < .25 * nil) &
            mito - nil + tol < 0.5)] <- interp.table[4, output]
  
    return(result)

}





qft.criteria.cellestis.aus <- function(qft.obj, output){
    # This method calculates the QFT interpretation based on
    # Cellestis' Australian criteria

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
    
    output <- "verbose"

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
    # Indeterminate due to high nil
    result[is.na(result) &
           nil + tol > 8.0] <- interp.table[1, output]

    # Positive
    result[is.na(result) &
           (tb - nil + tol > 0.35) & 
           (tb - nil + tol > .25 * nil)] <- interp.table[2, output]

    # Negative
    result[is.na(result) & 
           (tb - nil + tol < 0.35 | 
            tb - nil + tol < .25 * nil)] <- interp.table[3, output]

    # No indeterminate due to nil ~ mitogen
  
    return(result)

}





