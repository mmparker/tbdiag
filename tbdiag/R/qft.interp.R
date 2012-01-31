


# Set up a coordinating script to check data and call the generic
# criteria function




qft.interp <- function(nil, tb, mito,
                       criteria = "cellestis.usa",
                       output = "verbose"){

    # Given vectors of nil, TB antigen, and mitogen results in IU/mL,
    # this function computes QFT qualitative interpretations.  The function
    # uses the Cellestis USA criterion by default; other criteria
    # sets can be created as methods for the qft.criteria function.



#     # Check for valid output argument
#     if(!(output %in% names(interp.table))){
#         "An invalid output option was specified."}

    # Check for equal vector lengths - throw error if not equal
    if(any(!isTRUE(all.equal(length(nil), length(tb))),
           !isTRUE(all.equal(length(nil), length(mito)))
       )){stop(
    "The vectors of TB, nil, and mitogen values must all be the same length.")}


    # Check for numeric results - throw error if non-numeric
    if(any(!is.numeric(nil),
           !is.numeric(tb),
           !is.numeric(mito))){stop(
           "The vectors of TB, nil, and mitogen values must all be numeric.")}


    # Check for positive results - warn if negatives
    if(any(nil < 0)){warning("One or more nil values are negative - that probably shouldn't happen!")}
    if(any(tb < 0)){warning("One or more tb values are negative - that probably shouldn't happen!")}
    if(any(mito < 0)){warning("One or more mito values are negative - that probably shouldn't happen!")}


    # Censor to 10
    nil.cens <- nil
    nil.cens[nil.cens > 10] <- 10
    if(any(nil > 10)){warning("One more nil values were greater than 10 IU/mL and have been censored to 10 IU/mL.")}

    tb.cens <- tb
    tb.cens[tb.cens > 10] <- 10
    if(any(tb > 10)){warning("One more tb values were greater than 10 IU/mL and have been censored to 10 IU/mL.")}
    
    mito.cens <- mito
    mito.cens[mito.cens > 10] <- 10
    if(any(mito > 10)){warning("One more mito values were greater than 10 IU/mL and have been censored to 10 IU/mL.")}


    # Set up the interpretation object
    interp.this <- data.frame(nil = nil.cens,
                              tb = tb.cens,
                              mito = mito.cens
    )                                

    # Set the class so that the generic function knows which method to call
    class(interp.this) <- c("data.frame", criteria)

    # Call the generic function to apply the appropriate criteria
    res <- qft.criteria(interp.this)

    # Pare down the output as requested
    res.out <- if(output %in% "onechar"){substr(res, 1, 1)} else
                  if(output %in% "terse"){gsub(res, 
                                               pattern = " .*$", 
                                               replace = "")} else 
                     if(output %in% "verbose"){res}

    return(res.out)


}


# Define the generic function
qft.criteria <- function(interp.this){
    UseMethod("qft.criteria", interp.this)
}


qft.criteria.cellestis.usa <- function(qft.obj){
    # This method calculates the QFT interpretation based on
    # Cellestis' American criteria

    # Floating point comparisons can be a problem here.
    # Instead of >=, define a small value and add it to the number 
    # being compared.
    # In essence, convert any left-hand value that's truly equal
    # to the right-hand # value into one that is *greater* 
    # than the right-hand value.
    # This is the tolerance value used by all.equal().
    tol <- .Machine$double.eps ^ 0.5

    
    # Set up the results vector
    result <- rep(NA, times = length(qft.obj$nil)) 

    # Compute the results
    # Indeterminate due to high nil
    result[is.na(result) &
           qft.obj$nil + tol > 8.0] <- "Indeterminate - high nil"

    # Positive
    result[is.na(result) &
           (qft.obj$tb - qft.obj$nil + tol > 0.35) & 
           (qft.obj$tb - qft.obj$nil + tol > .25 * qft.obj$nil)] <- "Positive"

    # Negative
    result[is.na(result) & 
           (qft.obj$tb - qft.obj$nil + tol < 0.35 | 
            qft.obj$tb - qft.obj$nil + tol < .25 * qft.obj$nil) &
            !(qft.obj$mito - qft.obj$nil + tol < 0.5)] <- "Negative"

    # Indeterminate due to nil ~ mitogen
    result[is.na(result) &
           ((qft.obj$tb - qft.obj$nil + tol < 0.35 | 
             qft.obj$tb - qft.obj$nil + tol < .25 * qft.obj$nil) &
             qft.obj$mito - qft.obj$nil + tol < 0.5)] <- 
                 "Indeterminate - mitogen too close to nil"
  
    return(result)

}





qft.criteria.cellestis.aus <- function(qft.obj){
    # This method calculates the QFT interpretation based on
    # Cellestis' Australian criteria

    # Floating point comparisons can be a problem here.
    # Instead of >=, define a small value and add it to the number 
    # being compared.
    # In essence, convert any left-hand value that's truly equal
    # to the right-hand # value into one that is *greater* 
    # than the right-hand value.
    # This is the tolerance value used by all.equal().
    tol <- .Machine$double.eps ^ 0.5

    
    # Set up the results vector
    result <- rep(NA, times = length(qft.obj$nil)) 

    # Compute the results
    # Indeterminate due to high nil
    result[is.na(result) &
           qft.obj$nil + tol > 8.0] <- "Indeterminate - high nil"

    # Positive
    result[is.na(result) &
           (qft.obj$tb - qft.obj$nil + tol > 0.35) & 
           (qft.obj$tb - qft.obj$nil + tol > .25 * qft.obj$nil)] <- "Positive"

    # Negative
    result[is.na(result) & 
           (qft.obj$tb - qft.obj$nil + tol < 0.35 | 
            qft.obj$tb - qft.obj$nil + tol < .25 * qft.obj$nil)] <- "Negative"

    # No indeterminate due to nil ~ mitogen
  
    return(result)

}





