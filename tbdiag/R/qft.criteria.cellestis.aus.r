
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
