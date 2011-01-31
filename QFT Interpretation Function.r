#This function takes values of nil tube, TB tube, and mitogen scores, and returns Positive, Negative, or Indeterminate.
qft.interp <- function(nil, tb, mitogen){
    #Check for equal vector lengths
    if(!isTRUE(all.equal(length(nil), length(tb), length(mitogen)))){stop("The vectors of TB, nil, and mitogen values must all be the same length.")}
    
    #Setup the results vector
    result <- rep(NA, times = length(nil)) 

    #The floating point comparison will bite you in the ass for some of these.
    #Instead of >=, define a small value and add it to the number being compared
    #In essence, convert any left-hand value that's truly equal to the right-hand
    # value into one that is *greater* than the right-hand value.
    #Set an epsilon that is tiny relative to the numbers being compared.
    epsilon <- 1e-10

    #Iterate through each test.
    for(i in seq_along(result)){
        #If any test value is NA, no interpretation - skip
        if(is.na(tb[i]) | is.na(nil[i]) | is.na(mitogen[i])) {next}

        if(nil[i] + epsilon > 8.0) {result[i] <- "Indeterminate"} else {
            if(tb[i] - nil[i] + epsilon >= 0.35 & tb[i] - nil[i] + epsilon >= .25 * nil[i])
                {result[i] <- "Positive"} else {
                    if((tb[i] - nil[i] + epsilon < 0.35 | tb[i] - nil[i] + epsilon < .25 * nil[i]) &
                        !(mitogen[i] - nil[i] + epsilon < 0.5))
                        {result[i] <- "Negative"} else {
                            if((tb[i] - nil[i] + epsilon < 0.35 | tb[i] - nil[i] + epsilon < .25 * nil[i]) &
                                mitogen[i] - nil[i] + epsilon < 0.5)
                                {result[i] <- "Indeterminate"}
                }
            }
        }
    }
    return(result)
}




#Development notes: This algorithm is based on the flow chart L05995008A Apr 2007 from Cellestis.