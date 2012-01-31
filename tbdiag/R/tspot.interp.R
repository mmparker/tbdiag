

# Set up a coordinating function to check data and call the generic criteria
# function

tspot.interp <- function(nil, panel_a, panel_b, mito,
                         criteria = "oxford.usa",
                         output = "terse"){


    # Given vectors of nil, TB antigen (panels A and B), and mitogen results
    # in spots, this function computes TSPOT qualitative interpretations.
    # The function uses the Oxford Immunotec North America criterion by default;
    # alternative criteria sets can be created as methods for the
    # tspots.criteria function


    # Check for equal vector lengths - throw error if not equal
    if(any(!isTRUE(all.equal(length(nil), length(panel_a))),
           !isTRUE(all.equal(length(nil), length(panel_b))),
           !isTRUE(all.equal(length(nil), length(mito)))
       )){stop(
    "The vectors of TB, nil, and mitogen values must all be the same length.")}


    # Check for numeric results - throw error if non-numeric
    if(any(!is.numeric(nil),
           !is.numeric(panel_a),
           !is.numeric(panel_b),
           !is.numeric(mito))){stop(
           "The vectors of TB, nil, and mitogen values must all be numeric.")}


    # Check for positive results - warn if negatives
    if(any(nil < 0)){warning("One or more nil values are negative - that probably shouldn't happen!")}
    if(any(panel_a < 0)){warning("One or more panel_a values are negative - that probably shouldn't happen!")}
    if(any(panel_b < 0)){warning("One or more panel_b values are negative - that probably shouldn't happen!")}
    if(any(mito < 0)){warning("One or more mito values are negative - that probably shouldn't happen!")}

    # Check for non-integer results - warn if decimal
    if(any(!is.wholenumber(nil))){warning("One or more nil values aren't integers - that probably shouldn't happen!  See ?is.integer for help.")}
    if(any(!is.wholenumber(panel_a))){warning("One or more panel_a values aren't integers - that probably shouldn't happen!  See ?is.integer for help.")}
    if(any(!is.wholenumber(panel_b))){warning("One or more panel_b values aren't integers - that probably shouldn't happen!  See ?is.integer for help.")}
    if(any(!is.wholenumber(mito))){warning("One or more mito values aren't integers - that probably shouldn't happen!  See ?is.integer for help.")}


    # Censor to 20 spots
    nil.cens <- nil
    nil.cens[nil.cens > 20] <- 20
    if(any(nil > 20)){warning("One or more nil values were greater than 20 spots and have been censored to 20 spots.")}

    panel_a.cens <- panel_a
    panel_a.cens[panel_a.cens > 20] <- 20
    if(any(panel_a > 20)){warning("One or more panel_a values were greater than 20 spots and have been censored to 20 spots.")}

    panel_b.cens <- panel_b
    panel_b.cens[panel_b.cens > 20] <- 20
    if(any(panel_b > 20)){warning("One or more panel_b values were greater than 20 spots and have been censored to 20 spots.")}

    mito.cens <- mito
    mito.cens[mito.cens > 20] <- 20
    if(any(mito > 20)){warning("One or more mito values were greater than 20 spots and have been censored to 20 spots.")}


    # Set up the interpretation object
    interp.this <- data.frame(nil = nil.cens,
                              panel_a = panel_a.cens,
                              panel_b = panel_b.cens,
                              mito = mito.cens
    )

    # Set the class so that the generic function knows which method to call
    class(interp.this) <- c("data.frame", criteria)

    # Call the generic function to apply the appropriate criteria
    res <- tspot.criteria(interp.this)

    # Pare down the output as requested
    res.out <- if(output %in% "onechar"){substr(res, 1, 1)} else
                  if(output %in% "terse"){gsub(res, 
                                               pattern = " .*$", 
                                               replace = "")} else 
                     if(output %in% "verbose"){res}

    return(res.out)

}



tspot.criteria <- function(interp.this){
    UseMethod("tspot.criteria", interp.this)
}


tspot.criteria.oxford.usa <- function(interp.this){

    
    # Set up the results vector
    result <- rep(NA, times = length(nil)) 

    # Identify the maximum of Panel A - Nil and Panel B - Nil
    panel.max <- ifelse((panel_a - nil) > (panel_b - nil),
                        yes = (panel_a - nil), no = (panel_b - nil)
    )

    # Compute the results
    result[is.na(result) &
           nil > 10] <- "Invalid - high nil"

    result[is.na(result) &
           panel.max %in% c(5, 6, 7)] <- "Borderline"

    result[is.na(result) &
           panel.max >= 8] <- "Positive"

    result[is.na(result) &
           panel.max <= 4 &
           mitogen >= 20] <- "Negative"

    result[is.na(result) &
           panel.max <= 4 &
           mitogen < 20] <- "Invalid - low mitogen"

    return(result)
}




# Function to check for integerness of spots - see ?is.integer for explanation
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
    abs(x - round(x)) < tol
}
