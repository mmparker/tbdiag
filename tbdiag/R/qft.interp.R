

################################################################################
# Set up a coordinating script to check data and call the generic
# criteria function

qft.interp <- function(nil, tb, mito,
                       criteria = "cellestis.usa",
                       verbosity = "verbose"){

    # Given vectors of nil, TB antigen, and mitogen results in IU/mL,
    # this function computes QFT qualitative interpretations.  The function
    # uses the Cellestis USA criterion by default; other criteria
    # sets can be created as methods for the qft.criteria function.


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


    # Censor to 10 IU/mL
    nil.cens <- qft.cens(nil)
    tb.cens <- qft.cens(tb)
    mito.cens <- qft.cens(mito)


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
    res.out <- trim.qft.output(res, verbosity)

    return(res.out)


}






################################################################################
# Define the generic function
# The criteria argument from qft.interp sets the class of interp.this,
# which in turn determines which criteria set is dispatched by qft.criteria
qft.criteria <- function(interp.this){
    UseMethod("qft.criteria", interp.this)
}






################################################################################
# Helper function: censor values greater than 10
qft.cens <- function(x){
    if(any(x > 10)){
        x.cens <- x
        x.cens[x.cens > 10] <- 10
        warning("One or more values were greater than 10 IU/mL and have been censored to 10 IU/mL.")
        return(x.cens)
    } else return(x)
}








################################################################################
# Helper function: trim output values to the requested verbosity
trim.qft.output <- function(res, verbosity = "terse"){

    # Check for a valid verbosity argument; if not valid, 
    # default to terse and warn
    if(!verbosity %in% c("onechar", "terse", "verbose")){
        warning("'", verbosity, "' is not a valid choice for result verbosity; defaulting to 'terse'.")
        verbosity <- "terse"
    }

    
    # If verbosity is "onechar", output just the first character of result
    if(verbosity %in% "onechar"){substr(res, 1, 1)} else

        # If verbosity is "terse", just the first word
        if(verbosity %in% "terse"){gsub(res, 
                                        pattern = " .*$", 
                                        replace = "")} else 

            # If verbose, indeterminates indicate which criteria
            if(verbosity %in% "verbose"){res}
}
