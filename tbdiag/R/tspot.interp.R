

################################################################################
# Set up a coordinating function to check data and call the generic criteria
# function

tspot.interp <- function(nil, panel_a, panel_b, mito,
                         criteria = "oxford.usa",
                         verbosity = "terse",
                         ...){

    # Given vectors of nil, TB antigen (panels A and B), and mitogen results
    # in spots, this function computes TSPOT qualitative interpretations.
    # The function uses the Oxford Immunotec North America criterion by default;
    # alternative criteria sets can be created as methods for the
    # tspots.criteria function


    # Check for equal vector lengths - throw error if not equal
    equal.lengths(nil, panel_a, panel_b, mito)


    # Check for numeric results - throw error if non-numeric
    if(any(!is.numeric(nil),
           !is.numeric(panel_a),
           !is.numeric(panel_b),
           !is.numeric(mito))){stop(
           "The vectors of TB, nil, and mitogen values must all be numeric.")}


    # Check that input values are positive - warn if negative
    if(any(nil < 0, na.rm = TRUE)){warning("One or more nil values are negative - that probably shouldn't happen!")}
    if(any(panel_a < 0, na.rm = TRUE)){warning("One or more panel_a values are negative - that probably shouldn't happen!")}
    if(any(panel_b < 0, na.rm = TRUE)){warning("One or more panel_b values are negative - that probably shouldn't happen!")}
    if(any(mito < 0, na.rm = TRUE)){warning("One or more mito values are negative - that probably shouldn't happen!")}

    # Check for non-integer results - warn if decimal
    if(any(!is.wholenumber(nil), na.rm = TRUE)){warning("One or more nil values aren't integers - that probably shouldn't happen!")}
    if(any(!is.wholenumber(panel_a), na.rm = TRUE)){warning("One or more panel_a values aren't integers - that probably shouldn't happen!")}
    if(any(!is.wholenumber(panel_b), na.rm = TRUE)){warning("One or more panel_b values aren't integers - that probably shouldn't happen!")}
    if(any(!is.wholenumber(mito), na.rm = TRUE)){warning("One or more mito values aren't integers - that probably shouldn't happen!")}


    # Censor to 20 spots
    nil.cens <- tspot.cens(nil)
    panel_a.cens <- tspot.cens(panel_a)
    panel_b.cens <- tspot.cens(panel_b)
    mito.cens <- tspot.cens(mito)


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
    res.out <- trim.output(res, verbosity)

    return(res.out)

}



################################################################################
tspot.criteria <- function(interp.this){
    UseMethod("tspot.criteria", interp.this)
}



