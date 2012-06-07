


#' Interpret the results of the Cellestis Quantiferon Gold In-Tube assay for latent tuberculosis infection.

#' Given vectors of nil, TB antigen, and mitogen results in IU/mL,
#' this function computes QFT qualitative interpretations.  The function
#' uses the Cellestis USA criterion by default; other criteria
#' sets can be created as methods for the qft.criteria function.

#' @include equal.lengths.r
#' @include qft.cens.r

#' @export

qft.interp <- function(nil, tb, mito,
                       criteria = "cellestis.usa",
                       verbosity = "verbose",
                       ...){

#' @param nil A vector of nil results (in IU/mL)
#' @param tb A vector of TB antigen results (in IU/mL)
#' @param mito A vector of mitogen results (in IU/mL)
#' @param criteria The name of the desired result criteria (defaults to the Cellestis criteria for the United States).
#' @param verbosity The level of verbosity ("onechar", "terse", "verbose") of the output.
#' @param ... Other arguments passed to the crtieria evaluation function chosen by the "criteria" argument.



    # Check for equal vector lengths - throw error if not equal
    equal.lengths(nil, tb, mito, ...)


    # Check for numeric results - throw error if non-numeric
    if(any(!is.numeric(nil),
           !is.numeric(tb),
           !is.numeric(mito))){stop(
           "The vectors of TB, nil, and mitogen values must all be numeric.")}


    # Check that input values are positive - warn if negative
    if(any(nil < 0, na.rm = TRUE)){warning("One or more nil values are negative - that probably shouldn't happen!")}
    if(any(tb < 0, na.rm = TRUE)){warning("One or more tb values are negative - that probably shouldn't happen!")}
    if(any(mito < 0, na.rm = TRUE)){warning("One or more mito values are negative - that probably shouldn't happen!")}


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
    res.out <- trim.output(res, verbosity)

#' @return The function returns a vector of qualitative assay results according to the criteria chosen using the "criteria" argument.  The verbosity of the results depends on the value passed to the "verbosity" argument: 
#' \item{onechar }{Returns a single character indicating the result (N for Negative, P for Positive, I for Indeterminate).} 
#' \item{terse }{Returns a single word indicating the result (Negative, Positive, Indeterminate).} 
#' \item{verbose }{Returns the same results as "terse", with the addition of a short comment indicating the reason for an "Indeterminate" result.}
        
    return(res.out)


}


#' @references Cellestis <http://www.cellestis.com/> 

#' @note This function is provided purely as a convenience and is not a replacement for manual interpretation, manufacturer-provided software, or common sense.  Absolutely not for clinical use. 

#' @seealso \code{\link{tspot.interp}} for TSPOT.TB interpretation. 


################################################################################
# Define the generic function
# The criteria argument from qft.interp sets the class of interp.this,
# which in turn determines which criteria set is dispatched by qft.criteria
qft.criteria <- function(interp.this){
    UseMethod("qft.criteria", interp.this)
}


