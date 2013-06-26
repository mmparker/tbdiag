################################################################################
# Helper function: trim output values to the requested verbosity

#' Truncate Quantiferon and TSPOT qualitative results to desired verbosity.
#' 
#' This function truncates the qualitative results of the 
#' \code{\link{qft.interp}} and \code{\link{tspot.interp}} functions to the
#' users desired level of verbosity: 
#' \itemize{
#' \item a single character (\code{onechar}),
#' \item a terse, one-word result (\code{terse}), or
#' \item a more verbose answer indicating the specific reason for invalid and 
#'       indeterminate tests (\code{verbose}).
#' }
#' 
#' Intended for internal use only.
#' 
#' @param res A vector of qualitative results
#' @param verbosity The desired level of verbosity; defaults to \code{terse}
#' 
#' @return A vector of appropriately-verbose results.



trim.output <- function(res, verbosity = "terse"){

    # Check for a valid verbosity argument; if not valid, 
    # default to terse and warn
    if(!verbosity %in% c("onechar", "terse", "verbose")){
        warning("'", verbosity, "' is not a valid choice for result verbosity; defaulting to 'terse'.")
        verbosity <- "terse"
    }

    
    # If verbosity is "onechar", output just the first character of result
    if(verbosity %in% "onechar"){substr(res, 1, 1)} else

        # If verbosity is "terse", just the first word
        if(verbosity %in% "terse"){gsub(x = res, 
                                        pattern = " .*$", 
                                        replacement = "")} else 

            # If verbose, indeterminates indicate which criteria
            if(verbosity %in% "verbose"){res}
}

