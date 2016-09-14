#' Censor Quantiferon quantitative results to 10 IU/mL.
#'
#' This functions censors a vector of quantitative results of the 
#' Quantiferon (nil, antigen, mitogen) from values greater than 10 IU/mL to
#' 10 IU/mL, following the Cellestis test insert instructions.
#' Intended for internal use only.
#' 
#' @param x A vector of numeric results from the Quantiferon
#'
#' @return The input vector with all results greater than 10 censored to 10.
#' 
#' @seealso \code{\link{tspot.cens}}
#' 

qft.cens <- function(x){
    # If any values are greater than 10, censor those values to 10
    if(any(x > 10, na.rm = TRUE)){

        x.cens <- x
        x.cens[x.cens > 10] <- 10

        warning("One or more values were greater than 10 IU/mL and have been censored to 10 IU/mL.")

        return(x.cens)

    } else return(x)
}
