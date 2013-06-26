# This function censors TSPOT result values greater than 20 spots to 20 spots,
# per Oxford Immunotec's instructions

#' Censor TSPOT quantitative results to 20 spots.
#'
#' This functions censors a vector of quantitative results of the 
#' TSPOT (nil, Panel A, Panel B, mitogen) from counts greater than 20 spots to
#' 20 spots, following the Oxford test insert instructions.
#' Intended for internal use only.
#' 
#' @param x A vector of numeric results from the TSPOT
#'
#' @return The input vector with all results greater than 20 censored to 20.
#' 
#' @seealso \code{\link{qft.cens}}
#' 

tspot.cens <- function(x){
    if(any(x > 20, na.rm = TRUE)){

        x.cens <- x
        x.cens[x.cens > 20] <- 20

        warning("One or more values were greater than 20 spots and have been censored to 20 spots.")

        return(x.cens)

    } else return(x)
}
