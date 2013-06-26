#' Confirm that the input values are integers.
#'
#' This function validates whether the input values are whole integers or not.
#' It does this by rounding the input values to their nearest whole integer and
#' then evaluating the difference. If the difference is very, very small
#' (the default is \code{.Machine$double.eps^0.5}), the numbers will be
#' accepted as whole.
#' 
#' All that nonsense is required to avoid floating point comparison errors.
#' 
#' @param x A vector of numeric values to be tested.
#' @param tol The maximum allowable difference between an input number and the
#'        nearest whole number.
#'
#' @return Returns a TRUE if all of the values are integers, FALSE if one or
#'     more are not. Drawn directly from the examples in the 
#'     \code{\link{is.integer}} documentation.
#' 
#' @seealso \code{\link{is.integer}}
#' 

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
    abs(x - round(x)) < tol
}


