

################################################################################
# Helper function: check for equal length of all input vectors
equal.lengths <- function(...){

    vec.list <- list(...)

    # Test that at least one vector was supplied
    if(length(vec.list) < 1){stop("No vectors supplied.")}

    # Warn if only one vector was supplied
    if(length(vec.list) == 1){warning("Only one vector supplied - no basis for comparison of vector lengths.")}

    # Throw error if vectors are of unequal lengths
    if(!all(sapply(vec.list, length) == length(vec.list[[1]]))){
        stop("All input vectors must be the same length.")
    }

}

