

################################################################################
# Helper function: check for equal length of all input vectors
equal.lengths <- function(...){

    vec.list <- list(...)
    if(!all(sapply(vec.list, length) == length(vec.list[[1]]))){
        stop("All input vectors must be the same length.")
    }

}

