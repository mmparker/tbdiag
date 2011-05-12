
andrie.interp <- function(nil, tb, mitogen, tbnil.cutoff = 0.35){ 
# Set a tolerance to avoid floating point comparison troubles.
tol <- .Machine$double.eps ^ 0.5 

# Set up the results vector
result <- rep(NA, times = length(nil)) 

result[is.na(result) & nil + tol > 8.0] <- "Indeterminate" 

result[is.na(result) & (tb - nil + tol > tbnil.cutoff) & (tb - nil + tol > .25 * nil)] <- "Positive"

result[is.na(result) & (tb - nil + tol < tbnil.cutoff | tb - nil + tol < .25 * nil) &         
        !(mitogen - nil + tol < 0.5)] <- "Negative"

result[is.na(result) & ((tb - nil + tol < tbnil.cutoff | tb - nil + tol < .25 * nil) &           
        mitogen - nil + tol < 0.5)] <- "Indeterminate"

result 
}
