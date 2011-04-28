
tspot.interp <- function(nil, panel_a, panel_b, mitogen, output = "terse"){
# Given vectors of nil, TB antigen (panels A and B), and mitogen results
# in spots, this function computes TSPOT qualitative interpretations.
# The function uses the Oxford Immunotec North America criterion by default;
# alternative TB - nil cutoffs can be specified using the tbnil.cutoff
# argument.  Further changes to the criteria require modification of the
# function.

    # Set up the interpretation table with terse and verbose messages
    interp.table <- data.frame(onechar = c("I", "B", "P", "N", "I"),
                               terse = c("Invalid", "Borderline", "Positive", "Negative", "Invalid"),
                               verbose = c("Invalid - high nil", "Borderline", "Positive", "Negative", 
                                           "Invalid - low mitogen")
                               )

    # Check for valid output argument
    if(!(output %in% names(interp.table))){"An invalid output option was specified."}


    # Check for equal vector lengths
    if(!isTRUE(all.equal(length(nil), length(panel_a)) |
               all.equal(length(nil), length(panel_b)) |
               all.equal(length(nil), length(mitogen)))){stop(
               "The vectors of nil, Panel A, Panel B, and mitogen values must all be the same length.")}
    
    # Check for numeric results
    if(!isTRUE(is.numeric(nil) &
               is.numeric(panel_a) &
               is.numeric(panel_b) &
               is.numeric(mitogen))){stop(
               "The vectors of nil, TB - Panel A, TB - Panel B, and mitogen values must all be numeric.")}

    
    # Set up the results vector
    result <- rep(NA, times = length(nil)) 

    # Iterate through each test.
    for(i in seq_along(result)){
        # If any test value is NA, no interpretation - skip
        if(is.na(panel_a[i]) | is.na(panel_b[i]) | is.na(nil[i]) | is.na(mitogen[i])) {next}

        if(nil[i] > 10){result[i] <- interp.table[1, output]} else {
          
          if(max(panel_a[i] - nil[i], panel_b[i] - nil[i]) %in% c(5, 6, 7)){
            result[i] <- interp.table[2, output]} else {
            
              if((panel_a[i] - nil[i]) >= 8 | (panel_b[i] - nil[i]) >= 8) {result[i] <- interp.table[3, output]} else {
                
                if((panel_a[i] - nil[i]) <= 4 & (panel_b[i] - nil[i]) <= 4 & mitogen[i] >= 20)
                  {result[i] <- interp.table[4, output]} else {
                  
                  if((panel_a[i] - nil[i]) <= 4 & (panel_b[i] - nil[i]) <= 4 & mitogen[i] < 20)
                    {result[i] <- interp.table[5, output]} else {
                  
                    result[i] <- "Don't know what to say"}
            
                }
              }
            }
          }
        }
    return(result)
}


# Development note: This algorithm is based off of the TSPOT.TB Package Insert from 
# Oxford Immunotec (document ID looks to be PI-TB-US-V1).  

