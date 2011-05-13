
tspot.interp <- function(nil, panel_a, panel_b, mitogen){
# Given vectors of nil, TB antigen (panels A and B), and mitogen results
# in spots, this function computes TSPOT qualitative interpretations.
# The function uses the Oxford Immunotec North America criterion by default;
# alternative TB - nil cutoffs can be specified using the tbnil.cutoff
# argument.  Further changes to the criteria require modification of the
# function.

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
    
    # Setup the results vector
    result <- rep(NA, times = length(nil)) 

    # Identify the maximum of Panel A - Nil and Panel B - Nil
    panel.max <- ifelse((panel_a - nil) > (panel_b - nil),
                        yes = (panel_a - nil), no = (panel_b - nil)
    )

    # Compute the results
    result[is.na(result) &
           nil > 10] <- "Invalid - high nil"

    result[is.na(result) &
           panel.max %in% c(5, 6, 7)] <- "Borderline"

    result[is.na(result) &
           panel.max >= 8] <- "Positive"

    result[is.na(result) &
           panel.max <= 4 &
           mitogen >= 20] <- "Negative"

    result[is.na(result) &
           panel.max <= 4 &
           mitogen < 20] <- "Invalid - low mitogen"

    return(result)
}


# Development note: This algorithm is based off of the TSPOT.TB Package Insert from 
# Oxford Immunotec (document ID looks to be PI-TB-US-V1).  


# Write a test case sometime.     
# Possible values for things: NA, 1:21, text, factor elements, very high
# numbers, negative numbers...         

#Convert to allow varying cutoffs and borderline values     
  

