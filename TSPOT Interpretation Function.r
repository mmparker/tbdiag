
tspot.interp <- function(nil, panel_a, panel_b, mitogen){
# Given vectors of nil, TB antigen (panels A and B), and mitogen results in spots,
# this function computes TSPOT qualitative interpretations.  The function
# uses the Oxford Immunotec North America criterion by default;
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

    # Iterate through each test.
    for(i in seq_along(result)){
        # If any test value is NA, no interpretation - skip
        if(is.na(panel_a[i]) | is.na(panel_b[i]) | is.na(nil[i]) | is.na(mitogen[i])) {next}

        if(nil[i] > 10){result[i] <- "Invalid - high nil"} else {
          
          if(max(panel_a[i] - nil[i], panel_b[i] - nil[i]) %in% c(5, 6, 7)){
            result[i] <- "Borderline"} else {
            
              if((panel_a[i] - nil[i]) >= 8 | (panel_b[i] - nil[i]) >= 8) {result[i] <- "Positive"} else {
                
                if((panel_a[i] - nil[i]) <= 4 & (panel_b[i] - nil[i]) <= 4 & mitogen[i] >= 20)
                  {result[i] <- "Negative"} else {
                  
                  if((panel_a[i] - nil[i]) <= 4 & (panel_b[i] - nil[i]) <= 4 & mitogen[i] < 20)
                    {result[i] <- "Invalid - low mitogen"} else {
                  
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


# Write a test case sometime.     
# Possible values for things: NA, 1:21, text, factor elements, very high numbers,
# negative numbers...         

#Convert to allow varying cutoffs and borderline values     
  

