#This function takes values of nil tube, Panel A and Panel B, PHA/mitogen, and returns
#Positive, Negative, Indeterminate, or Failed.
tspot.interp <- function(nil, panel_a, panel_b, mitogen){
    #Check for equal vector lengths
    if(!isTRUE(all.equal(length(nil), length(panel_a), length(panel_b), length(mitogen)))){stop("The vectors of nil, Panel A, Panel B, and mitogen values must all be the same length.")}
    
    #Setup the results vector
    result <- rep(NA, times = length(nil)) 

    #Iterate through each test.
    for(i in seq_along(result)){
        #If any test value is NA, no interpretation - skip
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


#Development note: This algorithm is based off of the TSPOT.TB Package Insert from 
#Oxford Immunotec (document ID looks to be PI-TB-US-V1).  


#Write a test case sometime.     
#Possible values for things: NA, 1:21, text, factor elements, very high numbers,
#negative numbers...              
  

