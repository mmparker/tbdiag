
tspot.criteria.oxford.usa <- function(tspot.obj){

    
    # Set up the results vector
    result <- rep(NA, times = length(tspot.obj$nil)) 

    # Identify the maximum of Panel A - Nil and Panel B - Nil
    panel.max <- ifelse((panel_a - tspot.obj$nil) > (panel_b - tspot.obj$nil),
                        yes = (panel_a - tspot.obj$nil), 
                        no = (panel_b - tspot.obj$nil)
    )

    # Compute the results
    result[is.na(result) &
           tspot.obj$nil > 10] <- "Invalid - high nil"

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

