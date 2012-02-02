
tspot.criteria.oxford.global <- function(tspot.obj){

    
    # Set up the results vector
    result <- rep(NA, times = length(tspot.obj&nil)) 

    # Identify the maximum of Panel A - Nil and Panel B - Nil
    panel.max <- ifelse((tspot.obj$panel_a - tspot.obj$nil) > 
                             (tspot.obj$panel_b - tspot.obj$nil),
                         yes = (tspot.obj$panel_a - tspot.obj$nil), 
                         no = (tspot.obj$panel_b - tspot.obj$nil)
    )

    # Compute the results
    result[is.na(result) &
           tspot.obj$nil > 10] <- "Invalid - high nil"

    result[is.na(result) &
           panel.max >= 6] <- "Positive"

    result[is.na(result) &
           panel.max <= 5 &
           tspot.obj$mito >= 20] <- "Negative"

    result[is.na(result) &
           panel.max <= 4 &
           tspot.obj$mito < 20] <- "Invalid - low mitogen"

    return(result)
}

