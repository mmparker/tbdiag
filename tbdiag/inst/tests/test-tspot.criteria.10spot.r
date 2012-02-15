


################################################################################
# Standard negatives
test_that("Typical results for negatives are returned as negatives", {

# Low
expect_that(tspot.criteria.10spot(data.frame(nil = 1, 
                                             panel_a = 1, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Negative")
)

# Just below borderline - Panel A
expect_that(tspot.criteria.10spot(data.frame(nil = 1, 
                                             panel_a = 5, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Negative")
)

# Just below borderline - Panel B
expect_that(tspot.criteria.10spot(data.frame(nil = 1, 
                                             panel_a = 1, 
                                             panel_b = 5, 
                                             mito = 20)), 
            matches("Negative")
)


# Slightly higher
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 9, 
                                             panel_b = 9, 
                                             mito = 20)), 
            matches("Negative")
)


# Nil can be higher than TB
expect_that(tspot.criteria.10spot(data.frame(nil = 8, 
                                             panel_a = 1, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Negative")
)

})


################################################################################
# Standard positives
test_that("Typical results for positives are returned as positives", {

# Low edge of positive - Panel A
expect_that(tspot.criteria.10spot(data.frame(nil = 1, 
                                             panel_a = 11, 
                                             panel_b = 6, 
                                             mito = 20)), 
            matches("Positive")
)

# Low edge of positive - Panel B
expect_that(tspot.criteria.10spot(data.frame(nil = 1, 
                                             panel_a = 6, 
                                             panel_b = 11, 
                                             mito = 20)), 
            matches("Positive")
)

# Higher - Panel A
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 20, 
                                             panel_b = 6, 
                                             mito = 20)), 
            matches("Positive")
)

# Higher - Panel B
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 6, 
                                             panel_b = 20, 
                                             mito = 20)), 
            matches("Positive")
)


# Even when mito < 20
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 6, 
                                             panel_b = 16, 
                                             mito = 10)), 
            matches("Positive")
)




})



################################################################################
# Borderline

# 5 spots - Panel A
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 10, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Borderline")
)


# 6 spots - Panel A
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 11, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Borderline")
)


# 7 spots - Panel A
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 12, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Borderline")
)


# 8 spots - Panel A
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 13, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Borderline")
)



# 9 spots - Panel A
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 14, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Borderline")
)



# 5 spots - Panel B
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 1, 
                                             panel_b = 10, 
                                             mito = 20)), 
            matches("Borderline")
)


# 6 spots - Panel B
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 1, 
                                             panel_b = 11, 
                                             mito = 20)), 
            matches("Borderline")
)


# 7 spots - Panel B
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 1, 
                                             panel_b = 12, 
                                             mito = 20)), 
            matches("Borderline")
)


# 8 spots - Panel B
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 1, 
                                             panel_b = 13, 
                                             mito = 20)), 
            matches("Borderline")
)


# 9 spots - Panel B
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 1, 
                                             panel_b = 14, 
                                             mito = 20)), 
            matches("Borderline")
)


# Even when mito < 20
expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 12, 
                                             panel_b = 1, 
                                             mito = 10)), 
            matches("Borderline")
)

expect_that(tspot.criteria.10spot(data.frame(nil = 5, 
                                             panel_a = 1, 
                                             panel_b = 12, 
                                             mito = 10)), 
            matches("Borderline")
)





################################################################################
# Invalid - high nil
test_that("Typical results for high-nil invalids are returned as high-nil invalids", {

# Nil over 10, everything else standard
expect_that(tspot.criteria.10spot(data.frame(nil = 11, 
                                             panel_a = 1, 
                                             panel_b = 1, 
                                             mito = 20)), 
            matches("Invalid - high nil")
)

# Takes precedence over low mitogen
expect_that(tspot.criteria.10spot(data.frame(nil = 11, 
                                             panel_a = 1, 
                                             panel_b = 1, 
                                             mito = 10)), 
            matches("Invalid - high nil")
)

})


################################################################################
# Invalid - low mito
test_that("Typical results for low-mito invalids are returned as low-mito invalids", {

# Very low
expect_that(tspot.criteria.10spot(data.frame(nil = 1, 
                                             panel_a = 1, 
                                             panel_b = 1, 
                                             mito = 10)), 
            matches("Invalid - low mitogen")
)

# At the threshold
expect_that(tspot.criteria.10spot(data.frame(nil = 1, 
                                             panel_a = 1, 
                                             panel_b = 1, 
                                             mito = 19)), 
            matches("Invalid - low mitogen")
)

})





