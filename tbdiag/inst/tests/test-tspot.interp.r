


# Set a tolerance for numeric comparisons
tol <- .Machine$double.eps ^ 0.5


# TODO: iterate over available methods

################################################################################
# tspot.interp throws an error when given unequal vectors
test_that("tspot.interp throws an error when given unequal vectors", {

expect_that(tspot.interp(nil = 1:9, 
                         panel_a = 1:10, 
                         panel_b = 1:10, 
                         mito = 1:10), 
            throws_error()
)

expect_that(tspot.interp(nil = 1:10, 
                         panel_a = 1:9, 
                         panel_b = 1:10, 
                         mito = 1:10), 
            throws_error()
)

expect_that(tspot.interp(nil = 1:10, 
                         panel_a = 1:10, 
                         panel_b = 1:9, 
                         mito = 1:10), 
            throws_error()
)

expect_that(tspot.interp(nil = 1:10, 
                         panel_a = 1:10, 
                         panel_b = 1:10, 
                         mito = 1:9), 
            throws_error()
)

})

################################################################################
# tspot.interp throws an error when given non-numeric vectors
test_that("tspot.interp throws an error when given non-numeric vectors", {

expect_that(tspot.interp(letters[1:10], 1:10, 1:10), throws_error())
expect_that(tspot.interp(1:10, letters[1:10], 1:10), throws_error())
expect_that(tspot.interp(1:10, 1:10, letters[1:10]), throws_error())

})

################################################################################
# tspot.interp warns if given any negative values, and warning mentions which
# input variable has the problem
test_that("tspot.interp warns if given any negative values, and warning mentions which input variable has the problem", {

expect_that(tspot.interp(nil = 0 - tol, 
                         panel_a = 0, 
                         panel_b = 0, 
                         mito = 0), 
            gives_warning("nil")
)

expect_that(tspot.interp(nil = 0, 
                         panel_a = 0 - tol, 
                         panel_b = 0, 
                         mito = 0), 
            gives_warning("panel_a")
)

expect_that(tspot.interp(nil = 0, 
                         panel_a = 0, 
                         panel_b = 0 - tol, 
                         mito = 0), 
            gives_warning("panel_b")
)

expect_that(tspot.interp(nil = 0, 
                         panel_a = 0, 
                         panel_b = 0, 
                         mito = 0 - tol), 
            gives_warning("mito")
)

})


################################################################################
# tspot.interp warns if given values > 20
test_that("tspot.interp warns if given values > 20", {

expect_that(tspot.interp(nil = 20 + tol, 
                         panel_a = 20, 
                         panel_b = 20, 
                         mito = 20), 
            gives_warning()
)

expect_that(tspot.interp(nil = 20, 
                         panel_a = 20 + tol, 
                         panel_b = 20, 
                         mito = 20), 
            gives_warning()
)

expect_that(tspot.interp(nil = 20, 
                         panel_a = 20, 
                         panel_b = 20 + tol, 
                         mito = 20), 
            gives_warning()
)

expect_that(tspot.interp(nil = 20, 
                         panel_a = 20, 
                         panel_b = 20, 
                         mito = 20 + tol), 
            gives_warning()
)

})



################################################################################
test_that("tspot.cens warns and censors if given values > 20", {

# tspot.cens warns if given values > 20
expect_that(tspot.cens(20 + tol), gives_warning())

# tspot.cens censors if given values > 20
expect_that(tspot.cens(20 + tol), equals(20))

})


################################################################################
# tspot.interp returns a vector of acceptable results
test_that("tspot.interp returns a vector of acceptable results", {

expect_that(tspot.interp(1:10, 1:10, 1:10, 1:10), is_a("character"))

})



################################################################################
# If nil, panel_a, panel_b or mito is NA, tspot.interp returns an NA




