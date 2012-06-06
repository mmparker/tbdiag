

library(devtools)
library(testthat)
library(roxygen2)

pkg.path <- "I:/USERS/MParker/Rpackages/tbdiag/tbdiag"

# Load the tbdiag code
load_all(pkg.path, reset = TRUE)

# Run all tests in inst/tests/
test(pkg.path)

# Run roxygen to update documentation
# document("tbdiag")
roxygenize(pkg.path)
