

library(devtools)
library(testthat)
library(roxygen2)

# Identify the package path
pkg.path <- "tbdiag"

# Run roxygen to update documentation
roxygenise(pkg.path)

# Load the tbdiag code
load_all(pkg.path, reset = TRUE)

# Run all tests in inst/tests/
test(pkg.path)

