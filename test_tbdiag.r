

library(devtools)
library(testthat)

# Identify the package path
pkg.path <- "tbdiag"





# Load the tbdiag code
load_all(pkg.path, reset = TRUE)

# Run all tests in inst/tests/
test(pkg.path)

# Update the documentation
document(pkg.path)



# Build it
build(pkg.path)


# Check it
check(pkg.path)


# Install it
install(pkg.path)



