

library(devtools)
library(testthat)

# Load the tbdiag code
load_all("tbdiag", reset = TRUE)

# Run all tests in inst/tests/
test("tbdiag")

# Run roxygen to update documentation
# document("tbdiag")
