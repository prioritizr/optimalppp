# load packages
library(testthat)
library(optimalppp)

# load solver package
require(cplexAPI)

# run tests
test_check("optimalppp")
