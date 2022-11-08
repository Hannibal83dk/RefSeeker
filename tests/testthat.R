# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(RefSeeker)

# Setting HTTPUserAgent to prevent 406 unacceptable error when downloading Normfinder during tests
options("HTTPUserAgent" = "RStudio Desktop (2022.7.1.554)")

test_check("RefSeeker")
