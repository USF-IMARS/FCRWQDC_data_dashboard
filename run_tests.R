library(testthat)
library(here)
testthat::test_dir(here("./test"), filter=NULL)
testthat::test_file(here("./test/test-make_plot.R"))
