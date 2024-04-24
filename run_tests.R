library(testthat)
testthat::test_dir("./R", filter="*._test.R")  # TODO: this doesn't work
testthat::test_file("./R/make_plot_test.R")
