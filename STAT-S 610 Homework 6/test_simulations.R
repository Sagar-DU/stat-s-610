library(testthat)

context("Check simulations")
source("simulate_reads.R")
test_that("true seq creation function works", {
  expect_true(all(colSums(make_true_seqs(10, 5)) > 0))
  expect_true(all(colSums(make_true_seqs(20, 3)) < 3))
  expect_true(all(make_true_seqs(15, 4) %in% c(0,1)))
})

# NEW TESTS FOR NONZERO FEATURE
test_that("true seq creation with nonzero specification works", {
  # Test that exactly 2 nonzero entries appear in each column
  result <- make_true_seqs(10, 5, n_nonzero = 2)
  expect_true(all(colSums(result) == 2))
  
  # Test that exactly 1 nonzero entry appears in each column  
  result <- make_true_seqs(8, 4, n_nonzero = 1)
  expect_true(all(colSums(result) == 1))
  
  # Test edge case: all entries could be nonzero
  result <- make_true_seqs(5, 3, n_nonzero = 3)
  expect_true(all(colSums(result) == 3))
  
  # Test that invalid n_nonzero values are handled
  expect_error(make_true_seqs(5, 3, n_nonzero = 4)) # Can't have more nonzero than rows
  expect_error(make_true_seqs(5, 3, n_nonzero = 0)) # Can't have zero nonzero (would be all zeros)
})