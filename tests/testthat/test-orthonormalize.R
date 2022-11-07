# Project:   pcr-non-normal
# Objective: Test functions for orthonormalizations work correctly
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07

context("orthonormalization")

# Set seed
set.seed(20220105)

# Generate some random X
n <- 1e3
p <- 4
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# Orthogonalize it
O <- orthmat(X)
OtO <- round(t(O) %*% O, 3)

# Normalize it
N <- normmat(O)
NtN <- round(t(N) %*% N, 3)

# Tests
test_that("Matrix is orthogonal", {
  expect_true(all(OtO[upper.tri(matrix(NA, p, p))] == 0), 1)
})
test_that("Matrix is normalized", {
  expect_true(all.equal(diag(NtN), rep(1, p)), 1)
})