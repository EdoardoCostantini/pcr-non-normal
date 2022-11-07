# Project:   pcr-non-normal
# Objective: Test script for generateDV.R
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07

context("generateDV")

# Set seed
set.seed(20220105)

# Generate some X
n <- 1e3
p <- 4
X <- matrix(rnorm(n * p), nrow = n, ncol = p)

# Define a target R2
R2 <- 0.3

# Generate y
y <- generateDV(X = X, R2 = R2, beta = 1)

# Fit linear model
lm_out <- lm(y ~ -1 + X)
lm_sum <- summary(lm_out)

# Tests
test_that("R2 in y ~ X equal to target", {
  expect_true(abs(R2 - lm_sum$r.squared) < .01, 1)
})