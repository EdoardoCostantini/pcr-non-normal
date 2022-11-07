# Project:   pcr-non-normal
# Objective: Test script for generateDV.R
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07

context("generateXTP")

# Set up of test
set.seed(20220114)

# Define target parameters
N <- 1e3
P <- 9
CPVE <- 0.9
VAFsum <- 100 # total variance of the components?
VAFr <- c(.5, .4, .2) # variance of each components?
R <- length(VAFr)

# Use function
XTP <- generateXTP(I = N,
                   J = P,
                   VAFsum = VAFsum,
                   VAFr = VAFr,
                   CPVE = CPVE)

# Perform PCA
PCA <- prcomp(XTP$X)

# Extract values to check correct recovery
# Eigen values
eigen_svd <- round(svd(XTP$X)$d^2, 3)
eigen_pca <- round(PCA$sdev^2, 3)

# Explained variance
CPVE_svd <- cumsum(prop.table(eigen_svd))[R]
CPVE_pca <- cumsum(prop.table(eigen_pca))[R]

# Component Scores
T_svd <- round(svd(XTP$X)$u %*% diag(svd(XTP$X)$d), 3)
T_pca <- round(PCA$x, 3)

# Loadings
P_svd <- round(svd(XTP$X)$v, 3)
P_pca <- round(PCA$rotation, 3)

# Tests
test_that("Component scores obtained with SVD and PCA are the same", {
  expect_true(all.equal(T_svd, T_pca, check.attributes = FALSE), 1)
})
test_that("Eigen values obtained with SVD and PCA are the same", {
  expect_true(all.equal(cor(eigen_svd, eigen_pca), 1,
                        tolerance = .01,
                        check.attributes = FALSE), 1)
})
test_that("Target CPVE is obtained", {
  expect_true(abs(CPVE - CPVE_svd) < .05, 1) # equal to target
  expect_true(abs(CPVE_pca - CPVE_svd) < .01, 1) # equal between methods
})
test_that("Loadings obtained with SVD and PCA are the same", {
  expect_true(all.equal(P_svd, P_pca, check.attributes = FALSE), 1)
})