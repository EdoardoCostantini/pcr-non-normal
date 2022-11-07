# Project:   pcr-non-normal
# Objective: Notes on generating skewed components and items
# Author:    Soogeun Park, Edo
# Created:   2022-11-02
# Modified:  2022-11-02

# Load packages ---------------------------------------------------------------- 

library(sn) # for non-normal sampling

# Sample 2 normal factor scores
set.seed(131)
tmat <- MASS::mvrnorm(n = 10000, mu = c(-3, 3), Sigma = diag(2))

# Plot them
par(mfrow = c(1, 2))
plot(density(tmat[,1]))
plot(density(tmat[,2]))

# Define direct parameterization for the skew-t (ST) distribution
cpST <- c(0, 1, -2, 10)
dpST <- cp2dp(cpST, family = "ST")

# Sample from random skew-t distributed errors
skewed1 <- rst(n = 10000, dp = dpST)
skewed2 <- rst(n = 10000, dp = dpST)

# Plot them
plot(density(skewed1))
plot(density(skewed2))
plot(density(tmat[,1] + skewed1 * 1.5))
plot(density(tmat[,2] + skewed2 * 1.5))

# Cehck their variances
sd(skewed1)
sd(tmat[,1])

# Compose skewed items
x1 <- tmat[,1] + skewed1 * 1.5
x2 <- tmat[,2] + skewed2 * 1.5

# true regression coefficients
beta <- c(0.7, -0.9)

# ture dv
ytrue <- tmat %*% beta

# adding normal error for y
error <- rnorm(n = 10000, mean = 0, sd = 1)

# observed dv
y <- ytrue + error

# estimating reg coefficients
reg <- lm(y ~ x1 + x2)
reg$coefficients

# applying these coefficients to see if they work
1 - sum((reg$fitted.values - y)^2) / sum((mean(y) - y)^2) # R2 is not that good!
summary(reg)$r.squared

# Unseen data ----------------------------------------------------------------- 

# setting different seed
set.seed(222)
tmat_unseen <- MASS::mvrnorm(n = 10000, mu = c(-3, 3), Sigma = diag(2))

# Define direct parameterization for the skew-t (ST) distribution
cpST <- c(0, 1, -2, 10)
dpST <- cp2dp(cpST, family = "ST")

# Sample from random skew-t distributed errors
skewed1_unseen <- rst(n = 10000, dp = dpST)
skewed2_unseen <- rst(n = 10000, dp = dpST)

# Generate items
x1_unseen <- tmat_unseen[,1] + skewed1_unseen * 1.5
x2_unseen <- tmat_unseen[,2] + skewed2_unseen * 1.5

# using the previously defined beta
ytrue_unseen <- tmat_unseen %*% beta

# adding normal error for y
error_unseen <- rnorm(n = 10000, mean = 0, sd = 1)

y_unseen <- ytrue_unseen + error_unseen

# now, y_unseen = unseen outcome variable,
# x1_unseen, x2_unseen = unseen predictor variables

# applying the estimated regression coefficients on the unseen variables
ypred <- cbind(x1_unseen, x2_unseen) %*% reg$coefficients[-1] + reg$coefficients[1]

1 - sum((ypred - y_unseen)^2) / sum((y_unseen^2)) # R2 is still good
1 - sum((ypred - y_unseen)^2) / sum((mean(y_unseen) - y_unseen)^2) # R2 is not really great

# MSE
MLmetrics::MSE(y_true = y_unseen, y_pred = ypred)
MLmetrics::MSE(y_true = y_unseen, y_pred = rep(mean(y_unseen), length(y_unseen)))

# PCs and non-normal errors ----------------------------------------------------

# Extra package for outcomes
library(RegularizedSCA) # for Tucker congruence measure

# Extra packages for plotting
library(ggplot2)
library(ggExtra) # for marginal plots

# Parameters
I    <- 1e3 # sample size
J    <- 9 # number of variables
VAFr <- c(.5, .3, .2) # relative variance of each components
VAFsum <- 100 # total variance of the components
R <- length(VAFr) # number of components
CPVE <- 0.9 # proportion of explained variance by the R components
skewness <- rep(0, length(VAFr)) # skewness of the components
kurtosis <- rep(0, length(VAFr)) # kurtosis of the components

# Functions for matrix manipulation
normmat <- function(X) { # Normalize matrix
    X <- apply(X, 2, function(j) j / sqrt(sum(j^2)))
    return(X)
}
orthmat <- function(X, verbose = FALSE) { # Orthogonalize columns of matrix
    for (i in 2:ncol(X)) {
        for (j in 1:(i - 1)) {
            if (verbose == TRUE) {
                print(paste0("Adjusting piar ", i, "-", j))
            }
            A <- X[, j]
            b <- X[, i]

            # Find projection of b on A
            B <- as.vector(b - (t(A) %*% b / t(A) %*% A) %*% A)

            # Replace in original X the orthogonalized columns
            X[, j] <- A
            X[, i] <- B
        }
    }
    return(X)
}

# Normal random sample U
set.seed(20221102)
U <- matrix(
    data = rnorm(I * R),
    nrow = I,
    ncol = R
)

# Scale and orthonormalize
U <- scale(U, center = TRUE, scale = FALSE)
U <- orthmat(U, verbose = FALSE)
U <- normmat(U)

# Random sample P
V <- matrix(
    data = runif(J * R),
    nrow = J,
    ncol = R
)
V <- orthmat(V, verbose = FALSE)
P <- normmat(V)

# Define D
D <- diag(c(VAFsum * VAFr))

# Create component scores
T <- U %*% D

# Create X
Xtrue <- T %*% t(P)

# sample from normal distribution (Ex = Error of X)
Ex <- MASS::mvrnorm(n = I, mu = rep(0, J), Sigma = diag(J))

# centering and scaling the Ex matrix
Ex <- scale(Ex, center = TRUE, scale = FALSE)

# > Option 1: Apply NORTA on the errors ----------------------------------------

# Define direct parameterization for the skew-t (ST) distribution
cpST <- c(0, 1, -2, 10)
dpST <- cp2dp(cpST, family = "ST")

# Apply NORTA to make them skewed
Unif <- pnorm(Ex)
Ex_skew <- data.frame(apply(Unif, 2, qst, dp = dpST))
Ex <- Ex_skew

# Check distribution
Ex_skew_scatter <- ggplot(data.frame(Ex_skew), aes(x = X1, y = X2)) +
    geom_point()
ggMarginal(Ex_skew_scatter, type = "histogram")
cor(Ex_skew)["X1", "X2"]

# Apply NORTA to make them beta distributed
Ex_beta <- data.frame(apply(Unif, 2, qbeta, shape1 = .5, shape2 = .5))
Ex <- Ex_beta

# Check distribution
Ex_beta_scatter <- ggplot(data.frame(Ex_beta), aes(x = X1, y = X2)) +
    geom_point()
ggMarginal(Ex_beta_scatter, type = "histogram")
cor(Ex_skew)["X1", "X2"]

# sum of squares
ssqXtrue <- sum(Xtrue^2)
ssqEx <- sum(Ex^2)

# Compute noise scaling constant
Escale <- sqrt(ssqXtrue * (1 - CPVE) / (CPVE * ssqEx))

# Add scaled noise
X <- Xtrue + Escale * Ex

# Scale data for estimation
X <- scale(X)

# Compute svd
Xsvd <- svd(X)

# Check the PVE is still close to target
cumsum(prop.table(Xsvd$d^2))[R]
CPVE

# Compute estimated component scores
T_hat <- Xsvd$u[, 1:R] %*% diag(Xsvd$d[1:R])

# Generate true values dependent variable
y_true <- as.vector(T %*% rep(1, R))

# Generate random error
error <- rnorm(nrow(T))

# Make the error orthogonal to the Xs
e_ortho <- orthmat(cbind(T, error))[, "error"]

# sum of squares
ssqy <- sum(T^2)
ssqe <- sum(e_ortho^2)

# Rescale noise to the desired level
R2 <- 0.4
e_scale <- sqrt(ssqy * (1 - R2) / (R2 * ssqe))

# Generate samples for y
y <- y_true + e_scale * e_ortho

# Check predictions
lm_yT <- lm(y ~ -1 + T)
summary(lm_yT)$r.squared

# Check predictions
lm_yT <- lm(y ~ -1 + T)
summary(lm_yT)$r.squared

# Outcome measures
lm_yT_hat <- lm(y ~ -1 + T_hat)
summary(lm_yT_hat)$r.squared

lm_yX <- lm(y ~ -1 + X)
summary(lm_yX)$r.squared

TuckerCoef(T, T_hat)$tucker_vector

# > Option 2: Apply NORTA directly on the items --------------------------------

# sum of squares
ssqXtrue <- sum(Xtrue^2)
ssqEx <- sum(Ex^2)

# Compute noise scaling constant
Escale <- sqrt(ssqXtrue * (1 - CPVE) / (CPVE * ssqEx))

# Add scaled noise
X <- Xtrue + Escale * Ex

# Apply NORTA to make them beta distributed
Ex_beta <- data.frame(apply(Unif, 2, qbeta, shape1 = .5, shape2 = .5))
Ex <- Ex_beta

# Apply NORTA to make them beta distributed
Ex_beta <- data.frame(apply(Unif, 2, qpois, 3))
Ex <- Ex_beta

# Check distribution
Ex_beta_scatter <- ggplot(data.frame(Ex_beta), aes(x = X1, y = X2)) +
    geom_point()
Ex_beta_scatter
ggMarginal(Ex_beta_scatter, type = "histogram")
cor(Ex_skew)["X1", "X2"]

# Scale data for estimation
X <- scale(X)

# Compute svd
Xsvd <- svd(X)

# Check the PVE is still close to target
cumsum(prop.table(Xsvd$d^2))[R]
CPVE

# Compute estimated component scores
T_hat <- Xsvd$u[, 1:R] %*% diag(Xsvd$d[1:R])

# Generate true values dependent variable
y_true <- as.vector(T %*% rep(1, R))

# Generate random error
error <- rnorm(nrow(T))

# Make the error orthogonal to the Xs
e_ortho <- orthmat(cbind(T, error))[, "error"]

# sum of squares
ssqy <- sum(T^2)
ssqe <- sum(e_ortho^2)

# Rescale noise to the desired level
R2 <- 0.4
e_scale <- sqrt(ssqy * (1 - R2) / (R2 * ssqe))

# Generate samples for y
y <- y_true + e_scale * e_ortho

# Check predictions
lm_yT <- lm(y ~ -1 + T)
summary(lm_yT)$r.squared

# Check predictions
lm_yT <- lm(y ~ -1 + T)
summary(lm_yT)$r.squared

# Outcome measures
lm_yT_hat <- lm(y ~ -1 + T_hat)
summary(lm_yT_hat)$r.squared

lm_yX <- lm(y ~ -1 + X)
summary(lm_yX)$r.squared

TuckerCoef(T, T_hat)$tucker_vector

# Preserving rank correlation --------------------------------------------------

library(sn) # for non-normal sampling
library(e1071) # for skewness measures
library(ggplot2) # for plotting
library(ggExtra) # for marginal plots

# > Generate skewed data with errors -------------------------------------------

# Fix parameters
n <- 1e4 # smaple size
p <- 2 # number of variables
mu <- rep(0, p) # vector of means
Sigma <- matrix(.7, nrow = p, ncol = p)
diag(Sigma) <- 1 # correlation matrix

# Sample (future items generated by )
X <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)

# Define direct parameterization for the skew-t (ST) distribution
cpST <- c(0, 1, -2, 10)
dpST <- cp2dp(cpST, family = "ST")

# Sample from random skew-t distributed errors
X_s_e <- apply(X, 2, function(j) j + rst(n = n, dp = dpST))

# > Generate skewed data with nort ---------------------------------------------
# Transform to uniform marginals
U <- pnorm(X)

# Transform to Skew-t marginals
X_st <- apply(U, 2, qst, dp = dpST)

# > check visual distributions -------------------------------------------------

# collect data
listX <- list(original = X, skew_X = X_st, skew_error = X_s_e)

# apply skewness and kurtosis checks
sapply(listX, function(i) apply(i, 2, e1071::skewness))
sapply(listX, function(i) apply(i, 2, e1071::kurtosis))

# Note: Norta preserves target skewness easily

# visualize
X_scatter <- ggplot(data.frame(X), aes(x = X1, y = X2)) +
    geom_point()
ggMarginal(X_scatter, type = "histogram")

X_st_scatter <- ggplot(data.frame(X_st), aes(x = X1, y = X2)) +
    geom_point()
ggMarginal(X_st_scatter, type = "histogram")

X_s_e_scatter <- ggplot(data.frame(X_s_e), aes(x = X1, y = X2)) +
    geom_point()
ggMarginal(X_s_e_scatter, type = "histogram")

# > check preservation of dependency -------------------------------------------

# Compute all types of correlation on all datasets
round(
    data.frame(
        Pearson = sapply(listX, function(i) cor(i, method = "pearson")[1, 2]),
        Spearman = sapply(listX, function(i) cor(i, method = "spearman")[1, 2]),
        Kendall = sapply(listX, function(i) cor(i, method = "kendall")[1, 2])
    ), 3
)

# Note: norta preserves the rank order correlations, while errors do not