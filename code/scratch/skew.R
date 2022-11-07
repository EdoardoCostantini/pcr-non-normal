# Project:   pcr-non-normal
# Objective: Notes on generating skewed components and items
# Author:    Edoardo Costantini
# Created:   2022-10-28
# Modified:  2022-10-28
# Source:    https://github.com/trbKnl/SCaDS/blob/master/Simulation_study_4.1/generateData.R
#            https://github.com/soogs/SCD-CovR/blob/master/functions/spcovrdata.R

# Set up ----------------------------------------------------------------------

# Packages
library(sn) # for non-normal sampling
library(MASS) # for multivariate normal sampling
library(RegularizedSCA) # for Tucker congruence measure
library(fungible)

# Fixed parameters
I       <- 1e3  # sample size
J       <- 9    # number of items
VAFr    <- c(.5, .3, .2) # relative variance of each components
VAFsum  <- 100 # total variance of the components
CPVE    <- 0.9 # proportion of explained variance by the R components
skewness <- c(0, -2, -2) # PC1 normal, PC2 skewed left, PC3 skewed left 
kurtosis <- c(0, 10, 10) # adapt kurtosis to allow extreme skewness

# Functions
normmat <- function(X) {
    X <- apply(X, 2, function(j) j / sqrt(sum(j^2)))
    return(X)
}

orthmat <- function(X, verbose = FALSE) {
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

# Number of components
R <- length(VAFr)

# Sampling --------------------------------------------------------------------

# Sample component scores from the SN based on Azzalini
U <- sapply(1:R, function(j) {
    # Collect skewness and kurtosis in a data.frame
    cp_sk <- cbind(s = skewness, k = kurtosis)

    # Define direct parameterization for the skew-t (ST) distribution
    cpST <- c(0, 1, cp_sk[j, "s"], cp_sk[j, "k"])
    dpST <- cp2dp(cpST, family = "ST")

    # Sample from skew-t distribution
    rst(n = I, dp = dpST)
})

# Scale and orthonormalize
U <- scale(U, center = TRUE, scale = FALSE)
U <- orthmat(U, verbose = FALSE)
U <- normmat(U)

# Random sample V
V <- matrix(
    data = runif(J * R),
    nrow = J,
    ncol = R
)

# # Or if preferable, fixed decisions on V
# V <- cbind(
#     c(rep(1, 3), rep(0, 6)),
#     rep(0, 9),
#     c(rep(0, 6), rep(1, 3))
# )
# V[9, ] <- c(.001, .001, 1)

# Orthonormalize V
P <- normmat(orthmat(V, verbose = FALSE))

# Or do it differently
P <- qr.Q(qr(V))

# Define D
D <- diag(c(VAFsum * VAFr))

# Create true X
Xtrue <- U %*% D %*% t(P)

# sample noise from a normal distribution (Ex = Error of X)
Ex <- mvrnorm(n = I, mu = rep(0, J), Sigma = diag(J))

# centering and scaling the Ex matrix
Ex <- scale(Ex, center = TRUE, scale = FALSE)

# sum of squares
ssqXtrue <- sum(Xtrue^2)
ssqEx <- sum(Ex^2)

# Compute noise scaling constant
Escale <- sqrt(ssqXtrue * (1 - CPVE) / (CPVE * ssqEx))

# Add scaled noise
X <- Xtrue + Escale * Ex

# Compare results --------------------------------------------------------------

# > True components distributions ----------------------------------------------

Ttrue <- U %*% D # compute true PCs
par(mfrow = c(1, 3)) # set plotting device
sapply(
    1:R,
    function(j) {
        hist(
            Ttrue[, j], # True components
            freq = FALSE,
            main = "Distribution of true PCs",
            xlab = paste0("PC", j)
        )
    }
)

# > Estimated PCs on Xtrue -----------------------------------------------------

svd_true <- svd(Xtrue)
T_Xtrue <- svd_true$u %*% diag(svd_true$d) # components estimated on observed Xtrue
par(mfrow = c(1, 3)) # set plotting device
sapply(
    1:R,
    function(j) {
        hist(
            T_Xtrue[, j], # Estimated components on True
            freq = FALSE,
            main = "Distribution of estimated PCs on X (true)",
            xlab = paste0("PC", j)
        )
    }
)

# > Estimated PCs on X ---------------------------------------------------------

svd_obs <- svd(scale(X))
T_X <- svd_obs$u %*% diag(svd_obs$d) # components estiamted on observed X
par(mfrow = c(1, 3)) # set plotting device
sapply(
    1:R,
    function(j) {
        hist(
            T_X[, j], # Estimated components on True
            freq = FALSE,
            main = "Distribution of estimated PCs on X (obs)",
            xlab = paste0("PC", j)
        )
    }
)

# > True X distributions -------------------------------------------------------

par(mfrow = c(3, 3))
sapply(
    1:J,
    function(j) {
        hist(
            Xtrue[, j], # True X
            freq = FALSE,
            main = "Distribution of X (true)",
            xlab = paste0("x", j)
        )
    }
)

# > Observed X distributions ---------------------------------------------------

par(mfrow = c(3, 3))
sapply(
    1:J,
    function(j) {
        hist(
            X[, j], # True X
            freq = FALSE,
            main = "Distribution of X (obs)",
            xlab = paste0("x", j)
        )
    }
)

# > Check correlations between matrices ----------------------------------------

# Check correlations with true and untrue components
TuckerCoef(Ttrue, T_X)$tucker_vector
TuckerCoef(Ttrue, T_Xtrue)$tucker_vector
