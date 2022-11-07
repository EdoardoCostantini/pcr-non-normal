# Project:   pcr-non-normal
# Objective: Function to apply norta approach
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07
# Notes: 

norta <- function(
  X = matrix(rnorm(1e4), ncol = 4), 
  marginal = "beta"
){

# Internals -------------------------------------------------------------

    # X = matrix(rnorm(3e3), ncol = 12) # input data to transform
    # marginal = "beta" # target marginal distribution

# Body ------------------------------------------------------------------

    # Transform to uniform distribution (apply normal CDF to X)
    U <- apply(X, 2, pnorm)

    # Transform to a target distribution
    if (marginal == "normal") {
        X_norta <- X
    }
    if(marginal == "beta"){
        X_norta <- qbeta(U, shape1 = .5, shape2 = .5)
    }
    if (marginal == "skewt") {
        # Define the target skewness and kurtosis
        sk <- -2
        kt <- 10

        # Define direct parameterization for the skew-t (ST) distribution
        cpST <- c(0, 1, sk, kt)
        dpST <- cp2dp(cpST, family = "ST")

        # Transform to Skew-t (apply target inverse-CDF to X)
        X_norta <- apply(U, 2, qst, dp = dpST)
    }
    if (marginal == "mixed") {
        # Count the number of columns
        p <- ncol(X)

        # Define pool of possible marginals
        marg_pool <- c("normal", "beta", "skewt")

        # Parameters for skew-t
        sk <- -2
        kt <- 10
        cpST <- c(0, 1, sk, kt)
        dpST <- cp2dp(cpST, family = "ST")

        # How many marginals do you need?
        nm <- p / length(marg_pool)

        # Randomly assign marginals to columns of X
        marg_target <- sample(rep(marg_pool, nm))

        # Apply norta to every column based on mix of marginals
        X_norta <- sapply(1:p, function(j) {
            if (marg_target[j] == "normal") {
                x_temp <- X[, j]
            }
            if (marg_target[j] == "beta") {
                x_temp <- qbeta(U[, j], shape1 = .5, shape2 = .5)
            }
            if (marg_target[j] == "skewt") {
                x_temp <- qst(U[, j], dp = dpST)
            }
            x_temp
        })
    }

  # Define outputs
  return(X_norta)
}
