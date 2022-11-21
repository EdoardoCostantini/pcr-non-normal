# Project:   pcr-non-normal
# Objective: Subroutine runCell
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-21
# Note:      A "cell" is a cycle through the set of conditions.
#            The function in this script generates 1 data set, performs
#            imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    fs,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[256, ]
  # rp = 1

# Data Generation ---------------------------------------------------------

  # Generate data
  XTP <- generateXTP(
    I = parms$N,
    J = cond$J * length(parms$XTP_VAFr),
    VAFr = parms$XTP_VAFr,
    VAFsum = parms$XTP_VAFsum,
    CPVE = cond$XTP_R2
  )
  
  # Apply NORTA transformation
  X_norta <- norta(X = XTP$X, marginal = cond$marginals)

  # Scale X_norta
  X_norta <- scale(X_norta)

  # Parse true predictors
  if(cond$tp == "PCs"){
      X <- XTP$T
  }
  if(cond$tp == "items"){
      X <- as.matrix(X_norta)
  }

  # Generate a dependent variable on the true line
  y <- generateDV(X = X,
                  R2 = cond$yT_R2,
                  beta = parms$yT_beta)

  # Standardize y
  y <- scale(y)

  # Define Training and Testing data
  ind <- sample(1:nrow(X_norta))
  train <- ind[1:(.8 * nrow(X_norta))]
  test  <- ind[(.8*nrow(X_norta)+1) : nrow(X_norta)]

# Analysis ----------------------------------------------------------------

  # PCA Computation
  pcs <- extractPCs(
    dt = X_norta,
    keep = as.character(cond$npcs),
    cor_method = as.character(cond$ct)
  )

  # MSE
  mse_lm <- extractMSE(x = X_norta, y = y, train = train, test = test)
  mse_pcr <- extractMSE(x = pcs$T, y = y, train = train, test = test)

  # Tucker congruence
  tc <- RegularizedSCA::TuckerCoef(XTP$T, pcs$T)$tucker_value

  # Explained variance
  pve <- pcs$pve

# Store Output ------------------------------------------------------------

  # Define storing object
  output <- cbind(
    cond,
    rmse_lm = sqrt(mse_lm),
    rmse_pcr = sqrt(mse_pcr),
    tc = tc,
    pve = pve
  )

  # Return it
  saveRDS(output,
          file = paste0(fs$outDir,
                        "rep", rp,
                        "_cond", cond$id,
                        ".rds")
  )
}