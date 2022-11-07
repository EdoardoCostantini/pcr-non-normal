# Project:   pcr-non-normal
# Objective: Subroutine runCell
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07
# Note:      A "cell" is a cycle through the set of conditions.
#            The function in this script generates 1 data set, performs
#            imputations for every condition in the set.

runCell <- function(cond,
                    parms,
                    fs,
                    rp) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[56, ]
  # rp = 1

# Data Generation ---------------------------------------------------------

  # Generate data
  XTP <- generateXTP(
    I = parms$N,
    J = parms$P,
    VAFr = parms$XTP_VAFr,
    VAFsum = parms$XTP_VAFsum,
    CPVE = cond$XTP_R2
  )
  
  # Generate a dependent variable on the true line
  y <- generateDV(X = XTP$T,
                  R2 = cond$yT_R2,
                  beta = parms$yT_beta)

  # Apply NORTA transformation
  X_norta <- norta(X = XTP$X, marginal = cond$marginals)

  # Define Training and Testing data
  ind   <- sample(1 : nrow(dat_orig))
  train <- ind[1 : (.8*nrow(dat_orig))]
  test  <- ind[(.8*nrow(dat_orig)+1) : nrow(dat_orig)]

# Analysis ----------------------------------------------------------------

  # PCA Computation
  pcs <- extractPCs(
    dt = X_norta,
    keep = as.character(cond$npcs)
  )

  # PCR MSE
  mse <- extractMSE(x = pcs$T, y = y, train = train, test = test)

  # Tucker congruence
  tc <- RegularizedSCA::TuckerCoef(XTP$T, pcs$T)$tucker_value

  # Explained variance
  r2 <- pcs$r2

# Store Output ------------------------------------------------------------

  # Define storing object
  output <- cbind(cond, mse = mse, tc = tc, r2 = r2)

  # Return it
  saveRDS(output,
          file = paste0(fs$outDir,
                        "rep", rp,
                        "_cond", cond$tag,
                        ".rds")
  )
}