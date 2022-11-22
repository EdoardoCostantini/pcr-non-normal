# Project:   pcr-non-normal
# Objective: Extract Principal Components with different methods
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-21

extractPCs <- function(dt = matrix(), keep = 1L, cor_method = "pearson"){
# Description -------------------------------------------------------------

  # Given a data set A in matrix for, it extracts the first keep principal
  # components from A, and returns a dataset
  # with the first column of A combined with the extracted components.
  # It also returns the info regarding the proportion of explained variance
  # by the defined number of components
  # when @cor_tupe = "mixed", psych::principal recognizes which variables
  # need pearson, polyserial, polychoric, tetrachoric correlations

# Internals ---------------------------------------------------------------

  # dt = dat_disc # MASS::mvrnorm(1e2, rep(0, 3), diag(3))
  # keep = c("naf", "nkaiser", ".8", "3")[2] # how should we decide what pcs to keep?

# Body --------------------------------------------------------------------

  # Make sure data is scaled
  dt <- scale(dt)

  # Correlation matrix
  cormat <- cor(dt, method = cor_method)

  # Eigen-decomposition
  eigenmat <- eigen(cormat)

  # Compute PC scores
  T <- dt %*% eigenmat$vectors

  # CPVE
  CPVE <- cumsum(prop.table(eigenmat$values))

  # Check if keep is a non-graphical solution
  keep_nScree <- suppressWarnings(is.na(as.numeric(keep)))

  # Define npcs and CPVE based on type of keep
  if(keep_nScree){
    # Compute all non-graphical solutions
    non_graph_scree <- nScree(x = eigenmat$values)

    # Keep the result of the one with the desired name
    npcs <- non_graph_scree$Components[, keep]
  } else {
    # Convert keep to number
    keep <- as.numeric(as.character(keep))
    if(keep < 1) {
      # Set npcs to the firt PC that explains more than target
      npcs <- Position(function(x) x >= keep, CPVE)
    } else {
      # Set npcs to the integer value provided
      npcs <- keep
    }
  }

  # Store the CPVE associated with this npcs
  pve <- CPVE[npcs]

  # Store
  return(list(T    = T[, 1:npcs, drop = FALSE],
              V = eigenmat$vectors[, 1:npcs],
              npcs = npcs,
              pve  = round(pve, 3)))

}