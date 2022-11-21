# Project:   pcr-non-normal
# Objective: Creates a fs (file system) object file system management
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-21

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan",
                 "rlecuyer",
                 "parallel",
                 "MLmetrics",
                 "PCAmixdata",
                 "psych",
                 "stringr",
                 "dplyr",
                 "sn",          # for Multivariate Skewed Distribution
                 "fungible",    # for Multivariate Skewed Distribution
                 "testthat",
                 "moments",     # for skewness
                 "nFactors",    # for non-graphical solutions to npcs
                 "RegularizedSCA", # for Tucker congruence
                 "FactoMineR")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Functions ----------------------------------------------------------

  # Subroutines
  all_subs <- paste0("./subroutines/",
                     list.files("./subroutines/"))
  lapply(all_subs, source)

  # Functions
  all_funs <- paste0("./functions/",
                     list.files("./functions/"))
  lapply(all_funs, source)

  # Helper
  all_help <- paste0("./helper/",
                     list.files("./helper/"))
  lapply(all_help, source)

# File system parameters --------------------------------------------------

  fs <- list()
  fs$start_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
  fs$outDir <- paste0("../output/", fs$start_time, "/")
  fs$fileName_res <- fs$start_time
  fs$fileName_prog <- fs$start_time

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()

  # Simulation parameter
  parms$dt_rep   <- 1e3 # number of data repetitions
  parms$seed     <- 20221107
  parms$nStreams <- 1000

  # Data generation
  parms$N          <- 5e2 # sample size
  parms$P          <- 12  # number of variables
  parms$XTP_VAFr   <- c(.5, .3, .2) # relative variance of each component (length = number of pcs)
  parms$XTP_VAFsum <- 100 # total variance of the components
  parms$yT_beta    <- 1

# Experimental Conditions -------------------------------------------------

  # Explained variance by the true number of components
  XTP_R2 <- c(.5, .8, .9, .99)

  # Explained variance by the linear regression model
  yT_R2 <- c(.3, .7, .9, .99)
  
  # Distributions
  marginals <- c("normal", "beta", "skewt", "mixed")

  # Items per principal component / dimensionality of data
  J <- c(4, 150)

  # True predictors
  tp <- c("PCs", "items")

  # Correlation type
  ct <- c("pearson", "spearman")
  
  # Number of components kept by the PCA extraction
  npcs <- c(
    "naf", "nkaiser", # non-graphical screeplot solutions
    parms$XTP_R2, # (true) CPVE based
    1, # most summary
    length(parms$XTP_VAFr), # (true) number of components
    parms$P # least summary
  )[4]

  # Make Conditions based on vector experimental factors
  conds <- expand.grid(
    marginals = marginals,
    XTP_R2 = XTP_R2,
    yT_R2 = yT_R2, # ordinality degree
    npcs = npcs,
    J = J,
    tp = tp,
    ct = ct,
    stringsAsFactors = TRUE
  )

  # Condition tag
  conds$id <- 1:nrow(conds)