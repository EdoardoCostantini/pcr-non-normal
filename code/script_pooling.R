# Project:   pcr-non-normal
# Objective: Pooling results
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-12-05

  # Make sure we have a clean environment:
  rm(list = ls())

  # Support Functions
  source("./init.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  target_tar <- "20221121_112535.tar.gz" # using EVD, w/ SPearman
  output <- readTarGz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give a unique name to all objects
  names(output$out) <- output$file_names

  # Punt into a single data.frame
  out <- do.call(rbind, output$out)

# Restructure for plots -------------------------------------------------------
  id_column <- grep("id", colnames(out))
  gg_shape <- reshape2::melt(out, id.var = colnames(out)[1:id_column])

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_ggshape",
                        ".rds")
  )