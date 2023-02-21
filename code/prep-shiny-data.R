# Project:   pcr-non-normal
# Objective: pre-process input data for actual use in shiny app
# Author:    Edoardo Costantini
# Created:   2023-02-21
# Modified:  2023-02-21
# Notes:     This script prepares the input data for the shiny app plotpcrnotnormal.

# Read output
file_name <- "20221121_112535_ggshape.rds"
dataResults <- readRDS(paste0("../output/", file_name))

# New facet label names for XTP_R2 variable
XTP_R2.labs <- paste0(c("PVE = "), unique(dataResults$XTP_R2))
names(XTP_R2.labs) <- unique(dataResults$XTP_R2)

# New facet label names for yT_R2 variable
yT_R2.labs <- paste0(c("R2 = "), unique(dataResults$yT_R2))
names(yT_R2.labs) <- unique(dataResults$yT_R2)

# New names for outcomes
levels(dataResults$variable) <- gsub("rmse_", "", levels(dataResults$variable))

# Save the two objects as .rda ready for shiny app
save(dataResults, file = "../output/dataResults.rda")
