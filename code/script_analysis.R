# Project:   pcr-non-normal
# Objective: Analyzing results
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-08

  # Make sure we have a clean environment:
  rm(list = ls())

# Packages ----------------------------------------------------------------

  pack_list <- c("ggplot2",
                 "dplyr",
                 "forcats",
                 "stringr")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Results ----------------------------------------------------------

  # Support Functions
  source("./init.R")

  # Read output
  file_name <- grep("ggshape", list.files("../output/"), value = TRUE)[4]
  run_name <- gsub("_out.rds", "", file_name)
  gg_shape <- readRDS(paste0("../output/", file_name))

  # New facet label names for XTP_R2 variable
  XTP_R2.labs <- paste0(c("PVE = "), unique(gg_shape$XTP_R2))
  names(XTP_R2.labs) <- unique(gg_shape$XTP_R2)

  # New facet label names for yT_R2 variable
  yT_R2.labs <- paste0(c("R2 = "), unique(gg_shape$yT_R2))
  names(yT_R2.labs) <- unique(gg_shape$yT_R2)

# Plot RMSE -------------------------------------------------------------------

  # Make RMSE plot
  plot_RMSE <- gg_shape %>%
    # Subset
    filter(grepl("rmse", variable)) %>%
    # Main Plot
    ggplot(aes(x = variable, y = value, fill = marginals)) +
    geom_boxplot() +

    # Grid
    facet_grid(
      cols = vars(yT_R2),
      rows = vars(XTP_R2),
      labeller = labeller(yT_R2 = yT_R2.labs, XTP_R2 = XTP_R2.labs),
      scales = "free"
    ) +

    # Format
    theme(
      text = element_text(size = 15),
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 15),
      axis.text.x = element_text(angle = 45, hjust = 0.95),
      axis.title = element_text(size = 15)
    ) +
    labs(
      title = "RMSE",
      x = NULL,
      y = NULL
    )

  # Print plot
  plot_RMSE

# Plot TC -------------------------------------------------------------------

# Define which outcome measure to plot
result <- levels(gg_shape$variable)[3]

# Make PVE plot
plot_PVE <- gg_shape %>%
  # Subset
  filter(grepl(result, variable)) %>%
  # Main Plot
  ggplot(aes(x = marginals, y = value)) +
  geom_boxplot() +

  # Grid
  facet_grid(
    rows = vars(yT_R2),
    cols = vars(XTP_R2),
    labeller = labeller(yT_R2 = yT_R2.labs, XTP_R2 = XTP_R2.labs),
    scales = "free"
  ) +

  # Format
  theme(
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 0.95),
    axis.title = element_text(size = 15)
  ) +
  labs(
    title = "Tucker Congruence",
    x = NULL,
    y = NULL
  )

# Print plot
plot_PVE

# Plot PVE -------------------------------------------------------------------

# Define which outcome measure to plot
result <- levels(gg_shape$variable)[4]

# Make PVE plot
plot_PVE <- gg_shape %>%
  # Subset
  filter(grepl(result, variable)) %>%
  # Main Plot
  ggplot(aes(x = marginals, y = value)) +
  geom_boxplot() +

  # Grid
  facet_grid(
    rows = vars(yT_R2),
    cols = vars(XTP_R2),
    labeller = labeller(yT_R2 = yT_R2.labs, XTP_R2 = XTP_R2.labs),
    scales = "free"
  ) +

  # Format
  theme(
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 0.95),
    axis.title = element_text(size = 15)
  ) +
  labs(
    title = "PVE",
    x = NULL,
    y = NULL
  )

# Print plot
plot_PVE

# Save plots --------------------------------------------------------------

  file_format <- ".pdf"
  plot_name <- paste0("outcome_", stringr::str_remove(result, "\\."),
                      "_interval_", int_conditions,
                      "_discrete_", D_conditions)
  out_dir <- "~/Desktop/"
  file_name <- paste0(out_dir, plot_name, file_format)
  if(file_format == ".pdf"){
    pdf(file_name, width = 15, height = 15)
  }
  if(file_format == ".png"){
    png(file_name, width = 15, height = 15, units = "in", res = 384)
  }
  plot1
  dev.off()