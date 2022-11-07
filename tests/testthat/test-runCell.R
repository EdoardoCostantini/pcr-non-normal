# Project:   pcr-non-normal
# Objective: Test runCell can run for all values of the experimental factors
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07

context("runCell")

# Set up test
## Initialize the environment:
source("./init.R")

## Prepare storing results
source("./fs.R")

## Progress report file
dir.create(fs$outDir)

# Fix repetition
rp <- 1

# Set seed
set.seed(20220120)

# Loop it
for(i in 1 : nrow(conds)) {
  # Try running simulation for condition i, repetition rp
  runCell(cond = conds[i, ],
          parms = parms,
          fs = fs,
          rp = rp)
}

# How many files were saved?
n_files <- length(list.files(fs$outDir))

# Tests
test_that("A file for each condition was saved", {
  expect_true(n_files == nrow(conds), 1)
})

# Clean up
system(command = paste0("rm -rf ", fs$outDir))