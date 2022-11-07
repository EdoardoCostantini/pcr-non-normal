# Project:   pcr-non-normal
# Objective: set up and run tests
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07

rm(list = ls())

## Initialize the environment:
source("./init.R")

## Prepare storing results
source("./fs.R")

test_dir("../tests/testthat")
