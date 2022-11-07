# Project:   pcr-non-normal
# Objective: Creates an fs (file system) object file system management
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07

fs <- list()
fs$start_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
fs$outDir <- paste0("../output/", fs$start_time, "/")
fs$fileName_res <- fs$start_time
fs$fileName_prog <- fs$start_time