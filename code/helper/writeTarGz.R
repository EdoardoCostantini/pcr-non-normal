# Project:   pcr-non-normal
# Objective: function to compress output folder to zip folder
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-07

writeTarGz <- function(folder_name){
  # Description:
  # Given the name of a folder name in the "output" project folder
  # it zips it and deletes the original folder

  # Move to Output folder
  setwd("../output/")

  # Zip folder
  system(command = paste0("tar cvzf ", folder_name, ".tar.gz",
                          " ./", folder_name, "/"))

  # Delete Folder
  system(command = paste0("rm -rf ", folder_name))

  # Revert WD to code folder
  setwd("../code/")
}