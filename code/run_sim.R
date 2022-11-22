# Project:   pcr-non-normal
# Objective: Subroutine doRep (windows parallelization framework)
# Author:    Edoardo Costantini
# Created:   2022-11-07
# Modified:  2022-11-22

# Make sure we have a clean environment:
rm(list = ls())

# Initialize the environment:
source("./init.R")

# Progress report file
dir.create(fs$outDir)
file.create(paste0(fs$outDir, fs$fileName_prog, ".txt"))

cat(paste0("SIMULATION PROGRESS REPORT",
           "\n",
           "Starts at: ", Sys.time(),
           "\n", "------", "\n" ),
    file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
    sep = "\n",
    append = TRUE)

# Define repetitions and clusters (ie number of cores for parallelization)
reps <- 1 : 50
clus <- makeCluster(10)

# Export important objects to worker nodes
clusterExport( # export fs object from the global env
    cl = clus,
    varlist = "fs",
    envir = .GlobalEnv
)
clusterEvalQ( # export script to be executed
    cl = clus,
    expr = source("./init.R")
)

# Apply in parallel --------------------------------------------------------

# Take note of the start time
sim_start <- Sys.time()

# Run the computations in parallel on the 'clus' object:
out <- parLapply(cl    = clus,
                 X     = reps,
                 fun   = doRep,
                 conds = conds,
                 parms = parms,
                 fs = fs)

# Kill the cluster:
stopCluster(clus)

# Take note of the end time
sim_ends <- Sys.time()

# Close off report file
cat(paste0("\n", "------", "\n",
           "Ends at: ", Sys.time(), "\n",
           "Run time: ",
           round(difftime(sim_ends, sim_start, units = "hours"), 3), " h",
           "\n", "------", "\n"),
    file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
    sep = "\n",
    append = TRUE)

# Attach info object
out_support <- list()
out_support$parms <- parms
out_support$conds <- conds
out_support$session_info <- devtools::session_info()

# Save output -------------------------------------------------------------

saveRDS(out_support,
        paste0(fs$outDir, "sInfo.rds"))

# Zip output folder -------------------------------------------------------

writeTarGz(folder_name = fs$fileName_res)