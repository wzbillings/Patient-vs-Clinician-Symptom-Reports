###
# Wrapper script for analysis
# Zane Billings
# 2022-06-10
# This script is a wrapper for the analysis pipeline. If any changes are made
# to a single script, it is likely that all downstream sections will need to
# be rerun as well. This script facilitates making changes like this by
# rerunning all analysis scripts.
# TODO implement this with targets or makefile
###

#######################################################################
##### THE EASIEST WAY TO RUN THIS SCRIPT IS TO RUN ALL OR SOURCE. #####
#######################################################################

# Each block of code here is a try-catch block which attempts to source a script
# or knit a document in a clean environment, in the order they are supposed to
# be executed. 
# Hopefully if there is an issue with a particular script, you will get an error
# saying so and the pipeline will stop running.

# We need the rmarkdown and renv packages to initalize rendering process.
suppressPackageStartupMessages(
  {require(renv); require(rmarkdown)}
)
renv::restore()

# Function to knit .Rmd docs or source .R scripts in new environment
run_new_env <- function(pth, verbose = TRUE) {
  if (endsWith(pth, ".Rmd")) {
    if (verbose) {cat("\U2139 Knitting ", pth, "\n")}
    rmarkdown::render(input = pth, envir = new.env())
  } else if (endsWith(pth, ".R")) {
    if (verbose) {cat("\U2139 Running ", pth, "\n")}
    source(file = pth, local = new.env())
  }
}

# Define a set of paths to iterate over. This needs to be in the order that
# the scripts should execute. (That's why it has to be done manually.)
pths <- c(
  here::here("R", "Analysis", "CPR-Calculations.R"),
  here::here("R", "Analysis", "CPR-Validation.R"),
  here::here("R", "Analysis", "Dx-Agreement.R"),
  here::here("R", "Analysis", "Score-Agreement.R"),
  here::here("R", "Analysis", "Symptoms-IRR.R"),
  list.files(here::here("R", "Model-Building"), full.names = TRUE),
  here::here("R", "Analysis", "thresholds-sensitivity-analysis.R"),
  here::here("R", "Figures.R"),
  here::here("R", "Tables.R"),
  here::here("R", "Products", "Manuscript", "Manuscript.Rmd"),
  here::here("R", "Products", "Supplement", "Supplement.Rmd")
)

# Run a try-catch statement for each path in the vector.
for (i in 1:length(pths)) {
  pth = pths[[i]]
  tryCatch(
    run_new_env(pth),
    error = function(cond) {
      warning("\U2716 Error in ", pth, ":")
      warning(cond)
    }
  )
}
