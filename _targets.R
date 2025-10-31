# NHANES BMI Body Fat Analysis Pipeline using targets
# For reproducible, parallelized, and cached workflow

library(targets)
library(future)
library(furrr)
library(dplyr)
library(ggplot2)
library(survey)
library(foreign)
library(readr)
library(yaml)
library(checkmate)

# Set up parallel processing
plan(multisession, workers = availableCores() - 1)

# Load and validate project configuration
config <- load_and_validate_config("config/config.yml")

# Set targets options
tar_option_set(
  packages = c("dplyr", "ggplot2", "survey", "foreign", "readr", "yaml", "checkmate"),
  format = "rds", # Use RDS for faster serialization
  memory = "transient", # Memory management
  garbage_collection = TRUE, # Enable garbage collection
  storage = "worker", # Storage format
  retrieval = "worker" # Retrieval format
)

# Source utility functions
library(nhanesbmi)

# Source the modular pipeline definitions
source("R/load_config.R")
source("R/error_handling.R")
source("R/data_validation.R")
source("R/pipeline_steps.R")
source("R/config.R")
source("R/ml_functions.R")

# Combine all targets into a single list
list(
  data_processing_targets,
  analysis_targets,
  machine_learning_targets,
  outputs_targets
)
