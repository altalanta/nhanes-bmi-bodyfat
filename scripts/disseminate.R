#!/usr/bin/env Rscript

# Dissemination Engine Command-Line Interface
# Description: This script provides a CLI to generate manuscripts, dashboards,
#              and reproducibility packages.

# --- Setup ---
# Load project configurations and error handling
source("scripts/error_handling.R")
config <- safe_load_config()
ensure_output_dirs(config)

# Set library paths
.libPaths(c('~/R_libs', .libPaths()))

# Load required packages
required_packages <- c("quarto", "shiny", "zip", "readr", "jsonlite", "knitr", "ggplot2")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Source the core engine
source("R/dissemination_engine.R")

# --- Command-Line Argument Parsing ---
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Usage: Rscript scripts/disseminate.R [action] [options...]\n
       Actions:\n
       manuscript [--format=pdf|docx] [--name=output_name]\n
       dashboard  [--name=dashboard_name]\n
       package    [--name=package_name.zip]\n",
       call. = FALSE)
}

action <- args[1]
options <- args[-1]

# Helper to parse --key=value arguments
parse_option <- function(flag, default) {
  val <- grep(paste0("^", flag, "="), options, value = TRUE)
  if (length(val) > 0) {
    return(sub(paste0("^", flag, "="), "", val))
  }
  return(default)
}

# --- Main Execution ---
main <- function() {
  
  initialize_dissemination_engine()
  
  if (action == "manuscript") {
    format <- parse_option("--format", DISSEMINATION_CONFIG$manuscript$default_format)
    name <- parse_option("--name", paste0("manuscript_", Sys.Date()))
    
    safe_execute({
      generate_manuscript(format = format, output_filename = name)
    }, "Manuscript Generation", config)
    
  } else if (action == "dashboard") {
    name <- parse_option("--name", paste0("dashboard_", Sys.Date()))
    
    safe_execute({
      generate_interactive_dashboard(dashboard_name = name)
    }, "Dashboard Generation", config)
    
  } else if (action == "package") {
    name <- parse_option("--name", paste0("repro_package_", Sys.Date(), ".zip"))
    
    safe_execute({
      create_reproducibility_package(package_name = name)
    }, "Reproducibility Package Creation", config)
    
  } else {
    stop("Invalid action: '", action, "'. Please choose from 'manuscript', 'dashboard', or 'package'.", call. = FALSE)
  }
}

# Run the main function
main()
