#!/usr/bin/env Rscript

# Advanced Machine Learning Analysis Script
# Runs comprehensive ML modeling for BMI-body fat relationships

# Load error handling utilities
source("scripts/error_handling.R")
source("scripts/load_config.R")

# Load configuration
config <- safe_load_config()

# Set up logging
log_file <- file.path(config$outputs$logs_dir, "ml_analysis_log.txt")
log_msg <- function(msg) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", msg, "\n", file = log_file, append = TRUE)
  cat(msg, "\n")
}

# Main ML analysis function
main <- function() {
  log_msg("Starting advanced ML analysis...")

  tryCatch({
    # Load the ML module
    if (!require(nhanesbmi)) {
      # Source directly if package not installed
      source("R/advanced_analytics.R")
    }

    # Ensure required packages are installed
    required_packages <- c("caret", "randomForest", "xgboost", "glmnet", "rstanarm", "Metrics")
    for (pkg in required_packages) {
      if (!require(pkg, character.only = TRUE)) {
        stop(paste("Required package not available:", pkg))
      }
    }

    # Run ML analysis
    ml_results <- run_advanced_ml_analysis(config)

    # Save summary results
    summary_path <- file.path(config$outputs$tables_dir, "ml_summary.txt")
    cat("ML Analysis Summary\n", file = summary_path)
    cat("==================\n", file = summary_path, append = TRUE)
    cat("Best performing model:", ml_results$best_model, "\n", file = summary_path, append = TRUE)
    cat("Test RMSE:", min(ml_results$comparison$Test_RMSE), "\n", file = summary_path, append = TRUE)
    cat("Test R²:", max(ml_results$comparison$Test_R2), "\n", file = summary_path, append = TRUE)

    log_msg("Advanced ML analysis completed successfully")

  }, error = function(e) {
    log_msg(paste("Error in ML analysis:", e$message))
    stop(e)
  })
}

# Run if called directly
if (sys.nframe() == 0) {
  main()
}
