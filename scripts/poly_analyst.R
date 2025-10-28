#!/usr/bin/env Rscript

# Poly-Analyst Pipeline Integration Script
# Description: This script orchestrates the multi-source data integration, 
#              harmonization, and meta-analysis pipeline.

# --- Setup ---
# Load project configurations and error handling
source("scripts/error_handling.R")
config <- safe_load_config()
ensure_output_dirs(config)

# Set library paths, ensuring local project libraries are prioritized
.libPaths(c('~/R_libs', .libPaths()))

# Load required packages, installing them if necessary
required_packages <- c(
  "dplyr", "readr", "jsonlite", "meta", "metafor", "purrr", "ggplot2", "knitr"
)

# Automated package installation
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Source the core framework
source("R/poly_analyst_framework.R")

# Setup logging
log_file <- file.path(config$outputs$logs_dir, "poly_analyst_log.txt")
log_msg <- function(msg) {
  message(msg)
  cat(paste0(Sys.time(), ": ", msg, "\n"), file = log_file, append = TRUE)
}

# --- Main Execution ---
log_msg("--- Starting Poly-Analyst Pipeline ---")

# Execute the complete Poly-Analysis pipeline
poly_analysis_results <- NULL
safe_execute({
  log_msg("Executing the main Poly-Analysis pipeline...")
  poly_analysis_results <<- run_poly_analysis_pipeline(
    sources_to_analyze = c("nhanes", "uk_biobank_mock"),
    config = POLY_ANALYST_CONFIG
  )
  log_msg("Poly-Analysis pipeline completed successfully.")
}, "Poly-Analysis Main Pipeline Execution", config)


# Save the outputs of the pipeline
safe_execute({
  if (!is.null(poly_analysis_results)) {
    log_msg("Saving pipeline outputs...")
    
    # 1. Save integrated dataset
    integrated_data_path <- file.path(config$outputs$derived_data_dir, "integrated_harmonized_data.csv")
    write_csv(poly_analysis_results$integrated_data, integrated_data_path)
    log_msg(paste("Integrated dataset saved to:", integrated_data_path))
    
    # 2. Save meta-analysis results as RDS
    meta_results_path <- file.path(config$outputs$derived_data_dir, "meta_analysis_results.rds")
    saveRDS(poly_analysis_results$meta_analysis_result, meta_results_path)
    log_msg(paste("Meta-analysis results saved to:", meta_results_path))
    
    # 3. Save a summary of the meta-analysis to a text file
    meta_summary_path <- file.path(config$outputs$tables_dir, "meta_analysis_summary.txt")
    sink(meta_summary_path)
    print(summary(poly_analysis_results$meta_analysis_result))
    sink()
    log_msg(paste("Meta-analysis summary saved to:", meta_summary_path))
    
    # 4. Save the integration report summary
    report_summary_path <- file.path(config$outputs$tables_dir, "integration_report_summary.txt")
    sink(report_summary_path)
    print(poly_analysis_results$integration_report)
    sink()
    log_msg(paste("Integration report summary saved to:", report_summary_path))
    
    # 5. Save the forest plot
    forest_plot_path <- file.path(config$outputs$figures_dir, "meta_analysis_forest_plot.png")
    png(filename = forest_plot_path, width = 800, height = 600)
    forest(poly_analysis_results$meta_analysis_result,
           studlab = TRUE,
           comb.fixed = TRUE,
           comb.random = TRUE)
    dev.off()
    log_msg(paste("Forest plot saved to:", forest_plot_path))
    
    log_msg("All outputs saved successfully.")
  } else {
    log_msg("Skipping output saving due to failure in the main pipeline.")
  }
}, "Saving Poly-Analysis Outputs", config)

log_msg("--- Poly-Analyst Pipeline Finished ---")

