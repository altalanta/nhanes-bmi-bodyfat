#!/usr/bin/env Rscript

# Advanced Machine Learning Analysis Script with Performance Benchmarking
# Runs comprehensive ML modeling for BMI-body fat relationships with detailed performance tracking

# Load error handling and performance utilities
source("scripts/error_handling.R")
source("scripts/load_config.R")
source("R/performance.R")

# Initialize performance tracking for ML analysis
initialize_performance_tracking("nhanes_ml_analysis")

# Load configuration
config <- safe_load_config()

# Set up enhanced logging with performance tracking
log_file <- file.path(config$outputs$logs_dir, "ml_analysis_log.txt")
log_msg <- function(msg) {
  log_performance(msg, "INFO")
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", msg, "\n", file = log_file, append = TRUE)
  cat(msg, "\n")
}

# Enhanced main ML analysis function with comprehensive performance tracking
main <- function() {
  log_msg("Starting advanced ML analysis with performance benchmarking...")

  tryCatch({
    # Load the ML module with performance tracking
    ml_module_load <- benchmark_operation({
      if (!require(nhanesbmi, quietly = TRUE)) {
        # Source directly if package not installed
        source("R/advanced_analytics.R")
      }
    }, "ml_module_loading", metadata = list(package_required = "nhanesbmi"))

    # Package loading with performance tracking
    package_loading <- benchmark_operation({
      required_packages <- c("caret", "randomForest", "xgboost", "glmnet", "rstanarm", "Metrics")
      loaded_packages <- c()

      for (pkg in required_packages) {
        if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
          stop(NhanesError(
            paste("Required package not available:", pkg),
            code = "MISSING_ML_PACKAGE",
            details = list(package = pkg, required_packages = required_packages)
          ))
        }
        loaded_packages <- c(loaded_packages, pkg)
      }

      return(loaded_packages)
    }, "package_loading", metadata = list(n_packages = length(required_packages)))

    # Run ML analysis with comprehensive performance tracking
    ml_analysis <- benchmark_operation({
      ml_results <- run_advanced_ml_analysis(config)
      return(ml_results)
    }, "ml_analysis_execution",
    metadata = list(
      n_packages = length(loaded_packages),
      expected_models = c("linear", "random_forest", "xgboost", "elastic_net", "bayesian")
    ))

    # Save summary results with enhanced reporting
    summary_results <- benchmark_operation({
      summary_path <- file.path(config$outputs$tables_dir, "ml_summary.txt")

      # Enhanced summary with performance metrics
      summary_content <- paste0(
        "Advanced ML Analysis Summary\n",
        "===========================\n",
        "Analysis Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
        "Best performing model: ", ml_analysis$best_model, "\n",
        "Test RMSE: ", round(min(ml_analysis$comparison$Test_RMSE), 4), "\n",
        "Test R²: ", round(max(ml_analysis$comparison$Test_R2), 4), "\n",
        "Models compared: ", nrow(ml_analysis$comparison), "\n",
        "\nModel Performance Details:\n",
        paste(capture.output(print(ml_analysis$comparison)), collapse = "\n")
      )

      writeLines(summary_content, summary_path)
      return(summary_path)
    }, "results_summary", metadata = list(n_models = nrow(ml_analysis$comparison)))

    # Generate detailed performance report for ML operations
    ml_performance_report <- benchmark_operation({
      # Create ML-specific performance summary
      ml_perf_summary <- list(
        analysis_timestamp = Sys.time(),
        models_evaluated = nrow(ml_analysis$comparison),
        best_model = ml_analysis$best_model,
        best_rmse = min(ml_analysis$comparison$Test_RMSE),
        best_r2 = max(ml_analysis$comparison$Test_R2),
        computational_resources = list(
          packages_loaded = length(loaded_packages),
          total_benchmarks = length(performance_tracker$benchmarks)
        )
      )

      # Save ML performance summary
      ml_perf_file <- file.path(config$outputs$logs_dir, "ml_performance_summary.json")
      jsonlite::write_json(ml_perf_summary, ml_perf_file, pretty = TRUE, auto_unbox = TRUE)

      return(ml_perf_file)
    }, "ml_performance_reporting", metadata = list(n_models = nrow(ml_analysis$comparison)))

    log_msg("Advanced ML analysis completed successfully with comprehensive performance tracking")
    log_msg(paste("Best model:", ml_analysis$best_model))
    log_msg(paste("Performance report saved to:", ml_performance_report))

  }, error = function(e) {
    log_performance(paste("Error in ML analysis:", e$message), "ERROR")
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("ML analysis failed:", e$message),
        code = "ML_ANALYSIS_ERROR",
        details = list(original_error = e$message)
      ))
    }
  })
}

# Run if called directly
if (sys.nframe() == 0) {
  main()
}

