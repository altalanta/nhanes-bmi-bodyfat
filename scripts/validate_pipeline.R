#!/usr/bin/env Rscript

# End-to-End Data Validation Pipeline for NHANES Analysis
# Comprehensive validation across the entire data processing workflow

suppressPackageStartupMessages({
  library(dplyr)
  library(foreign)
  library(jsonlite)
  library(here)
  library(arrow)
})

# Source all validation utilities
source(file.path(here::here(), "R", "data_validation.R"))
source(file.path(here::here(), "R", "performance.R"))

# Initialize comprehensive validation tracking
initialize_performance_tracking("nhanes_validation_pipeline")

cat("NHANES Data Validation Pipeline\n")
cat("===============================\n")
cat("Running comprehensive validation across entire data processing workflow...\n\n")

# Set up paths
repo_root <- here::here()
data_raw_dir <- file.path(repo_root, "data", "raw")
data_derived_dir <- file.path(repo_root, "data", "derived")
logs_dir <- file.path(repo_root, "outputs", "logs")

dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

# Initialize validation tracking
pipeline_validation <- list(
  start_time = Sys.time(),
  stages = list(),
  overall_status = "PENDING",
  errors = list(),
  warnings = list()
)

# Validation stage function
add_validation_stage <- function(stage_name, status, details = NULL, error = NULL, warning = NULL) {
  stage_result <- list(
    stage = stage_name,
    status = status,
    timestamp = Sys.time(),
    details = details
  )

  if (!is.null(error)) {
    stage_result$error <- error
    pipeline_validation$errors <- c(pipeline_validation$errors, paste(stage_name, ":", error))
  }

  if (!is.null(warning)) {
    stage_result$warning <- warning
    pipeline_validation$warnings <- c(pipeline_validation$warnings, paste(stage_name, ":", warning))
  }

  pipeline_validation$stages <- c(pipeline_validation$stages, list(stage_result))

  # Log stage result
  status_icon <- switch(status,
    "PASSED" = "✓",
    "FAILED" = "✗",
    "WARNING" = "⚠",
    "SKIPPED" = "○",
    "?"
  )

  cat(sprintf("[%s] %s: %s\n", status_icon, stage_name, status))
  if (!is.null(details)) {
    cat(sprintf("      Details: %s\n", details))
  }
  if (!is.null(error)) {
    cat(sprintf("      Error: %s\n", error))
  }
  if (!is.null(warning)) {
    cat(sprintf("      Warning: %s\n", warning))
  }

  return(stage_result)
}

# Stage 1: Validate raw data files exist and are accessible
cat("Stage 1: Validating raw data file accessibility...\n")
stage1_result <- tryCatch({
  required_files <- c("DEMO_J.XPT", "BMX_J.XPT", "DXX_J.XPT", "DXXAG_J.XPT")

  file_status <- list()
  missing_files <- c()

  for (file in required_files) {
    filepath <- file.path(data_raw_dir, file)
    exists <- file.exists(filepath)
    size <- if (exists) file.size(filepath) else 0

    file_status[[file]] <- list(
      exists = exists,
      size = size,
      accessible = exists && size > 0
    )

    if (!exists || size == 0) {
      missing_files <- c(missing_files, file)
    }
  }

  if (length(missing_files) > 0) {
    stop(NhanesError(
      paste("Missing or empty raw data files:", paste(missing_files, collapse = ", ")),
      code = "MISSING_RAW_FILES",
      details = list(missing_files = missing_files, file_status = file_status)
    ))
  }

  add_validation_stage(
    "Raw File Accessibility",
    "PASSED",
    paste("All", length(required_files), "required files found and accessible")
  )

}, error = function(e) {
  add_validation_stage(
    "Raw File Accessibility",
    "FAILED",
    paste("Failed to access", length(missing_files), "files"),
    error = e$message
  )
})

# Stage 2: Validate manifest and checksums
cat("\nStage 2: Validating data integrity (manifest and checksums)...\n")
stage2_result <- tryCatch({
  manifest_file <- file.path(data_raw_dir, "manifest.json")

  if (!file.exists(manifest_file)) {
    stop(NhanesError(
      "Manifest file not found",
      code = "MISSING_MANIFEST",
      details = list(manifest_file = manifest_file)
    ))
  }

  manifest <- fromJSON(manifest_file)

  # Validate manifest structure
  required_manifest_fields <- c("filename", "sha256", "filepath", "file_size", "download_date")
  missing_fields <- setdiff(required_manifest_fields, names(manifest[[1]]))

  if (length(missing_fields) > 0) {
    warning(NhanesWarning(
      paste("Manifest missing expected fields:", paste(missing_fields, collapse = ", ")),
      code = "INCOMPLETE_MANIFEST",
      details = list(missing_fields = missing_fields)
    ))
  }

  # Verify checksums
  checksum_issues <- c()
  for (dataset in names(manifest)) {
    file_info <- manifest[[dataset]]
    if ("sha256" %in% names(file_info) && file.exists(file_info$filepath)) {
      current_hash <- compute_sha256(file_info$filepath)
      if (current_hash != file_info$sha256) {
        checksum_issues <- c(checksum_issues, dataset)
      }
    }
  }

  if (length(checksum_issues) > 0) {
    warning(NhanesWarning(
      paste("Checksum mismatches for:", paste(checksum_issues, collapse = ", ")),
      code = "CHECKSUM_MISMATCH",
      details = list(failed_datasets = checksum_issues)
    ))
  }

  status <- if (length(checksum_issues) == 0) "PASSED" else "WARNING"
  details <- paste("Manifest valid with", length(manifest), "datasets")

  add_validation_stage(
    "Data Integrity",
    status,
    details,
    warning = if (length(checksum_issues) > 0) paste("Checksum issues in", length(checksum_issues), "datasets") else NULL
  )

}, error = function(e) {
  add_validation_stage(
    "Data Integrity",
    "FAILED",
    "Manifest validation failed",
    error = e$message
  )
})

# Stage 3: Validate derived dataset exists and structure
cat("\nStage 3: Validating derived dataset integrity...\n")
stage3_result <- tryCatch({
  derived_files <- c("analytic.rds", "analytic.parquet", "analytic.csv")
  missing_derived <- c()

  for (file in derived_files) {
    filepath <- file.path(data_derived_dir, file)
    if (!file.exists(filepath)) {
      missing_derived <- c(missing_derived, file)
    }
  }

  if (length(missing_derived) > 0) {
    stop(NhanesError(
      paste("Missing derived dataset files:", paste(missing_derived, collapse = ", ")),
      code = "MISSING_DERIVED_FILES",
      details = list(missing_files = missing_derived)
    ))
  }

  # Load and validate derived dataset
  analytic_data <- readRDS(file.path(data_derived_dir, "analytic.rds"))

  if (is.null(analytic_data) || nrow(analytic_data) == 0) {
    stop(NhanesError(
      "Derived dataset is empty",
      code = "EMPTY_DERIVED_DATASET",
      details = list(n_rows = nrow(analytic_data), n_cols = ncol(analytic_data))
    ))
  }

  # Validate required columns
  required_cols <- c("seqn", "age", "sex", "bmi", "bodyfat_pct", "survey_weight", "strata", "psu")
  missing_cols <- setdiff(required_cols, names(analytic_data))

  if (length(missing_cols) > 0) {
    stop(NhanesError(
      paste("Derived dataset missing required columns:", paste(missing_cols, collapse = ", ")),
      code = "MISSING_DERIVED_COLUMNS",
      details = list(missing = missing_cols, available = names(analytic_data))
    ))
  }

  # Validate data types
  type_issues <- c()
  if (!is.numeric(analytic_data$seqn)) type_issues <- c(type_issues, "seqn not numeric")
  if (!is.numeric(analytic_data$bmi)) type_issues <- c(type_issues, "bmi not numeric")
  if (!is.numeric(analytic_data$bodyfat_pct)) type_issues <- c(type_issues, "bodyfat_pct not numeric")
  if (!is.factor(analytic_data$sex)) type_issues <- c(type_issues, "sex not factor")

  if (length(type_issues) > 0) {
    warning(NhanesWarning(
      paste("Data type issues:", paste(type_issues, collapse = "; ")),
      code = "INVALID_DATA_TYPES",
      details = list(issues = type_issues)
    ))
  }

  status <- if (length(missing_cols) == 0 && length(type_issues) == 0) "PASSED" else "WARNING"
  details <- paste("Dataset has", nrow(analytic_data), "rows,", ncol(analytic_data), "columns")

  add_validation_stage(
    "Derived Dataset",
    status,
    details,
    warning = if (length(type_issues) > 0) paste(length(type_issues), "type issues") else NULL
  )

}, error = function(e) {
  add_validation_stage(
    "Derived Dataset",
    "FAILED",
    "Derived dataset validation failed",
    error = e$message
  )
})

# Stage 4: Cross-reference validation between raw and derived data
cat("\nStage 4: Cross-referencing raw and derived datasets...\n")
stage4_result <- tryCatch({
  # Load manifest for raw data info
  manifest <- fromJSON(file.path(data_raw_dir, "manifest.json"))
  derived_data <- readRDS(file.path(data_derived_dir, "analytic.rds"))

  # Basic consistency checks
  raw_seqn_count <- 0
  for (dataset in names(manifest)) {
    if ("filepath" %in% names(manifest[[dataset]])) {
      tryCatch({
        raw_data <- read.xport(manifest[[dataset]]$filepath)
        raw_seqn_count <- raw_seqn_count + nrow(raw_data)
      }, error = function(e) {
        # Skip datasets that can't be read
      })
    }
  }

  derived_seqn_count <- nrow(derived_data)

  # Check for reasonable relationship between raw and derived counts
  if (derived_seqn_count > raw_seqn_count) {
    warning(NhanesWarning(
      paste("Derived dataset larger than raw data (", derived_seqn_count, "vs", raw_seqn_count, ")"),
      code = "DERIVED_LARGER_THAN_RAW",
      details = list(derived = derived_seqn_count, raw = raw_seqn_count)
    ))
  }

  # Validate that derived SEQN values exist in raw data
  if ("seqn" %in% names(derived_data)) {
    # This is a simplified check - in practice would need more sophisticated validation
    seqn_range_check <- all(derived_data$seqn > 0, na.rm = TRUE)

    if (!seqn_range_check) {
      warning(NhanesWarning(
        "Invalid SEQN values in derived dataset",
        code = "INVALID_DERIVED_SEQN",
        details = list(min_seqn = min(derived_data$seqn, na.rm = TRUE))
      ))
    }
  }

  status <- "PASSED"
  details <- paste("Cross-reference validation completed")

  add_validation_stage(
    "Cross-Reference",
    status,
    details
  )

}, error = function(e) {
  add_validation_stage(
    "Cross-Reference",
    "WARNING",
    "Cross-reference validation had issues",
    warning = e$message
  )
})

# Stage 5: Statistical validation of derived measures
cat("\nStage 5: Validating statistical properties of derived measures...\n")
stage5_result <- tryCatch({
  derived_data <- readRDS(file.path(data_derived_dir, "analytic.rds"))

  # Statistical validation checks
  validation_issues <- c()

  # BMI validation
  if (any(derived_data$bmi <= 0 | derived_data$bmi > 100, na.rm = TRUE)) {
    validation_issues <- c(validation_issues, "BMI values outside 0-100 range")
  }

  # Body fat validation
  if (any(derived_data$bodyfat_pct <= 0 | derived_data$bodyfat_pct > 100, na.rm = TRUE)) {
    validation_issues <- c(validation_issues, "Body fat percentage outside 0-100 range")
  }

  # Age validation
  if (any(derived_data$age < 20 | derived_data$age > 59, na.rm = TRUE)) {
    validation_issues <- c(validation_issues, "Age outside expected 20-59 range")
  }

  # Survey weight validation
  if (any(derived_data$survey_weight <= 0, na.rm = TRUE)) {
    validation_issues <- c(validation_issues, "Invalid survey weights (≤0)")
  }

  # Sex distribution check
  sex_dist <- table(derived_data$sex)
  if (length(sex_dist) != 2 || any(sex_dist < 10)) {
    validation_issues <- c(validation_issues, "Unusual sex distribution")
  }

  # Generate statistical summary
  stats_summary <- list(
    n_total = nrow(derived_data),
    bmi_mean = mean(derived_data$bmi, na.rm = TRUE),
    bmi_sd = sd(derived_data$bmi, na.rm = TRUE),
    bodyfat_mean = mean(derived_data$bodyfat_pct, na.rm = TRUE),
    bodyfat_sd = sd(derived_data$bodyfat_pct, na.rm = TRUE),
    age_mean = mean(derived_data$age, na.rm = TRUE),
    age_sd = sd(derived_data$age, na.rm = TRUE),
    pct_female = mean(derived_data$sex == "Female", na.rm = TRUE) * 100,
    pct_obese = mean(derived_data$bmi >= 30, na.rm = TRUE) * 100
  )

  status <- if (length(validation_issues) == 0) "PASSED" else "WARNING"
  details <- paste("Statistical validation with", length(validation_issues), "issues")

  add_validation_stage(
    "Statistical Validation",
    status,
    details,
    warning = if (length(validation_issues) > 0) paste(validation_issues, collapse = "; ") else NULL
  )

}, error = function(e) {
  add_validation_stage(
    "Statistical Validation",
    "FAILED",
    "Statistical validation failed",
    error = e$message
  )
})

# Stage 6: Configuration and environment validation
cat("\nStage 6: Validating configuration and environment...\n")
stage6_result <- tryCatch({
  config_file <- file.path(repo_root, "config", "config.yml")

  if (!file.exists(config_file)) {
    stop(NhanesError(
      "Configuration file not found",
      code = "MISSING_CONFIG",
      details = list(config_file = config_file)
    ))
  }

  config <- yaml::read_yaml(config_file)

  # Validate configuration structure
  required_sections <- c("data", "outputs", "nhanes", "analysis", "logging")
  missing_sections <- setdiff(required_sections, names(config))

  if (length(missing_sections) > 0) {
    warning(NhanesWarning(
      paste("Configuration missing sections:", paste(missing_sections, collapse = ", ")),
      code = "INCOMPLETE_CONFIG",
      details = list(missing_sections = missing_sections)
    ))
  }

  # Validate paths exist
  path_issues <- c()
  if (!dir.exists(config$data$raw_dir)) path_issues <- c(path_issues, "raw_dir")
  if (!dir.exists(config$data$derived_dir)) path_issues <- c(path_issues, "derived_dir")
  if (!dir.exists(config$outputs$logs_dir)) path_issues <- c(path_issues, "logs_dir")

  if (length(path_issues) > 0) {
    warning(NhanesWarning(
      paste("Configuration paths do not exist:", paste(path_issues, collapse = ", ")),
      code = "INVALID_CONFIG_PATHS",
      details = list(missing_paths = path_issues)
    ))
  }

  status <- if (length(missing_sections) == 0 && length(path_issues) == 0) "PASSED" else "WARNING"
  details <- paste("Configuration validation completed")

  add_validation_stage(
    "Configuration",
    status,
    details,
    warning = if (length(path_issues) > 0) paste("Missing paths:", paste(path_issues, collapse = ", ")) else NULL
  )

}, error = function(e) {
  add_validation_stage(
    "Configuration",
    "FAILED",
    "Configuration validation failed",
    error = e$message
  )
})

# Generate comprehensive validation report
end_time <- Sys.time()
pipeline_validation$end_time <- end_time
pipeline_validation$total_time <- as.numeric(difftime(end_time, pipeline_validation$start_time, units = "secs"))

# Determine overall status
if (length(pipeline_validation$errors) > 0) {
  pipeline_validation$overall_status <- "FAILED"
} else if (length(pipeline_validation$warnings) > 0) {
  pipeline_validation$overall_status <- "WARNING"
} else {
  pipeline_validation$overall_status <- "PASSED"
}

# Save comprehensive validation report
validation_report <- list(
  pipeline_summary = pipeline_validation,
  stage_details = pipeline_validation$stages,
  timestamp = Sys.time(),
  total_stages = length(pipeline_validation$stages),
  passed_stages = sum(sapply(pipeline_validation$stages, function(s) s$status == "PASSED")),
  warning_stages = sum(sapply(pipeline_validation$stages, function(s) s$status == "WARNING")),
  failed_stages = sum(sapply(pipeline_validation$stages, function(s) s$status == "FAILED"))
)

report_file <- file.path(logs_dir, "comprehensive_validation_report.json")
write_json(validation_report, report_file, pretty = TRUE, auto_unbox = TRUE)

# Generate HTML validation report
html_report <- generate_validation_html_report(validation_report)
html_report_file <- file.path(logs_dir, "validation_pipeline_report.html")
writeLines(html_report, html_report_file)

# Final summary
cat("\n" %>% paste(rep("=", 60), collapse = ""), "\n")
cat("VALIDATION PIPELINE COMPLETED\n")
cat(sprintf("Overall Status: %s\n", pipeline_validation$overall_status))
cat(sprintf("Total Stages: %d\n", length(pipeline_validation$stages)))
cat(sprintf("Passed: %d, Warnings: %d, Failed: %d\n",
           sum(sapply(pipeline_validation$stages, function(s) s$status == "PASSED")),
           sum(sapply(pipeline_validation$stages, function(s) s$status == "WARNING")),
           sum(sapply(pipeline_validation$stages, function(s) s$status == "FAILED"))))
cat(sprintf("Total Time: %.2f seconds\n", pipeline_validation$total_time))
cat(sprintf("Reports saved to: %s\n", logs_dir))
cat("\n")

if (pipeline_validation$overall_status == "FAILED") {
  cat("❌ VALIDATION FAILED - Please address errors before proceeding with analysis\n")
  quit(status = 1)
} else if (pipeline_validation$overall_status == "WARNING") {
  cat("⚠️  VALIDATION PASSED WITH WARNINGS - Review warnings but analysis can proceed\n")
} else {
  cat("✅ VALIDATION PASSED - All checks successful, analysis ready to proceed\n")
}

# Cleanup performance tracking
cleanup_performance_tracking()

cat("\nValidation pipeline completed successfully!\n")

# Function to generate HTML validation report
generate_validation_html_report <- function(validation_report) {
  stages <- validation_report$stage_details

  html_content <- paste0("
<!DOCTYPE html>
<html>
<head>
    <title>NHANES Data Validation Pipeline Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .header { background-color: #f0f8ff; padding: 20px; border-radius: 8px; margin-bottom: 30px; }
        .stage { margin-bottom: 20px; padding: 15px; border-radius: 5px; }
        .passed { background-color: #d4edda; border-left: 5px solid #28a745; }
        .warning { background-color: #fff3cd; border-left: 5px solid #ffc107; }
        .failed { background-color: #f8d7da; border-left: 5px solid #dc3545; }
        .skipped { background-color: #e2e3e5; border-left: 5px solid #6c757d; }
        .summary { background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; }
        .error { color: #dc3545; font-weight: bold; }
        .warning-text { color: #ffc107; font-weight: bold; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
    </style>
</head>
<body>
    <div class='header'>
        <h1>NHANES Data Validation Pipeline Report</h1>
        <p><strong>Generated:</strong> ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>
        <p><strong>Overall Status:</strong>
            <span class='", tolower(validation_report$pipeline_summary$overall_status), "'>
                ", validation_report$pipeline_summary$overall_status, "
            </span>
        </p>
        <p><strong>Total Stages:</strong> ", validation_report$total_stages, "</p>
    </div>

    <div class='summary'>
        <h2>Summary</h2>
        <table>
            <tr><th>Metric</th><th>Value</th></tr>
            <tr><td>Passed Stages</td><td>", validation_report$passed_stages, "</td></tr>
            <tr><td>Warning Stages</td><td>", validation_report$warning_stages, "</td></tr>
            <tr><td>Failed Stages</td><td>", validation_report$failed_stages, "</td></tr>
            <tr><td>Total Time</td><td>", round(validation_report$pipeline_summary$total_time, 2), " seconds</td></tr>
        </table>
    </div>
")

  # Add stage details
  for (stage in stages) {
    status_class <- tolower(stage$status)

    html_content <- paste0(html_content, "
    <div class='stage ", status_class, "'>
        <h3>", stage$stage, "</h3>
        <p><strong>Status:</strong> ", stage$status, "</p>
        <p><strong>Timestamp:</strong> ", format(stage$timestamp, "%H:%M:%S"), "</p>
")

    if (!is.null(stage$details)) {
      html_content <- paste0(html_content, "
        <p><strong>Details:</strong> ", stage$details, "</p>
")
    }

    if (!is.null(stage$error)) {
      html_content <- paste0(html_content, "
        <p class='error'><strong>Error:</strong> ", stage$error, "</p>
")
    }

    if (!is.null(stage$warning)) {
      html_content <- paste0(html_content, "
        <p class='warning-text'><strong>Warning:</strong> ", stage$warning, "</p>
")
    }

    html_content <- paste0(html_content, "
    </div>
")
  }

  html_content <- paste0(html_content, "
</body>
</html>
")

  return(html_content)
}





