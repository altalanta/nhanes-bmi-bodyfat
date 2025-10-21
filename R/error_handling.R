# Comprehensive error handling and validation utilities for NHANES analysis

#' Custom error classes for NHANES analysis
#'
#' Creates a custom error object with structured information for NHANES analysis.
#'
#' @param message A character string describing the error
#' @param code A character string representing the error code
#' @param details A list containing additional error details
#' @return An error object of class NhanesError
#' @export
NhanesError <- function(message, code = NULL, details = NULL) {
  error <- list(
    message = message,
    code = code,
    details = details,
    timestamp = Sys.time(),
    session_info = sessionInfo()
  )
  class(error) <- c("NhanesError", "error", "condition")
  error
}

#' Data validation error class
#'
#' Creates a specific error type for data validation issues.
#'
#' @param message Error message
#' @param field The field that failed validation
#' @param expected Expected value or condition
#' @param actual Actual value that caused the error
#' @return An error object of class DataValidationError
#' @export
DataValidationError <- function(message, field = NULL, expected = NULL, actual = NULL) {
  error <- NhanesError(
    message = message,
    code = "DATA_VALIDATION_ERROR",
    details = list(
      field = field,
      expected = expected,
      actual = actual
    )
  )
  class(error) <- c("DataValidationError", "NhanesError", "error", "condition")
  error
}

#' File not found error class
#'
#' Creates a specific error type for missing files.
#'
#' @param message Error message
#' @param file_path Path to the missing file
#' @return An error object of class FileNotFoundError
#' @export
FileNotFoundError <- function(message, file_path = NULL) {
  error <- NhanesError(
    message = message,
    code = "FILE_NOT_FOUND",
    details = list(file_path = file_path)
  )
  class(error) <- c("FileNotFoundError", "NhanesError", "error", "condition")
  error
}


#' Enhanced logging function with error handling
#'
#' Logs messages to file and console with timestamps and levels.
#'
#' @param message Character string to log
#' @param level Log level (INFO, WARNING, ERROR)
#' @param config Configuration list (optional)
#' @return None
#' @export
safe_log <- function(message, level = "INFO", config = NULL) {
  if (!is.null(config) && !is.null(config$log_file_path)) {
    log_file <- config$log_file_path
  } else {
    log_file <- "outputs/logs/analysis.log"
  }

  # Ensure log directory exists
  dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)

  # Format message with timestamp and level
  formatted_message <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " [", level, "] ",
    message,
    "\n"
  )

  # Write to file
  tryCatch({
    cat(formatted_message, file = log_file, append = TRUE)
  }, error = function(e) {
    warning("Could not write to log file: ", log_file)
  })

  # Also print to console for immediate feedback
  if (level %in% c("ERROR", "WARNING")) {
    cat(formatted_message)
  }
}

#' Safe file reading with error handling
#'
#' Reads XPT files with comprehensive error handling and validation.
#'
#' @param file_path Path to the XPT file
#' @param description Description of the file for error messages
#' @return Data frame from the XPT file
#' @export
safe_read_xpt <- function(file_path, description = "XPT file") {
  if (!file.exists(file_path)) {
    stop(FileNotFoundError(
      paste(description, "not found:", file_path),
      file_path = file_path
    ))
  }

  tryCatch({
    data <- foreign::read.xport(file_path)
    safe_log(paste("Successfully loaded", description, "-", nrow(data), "records"), "INFO")
    return(data)
  }, error = function(e) {
    stop(NhanesError(
      paste("Failed to read", description, ":", e$message),
      code = "FILE_READ_ERROR",
      details = list(file_path = file_path, original_error = e$message)
    ))
  })
}

# Data validation functions
validate_nhanes_data <- function(data, data_name, required_vars = NULL) {
  safe_log(paste("Validating", data_name, "dataset"), "INFO")

  # Check if data exists
  if (is.null(data) || nrow(data) == 0) {
    stop(DataValidationError(
      paste(data_name, "dataset is empty or NULL"),
      field = "dataset",
      expected = "non-empty data frame",
      actual = paste("rows:", nrow(data) %||% 0)
    ))
  }

  # Check for required variables
  if (!is.null(required_vars)) {
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
      stop(DataValidationError(
        paste("Missing required variables in", data_name),
        field = "variables",
        expected = required_vars,
        actual = paste("missing:", paste(missing_vars, collapse = ", "))
      ))
    }
  }

  # Check for reasonable number of observations
  if (nrow(data) < 10) {
    warning(NhanesError(
      paste(data_name, "has very few observations (", nrow(data), ")"),
      code = "FEW_OBSERVATIONS",
      details = list(dataset = data_name, n_obs = nrow(data))
    ))
  }

  safe_log(paste(data_name, "validation passed"), "INFO")
  return(TRUE)
}

# Safe configuration loading with fallbacks
safe_load_config <- function(config_file = "config/config.yml") {
  tryCatch({
    source("scripts/load_config.R")
    config <- load_config(config_file)
    safe_log("Configuration loaded successfully", "INFO")
    return(config)
  }, error = function(e) {
    safe_log(paste("Configuration loading failed:", e$message), "WARNING")
    safe_log("Using default configuration", "INFO")

    # Return default configuration
    return(list(
      data_raw_path = "data/raw",
      data_derived_path = "data/derived",
      outputs_tables_path = "outputs/tables",
      outputs_figures_path = "outputs/figures",
      outputs_logs_path = "outputs/logs",
      outputs_report_path = "outputs/report",
      log_file_path = "outputs/logs/analysis.log",
      age_min = 20,
      age_max = 59,
      survey_weights_col = "WTMEC2YR",
      strata_col = "SDMVSTRA",
      psu_col = "SDMVPSU"
    ))
  })
}

# Safe directory creation
ensure_output_dirs <- function(config) {
  dirs <- c(
    config$data_raw_path,
    config$data_derived_path,
    config$outputs_tables_path,
    config$outputs_figures_path,
    config$outputs_logs_path,
    config$outputs_report_path
  )

  for (dir in dirs) {
    tryCatch({
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        safe_log(paste("Created directory:", dir), "INFO")
      }
    }, error = function(e) {
      stop(NhanesError(
        paste("Failed to create directory:", dir),
        code = "DIRECTORY_CREATION_ERROR",
        details = list(directory = dir, error = e$message)
      ))
    })
  }
}

# Safe data export with validation
safe_save_data <- function(data, file_path, format = "rds") {
  tryCatch({
    dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

    if (format == "rds") {
      saveRDS(data, file_path)
    } else if (format == "csv") {
      readr::write_csv(data, file_path)
    } else {
      stop(NhanesError(
        "Unsupported export format",
        code = "UNSUPPORTED_FORMAT",
        details = list(format = format, supported = c("rds", "csv"))
      ))
    }

    safe_log(paste("Data exported to:", file_path), "INFO")
    return(TRUE)
  }, error = function(e) {
    stop(NhanesError(
      paste("Failed to export data to", file_path),
      code = "EXPORT_ERROR",
      details = list(file_path = file_path, format = format, error = e$message)
    ))
  })
}

# Validate survey design parameters
validate_survey_design <- function(data, weights_col, strata_col, psu_col) {
  safe_log("Validating survey design parameters", "INFO")

  # Check for missing values in design variables
  missing_weights <- sum(is.na(data[[weights_col]]))
  missing_strata <- sum(is.na(data[[strata_col]]))
  missing_psu <- sum(is.na(data[[psu_col]]))

  if (missing_weights > 0) {
    stop(DataValidationError(
      "Missing survey weights",
      field = weights_col,
      expected = "no missing values",
      actual = paste(missing_weights, "missing values")
    ))
  }

  if (missing_strata > 0) {
    warning(NhanesError(
      paste("Missing strata values (", missing_strata, ")"),
      code = "MISSING_STRATA",
      details = list(column = strata_col, missing_count = missing_strata)
    ))
  }

  if (missing_psu > 0) {
    warning(NhanesError(
      paste("Missing PSU values (", missing_psu, ")"),
      code = "MISSING_PSU",
      details = list(column = psu_col, missing_count = missing_psu)
    ))
  }

  # Check for reasonable weight distribution
  weight_summary <- summary(data[[weights_col]])
  if (weight_summary["Min."] <= 0) {
    stop(DataValidationError(
      "Invalid survey weights (negative or zero)",
      field = weights_col,
      expected = "positive weights",
      actual = paste("min:", weight_summary["Min."])
    ))
  }

  safe_log("Survey design validation passed", "INFO")
  return(TRUE)
}

# Global error handler
handle_error <- function(e) {
  if (inherits(e, "NhanesError")) {
    safe_log(paste("NHANES Error [", e$code, "]:", e$message), "ERROR")
    if (!is.null(e$details)) {
      safe_log(paste("Details:", capture.output(str(e$details))), "ERROR")
    }
  } else {
    safe_log(paste("Unexpected error:", e$message), "ERROR")
  }

  # Re-throw the error to stop execution
  stop(e)
}

# Wrapper for safe execution of analysis steps
safe_execute <- function(expr, step_name, config = NULL) {
  tryCatch({
    safe_log(paste("Starting step:", step_name), "INFO")
    result <- eval(expr)
    safe_log(paste("Completed step:", step_name), "INFO")
    return(result)
  }, error = function(e) {
    safe_log(paste("Error in step", step_name, ":", e$message), "ERROR")
    handle_error(e)
  })
}

# Enhanced error suggestion system
get_error_suggestions <- function(error_code, error_message) {
  suggestions <- list()

  # Data Loading errors
  if (grepl("FILE_NOT_FOUND", error_code) || grepl("not found", error_message, ignore.case = TRUE)) {
    suggestions <- c(
      "📥 Download NHANES data: Run 'make fetch' to download required files",
      "📁 Check file paths in config/config.yml",
      "🔍 Verify internet connection for data download",
      "💾 Ensure sufficient disk space (NHANES files ~100MB)"
    )
  }

  # Data Validation errors
  if (grepl("DATA_VALIDATION_ERROR", error_code) || grepl("missing|empty", error_message, ignore.case = TRUE)) {
    suggestions <- c(
      "📊 Check NHANES data dictionary for correct variable names",
      "🔍 Verify data file version (should be 2017-2018)",
      "📈 Review sample size requirements (minimum 30 observations)",
      "🔧 Check data types (numeric, factor, etc.)"
    )
  }

  # Survey Design errors
  if (grepl("survey|weight|strata", error_message, ignore.case = TRUE)) {
    suggestions <- c(
      "⚖️ Verify survey weight column name (typically 'WTMEC2YR')",
      "🏗️ Check strata and PSU column names in configuration",
      "📖 Review NHANES survey methodology documentation",
      "🔍 Ensure survey design variables are not missing"
    )
  }

  # Statistical Analysis errors
  if (grepl("correlation|regression|model", error_message, ignore.case = TRUE)) {
    suggestions <- c(
      "📏 Check for sufficient sample size in subgroups",
      "🔢 Verify outcome and predictor variables are numeric",
      "📊 Review statistical model assumptions",
      "🔍 Check for multicollinearity in predictor variables"
    )
  }

  # Package/Dependency errors
  if (grepl("package|library|install", error_message, ignore.case = TRUE)) {
    suggestions <- c(
      "📦 Install missing packages: install.packages(c('dplyr', 'ggplot2', 'survey', 'foreign'))",
      "🔄 Update R to latest version if needed",
      "💻 Check system dependencies (e.g., gfortran for some packages)",
      "🔧 Use renv::restore() to install project dependencies"
    )
  }

  # Configuration errors
  if (grepl("config|yml|yaml", error_message, ignore.case = TRUE)) {
    suggestions <- c(
      "⚙️ Use Configuration Wizard: R -e 'shiny::runApp(\"app.R\")'",
      "📋 Check config/config.yml exists and is valid YAML",
      "🔍 Verify all required configuration sections are present",
      "📖 Review configuration documentation in tutorials/"
    )
  }

  # File/Directory errors
  if (grepl("directory|permission|disk", error_message, ignore.case = TRUE)) {
    suggestions <- c(
      "📁 Create required directories: mkdir -p data/raw data/derived outputs/{tables,figures,logs,report}",
      "🔐 Check file and directory permissions",
      "💾 Ensure sufficient disk space",
      "🔧 Verify directory paths are valid"
    )
  }

  # Generic suggestions if no specific category matches
  if (length(suggestions) == 0) {
    suggestions <- c(
      "📖 Check complete error log in outputs/logs/",
      "🔧 Verify your R environment and package versions",
      "📚 Review troubleshooting guide in tutorials/",
      "🐛 Check GitHub issues for similar problems",
      "📧 Contact support if issue persists"
    )
  }

  return(suggestions)
}

# Enhanced error display for user-friendly troubleshooting
display_user_friendly_error <- function(error_obj) {
  if (!inherits(error_obj, "NhanesError")) {
    cat("❌ An unexpected error occurred. Please check the logs for details.\n")
    return(invisible())
  }

  cat("❌ NHANES Analysis Error\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Message: ", error_obj$message, "\n\n")

  if (!is.null(error_obj$code)) {
    cat("🔍 Error Code: ", error_obj$code, "\n")
  }

  if (!is.null(error_obj$details)) {
    cat("📋 Details:\n")
    if (is.list(error_obj$details)) {
      for (key in names(error_obj$details)) {
        cat("  • ", key, ": ", error_obj$details[[key]], "\n")
      }
    } else {
      cat("  ", error_obj$details, "\n")
    }
    cat("\n")
  }

  # Get and display suggestions
  suggestions <- get_error_suggestions(error_obj$code %||% "", error_obj$message)
  if (length(suggestions) > 0) {
    cat("💡 Suggested Solutions:\n")
    for (i in seq_along(suggestions)) {
      cat("  ", i, ". ", suggestions[i], "\n")
    }
    cat("\n")
  }

  # Help resources
  cat("📚 Help Resources:\n")
  cat("  • Interactive Tutorial: tutorials/getting_started.Rmd\n")
  cat("  • Troubleshooting Guide: tutorials/help_troubleshooting.Rmd\n")
  cat("  • Complete Logs: outputs/logs/analysis_log.txt\n")
  cat("  • Configuration Wizard: R -e 'shiny::runApp(\"app.R\")'\n")

  if (!is.null(error_obj$code)) {
    cat("  • Report Issue: https://github.com/altalanta/nhanes-bmi-bodyfat/issues\n")
  }

  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  return(invisible())
}

# Pipeline health check function
check_pipeline_health <- function() {
  health_issues <- list()

  # Check required directories
  required_dirs <- c("data/raw", "data/derived", "outputs/tables", "outputs/figures", "outputs/logs", "config")
  missing_dirs <- required_dirs[!dir.exists(required_dirs)]

  if (length(missing_dirs) > 0) {
    health_issues$directories <- paste("❌ Missing directories:", paste(missing_dirs, collapse = ", "))
  }

  # Check configuration file
  if (!file.exists("config/config.yml")) {
    health_issues$config <- "❌ Configuration file not found"
  } else {
    # Validate configuration structure
    tryCatch({
      config <- yaml::read_yaml("config/config.yml")
      required_sections <- c("data", "outputs", "nhanes", "analysis", "logging")
      missing_sections <- required_sections[!required_sections %in% names(config)]
      if (length(missing_sections) > 0) {
        health_issues$config <- paste("❌ Incomplete configuration:", paste(missing_sections, collapse = ", "))
      }
    }, error = function(e) {
      health_issues$config <- paste("❌ Invalid configuration file:", e$message)
    })
  }

  # Check required packages
  required_packages <- c("dplyr", "ggplot2", "survey", "foreign", "yaml", "future", "furrr")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    health_issues$packages <- paste("❌ Missing packages:", paste(missing_packages, collapse = ", "))
  }

  # Check data files
  nhanes_files <- c("data/raw/DEMO_J.XPT", "data/raw/BMX_J.XPT", "data/raw/DXX_J.XPT")
  missing_files <- nhanes_files[!file.exists(nhanes_files)]

  if (length(missing_files) > 0) {
    health_issues$data <- paste("❌ Missing NHANES data files:", paste(missing_files, collapse = ", "))
  }

  return(health_issues)
}

# Display pipeline health status with actionable suggestions
display_pipeline_health <- function() {
  health_issues <- check_pipeline_health()

  if (length(health_issues) == 0) {
    cat("✅ PIPELINE HEALTH CHECK: All systems operational!\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("🚀 Ready to run analysis with: make parallel-pipeline\n")
    cat("📚 Get started with: R -e 'learnr::run_tutorial(\"tutorials/getting_started\", package = \"learnr\")'\n")
    cat("⚙️ Configure settings with: R -e 'shiny::runApp(\"app.R\")'\n")
  } else {
    cat("⚠️ PIPELINE HEALTH ISSUES DETECTED\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

    for (issue_name in names(health_issues)) {
      cat(health_issues[[issue_name]], "\n")
    }

    cat("\n💡 Quick Fixes:\n")
    cat("  1. Create directories: mkdir -p data/raw data/derived outputs/{tables,figures,logs,report}\n")
    cat("  2. Install packages: install.packages(c('dplyr', 'ggplot2', 'survey', 'foreign', 'yaml', 'future', 'furrr'))\n")
    cat("  3. Download data: make fetch\n")
    cat("  4. Configure settings: R -e 'shiny::runApp(\"app.R\")'\n")

    cat("\n📖 For detailed help, see:\n")
    cat("  • Interactive Tutorial: tutorials/getting_started.Rmd\n")
    cat("  • Troubleshooting Guide: tutorials/help_troubleshooting.Rmd\n")
    cat("  • Configuration Wizard: R -e 'shiny::runApp(\"app.R\")'\n")
  }

  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
}

# Enhanced error handler for the entire pipeline
handle_pipeline_error <- function(error_obj) {
  if (inherits(error_obj, "NhanesError")) {
    safe_log(paste("NHANES Error [", error_obj$code, "]:", error_obj$message), "ERROR")

    # Display user-friendly error message
    display_user_friendly_error(error_obj)

    # Also save to error log for debugging
    error_log_file <- "outputs/logs/error_details.txt"
    dir.create(dirname(error_log_file), showWarnings = FALSE, recursive = TRUE)

    error_details <- paste0(
      "Timestamp: ", error_obj$timestamp, "\n",
      "Error Code: ", error_obj$code %||% "Unknown", "\n",
      "Message: ", error_obj$message, "\n",
      "Details: ", paste(capture.output(str(error_obj$details)), collapse = "\n"), "\n",
      "Suggestions: ", paste(get_error_suggestions(error_obj$code %||% "", error_obj$message), collapse = "\n"), "\n",
      "Session Info: ", paste(capture.output(sessionInfo()), collapse = "\n"), "\n",
      "=====================================\n"
    )

    write(error_details, file = error_log_file, append = TRUE)
  } else {
    safe_log(paste("Unexpected error:", error_obj$message), "ERROR")
    display_user_friendly_error(error_obj)
  }
}

# Utility function for safe NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
