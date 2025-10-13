#!/usr/bin/env Rscript

# Comprehensive error handling and validation utilities for NHANES analysis

# Custom error classes
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

FileNotFoundError <- function(message, file_path = NULL) {
  error <- NhanesError(
    message = message,
    code = "FILE_NOT_FOUND",
    details = list(file_path = file_path)
  )
  class(error) <- c("FileNotFoundError", "NhanesError", "error", "condition")
  error
}

# Enhanced logging function
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

# Safe file operations with error handling
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

# Utility function for safe NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

