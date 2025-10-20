# Comprehensive Data Validation Functions for NHANES Analysis

#' Perform comprehensive data quality assessment
#'
#' Conducts extensive validation of NHANES datasets including missing data patterns,
#' outlier detection, and data integrity checks.
#'
#' @param data Data frame to validate
#' @param dataset_name Name of the dataset for reporting
#' @param config Configuration list
#' @return List containing validation results and quality metrics
#' @export
assess_data_quality <- function(data, dataset_name, config = NULL) {
  safe_log(paste("Starting data quality assessment for", dataset_name), "INFO")

  # Basic data structure checks
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  missing_total <- sum(is.na(data))

  # Missing data patterns
  missing_by_col <- colSums(is.na(data))
  cols_with_missing <- sum(missing_by_col > 0)
  cols_completely_missing <- sum(missing_by_col == n_rows)

  # Data type consistency
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  categorical_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

  # Outlier detection for numeric variables
  outlier_summary <- list()
  for (col in numeric_cols) {
    if (sum(!is.na(data[[col]])) > 10) {  # Only check if we have enough data
      values <- data[[col]][!is.na(data[[col]])]
      q1 <- quantile(values, 0.25, na.rm = TRUE)
      q3 <- quantile(values, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr

      outliers <- values[values < lower_bound | values > upper_bound]
      outlier_summary[[col]] <- list(
        n_outliers = length(outliers),
        outlier_pct = length(outliers) / length(values) * 100,
        lower_bound = lower_bound,
        upper_bound = upper_bound
      )
    }
  }

  # Value range checks for key variables
  range_checks <- list()

  # BMI range check (15-60 is reasonable for adults)
  if ("BMXBMI" %in% names(data)) {
    bmi_values <- data$BMXBMI[!is.na(data$BMXBMI)]
    range_checks$BMI <- list(
      min = min(bmi_values),
      max = max(bmi_values),
      valid_range = c(15, 60),
      n_outside_range = sum(bmi_values < 15 | bmi_values > 60)
    )
  }

  # Age range check (should be 20-59 for our study)
  if ("RIDAGEYR" %in% names(data)) {
    age_values <- data$RIDAGEYR[!is.na(data$RIDAGEYR)]
    range_checks$Age <- list(
      min = min(age_values),
      max = max(age_values),
      valid_range = c(20, 59),
      n_outside_range = sum(age_values < 20 | age_values > 59)
    )
  }

  # Body fat percentage range check (reasonable: 5-50%)
  if ("bodyfat_pct" %in% names(data)) {
    bf_values <- data$bodyfat_pct[!is.na(data$bodyfat_pct)]
    range_checks$BodyFat <- list(
      min = min(bf_values),
      max = max(bf_values),
      valid_range = c(5, 50),
      n_outside_range = sum(bf_values < 5 | bf_values > 50)
    )
  }

  # Survey weight validation
  weight_checks <- list()
  if (config$survey_weights_col %in% names(data)) {
    weights <- data[[config$survey_weights_col]][!is.na(data[[config$survey_weights_col]])]
    weight_checks <- list(
      min_weight = min(weights),
      max_weight = max(weights),
      mean_weight = mean(weights),
      median_weight = median(weights),
      n_negative_weights = sum(weights <= 0),
      n_extreme_weights = sum(weights > 1000)  # Very large weights are suspicious
    )
  }

  # Create quality report
  quality_report <- list(
    dataset_name = dataset_name,
    timestamp = Sys.time(),
    basic_metrics = list(
      n_rows = n_rows,
      n_cols = n_cols,
      total_missing = missing_total,
      missing_rate = missing_total / (n_rows * n_cols) * 100
    ),
    missing_data = list(
      cols_with_missing = cols_with_missing,
      cols_completely_missing = cols_completely_missing,
      missing_by_column = missing_by_col[missing_by_col > 0]
    ),
    data_types = list(
      numeric_columns = length(numeric_cols),
      categorical_columns = length(categorical_cols)
    ),
    outlier_summary = outlier_summary,
    range_checks = range_checks,
    weight_checks = weight_checks,
    validation_status = "PASSED"
  )

  # Determine overall validation status
  if (cols_completely_missing > 0) {
    quality_report$validation_status <- "FAILED"
    quality_report$validation_issues <- c(
      quality_report$validation_issues %||% character(0),
      paste("Completely missing columns:", cols_completely_missing)
    )
  }

  if (length(range_checks) > 0) {
    for (check_name in names(range_checks)) {
      check <- range_checks[[check_name]]
      if (check$n_outside_range > 0) {
        quality_report$validation_status <- "WARNING"
        quality_report$validation_issues <- c(
          quality_report$validation_issues %||% character(0),
          paste(check_name, "has", check$n_outside_range, "values outside expected range")
        )
      }
    }
  }

  if (weight_checks$n_negative_weights > 0 || weight_checks$n_extreme_weights > 0) {
    quality_report$validation_status <- "WARNING"
    quality_report$validation_issues <- c(
      quality_report$validation_issues %||% character(0),
      "Survey weights have issues (negative or extreme values)"
    )
  }

  # Save quality report
  if (!is.null(config)) {
    report_file <- file.path(config$outputs_logs_path, paste0(dataset_name, "_quality_report.rds"))
    safe_save_data(quality_report, report_file)
  }

  safe_log(paste("Data quality assessment completed for", dataset_name), "INFO")
  safe_log(paste("Status:", quality_report$validation_status), "INFO")

  return(quality_report)
}

#' Validate data consistency across datasets
#'
#' Checks for consistency issues when merging multiple NHANES datasets.
#'
#' @param datasets List of datasets to validate
#' @return List containing consistency validation results
#' @export
validate_data_consistency <- function(datasets) {
  safe_log("Validating data consistency across datasets", "INFO")

  consistency_issues <- list()

  # Check for common SEQN values across datasets
  seqn_sets <- list()
  for (dataset_name in names(datasets)) {
    seqn_sets[[dataset_name]] <- unique(datasets[[dataset_name]]$SEQN)
  }

  # Check for overlapping SEQN values
  common_seqn <- Reduce(intersect, seqn_sets)

  if (length(common_seqn) < min(sapply(seqn_sets, length)) * 0.5) {
    consistency_issues <- c(
      consistency_issues,
      paste("Low overlap in SEQN values across datasets (",
            round(length(common_seqn) / min(sapply(seqn_sets, length)) * 100, 1),
            "% overlap)")
    )
  }

  # Check for dataset size consistency
  dataset_sizes <- data.frame(
    dataset = names(datasets),
    size = sapply(datasets, nrow),
    stringsAsFactors = FALSE
  )

  # DEMO should typically be the largest dataset
  if ("demo" %in% dataset_sizes$dataset) {
    demo_size <- dataset_sizes$size[dataset_sizes$dataset == "demo"]
    other_datasets <- dataset_sizes[dataset_sizes$dataset != "demo", ]

    for (i in 1:nrow(other_datasets)) {
      if (other_datasets$size[i] > demo_size * 1.2) {
        consistency_issues <- c(
          consistency_issues,
          paste("Unusual size relationship:", other_datasets$dataset[i],
                "is larger than DEMO dataset")
        )
      }
    }
  }

  # Check for duplicate SEQN within datasets
  for (dataset_name in names(datasets)) {
    data <- datasets[[dataset_name]]
    duplicate_seqn <- sum(duplicated(data$SEQN))
    if (duplicate_seqn > 0) {
      consistency_issues <- c(
        consistency_issues,
        paste(dataset_name, "has", duplicate_seqn, "duplicate SEQN values")
      )
    }
  }

  consistency_report <- list(
    common_seqn = length(common_seqn),
    dataset_sizes = dataset_sizes,
    issues = consistency_issues,
    validation_status = if (length(consistency_issues) == 0) "PASSED" else "WARNING"
  )

  safe_log(paste("Data consistency validation completed"), "INFO")
  if (length(consistency_issues) > 0) {
    safe_log(paste("Issues found:", length(consistency_issues)), "WARNING")
  }

  return(consistency_report)
}

#' Generate data quality report
#'
#' Creates a comprehensive HTML report of data quality assessments.
#'
#' @param quality_reports List of quality assessment results
#' @param consistency_report Consistency validation results
#' @param output_file Output file path
#' @export
generate_quality_report <- function(quality_reports, consistency_report, output_file = NULL) {
  if (is.null(output_file)) {
    output_file <- "outputs/reports/data_quality_report.html"
  }

  # Create report content
  report_content <- paste0("
<!DOCTYPE html>
<html>
<head>
    <title>NHANES Data Quality Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .section { margin-bottom: 30px; }
        .metric { background-color: #f5f5f5; padding: 10px; margin: 5px 0; }
        .warning { background-color: #fff3cd; border-left: 4px solid #ffc107; }
        .error { background-color: #f8d7da; border-left: 4px solid #dc3545; }
        .success { background-color: #d4edda; border-left: 4px solid #28a745; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
    </style>
</head>
<body>
    <h1>NHANES Data Quality Assessment Report</h1>
    <p><strong>Generated:</strong> ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>

    <div class='section'>
        <h2>Dataset Overview</h2>
")

  # Add dataset summaries
  for (report in quality_reports) {
    status_class <- switch(report$validation_status,
      "PASSED" = "success",
      "WARNING" = "warning",
      "FAILED" = "error",
      "metric"
    )

    report_content <- paste0(report_content, "
        <div class='metric ", status_class, "'>
            <h3>", report$dataset_name, " Dataset</h3>
            <p><strong>Status:</strong> ", report$validation_status, "</p>
            <p><strong>Observations:</strong> ", report$basic_metrics$n_rows, "</p>
            <p><strong>Variables:</strong> ", report$basic_metrics$n_cols, "</p>
            <p><strong>Missing Rate:</strong> ", round(report$basic_metrics$missing_rate, 2), "%</p>
        </div>
    ")
  }

  # Add consistency report
  status_class <- switch(consistency_report$validation_status,
    "PASSED" = "success",
    "WARNING" = "warning",
    "FAILED" = "error",
    "metric"
  )

  report_content <- paste0(report_content, "
    <div class='section'>
        <h2>Data Consistency</h2>
        <div class='metric ", status_class, "'>
            <p><strong>Status:</strong> ", consistency_report$validation_status, "</p>
            <p><strong>Common SEQN:</strong> ", consistency_report$common_seqn, "</p>
        </div>
    </div>
  ")

  # Close HTML
  report_content <- paste0(report_content, "
</body>
</html>
")

  # Write report
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(report_content, output_file)

  safe_log(paste("Data quality report generated:", output_file), "INFO")
  return(output_file)
}

#' Custom error class for NHANES-specific errors
#'
#' @param message Error message
#' @param code Error code for categorization
#' @param details Additional error details
#' @export
NhanesError <- function(message, code = "GENERAL_ERROR", details = NULL) {
  error <- list(
    message = message,
    code = code,
    details = details,
    timestamp = Sys.time()
  )
  class(error) <- "NhanesError"
  return(error)
}

#' Custom warning class for NHANES-specific warnings
#'
#' @param message Warning message
#' @param code Warning code for categorization
#' @param details Additional warning details
#' @export
NhanesWarning <- function(message, code = "GENERAL_WARNING", details = NULL) {
  warning <- list(
    message = message,
    code = code,
    details = details,
    timestamp = Sys.time()
  )
  class(warning) <- "NhanesWarning"
  return(warning)
}

#' Safe file reading with comprehensive error handling
#'
#' @param file_path Path to file to read
#' @param dataset_name Name of dataset for error messages
#' @param expected_vars Expected variables in the dataset
#' @return Data frame or throws NhanesError
#' @export
safe_read_xpt <- function(file_path, dataset_name, expected_vars = NULL) {
  tryCatch({
    if (!file.exists(file_path)) {
      stop(NhanesError(
        paste("File not found:", file_path),
        code = "FILE_NOT_FOUND",
        details = list(file_path = file_path, dataset_name = dataset_name)
      ))
    }

    # Check file size (should be reasonable for XPT files)
    file_size <- file.info(file_path)$size
    if (file_size > 500 * 1024 * 1024) {  # 500MB limit
      warning(NhanesWarning(
        paste("Large file size detected:", round(file_size / 1024 / 1024, 2), "MB"),
        code = "LARGE_FILE",
        details = list(file_path = file_path, size_mb = file_size / 1024 / 1024)
      ))
    }

    # Read the file
    data <- foreign::read.xport(file_path)

    if (is.null(data) || nrow(data) == 0) {
      stop(NhanesError(
        paste("Empty or invalid dataset:", dataset_name),
        code = "EMPTY_DATASET",
        details = list(file_path = file_path, dataset_name = dataset_name)
      ))
    }

    # Validate expected variables if provided
    if (!is.null(expected_vars)) {
      missing_vars <- setdiff(expected_vars, names(data))
      if (length(missing_vars) > 0) {
        warning(NhanesWarning(
          paste("Missing expected variables in", dataset_name, ":",
                paste(missing_vars, collapse = ", ")),
          code = "MISSING_VARIABLES",
          details = list(dataset_name = dataset_name, missing_vars = missing_vars)
        ))
      }
    }

    # Add dataset metadata
    attr(data, "source_file") <- file_path
    attr(data, "dataset_name") <- dataset_name
    attr(data, "read_timestamp") <- Sys.time()
    attr(data, "n_rows") <- nrow(data)
    attr(data, "n_cols") <- ncol(data)

    return(data)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Failed to read", dataset_name, ":", e$message),
        code = "READ_ERROR",
        details = list(file_path = file_path, original_error = e$message)
      ))
    }
  })
}

#' Validate survey design parameters
#'
#' @param data Dataset with survey variables
#' @param weights_col Weight variable column name
#' @param strata_col Strata variable column name
#' @param psu_col PSU variable column name
#' @return Validation results or throws NhanesError
#' @export
validate_survey_design <- function(data, weights_col, strata_col, psu_col) {

  validation_results <- list(
    passed = TRUE,
    warnings = list(),
    errors = list()
  )

  # Check if survey variables exist
  survey_vars <- c(weights_col, strata_col, psu_col)
  for (var in survey_vars) {
    if (!(var %in% names(data))) {
      error_msg <- paste("Survey variable not found:", var)
      validation_results$errors <- c(validation_results$errors, error_msg)
      validation_results$passed <- FALSE
    }
  }

  if (!validation_results$passed) {
    error_messages <- paste(validation_results$errors, collapse = "; ")
    stop(NhanesError(
      paste("Survey design validation failed:", error_messages),
      code = "INVALID_SURVEY_VARS",
      details = list(missing_vars = survey_vars[!survey_vars %in% names(data)])
    ))
  }

  # Check for missing values in survey variables
  for (var in survey_vars) {
    missing_count <- sum(is.na(data[[var]]))
    if (missing_count > 0) {
      error_msg <- paste("Missing values in survey variable", var, ":", missing_count)
      validation_results$errors <- c(validation_results$errors, error_msg)
      validation_results$passed <- FALSE
    }
  }

  if (!validation_results$passed) {
    error_messages <- paste(validation_results$errors, collapse = "; ")
    stop(NhanesError(
      paste("Survey variables have missing values:", error_messages),
      code = "MISSING_SURVEY_VALUES",
      details = list(missing_counts = survey_vars)
    ))
  }

  # Check weight distribution
  weights <- data[[weights_col]]
  if (any(weights <= 0, na.rm = TRUE)) {
    error_msg <- paste("Invalid weights (≤0) in", weights_col)
    validation_results$errors <- c(validation_results$errors, error_msg)
    validation_results$passed <- FALSE
  }

  if (max(weights, na.rm = TRUE) / min(weights, na.rm = TRUE) > 10000) {
    warning_msg <- paste("Very large weight ratio in", weights_col,
                        "(max/min =", round(max(weights, na.rm = TRUE) / min(weights, na.rm = TRUE), 2), ")")
    validation_results$warnings <- c(validation_results$warnings, warning_msg)
  }

  # Check strata distribution
  strata <- data[[strata_col]]
  n_strata <- length(unique(strata))
  if (n_strata < 5) {
    warning_msg <- paste("Few strata in", strata_col, "(", n_strata, "strata)")
    validation_results$warnings <- c(validation_results$warnings, warning_msg)
  }

  # Check PSU distribution
  psu <- data[[psu_col]]
  n_psu <- length(unique(psu))
  if (n_psu < 10) {
    warning_msg <- paste("Few PSUs in", psu_col, "(", n_psu, "PSUs)")
    validation_results$warnings <- c(validation_results$warnings, warning_msg)
  }

  # Check for singleton PSUs
  psu_counts <- table(psu)
  singleton_psus <- sum(psu_counts == 1)
  if (singleton_psus > 0) {
    warning_msg <- paste("Singleton PSUs found in", psu_col, "(", singleton_psus, "singletons)")
    validation_results$warnings <- c(validation_results$warnings, warning_msg)
  }

  # Report warnings if any
  if (length(validation_results$warnings) > 0) {
    warning_messages <- paste(validation_results$warnings, collapse = "; ")
    warning(NhanesWarning(
      paste("Survey design validation warnings:", warning_messages),
      code = "SURVEY_DESIGN_WARNINGS",
      details = list(validation_results = validation_results)
    ))
  }

  return(validation_results)
}

#' Safe execution wrapper with comprehensive error handling and logging
#'
#' @param expr Expression to execute
#' @param operation_name Name of operation for logging
#' @param config Configuration object
#' @param log_file Optional log file path
#' @return Result of expression or throws NhanesError
#' @export
safe_execute <- function(expr, operation_name, config, log_file = NULL) {
  start_time <- Sys.time()

  tryCatch({
    # Execute the expression
    result <- eval(expr)

    # Log success
    end_time <- Sys.time()
    duration <- end_time - start_time

    success_msg <- paste0("[SUCCESS] ", operation_name, " completed in ",
                         round(duration, 2), " seconds")

    if (!is.null(log_file)) {
      cat(success_msg, "\n", file = log_file, append = TRUE)
    }
    message(success_msg)

    return(result)

  }, error = function(e) {
    end_time <- Sys.time()
    duration <- end_time - start_time

    error_msg <- paste0("[ERROR] ", operation_name, " failed after ",
                       round(duration, 2), " seconds: ", e$message)

    if (!is.null(log_file)) {
      cat(error_msg, "\n", file = log_file, append = TRUE)
    }

    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Operation failed:", e$message),
        code = "OPERATION_FAILED",
        details = list(operation = operation_name, original_error = e$message)
      ))
    }
  })
}

#' Load configuration with validation
#'
#' @param config_file Path to configuration file
#' @return Configuration object or throws NhanesError
#' @export
safe_load_config <- function(config_file = "config/config.yml") {
  tryCatch({
    if (!file.exists(config_file)) {
      stop(NhanesError(
        paste("Configuration file not found:", config_file),
        code = "CONFIG_FILE_NOT_FOUND",
        details = list(config_file = config_file)
      ))
    }

    config <- yaml::read_yaml(config_file)

    # Validate required configuration sections
    required_sections <- c("data", "outputs", "nhanes", "analysis", "logging")
    missing_sections <- setdiff(required_sections, names(config))

    if (length(missing_sections) > 0) {
      stop(NhanesError(
        paste("Missing required configuration sections:",
              paste(missing_sections, collapse = ", ")),
        code = "INVALID_CONFIG",
        details = list(missing_sections = missing_sections)
      ))
    }

    # Validate file paths
    nhanes_files <- c(
      config$nhanes$demo_file,
      config$nhanes$bmx_file,
      config$nhanes$dxx_file,
      config$nhanes$dxxag_file
    )

    for (file in nhanes_files) {
      if (!file.exists(file.path(config$data$raw_dir, file))) {
        warning(NhanesWarning(
          paste("NHANES file may not exist:", file),
          code = "MISSING_NHANES_FILE",
          details = list(file = file, path = file.path(config$data$raw_dir, file))
        ))
      }
    }

    return(config)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Failed to load configuration:", e$message),
        code = "CONFIG_LOAD_ERROR",
        details = list(original_error = e$message)
      ))
    }
  })
}

#' Ensure output directories exist
#'
#' @param config Configuration object
#' @return NULL or throws NhanesError
#' @export
ensure_output_dirs <- function(config) {
  tryCatch({
    output_dirs <- c(
      config$data$raw_dir,
      config$data$derived_dir,
      config$outputs$tables_dir,
      config$outputs$figures_dir,
      config$outputs$logs_dir,
      config$outputs$report_dir
    )

    for (dir in output_dirs) {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
        message(paste("Created directory:", dir))
      }
    }

  }, error = function(e) {
    stop(NhanesError(
      paste("Failed to create output directories:", e$message),
      code = "DIRECTORY_CREATION_FAILED",
      details = list(original_error = e$message)
    ))
  })
}

#' Run complete data validation pipeline
#'
#' Orchestrates comprehensive data validation for all NHANES datasets.
#'
#' @param datasets List of NHANES datasets
#' @param config Configuration list
#' @return List containing all validation results
#' @export
run_data_validation <- function(datasets, config = NULL) {
  safe_log("Starting comprehensive data validation", "INFO")

  # Individual dataset quality assessments
  quality_reports <- list()
  for (dataset_name in names(datasets)) {
    quality_reports[[dataset_name]] <- assess_data_quality(
      datasets[[dataset_name]],
      dataset_name,
      config
    )
  }

  # Cross-dataset consistency validation
  consistency_report <- validate_data_consistency(datasets)

  # Generate comprehensive report
  if (!is.null(config)) {
    report_file <- file.path(config$outputs$logs_dir, "data_quality_report.html")
    generate_quality_report(quality_reports, consistency_report, report_file)
  }

  # Overall validation status
  overall_status <- "PASSED"
  issues_found <- 0

  for (report in quality_reports) {
    if (report$validation_status == "FAILED") {
      overall_status <- "FAILED"
      issues_found <- issues_found + 1
    } else if (report$validation_status == "WARNING" && overall_status != "FAILED") {
      overall_status <- "WARNING"
    }
  }

  if (consistency_report$validation_status == "WARNING" && overall_status == "PASSED") {
    overall_status <- "WARNING"
  }

  safe_log(paste("Data validation completed with status:", overall_status), "INFO")
  if (issues_found > 0) {
    safe_log(paste("Issues found:", issues_found), "WARNING")
  }

  return(list(
    quality_reports = quality_reports,
    consistency_report = consistency_report,
    overall_status = overall_status,
    timestamp = Sys.time()
  ))
}

