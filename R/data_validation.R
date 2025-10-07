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
    report_file <- file.path(config$outputs_logs_path, "data_quality_report.html")
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
