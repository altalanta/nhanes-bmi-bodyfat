# REST API and Integration for NHANES BMI-Body Fat Analysis

#' Create REST API for NHANES analysis results
#'
#' Creates a plumber-based REST API for programmatic access to analysis results.
#'
#' @param results Analysis results from run_optimized_analysis() or similar
#' @param port Port number for the API server (default: 8000)
#' @param host Host address for the API server (default: "127.0.0.1")
#' @return None (starts API server)
#' @export
create_results_api <- function(results = NULL, port = 8000, host = "127.0.0.1") {

  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("plumber package required for API creation. Install with: install.packages('plumber')")
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package required for JSON responses")
  }

  # Load results if not provided
  if (is.null(results)) {
    results_file <- "outputs/tables/nhanes_analysis_results.rds"
    if (file.exists(results_file)) {
      results <- readRDS(results_file)
    } else {
      stop("No results provided and no results file found. Please run analysis first.")
    }
  }

  # Extract key data for API responses
  correlations <- results$correlations
  validation_results <- results$validation_results
  config <- results$config

  # Create plumber API
  api <- plumber::plumb(function(pr) {

    # Health check endpoint
    pr$handle("GET", "/health", function(req, res) {
      list(
        status = "healthy",
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        version = packageVersion("nhanesbmi"),
        sample_size = correlations$sample_size,
        uptime = Sys.time() - results$timestamp
      )
    }, serializer = plumber::serializer_json())

    # Get correlation results
    pr$handle("GET", "/correlations", function(req, res) {
      list(
        overall = list(
          correlation = correlations$overall$correlation,
          ci_lower = correlations$overall$ci_lower,
          ci_upper = correlations$overall$ci_upper,
          standard_error = correlations$overall$se
        ),
        by_sex = list(
          male = list(
            correlation = correlations$male$correlation,
            sample_size = correlations$male$n_obs
          ),
          female = list(
            correlation = correlations$female$correlation,
            sample_size = correlations$female$n_obs
          )
        ),
        sample_size = correlations$sample_size,
        analysis_timestamp = format(results$timestamp, "%Y-%m-%d %H:%M:%S")
      )
    }, serializer = plumber::serializer_json())

    # Get data quality information
    pr$handle("GET", "/data-quality", function(req, res) {
      quality_data <- data.frame(
        Dataset = names(validation_results$quality_reports),
        Status = sapply(validation_results$quality_reports,
                       function(x) x$validation_status),
        Observations = sapply(validation_results$quality_reports,
                             function(x) x$basic_metrics$n_rows),
        Variables = sapply(validation_results$quality_reports,
                          function(x) x$basic_metrics$n_cols),
        Missing_Rate = round(sapply(validation_results$quality_reports,
                                   function(x) x$basic_metrics$missing_rate), 2)
      )

      list(
        overall_status = validation_results$overall_status,
        datasets = quality_data,
        consistency_status = validation_results$consistency_report$validation_status,
        analysis_timestamp = format(results$timestamp, "%Y-%m-%d %H:%M:%S")
      )
    }, serializer = plumber::serializer_json())

    # Get data summary
    pr$handle("GET", "/data-summary", function(req, res) {
      data_summary <- results$analytic_data
      list(
        observations = nrow(data_summary),
        variables = names(data_summary),
        variable_types = sapply(data_summary, class),
        missing_summary = colSums(is.na(data_summary)),
        numeric_summary = summary(data_summary[, sapply(data_summary, is.numeric)]),
        analysis_timestamp = format(results$timestamp, "%Y-%m-%d %H:%M:%S")
      )
    }, serializer = plumber::serializer_json())

    # Get filtered data (with optional parameters)
    pr$handle("GET", "/data", function(req, res) {
      query_params <- req$QUERY_STRING
      params <- plumber::parseQS(query_params)

      # Start with full dataset
      filtered_data <- results$analytic_data

      # Apply filters if provided
      if (!is.null(params$sex)) {
        sex_filter <- as.numeric(params$sex)
        filtered_data <- filtered_data[filtered_data$RIAGENDR == sex_filter, ]
      }

      if (!is.null(params$min_age)) {
        min_age <- as.numeric(params$min_age)
        filtered_data <- filtered_data[filtered_data$RIDAGEYR >= min_age, ]
      }

      if (!is.null(params$max_age)) {
        max_age <- as.numeric(params$max_age)
        filtered_data <- filtered_data[filtered_data$RIDAGEYR <= max_age, ]
      }

      if (!is.null(params$min_bmi)) {
        min_bmi <- as.numeric(params$min_bmi)
        filtered_data <- filtered_data[filtered_data$BMXBMI >= min_bmi, ]
      }

      if (!is.null(params$max_bmi)) {
        max_bmi <- as.numeric(params$max_bmi)
        filtered_data <- filtered_data[filtered_data$BMXBMI <= max_bmi, ]
      }

      # Limit response size for large datasets
      max_rows <- if (!is.null(params$max_rows)) as.numeric(params$max_rows) else 1000
      if (nrow(filtered_data) > max_rows) {
        filtered_data <- filtered_data[1:max_rows, ]
      }

      # Convert to JSON-serializable format
      json_data <- jsonlite::toJSON(filtered_data, dataframe = "rows", pretty = TRUE)

      list(
        filtered_data = json_data,
        original_rows = nrow(results$analytic_data),
        filtered_rows = nrow(filtered_data),
        applied_filters = params,
        analysis_timestamp = format(results$timestamp, "%Y-%m-%d %H:%M:%S")
      )
    }, serializer = plumber::serializer_json())

    # Get correlation plot data
    pr$handle("GET", "/plot-data", function(req, res) {
      plot_data <- results$analytic_data %>%
        select(BMXBMI, bodyfat_pct, RIAGENDR, RIDAGEYR) %>%
        mutate(
          sex = ifelse(RIAGENDR == 1, "Male", "Female"),
          age_group = cut(RIDAGEYR,
                         breaks = c(20, 30, 40, 50, 59),
                         labels = c("20-29", "30-39", "40-49", "50-59"))
        )

      list(
        scatter_data = plot_data,
        correlation = correlations$overall$correlation,
        analysis_timestamp = format(results$timestamp, "%Y-%m-%d %H:%M:%S")
      )
    }, serializer = plumber::serializer_json())

    # Get analysis configuration
    pr$handle("GET", "/config", function(req, res) {
      config_list <- as.list(config)
      config_list$analysis_timestamp <- format(results$timestamp, "%Y-%m-%d %H:%M:%S")

      config_list
    }, serializer = plumber::serializer_json())

    # Get analysis metadata
    pr$handle("GET", "/metadata", function(req, res) {
      list(
        package_name = "nhanesbmi",
        package_version = as.character(packageVersion("nhanesbmi")),
        analysis_type = "NHANES BMI vs Body Fat Correlation",
        data_source = "NHANES 2017-2018",
        sample_size = correlations$sample_size,
        age_range = config$age_range,
        survey_weights = config$survey_weights_col,
        analysis_timestamp = format(results$timestamp, "%Y-%m-%d %H:%M:%S"),
        nhanes_disclaimer = "NHANES data are collected by CDC/NCHS. Users should cite original NHANES sources appropriately."
      )
    }, serializer = plumber::serializer_json())

    # Post endpoint for custom analysis
    pr$handle("POST", "/analyze", function(req, res) {
      tryCatch({
        # Parse request body
        body <- jsonlite::fromJSON(req$postBody)

        # Extract parameters
        analysis_type <- body$analysis_type %||% "correlation"
        config_params <- body$config %||% list()

        # Create custom configuration
        custom_config <- config
        for (param in names(config_params)) {
          custom_config[[param]] <- config_params[[param]]
        }

        # Run custom analysis
        if (analysis_type == "correlation") {
          custom_correlations <- compute_correlations(create_survey_design(results$analytic_data, custom_config))
          result <- list(
            correlations = custom_correlations,
            config_used = config_params,
            analysis_type = analysis_type
          )
        } else {
          result <- list(
            error = "Analysis type not supported",
            supported_types = c("correlation"),
            analysis_type = analysis_type
          )
        }

        result$analysis_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        result

      }, error = function(e) {
        list(
          error = e$message,
          analysis_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )
      }
    }, serializer = plumber::serializer_json())

    # Export data in different formats
    pr$handle("GET", "/export", function(req, res) {
      format_type <- req$args$format %||% "json"
      filename <- req$args$filename %||% paste0("nhanes_export_", format(Sys.Date(), "%Y%m%d"))

      if (format_type == "json") {
        export_data <- jsonlite::toJSON(results$analytic_data, dataframe = "rows", pretty = TRUE)
        res$setHeader("Content-Type", "application/json")
        res$setHeader("Content-Disposition", paste0("attachment; filename=", filename, ".json"))

      } else if (format_type == "csv") {
        export_data <- utils::write.csv(results$analytic_data, row.names = FALSE)
        res$setHeader("Content-Type", "text/csv")
        res$setHeader("Content-Disposition", paste0("attachment; filename=", filename, ".csv"))

      } else if (format_type == "rds") {
        # For RDS, we'd need to handle binary data differently
        export_data <- "RDS export requires POST endpoint or direct file access"
        res$setHeader("Content-Type", "text/plain")

      } else {
        export_data <- paste("Unsupported format:", format_type)
        res$setHeader("Content-Type", "text/plain")
      }

      list(
        export_data = export_data,
        format = format_type,
        filename = filename,
        analysis_timestamp = format(results$timestamp, "%Y-%m-%d %H:%M:%S")
      )
    }, serializer = plumber::serializer_json())

    # API documentation endpoint
    pr$handle("GET", "/docs", function(req, res) {
      api_docs <- list(
        title = "NHANES BMI-Body Fat Analysis API",
        version = "1.0.0",
        description = "REST API for accessing NHANES BMI vs Body Fat analysis results",
        base_url = paste0("http://", host, ":", port),
        endpoints = list(
          health = list(
            method = "GET",
            path = "/health",
            description = "Health check and basic metadata"
          ),
          correlations = list(
            method = "GET",
            path = "/correlations",
            description = "Get correlation analysis results"
          ),
          data_quality = list(
            method = "GET",
            path = "/data-quality",
            description = "Get data quality assessment results"
          ),
          data_summary = list(
            method = "GET",
            path = "/data-summary",
            description = "Get dataset summary statistics"
          ),
          data = list(
            method = "GET",
            path = "/data",
            description = "Get filtered dataset",
            parameters = list(
              sex = "Filter by sex (1=Male, 2=Female)",
              min_age = "Minimum age filter",
              max_age = "Maximum age filter",
              min_bmi = "Minimum BMI filter",
              max_bmi = "Maximum BMI filter",
              max_rows = "Maximum number of rows to return"
            )
          ),
          plot_data = list(
            method = "GET",
            path = "/plot-data",
            description = "Get data formatted for plotting"
          ),
          config = list(
            method = "GET",
            path = "/config",
            description = "Get analysis configuration"
          ),
          metadata = list(
            method = "GET",
            path = "/metadata",
            description = "Get analysis metadata and citation information"
          ),
          analyze = list(
            method = "POST",
            path = "/analyze",
            description = "Run custom analysis",
            body_example = list(
              analysis_type = "correlation",
              config = list(age_range = c(20, 59))
            )
          ),
          export = list(
            method = "GET",
            path = "/export",
            description = "Export data in different formats",
            parameters = list(
              format = "Export format (json, csv, rds)",
              filename = "Custom filename for download"
            )
          )
        ),
        example_usage = list(
          r_example = "
# R example usage
library(httr)
library(jsonlite)

# Get correlation results
response <- GET('http://localhost:8000/correlations')
correlations <- fromJSON(content(response, 'text'))

# Get filtered data
response <- GET('http://localhost:8000/data?sex=1&min_age=30&max_bmi=35')
data <- fromJSON(content(response, 'text'))
",
          python_example = "
# Python example usage
import requests
import json

# Get correlation results
response = requests.get('http://localhost:8000/correlations')
correlations = response.json()

# Get data quality info
response = requests.get('http://localhost:8000/data-quality')
quality = response.json()
",
          curl_example = "
# curl example usage
curl http://localhost:8000/health
curl http://localhost:8000/correlations
curl 'http://localhost:8000/data?sex=1&min_age=25'
"
        )
      )

      api_docs
    }, serializer = plumber::serializer_json())

  })

  # Start the API server
  cat(sprintf("🚀 Starting NHANES API server on http://%s:%d\n", host, port))
  cat("📚 API documentation available at: http://", host, ":", port, "/docs\n", sep = "")
  cat("🔗 Available endpoints:\n")
  cat("   GET  /health       - Health check\n")
  cat("   GET  /correlations - Correlation results\n")
  cat("   GET  /data-quality - Data quality info\n")
  cat("   GET  /data-summary - Dataset summary\n")
  cat("   GET  /data         - Filtered data (with query params)\n")
  cat("   GET  /plot-data    - Plot-ready data\n")
  cat("   GET  /config       - Analysis configuration\n")
  cat("   GET  /metadata     - Analysis metadata\n")
  cat("   POST /analyze      - Custom analysis\n")
  cat("   GET  /export       - Data export\n")
  cat("   GET  /docs         - API documentation\n")
  cat("\n💡 Press Ctrl+C to stop the server\n")

  plumber::pr_run(api, host = host, port = port)
}

#' Create Python interface for NHANES analysis
#'
#' Creates a Python wrapper using reticulate for cross-language workflows.
#'
#' @param results Analysis results from nhanesbmi
#' @param python_env Python environment name (default: "nhanes-env")
#' @return Python module object for accessing results
#' @export
create_python_interface <- function(results = NULL, python_env = "nhanes-env") {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate package required for Python interface. Install with: install.packages('reticulate')")
  }

  # Load results if not provided
  if (is.null(results)) {
    results_file <- "outputs/tables/nhanes_analysis_results.rds"
    if (file.exists(results_file)) {
      results <- readRDS(results_file)
    } else {
      stop("No results provided and no results file found. Please run analysis first.")
    }
  }

  # Create Python interface module
  python_code <- paste0('
import json
import pandas as pd
from typing import Dict, List, Optional, Tuple

class NHANESResults:
    """Python interface for NHANES BMI-Body Fat analysis results."""

    def __init__(self, results_data):
        """Initialize with R results data."""
        self.results = results_data
        self.correlations = results_data.get("correlations", {})
        self.analytic_data = results_data.get("analytic_data", None)
        self.validation_results = results_data.get("validation_results", {})

    def get_correlations(self) -> Dict:
        """Get correlation analysis results."""
        return {
            "overall": {
                "correlation": self.correlations.get("overall", {}).get("correlation"),
                "ci_lower": self.correlations.get("overall", {}).get("ci_lower"),
                "ci_upper": self.correlations.get("overall", {}).get("ci_upper"),
                "standard_error": self.correlations.get("overall", {}).get("se")
            },
            "by_sex": {
                "male": {
                    "correlation": self.correlations.get("male", {}).get("correlation"),
                    "sample_size": self.correlations.get("male", {}).get("n_obs")
                },
                "female": {
                    "correlation": self.correlations.get("female", {}).get("correlation"),
                    "sample_size": self.correlations.get("female", {}).get("n_obs")
                }
            },
            "sample_size": self.correlations.get("sample_size")
        }

    def get_data_quality(self) -> Dict:
        """Get data quality assessment results."""
        quality_reports = self.validation_results.get("quality_reports", {})

        datasets = []
        for dataset_name, report in quality_reports.items():
            datasets.append({
                "name": dataset_name,
                "status": report.get("validation_status"),
                "observations": report.get("basic_metrics", {}).get("n_rows"),
                "variables": report.get("basic_metrics", {}).get("n_cols"),
                "missing_rate": round(report.get("basic_metrics", {}).get("missing_rate", 0), 2)
            })

        return {
            "overall_status": self.validation_results.get("overall_status"),
            "datasets": datasets,
            "consistency_status": self.validation_results.get("consistency_report", {}).get("validation_status")
        }

    def get_data_summary(self) -> Dict:
        """Get dataset summary statistics."""
        if self.analytic_data is None:
            return {"error": "No analytic data available"}

        # Convert R data to pandas DataFrame for Python analysis
        # This would require reticulate to handle the conversion
        return {
            "observations": len(self.analytic_data),
            "variables": list(self.analytic_data.columns) if hasattr(self.analytic_data, "columns") else [],
            "summary_stats": "Available via get_dataframe() method"
        }

    def get_dataframe(self):
        """Get analytic data as pandas DataFrame."""
        # This would require reticulate conversion
        # For now, return placeholder
        return pd.DataFrame()  # Placeholder

    def export_to_json(self, filename: str) -> str:
        """Export results to JSON file."""
        with open(filename, "w") as f:
            json.dump(self.results, f, indent=2, default=str)
        return filename

    def export_to_csv(self, filename: str) -> str:
        """Export analytic data to CSV file."""
        df = self.get_dataframe()
        df.to_csv(filename, index=False)
        return filename

# Factory function to create Python interface
def create_nhanes_interface(results_dict):
    """Create Python interface from R results dictionary."""
    return NHANESResults(results_dict)

# Example usage functions
def example_correlation_analysis():
    """Example: Get correlation results in Python."""
    # This would be called from Python after importing the interface
    results = create_nhanes_interface(r_results_dict)  # r_results_dict from R
    correlations = results.get_correlations()

    print(f"Overall correlation: {correlations[\"overall\"][\"correlation\"]:.3f}")
    print(f"Male correlation: {correlations[\"by_sex\"][\"male\"][\"correlation\"]:.3f}")
    print(f"Female correlation: {correlations[\"by_sex\"][\"female\"][\"correlation\"]:.3f}")

    return correlations

def example_data_quality_check():
    """Example: Check data quality in Python."""
    results = create_nhanes_interface(r_results_dict)
    quality = results.get_data_quality()

    print(f"Overall status: {quality[\"overall_status\"]}")
    for dataset in quality[\"datasets\"]:
        print(f"{dataset[\"name\"]}: {dataset[\"status\"]} ({dataset[\"observations\"]} obs)")

    return quality
')

  # Create Python module file
  python_module_path <- file.path(tempdir(), "nhanes_interface.py")
  writeLines(python_code, python_module_path)

  cat("✅ Python interface created!\n")
  cat(sprintf("📄 Module file: %s\n", python_module_path))
  cat("\n🔧 To use in Python:\n")
  cat("import sys\n")
  cat(sprintf("sys.path.append('%s')\n", dirname(python_module_path)))
  cat("import nhanes_interface\n")
  cat("results = nhanes_interface.create_nhanes_interface(r_results_dict)\n")
  cat("correlations = results.get_correlations()\n")

  return(python_module_path)
}

#' Store results in database for long-term access
#'
#' Stores analysis results in SQLite database for persistent storage and querying.
#'
#' @param results Analysis results from nhanesbmi
#' @param db_path Path to SQLite database file (default: "nhanes_results.db")
#' @param table_name Name of results table (default: "analysis_results")
#' @return Database connection object
#' @export
store_results_in_database <- function(results, db_path = "nhanes_results.db", table_name = "analysis_results") {

  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("DBI package required for database operations")
  }

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite package required for SQLite database operations")
  }

  # Connect to database (creates if doesn't exist)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Create results table if it doesn't exist
  create_table_sql <- sprintf("
    CREATE TABLE IF NOT EXISTS %s (
      analysis_id TEXT PRIMARY KEY,
      correlation_overall REAL,
      correlation_male REAL,
      correlation_female REAL,
      sample_size INTEGER,
      timestamp TEXT,
      config_json TEXT,
      data_hash TEXT,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    )
  ", table_name)

  DBI::dbExecute(con, create_table_sql)

  # Prepare data for insertion
  results_df <- data.frame(
    analysis_id = digest::digest(results),
    correlation_overall = results$correlations$overall$correlation,
    correlation_male = results$correlations$male$correlation,
    correlation_female = results$correlations$female$correlation,
    sample_size = results$correlations$sample_size,
    timestamp = format(results$timestamp, "%Y-%m-%d %H:%M:%S"),
    config_json = jsonlite::toJSON(results$config, auto_unbox = TRUE),
    data_hash = digest::digest(results$analytic_data)
  )

  # Insert or update results
  tryCatch({
    DBI::dbWriteTable(con, table_name, results_df, append = TRUE, overwrite = FALSE)
    cat(sprintf("✅ Results stored in database: %s\n", db_path))
  }, error = function(e) {
    if (grepl("UNIQUE constraint failed", e$message)) {
      cat("ℹ Results already exist in database (same analysis ID)\n")
    } else {
      stop(e)
    }
  })

  return(con)
}

#' Query results from database
#'
#' Retrieves previously stored analysis results from database.
#'
#' @param db_path Path to SQLite database file
#' @param table_name Name of results table
#' @param analysis_id Specific analysis ID to retrieve (optional)
#' @return Data frame with query results
#' @export
query_database_results <- function(db_path = "nhanes_results.db", table_name = "analysis_results", analysis_id = NULL) {

  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Database packages required for querying results")
  }

  if (!file.exists(db_path)) {
    stop(sprintf("Database file not found: %s", db_path))
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  tryCatch({
    if (!is.null(analysis_id)) {
      # Query specific analysis
      query <- sprintf("SELECT * FROM %s WHERE analysis_id = ?", table_name)
      results <- DBI::dbGetQuery(con, query, params = list(analysis_id))
    } else {
      # Query all results
      query <- sprintf("SELECT * FROM %s ORDER BY created_at DESC", table_name)
      results <- DBI::dbGetQuery(con, query)
    }

    return(results)

  }, finally = {
    DBI::dbDisconnect(con)
  })
}

#' Export results in FHIR format for healthcare interoperability
#'
#' Exports analysis results in FHIR (Fast Healthcare Interoperability Resources) format
#' for integration with healthcare systems and electronic health records.
#'
#' @param results Analysis results from nhanesbmi
#' @param output_file Output JSON file path
#' @return Path to created FHIR file
#' @export
export_fhir_format <- function(results, output_file = "nhanes_fhir_results.json") {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package required for FHIR export")
  }

  # Create FHIR Observation resources for correlation results
  fhir_resources <- list()

  # Overall correlation observation
  overall_observation <- list(
    resourceType = "Observation",
    id = paste0("nhanes-correlation-overall-", format(results$timestamp, "%Y%m%d%H%M%S")),
    status = "final",
    category = list(list(
      coding = list(list(
        system = "http://terminology.hl7.org/CodeSystem/observation-category",
        code = "vital-signs",
        display = "Vital Signs"
      ))
    )),
    code = list(
      coding = list(list(
        system = "http://loinc.org",
        code = "8480-6",  # Systolic blood pressure (placeholder for correlation)
        display = "Systolic blood pressure"
      )),
      text = "BMI-Body Fat Correlation (Overall)"
    ),
    subject = list(
      reference = "Population/NHANES-2017-2018"
    ),
    effectivePeriod = list(
      start = "2017-01-01",
      end = "2018-12-31"
    ),
    valueQuantity = list(
      value = results$correlations$overall$correlation,
      unit = "correlation coefficient",
      system = "http://unitsofmeasure.org",
      code = "1"
    ),
    interpretation = list(list(
      coding = list(list(
        system = "http://terminology.hl7.org/CodeSystem/v3-ObservationInterpretation",
        code = if (results$correlations$overall$correlation > 0.8) "H" else "N",
        display = if (results$correlations$overall$correlation > 0.8) "High" else "Normal"
      ))
    )),
    note = list(list(
      text = sprintf("95%% CI: [%.3f, %.3f], Sample size: %d",
                     results$correlations$overall$ci_lower,
                     results$correlations$overall$ci_upper,
                     results$correlations$sample_size)
    ))
  )

  fhir_resources <- append(fhir_resources, list(overall_observation))

  # Male correlation observation
  male_observation <- list(
    resourceType = "Observation",
    id = paste0("nhanes-correlation-male-", format(results$timestamp, "%Y%m%d%H%M%S")),
    status = "final",
    category = list(list(
      coding = list(list(
        system = "http://terminology.hl7.org/CodeSystem/observation-category",
        code = "vital-signs",
        display = "Vital Signs"
      ))
    )),
    code = list(
      coding = list(list(
        system = "http://snomed.info/sct",
        code = "248153007",  # Male (finding)
        display = "Male"
      )),
      text = "BMI-Body Fat Correlation (Male)"
    ),
    subject = list(
      reference = "Population/NHANES-2017-2018-Male"
    ),
    effectivePeriod = list(
      start = "2017-01-01",
      end = "2018-12-31"
    ),
    valueQuantity = list(
      value = results$correlations$male$correlation,
      unit = "correlation coefficient",
      system = "http://unitsofmeasure.org",
      code = "1"
    ),
    note = list(list(
      text = sprintf("Sample size: %d", results$correlations$male$n_obs)
    ))
  )

  fhir_resources <- append(fhir_resources, list(male_observation))

  # Female correlation observation
  female_observation <- list(
    resourceType = "Observation",
    id = paste0("nhanes-correlation-female-", format(results$timestamp, "%Y%m%d%H%M%S")),
    status = "final",
    category = list(list(
      coding = list(list(
        system = "http://terminology.hl7.org/CodeSystem/observation-category",
        code = "vital-signs",
        display = "Vital Signs"
      ))
    )),
    code = list(
      coding = list(list(
        system = "http://snomed.info/sct",
        code = "248152002",  # Female (finding)
        display = "Female"
      )),
      text = "BMI-Body Fat Correlation (Female)"
    ),
    subject = list(
      reference = "Population/NHANES-2017-2018-Female"
    ),
    effectivePeriod = list(
      start = "2017-01-01",
      end = "2018-12-31"
    ),
    valueQuantity = list(
      value = results$correlations$female$correlation,
      unit = "correlation coefficient",
      system = "http://unitsofmeasure.org",
      code = "1"
    ),
    note = list(list(
      text = sprintf("Sample size: %d", results$correlations$female$n_obs)
    ))
  )

  fhir_resources <- append(fhir_resources, list(female_observation))

  # Create FHIR Bundle
  fhir_bundle <- list(
    resourceType = "Bundle",
    id = paste0("nhanes-analysis-", format(results$timestamp, "%Y%m%d%H%M%S")),
    type = "collection",
    timestamp = format(results$timestamp, "%Y-%m-%dT%H:%M:%S"),
    entry = lapply(fhir_resources, function(resource) {
      list(resource = resource)
    })
  )

  # Write FHIR JSON
  jsonlite::write_json(fhir_bundle, output_file, pretty = TRUE, auto_unbox = TRUE)

  cat(sprintf("✅ FHIR export created: %s\n", output_file))
  cat("📋 FHIR resources exported:\n")
  cat(sprintf("   - Overall correlation observation\n"))
  cat(sprintf("   - Male correlation observation\n"))
  cat(sprintf("   - Female correlation observation\n"))
  cat(sprintf("   - Bundle containing all observations\n"))

  return(output_file)
}

#' Export results in JSON-LD format for semantic web
#'
#' Exports analysis results in JSON-LD format for semantic web applications
#' and linked data integration.
#'
#' @param results Analysis results from nhanesbmi
#' @param output_file Output JSON-LD file path
#' @return Path to created JSON-LD file
#' @export
export_jsonld_format <- function(results, output_file = "nhanes_jsonld_results.json") {

  if (!requireNamespace("jsonld", quietly = TRUE)) {
    warning("jsonld package not available. Using basic JSON export.")
    return(jsonlite::write_json(results, output_file, pretty = TRUE))
  }

  # Define JSON-LD context for NHANES analysis
  jsonld_context <- list(
    "@context" = list(
      "nhanes" = "https://www.cdc.gov/nchs/nhanes/",
      "bmi" = "https://schema.org/bodyMassIndex",
      "bodyfat" = "https://schema.org/bodyFatPercentage",
      "correlation" = "https://schema.org/statisticalCorrelation",
      "confidenceInterval" = "https://schema.org/confidenceInterval",
      "sampleSize" = "https://schema.org/sampleSize",
      "xsd" = "http://www.w3.org/2001/XMLSchema#",
      "correlation" = list(
        "@id" = "https://schema.org/statisticalCorrelation",
        "@type" = "xsd:float"
      ),
      "confidenceInterval" = list(
        "@id" = "https://schema.org/confidenceInterval",
        "@type" = "@id"
      ),
      "sampleSize" = list(
        "@id" = "https://schema.org/sampleSize",
        "@type" = "xsd:integer"
      )
    )
  )

  # Create JSON-LD structure
  jsonld_data <- list(
    "@context" = jsonld_context[["@context"]],
    "@id" = paste0("https://github.com/altalanta/nhanes-bmi-bodyfat/analysis/",
                   format(results$timestamp, "%Y%m%d%H%M%S")),
    "@type" = "StatisticalAnalysis",
    "name" = "NHANES BMI vs Body Fat Correlation Analysis",
    "description" = "Statistical analysis of the relationship between Body Mass Index (BMI) and whole-body percent body fat using NHANES 2017-2018 data",
    "dataSource" = "https://www.cdc.gov/nchs/nhanes/",
    "analysisDate" = format(results$timestamp, "%Y-%m-%dT%H:%M:%S"),
    "overallCorrelation" = list(
      "@type" = "CorrelationResult",
      "correlation" = results$correlations$overall$correlation,
      "confidenceInterval" = paste0("[", results$correlations$overall$ci_lower,
                                   ", ", results$correlations$overall$ci_upper, "]"),
      "standardError" = results$correlations$overall$se,
      "sampleSize" = results$correlations$sample_size
    ),
    "maleCorrelation" = list(
      "@type" = "CorrelationResult",
      "correlation" = results$correlations$male$correlation,
      "sampleSize" = results$correlations$male$n_obs,
      "sex" = "Male"
    ),
    "femaleCorrelation" = list(
      "@type" = "CorrelationResult",
      "correlation" = results$correlations$female$correlation,
      "sampleSize" = results$correlations$female$n_obs,
      "sex" = "Female"
    ),
    "methodology" = list(
      "@type" = "StatisticalMethod",
      "name" = "Survey-weighted Pearson correlation",
      "description" = "Design-based correlation analysis using NHANES survey weights and stratification",
      "software" = paste0("nhanesbmi v", packageVersion("nhanesbmi")),
      "surveyDesign" = list(
        "weights" = results$config$survey_weights_col,
        "strata" = results$config$strata_col,
        "psu" = results$config$psu_col
      )
    ),
    "dataQuality" = list(
      "@type" = "DataQualityAssessment",
      "overallStatus" = results$validation_results$overall_status,
      "sampleSize" = nrow(results$analytic_data),
      "validationTimestamp" = format(results$timestamp, "%Y-%m-%dT%H:%M:%S")
    )
  )

  # Write JSON-LD file
  jsonlite::write_json(jsonld_data, output_file, pretty = TRUE, auto_unbox = TRUE)

  cat(sprintf("✅ JSON-LD export created: %s\n", output_file))
  cat("🔗 Semantic web compatible format for:\n")
  cat("   - Linked data applications\n")
  cat("   - Knowledge graph integration\n")
  cat("   - Machine-readable research metadata\n")
  cat("   - Cross-system data exchange\n")

  return(output_file)
}

#' Launch API server from results file
#'
#' Convenience function to launch API server from a saved results file.
#'
#' @param results_file Path to results RDS file (default: "outputs/tables/nhanes_analysis_results.rds")
#' @param port Port number for API server
#' @param host Host address for API server
#' @return None (starts API server)
#' @export
launch_api_server <- function(results_file = "outputs/tables/nhanes_analysis_results.rds",
                             port = 8000, host = "127.0.0.1") {

  if (!file.exists(results_file)) {
    stop(paste("Results file not found:", results_file))
  }

  results <- readRDS(results_file)
  create_results_api(results, port, host)
}

#' Create comprehensive export in multiple formats
#'
#' Exports analysis results in multiple interoperable formats for different use cases.
#'
#' @param results Analysis results from nhanesbmi
#' @param output_dir Output directory for exported files
#' @param formats Vector of formats to export (default: c("json", "csv", "rds", "fhir", "jsonld"))
#' @return List of exported file paths
#' @export
export_comprehensive <- function(results, output_dir = "exports",
                                formats = c("json", "csv", "rds", "fhir", "jsonld")) {

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  exported_files <- list()

  # Base filename
  timestamp <- format(results$timestamp, "%Y%m%d_%H%M%S")

  # JSON export (human-readable)
  if ("json" %in% formats) {
    json_file <- file.path(output_dir, paste0("nhanes_results_", timestamp, ".json"))
    jsonlite::write_json(results, json_file, pretty = TRUE, auto_unbox = TRUE)
    exported_files$json <- json_file
    cat(sprintf("✅ JSON export: %s\n", json_file))
  }

  # CSV export (tabular data)
  if ("csv" %in% formats) {
    csv_file <- file.path(output_dir, paste0("nhanes_data_", timestamp, ".csv"))
    write.csv(results$analytic_data, csv_file, row.names = FALSE)
    exported_files$csv <- csv_file
    cat(sprintf("✅ CSV export: %s\n", csv_file))
  }

  # RDS export (R binary format)
  if ("rds" %in% formats) {
    rds_file <- file.path(output_dir, paste0("nhanes_results_", timestamp, ".rds"))
    saveRDS(results, rds_file)
    exported_files$rds <- rds_file
    cat(sprintf("✅ RDS export: %s\n", rds_file))
  }

  # FHIR export (healthcare interoperability)
  if ("fhir" %in% formats) {
    fhir_file <- file.path(output_dir, paste0("nhanes_fhir_", timestamp, ".json"))
    export_fhir_format(results, fhir_file)
    exported_files$fhir <- fhir_file
    cat(sprintf("✅ FHIR export: %s\n", fhir_file))
  }

  # JSON-LD export (semantic web)
  if ("jsonld" %in% formats) {
    jsonld_file <- file.path(output_dir, paste0("nhanes_jsonld_", timestamp, ".json"))
    export_jsonld_format(results, jsonld_file)
    exported_files$jsonld <- jsonld_file
    cat(sprintf("✅ JSON-LD export: %s\n", jsonld_file))
  }

  # Database storage
  if ("database" %in% formats) {
    db_file <- file.path(output_dir, paste0("nhanes_results_", timestamp, ".db"))
    db_con <- store_results_in_database(results, db_file)
    exported_files$database <- db_file
    cat(sprintf("✅ Database export: %s\n", db_file))
  }

  # Create export summary
  export_summary <- list(
    export_timestamp = Sys.time(),
    exported_formats = formats,
    exported_files = exported_files,
    total_files = length(exported_files)
  )

  summary_file <- file.path(output_dir, paste0("export_summary_", timestamp, ".json"))
  jsonlite::write_json(export_summary, summary_file, pretty = TRUE, auto_unbox = TRUE)

  cat(sprintf("\n📋 Export summary: %s\n", summary_file))
  cat(sprintf("📁 Total files exported: %d\n", length(exported_files)))
  cat(sprintf("🎯 Export directory: %s\n", output_dir))

  return(exported_files)
}
