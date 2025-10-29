#
# NHANES BMI Body Fat Analysis - REST API
# Provides programmatic access to analysis results, data quality, and system monitoring
#

# Load required packages
library(plumber)
library(jsonlite)
library(yaml)
library(digest)
library(dplyr)

# Source core functions
source("../R/error_handling.R")
source("../R/data_versioning.R")
source("../parallel_pipeline.R")

# Global configuration
API_CONFIG <- list(
  version = "1.0.0",
  title = "NHANES BMI Body Fat Analysis API",
  description = "REST API for accessing NHANES analysis results and system monitoring",
  contact = list(
    name = "NHANES Analysis Team",
    email = "analysis@nhanes-bmi.org"
  ),
  license = list(
    name = "MIT",
    url = "https://opensource.org/licenses/MIT"
  )
)

#* @apiTitle NHANES BMI Body Fat Analysis API
#* @apiDescription REST API for accessing NHANES BMI analysis results, data quality information, and system monitoring
#* @apiVersion 1.0.0
#* @apiContact list(name = "NHANES Analysis Team", email = "analysis@nhanes-bmi.org")
#* @apiLicense list(name = "MIT", url = "https://opensource.org/licenses/MIT")

#* Health check endpoint
#* @get /health
#* @serializer json
function() {
  tryCatch({
    # Check system health
    health_issues <- check_pipeline_health()

    # Get API status
    api_status <- list(
      status = "healthy",
      timestamp = as.character(Sys.time()),
      version = API_CONFIG$version,
      uptime = as.numeric(Sys.time() - .GlobalEnv$api_start_time),
      memory_usage = memory.size(),
      active_connections = length(.GlobalEnv$active_requests) %||% 0
    )

    # Determine overall health
    if (length(health_issues) == 0) {
      api_status$status <- "healthy"
      api_status$health_score <- 100
    } else {
      api_status$status <- "degraded"
      api_status$health_score <- 50
      api_status$issues <- health_issues
    }

    return(api_status)

  }, error = function(e) {
    list(
      status = "error",
      error = e$message,
      timestamp = as.character(Sys.time())
    )
  })
}

#* Get API information
#* @get /api/info
#* @serializer json
function() {
  list(
    title = API_CONFIG$title,
    description = API_CONFIG$description,
    version = API_CONFIG$version,
    endpoints = list(
      health = "/health",
      correlations = "/results/correlations",
      bmi_stats = "/results/bmi-stats",
      data_quality = "/data/quality",
      data_registry = "/data/registry",
      system_status = "/system/status"
    ),
    supported_formats = c("json", "csv"),
    rate_limits = list(
      requests_per_minute = 60,
      requests_per_hour = 1000
    )
  )
}

#* Get correlation analysis results
#* @get /results/correlations
#* @param format Format of response (json, csv)
#* @param group Filter by demographic group (Overall, Male, Female)
#* @serializer json
function(format = "json", group = NULL) {
  tryCatch({
    # Load results if available
    results_file <- "../outputs/tables/corr_bmi_bodyfat_overall_and_by_sex.csv"

    if (!file.exists(results_file)) {
      return(list(
        error = "Results not available",
        message = "Please run the analysis pipeline first",
        suggestion = "Execute: make parallel-pipeline"
      ))
    }

    results <- read.csv(results_file)

    # Filter by group if specified
    if (!is.null(group) && group != "all") {
      results <- results %>% filter(group == !!group)
    }

    # Return in requested format
    if (format == "csv") {
      # For CSV format, we'd need to handle this differently in plumber
      # For now, return JSON with CSV data
      return(list(
        format = "csv_data",
        data = results,
        filename = "correlations.csv"
      ))
    }

    return(list(
      success = TRUE,
      data = results,
      metadata = list(
        rows = nrow(results),
        columns = ncol(results),
        generated_at = as.character(Sys.time())
      )
    ))

  }, error = function(e) {
    list(
      error = "Failed to retrieve correlation results",
      message = e$message,
      suggestion = "Check if analysis has been run and results exist"
    )
  })
}

#* Get BMI class statistics
#* @get /results/bmi-stats
#* @param format Format of response (json, csv)
#* @param bmi_category Filter by BMI category
#* @param sex Filter by sex (Male, Female)
#* @serializer json
function(format = "json", bmi_category = NULL, sex = NULL) {
  tryCatch({
    results_file <- "../outputs/tables/bodyfat_by_bmi_class_by_sex.csv"

    if (!file.exists(results_file)) {
      return(list(
        error = "BMI statistics not available",
        message = "Please run the analysis pipeline first",
        suggestion = "Execute: make parallel-pipeline"
      ))
    }

    results <- read.csv(results_file)

    # Apply filters
    if (!is.null(bmi_category)) {
      results <- results %>% filter(bmi_cat == bmi_category)
    }

    if (!is.null(sex)) {
      results <- results %>% filter(sex == sex)
    }

    return(list(
      success = TRUE,
      data = results,
      metadata = list(
        rows = nrow(results),
        columns = ncol(results),
        filters_applied = list(bmi_category = bmi_category, sex = sex),
        generated_at = as.character(Sys.time())
      )
    ))

  }, error = function(e) {
    list(
      error = "Failed to retrieve BMI statistics",
      message = e$message,
      suggestion = "Check if analysis has been run and results exist"
    )
  })
}

#* Get data quality information
#* @get /data/quality
#* @serializer json
function() {
  tryCatch({
    # Generate fresh quality report
    quality_report <- generate_quality_report()

    return(list(
      success = TRUE,
      data = quality_report,
      generated_at = as.character(Sys.time())
    ))

  }, error = function(e) {
    list(
      error = "Failed to generate quality report",
      message = e$message,
      suggestion = "Check data registry and file integrity"
    )
  })
}

#* Get data registry information
#* @get /data/registry
#* @param format Format of response (json, summary)
#* @serializer json
function(format = "json") {
  tryCatch({
    registry <- load_data_registry()

    if (format == "summary") {
      # Return summary information
      return(list(
        success = TRUE,
        summary = list(
          total_files = length(registry$entries),
          active_files = sum(sapply(registry$entries, function(x) x$status == "active")),
          total_size = sum(sapply(registry$entries, function(x) x$file_size)),
          registry_created = registry$metadata$created,
          last_updated = max(sapply(registry$entries, function(x) x$registry_timestamp))
        ),
        generated_at = as.character(Sys.time())
      ))
    }

    # Return full registry
    return(list(
      success = TRUE,
      data = registry,
      generated_at = as.character(Sys.time())
    ))

  }, error = function(e) {
    list(
      error = "Failed to retrieve data registry",
      message = e$message,
      suggestion = "Check if data registry has been initialized"
    )
  })
}

#* Run analysis pipeline via API
#* @post /analysis/run
#* @param force Force re-run even if cached
#* @serializer json
function(force = "false") {
  tryCatch({
    # Convert force parameter
    force_rerun <- as.logical(force)

    # Run analysis pipeline
    if (force_rerun) {
      # Clear cache if forcing rerun
      if (dir.exists("../cache")) {
        unlink("../cache", recursive = TRUE)
      }
    }

    # Execute pipeline
    results <- run_parallel_pipeline()

    return(list(
      success = TRUE,
      message = "Analysis completed successfully",
      execution_time = results$processing_time,
      results_available = list(
        correlations = "../outputs/tables/corr_bmi_bodyfat_overall_and_by_sex.csv",
        bmi_stats = "../outputs/tables/bodyfat_by_bmi_class_by_sex.csv",
        plots = results$plot_files,
        report = "../outputs/report/report.html"
      ),
      completed_at = as.character(Sys.time())
    ))

  }, error = function(e) {
    list(
      error = "Analysis execution failed",
      message = e$message,
      suggestion = "Check system requirements and data availability"
    )
  })
}

#* Get system performance metrics
#* @get /system/performance
#* @serializer json
function() {
  tryCatch({
    performance_metrics <- list(
      timestamp = as.character(Sys.time()),
      memory_usage = memory.size(),
      memory_limit = memory.limit(),
      cpu_cores = availableCores(),
      r_version = R.version.string,
      system_info = list(
        os = Sys.info()["sysname"],
        user = Sys.info()["user"],
        nodename = Sys.info()["nodename"]
      ),
      cache_status = list(
        cache_exists = dir.exists("../cache"),
        cache_size = if (dir.exists("../cache")) {
          sum(file.info(list.files("../cache", full.names = TRUE))$size)
        } else {
          0
        }
      )
    )

    return(list(
      success = TRUE,
      data = performance_metrics
    ))

  }, error = function(e) {
    list(
      error = "Failed to retrieve performance metrics",
      message = e$message
    )
  })
}

#* Get available endpoints
#* @get /endpoints
#* @serializer json
function() {
  list(
    endpoints = list(
      "GET /health" = "System health check",
      "GET /api/info" = "API information and capabilities",
      "GET /results/correlations" = "Correlation analysis results",
      "GET /results/bmi-stats" = "BMI class statistics",
      "GET /data/quality" = "Data quality report",
      "GET /data/registry" = "Data registry information",
      "POST /analysis/run" = "Execute analysis pipeline",
      "GET /system/performance" = "System performance metrics",
      "GET /endpoints" = "This endpoint list"
    ),
    base_url = "http://localhost:8000",
    version = API_CONFIG$version
  )
}

#* Error handler for invalid endpoints
#* @404
function() {
  list(
    error = "Endpoint not found",
    message = "The requested API endpoint does not exist",
    suggestion = "Use GET /endpoints to see available endpoints",
    timestamp = as.character(Sys.time())
  )
}

#* Global error handler
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }

  plumber::forward()
}

#* Request logging middleware
#* @filter logger
function(req) {
  # Log API requests (simplified)
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),
      req$REQUEST_METHOD, req$PATH_INFO, "\n")

  # Track active requests
  if (!exists("active_requests", .GlobalEnv)) {
    .GlobalEnv$active_requests <- list()
  }
  .GlobalEnv$active_requests <- c(.GlobalEnv$active_requests, list(req$PATH_INFO))

  plumber::forward()
}

#* Rate limiting middleware
#* @filter rate-limit
function(req) {
  # Simple rate limiting (in production, use Redis or similar)
  current_minute <- format(Sys.time(), "%Y-%m-%d %H:%M")

  if (!exists("request_counts", .GlobalEnv)) {
    .GlobalEnv$request_counts <- list()
  }

  count_key <- paste(current_minute, req$REMOTE_ADDR %||% "unknown")
  current_count <- .GlobalEnv$request_counts[[count_key]] %||% 0
  .GlobalEnv$request_counts[[count_key]] <- current_count + 1

  # Allow up to 60 requests per minute
  if (current_count > 60) {
    stop(list(
      error = "Rate limit exceeded",
      message = "Too many requests. Please try again later.",
      retry_after = 60
    ))
  }

  plumber::forward()
}

# API startup and configuration
#* @plumber
function(pr) {
  # Set API start time
  .GlobalEnv$api_start_time <- Sys.time()

  # Configure API settings
  pr %>%
    pr_set_api_spec(function(spec) {
      spec$info$title <- API_CONFIG$title
      spec$info$description <- API_CONFIG$description
      spec$info$version <- API_CONFIG$version
      spec$info$contact <- API_CONFIG$contact
      spec$info$license <- API_CONFIG$license
      spec
    }) %>%
    pr_set_parsers("application/json", "text/csv") %>%
    pr_set_serializer("json", function(val, req, res, errorHandler) {
      tryCatch({
        if (is.data.frame(val)) {
          return(jsonlite::toJSON(val, pretty = TRUE))
        }
        return(jsonlite::toJSON(val, auto_unbox = TRUE, pretty = TRUE))
      }, error = errorHandler)
    })

  pr
}

# Launch API server
# R -e "pr <- plumb('api/api.R'); pr$run(port = 8000, host = '0.0.0.0')"





