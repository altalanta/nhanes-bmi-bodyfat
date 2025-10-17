#!/usr/bin/env Rscript

# NHANES BMI Body Fat Analysis API Server with Performance Benchmarking
# Starts the REST API server for programmatic access to results with comprehensive performance tracking

# Load required libraries
library(plumber)
library(nhanesbmi)

# Load configuration and performance utilities
source("scripts/error_handling.R")
source("scripts/load_config.R")
source("R/performance.R")

# Initialize performance tracking for API server
initialize_performance_tracking("nhanes_api_server")

# Load configuration
config <- safe_load_config()

# Set up enhanced logging with performance tracking
log_file <- file.path(config$outputs$logs_dir, "api_server.log")
api_requests_file <- file.path(config$outputs$logs_dir, "api_requests.json")

log_msg <- function(msg, level = "INFO") {
  log_performance(msg, level)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", msg, "\n", file = log_file, append = TRUE)
  cat(msg, "\n")
}

# Enhanced API server with comprehensive performance benchmarking
start_api_server <- function(port = 8000, host = "0.0.0.0") {
  log_msg("Starting NHANES BMI Body Fat API server with performance tracking...")

  # Initialize API request tracking
  api_requests <- list()
  server_start_time <- Sys.time()

  # Load the API definition with performance tracking
  api_loading <- benchmark_operation({
    api_file <- system.file("api.R", package = "nhanesbmi")

    if (api_file == "") {
      # Fallback to local file if not in package
      api_file <- "R/api.R"
    }

    if (!file.exists(api_file)) {
      stop(NhanesError(
        paste("API definition file not found:", api_file),
        code = "API_FILE_NOT_FOUND",
        details = list(api_file = api_file, searched_package = "nhanesbmi")
      ))
    }

    # Create plumber API
    api <- plumb(api_file)
    return(list(api = api, api_file = api_file))
  }, "api_definition_loading", metadata = list(package = "nhanesbmi"))

  # Enhanced middleware for logging, error handling, and performance tracking
  enhanced_api <- benchmark_operation({
    api <- api_loading$api

    api <- api %>%
      # Request logging middleware with performance tracking
      pr_set_docs_callback(function(req, res) {
        # Track API documentation access
        request_info <- list(
          timestamp = Sys.time(),
          endpoint = "/docs",
          method = "GET",
          user_agent = req$HTTP_USER_AGENT %||% "Unknown",
          ip = req$HTTP_X_REAL_IP %||% req$REMOTE_ADDR %||% "Unknown"
        )
        api_requests <<- c(api_requests, list(request_info))
        log_performance("API documentation accessed", "INFO")
      }) %>%

      # Request middleware for tracking all API calls
      pr_set_request_handler(function(req) {
        request_info <- list(
          timestamp = Sys.time(),
          endpoint = req$PATH_INFO,
          method = req$REQUEST_METHOD,
          user_agent = req$HTTP_USER_AGENT %||% "Unknown",
          ip = req$HTTP_X_REAL_IP %||% req$REMOTE_ADDR %||% "Unknown",
          query_params = req$QUERY_STRING %||% ""
        )
        api_requests <<- c(api_requests, list(request_info))
      }) %>%

      # Response middleware for tracking response times
      pr_set_response_handler(function(res) {
        if (length(api_requests) > 0) {
          last_request <- api_requests[[length(api_requests)]]
          last_request$response_time <- Sys.time()
          last_request$status_code <- res$status
          api_requests[[length(api_requests)]] <<- last_request

          # Log slow requests
          if (as.numeric(difftime(last_request$response_time, last_request$timestamp, units = "secs")) > 5) {
            log_performance(
              paste("Slow API request:", last_request$endpoint, "took",
                   round(as.numeric(difftime(last_request$response_time, last_request$timestamp, units = "secs")), 2), "seconds"),
              "WARNING"
            )
          }
        }
      }) %>%

      # Enhanced error handler with performance logging
      pr_set_error_handler(function(req, res, err) {
        log_performance(paste("API Error on", req$PATH_INFO, ":", err$message), "ERROR")

        # Track error in request log
        if (length(api_requests) > 0) {
          last_request <- api_requests[[length(api_requests)]]
          last_request$error <- err$message
          last_request$error_timestamp <- Sys.time()
          api_requests[[length(api_requests)]] <<- last_request
        }

        res$status <- 500
        list(error = "Internal server error", message = err$message)
      }) %>%

      # JSON serializer
      pr_set_serializer(serializer_json())

    return(api)
  }, "api_middleware_setup", metadata = list(middleware_types = c("docs", "request", "response", "error")))

  # Server startup with performance tracking
  server_startup <- benchmark_operation({
    log_msg(paste("API server starting on", host, "port", port))

    tryCatch({
      enhanced_api$run(host = host, port = port)
    }, interrupt = function(e) {
      log_performance("API server interrupted by user", "INFO")
      # Generate final performance report
      generate_api_performance_report(api_requests, server_start_time)
    }, error = function(e) {
      log_performance(paste("API server error:", e$message), "ERROR")
      stop(e)
    })
  }, "api_server_startup", metadata = list(host = host, port = port))

  return(invisible(list(api = enhanced_api, requests = api_requests)))
}

# Generate comprehensive API performance report
generate_api_performance_report <- function(requests, server_start_time) {
  if (length(requests) == 0) {
    log_performance("No API requests to report", "INFO")
    return()
  }

  # Calculate API performance metrics
  api_metrics <- benchmark_operation({
    request_df <- do.call(rbind, lapply(requests, as.data.frame))

    # Calculate request statistics
    total_requests <- nrow(request_df)
    unique_endpoints <- length(unique(request_df$endpoint))
    unique_ips <- length(unique(request_df$ip))

    # Response time analysis (for requests with response_time)
    response_times <- as.numeric(difftime(request_df$response_time, request_df$timestamp, units = "secs"))
    response_times <- response_times[!is.na(response_times)]

    # Error analysis
    error_requests <- sum(!is.na(request_df$error))
    error_rate <- error_requests / total_requests

    # Endpoint popularity
    endpoint_stats <- table(request_df$endpoint)
    most_popular <- names(endpoint_stats)[which.max(endpoint_stats)]

    # Performance summary
    api_summary <- list(
      server_uptime = as.numeric(difftime(Sys.time(), server_start_time, units = "secs")),
      total_requests = total_requests,
      unique_endpoints = unique_endpoints,
      unique_clients = unique_ips,
      error_rate = error_rate,
      most_popular_endpoint = most_popular,
      avg_response_time = if (length(response_times) > 0) mean(response_times) else NA,
      median_response_time = if (length(response_times) > 0) median(response_times) else NA,
      max_response_time = if (length(response_times) > 0) max(response_times) else NA,
      requests_per_minute = total_requests / (as.numeric(difftime(Sys.time(), server_start_time, units = "mins")) + 0.01)
    )

    # Save API performance metrics
    jsonlite::write_json(api_summary, api_requests_file, pretty = TRUE, auto_unbox = TRUE)

    # Generate human-readable summary
    summary_text <- paste0(
      "\nAPI Server Performance Summary\n",
      "=============================\n",
      "Server Uptime: ", round(api_summary$server_uptime, 2), " seconds\n",
      "Total Requests: ", api_summary$total_requests, "\n",
      "Unique Endpoints: ", api_summary$unique_endpoints, "\n",
      "Unique Clients: ", api_summary$unique_clients, "\n",
      "Error Rate: ", round(api_summary$error_rate * 100, 2), "%\n",
      "Most Popular Endpoint: ", api_summary$most_popular_endpoint, "\n",
      "Requests per Minute: ", round(api_summary$requests_per_minute, 2), "\n"
    )

    if (length(response_times) > 0) {
      summary_text <- paste0(summary_text,
        "Response Time (seconds):\n",
        "  Average: ", round(api_summary$avg_response_time, 3), "\n",
        "  Median: ", round(api_summary$median_response_time, 3), "\n",
        "  Maximum: ", round(api_summary$max_response_time, 3), "\n"
      )
    }

    cat(summary_text, file = log_file, append = TRUE)

    return(api_summary)
  }, "api_performance_analysis", metadata = list(n_requests = length(requests)))

  log_performance("API performance report generated", "INFO")
  return(api_metrics)
}

# Enhanced command line interface with performance tracking
main <- function() {
  log_msg("API Server main function started")

  args <- commandArgs(trailingOnly = TRUE)

  # Default values
  port <- 8000
  host <- "0.0.0.0"

  # Parse arguments with performance tracking
  args_parsing <- benchmark_operation({
    i <- 1
    while (i <= length(args)) {
      if (args[i] == "--port" && i + 1 <= length(args)) {
        port <- as.integer(args[i + 1])
        i <- i + 2
      } else if (args[i] == "--host" && i + 1 <= length(args)) {
        host <- args[i + 1]
        i <- i + 2
      } else if (args[i] == "--help" || args[i] == "-h") {
        cat("NHANES BMI Body Fat Analysis API Server with Performance Benchmarking\n")
        cat("Usage: Rscript api_server.R [--port PORT] [--host HOST]\n")
        cat("\nOptions:\n")
        cat("  --port PORT    Port to listen on (default: 8000)\n")
        cat("  --host HOST    Host to bind to (default: 0.0.0.0)\n")
        cat("  --help, -h     Show this help message\n")
        cat("\nExamples:\n")
        cat("  Rscript api_server.R\n")
        cat("  Rscript api_server.R --port 8080\n")
        cat("  Rscript api_server.R --host 127.0.0.1 --port 9000\n")
        cat("\nFeatures:\n")
        cat("  - Comprehensive performance benchmarking\n")
        cat("  - Request/response time tracking\n")
        cat("  - API usage analytics\n")
        cat("  - Error monitoring and logging\n")
        quit(status = 0)
      } else {
        cat("Unknown option:", args[i], "\n")
        cat("Use --help for usage information\n")
        quit(status = 1)
      }
    }

    return(list(port = port, host = host))
  }, "argument_parsing", metadata = list(n_args = length(args)))

  log_msg(paste("Parsed arguments - Port:", args_parsing$port, "Host:", args_parsing$host))

  # Start the server with error handling
  tryCatch({
    server_result <- start_api_server(port = args_parsing$port, host = args_parsing$host)
    log_msg("API server started successfully")

    return(server_result)
  }, error = function(e) {
    log_performance(paste("Failed to start API server:", e$message), "ERROR")
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("API server startup failed:", e$message),
        code = "API_SERVER_STARTUP_ERROR",
        details = list(port = port, host = host, original_error = e$message)
      ))
    }
  })
}

# Run if called directly
if (sys.nframe() == 0) {
  main()
}

