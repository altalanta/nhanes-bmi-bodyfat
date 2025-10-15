#!/usr/bin/env Rscript

# NHANES BMI Body Fat Analysis API Server
# Starts the REST API server for programmatic access to results

# Load required libraries
library(plumber)
library(nhanesbmi)

# Load configuration
config <- safe_load_config()

# Set up logging
log_file <- file.path(config$outputs$logs_dir, "api_server.log")
log_msg <- function(msg) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", msg, "\n", file = log_file, append = TRUE)
  cat(msg, "\n")
}

# Start API server
start_api_server <- function(port = 8000, host = "0.0.0.0") {
  log_msg("Starting NHANES BMI Body Fat API server...")

  # Load the API definition
  api_file <- system.file("api.R", package = "nhanesbmi")

  if (api_file == "") {
    # Fallback to local file if not in package
    api_file <- "R/api.R"
  }

  if (!file.exists(api_file)) {
    stop("API definition file not found: ", api_file)
  }

  # Create plumber API
  api <- plumb(api_file)

  # Add middleware for logging and error handling
  api <- api %>%
    pr_set_error_handler(function(req, res, err) {
      log_msg(paste("API Error:", err$message))
      res$status <- 500
      list(error = "Internal server error", message = err$message)
    }) %>%
    pr_set_serializer(serializer_json())

  # Start server
  log_msg(paste("API server starting on", host, "port", port))

  tryCatch({
    api$run(host = host, port = port)
  }, interrupt = function(e) {
    log_msg("API server interrupted by user")
  }, error = function(e) {
    log_msg(paste("API server error:", e$message))
    stop(e)
  })
}

# Command line interface
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  # Default values
  port <- 8000
  host <- "0.0.0.0"

  # Parse arguments
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--port" && i + 1 <= length(args)) {
      port <- as.integer(args[i + 1])
      i <- i + 2
    } else if (args[i] == "--host" && i + 1 <= length(args)) {
      host <- args[i + 1]
      i <- i + 2
    } else if (args[i] == "--help" || args[i] == "-h") {
      cat("NHANES BMI Body Fat Analysis API Server\n")
      cat("Usage: Rscript api_server.R [--port PORT] [--host HOST]\n")
      cat("\nOptions:\n")
      cat("  --port PORT    Port to listen on (default: 8000)\n")
      cat("  --host HOST    Host to bind to (default: 0.0.0.0)\n")
      cat("  --help, -h     Show this help message\n")
      cat("\nExamples:\n")
      cat("  Rscript api_server.R\n")
      cat("  Rscript api_server.R --port 8080\n")
      cat("  Rscript api_server.R --host 127.0.0.1 --port 9000\n")
      quit(status = 0)
    } else {
      cat("Unknown option:", args[i], "\n")
      cat("Use --help for usage information\n")
      quit(status = 1)
    }
  }

  # Start the server
  start_api_server(port = port, host = host)
}

# Run if called directly
if (sys.nframe() == 0) {
  main()
}

