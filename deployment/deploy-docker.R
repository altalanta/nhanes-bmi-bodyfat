#!/usr/bin/env Rscript

# Docker deployment script for NHANES BMI Body Fat Analysis
# Usage: Rscript deployment/deploy-docker.R [build|test|push]

# Load project utilities
source("scripts/load_config.R")
source("scripts/error_handling.R")

# Check if Docker is available
check_docker <- function() {
  tryCatch({
    version <- system("docker --version", intern = TRUE, ignore.stderr = TRUE)
    if (length(version) == 0) {
      stop("Docker is not available. Please install Docker first.")
    }
    cat("✅ Docker version:", version, "\n")
    return(TRUE)
  }, error = function(e) {
    stop("Docker check failed:", e$message)
  })
}

# Build Docker image
build_image <- function(image_name = "nhanes-bmi-bodyfat:latest", no_cache = FALSE) {
  cat("🔨 Building Docker image:", image_name, "\n")

  cmd <- sprintf("docker build -t %s", image_name)
  if (no_cache) cmd <- paste(cmd, "--no-cache")

  result <- system(cmd)
  if (result != 0) {
    stop("Failed to build Docker image")
  }

  cat("✅ Successfully built Docker image:", image_name, "\n")
}

# Test Docker container
test_container <- function(image_name = "nhanes-bmi-bodyfat:latest", test_type = "analyze") {
  cat("🧪 Testing Docker container with", test_type, "command...\n")

    # Run test command (analysis or api)
    if (test_type == "api") {
      # Test API server
      container_id <- system(sprintf("docker run -d -p 8000:8000 %s api", image_name),
                            intern = TRUE, ignore.stderr = TRUE)
      # Give API time to start
      Sys.sleep(3)
      # Test API health endpoint
      api_test <- system("curl -s http://localhost:8000/health", intern = TRUE)
      if (length(api_test) > 0 && grepl("healthy", api_test)) {
        cat("✅ API server test passed!\n")
        cat("API available at: http://localhost:8000\n")
        cat("API documentation: http://localhost:8000/__docs__/\n")
      } else {
        cat("❌ API server test failed\n")
        return(FALSE)
      }
    } else {
      # Test analysis
      container_id <- system(sprintf("docker run --rm %s %s", image_name, test_type),
                            intern = TRUE, ignore.stderr = TRUE)
    }

  if (test_type == "api") {
    # API test already handled above
    return(TRUE)
  } else {
    # Analysis test
    if (length(container_id) > 0 && !grepl("error|Error|ERROR", container_id[1])) {
      cat("✅ Container test passed!\n")
      cat("Output files should be in outputs/ directory\n")
      return(TRUE)
    } else {
      cat("❌ Container test failed\n")
      return(FALSE)
    }
  }
}

# Push to registry
push_to_registry <- function(image_name = "nhanes-bmi-bodyfat:latest") {
  cat("🚀 Pushing to Docker registry...\n")

  # Check if user is logged in to Docker Hub
  login_check <- system("docker info", intern = TRUE, ignore.stderr = TRUE)
  if (any(grepl("Username:", login_check))) {
    cat("✅ Docker Hub login detected\n")
  } else {
    cat("⚠️  Please ensure you're logged in to Docker Hub first:\n")
    cat("   docker login\n")
    return(FALSE)
  }

  # Tag for Docker Hub (assumes username is altalanta)
  hub_image <- "altalanta/nhanes-bmi-bodyfat:latest"
  system(sprintf("docker tag %s %s", image_name, hub_image))

  result <- system(sprintf("docker push %s", hub_image))
  if (result == 0) {
    cat("✅ Successfully pushed to Docker Hub!\n")
    cat("📦 Image available at: https://hub.docker.com/r/altalanta/nhanes-bmi-bodyfat\n")
    return(TRUE)
  } else {
    cat("❌ Failed to push to Docker Hub\n")
    return(FALSE)
  }
}

# Main deployment function
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  action <- if (length(args) > 0) args[1] else "interactive"

  cat("🐳 NHANES BMI Body Fat Analysis - Docker Deployment\n")
  cat("================================================\n\n")

  # Check Docker availability
  check_docker()

  image_name <- "nhanes-bmi-bodyfat:latest"

  switch(action,
    "build" = {
      build_image(image_name)
    },
    "test" = {
      cat("Select test type (analyze/api): ")
      test_type <- readLines("stdin", n = 1)
      if (!test_container(image_name, test_type)) {
        stop("Container test failed")
      }
    },
    "push" = {
      if (push_to_registry(image_name)) {
        cat("🎉 Deployment complete!\n")
      }
    },
    # Interactive mode
    {
      cat("Interactive deployment mode\n")
      cat("1. Build image\n")
      cat("2. Test container\n")
      cat("3. Push to registry (optional)\n\n")

      # Build image
      build_image(image_name)

      # Test container
      if (test_container(image_name, "analyze")) {
        # Ask about pushing to registry
        cat("\nPush to Docker Hub? (y/N): ")
        push_answer <- readLines("stdin", n = 1)

        if (tolower(push_answer) == "y") {
          push_to_registry(image_name)
        }
      }

      cat("🎉 Docker deployment complete!\n")
    }
  )
}

# Run main function with error handling
tryCatch({
  main()
}, error = function(e) {
  cat("❌ Deployment failed:", e$message, "\n")
  quit(status = 1)
})
