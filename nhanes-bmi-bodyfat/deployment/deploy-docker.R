#!/usr/bin/env Rscript

# Docker deployment script
# Usage: Rscript deployment/deploy-docker.R

library(nhanesbmi)

# Check if Docker is available
docker_available <- system("docker --version", intern = TRUE)
if (length(docker_available) == 0) {
  stop("Docker is not available. Please install Docker first.")
}

# Load configuration
config <- safe_load_config()

# Run analysis to generate fresh results
cat("Running analysis to generate fresh results...\n")
results <- run_optimized_analysis()

# Build Docker image
cat("Building Docker image...\n")
image_name <- "nhanes-bmi-bodyfat:latest"

system(sprintf("docker build -t %s .", image_name))

# Run container for testing
cat("Testing Docker container...\n")
container_id <- system(sprintf("docker run -d -p 3838:3838 %s dashboard", image_name),
                      intern = TRUE)

if (length(container_id) > 0) {
  cat("✅ Docker container running!\n")
  cat(sprintf("Dashboard available at: http://localhost:3838\n"))
  cat(sprintf("Container ID: %s\n", container_id))

  # Wait for user input to stop container
  cat("Press Enter to stop the container...\n")
  readLines("stdin", n = 1)

  # Stop container
  system(sprintf("docker stop %s", container_id))
  system(sprintf("docker rm %s", container_id))

  cat("✅ Container stopped and removed.\n")
} else {
  cat("❌ Failed to start Docker container.\n")
}

# Optional: Push to Docker registry
cat("Would you like to push to Docker Hub? (y/N): ")
push_answer <- readLines("stdin", n = 1)

if (tolower(push_answer) == "y") {
  cat("Enter your Docker Hub username: ")
  docker_user <- readLines("stdin", n = 1)

  cat("Pushing to Docker Hub...\n")
  system(sprintf("docker tag %s %s/%s", image_name, docker_user, image_name))
  system(sprintf("docker push %s/%s", docker_user, image_name))

  cat("✅ Pushed to Docker Hub!\n")
  cat(sprintf("Image available at: https://hub.docker.com/r/%s/%s\n", docker_user, image_name))
}
