#!/usr/bin/env Rscript

# Deployment script for shinyapps.io
# Usage: Rscript deployment/deploy-shinyapps.R

library(nhanesbmi)
library(rsconnect)

# Check if rsconnect is configured
if (!file.exists("~/.rsconnect/")) {
  stop("rsconnect not configured. Please run rsconnect::setAccountInfo() first")
}

# Load configuration
config <- safe_load_config()

# Run analysis to generate fresh results
cat("Running analysis to generate fresh results...\n")
results <- run_optimized_analysis()

# Create standalone app
cat("Creating standalone Shiny app...\n")
app_dir <- create_standalone_app(
  "outputs/tables/nhanes_analysis_results.rds",
  "deployment_app"
)

# Deploy to shinyapps.io
cat("Deploying to shinyapps.io...\n")
deployApp(
  appDir = file.path(app_dir, "app"),
  appName = "nhanes-bmi-bodyfat",
  appTitle = "NHANES BMI vs Body Fat Analysis Dashboard",
  forceUpdate = TRUE,
  launch.browser = FALSE
)

cat("✅ Deployment complete!\n")
cat("App available at: https://altalanta.shinyapps.io/nhanes-bmi-bodyfat/\n")
