#!/usr/bin/env Rscript

# NHANES BMI Body Fat Analysis API Example
# Demonstrates how to use the REST API programmatically

library(httr)
library(jsonlite)

# Base URL for the API (adjust if running on different port/host)
API_BASE_URL <- "http://localhost:8000"

# Function to make API requests with error handling
api_request <- function(endpoint, query_params = NULL) {
  url <- paste0(API_BASE_URL, endpoint)
  response <- GET(url, query = query_params)

  if (status_code(response) != 200) {
    cat("API Error:", status_code(response), "\n")
    return(NULL)
  }

  content(response, "parsed")
}

# Example 1: Health check
cat("1. Health Check\n")
cat("==============\n")
health <- api_request("/health")
print(health)
cat("\n")

# Example 2: API Information
cat("2. API Information\n")
cat("=================\n")
info <- api_request("/api/info")
print(info)
cat("\n")

# Example 3: Get all correlations
cat("3. BMI-Body Fat Correlations\n")
cat("===========================\n")
correlations <- api_request("/api/correlations")
if (!is.null(correlations)) {
  print(correlations$data)
  cat("Total records:", correlations$count, "\n")
}
cat("\n")

# Example 4: Get correlations for males only
cat("4. Male Correlations\n")
cat("===================\n")
male_correlations <- api_request("/api/correlations", list(group = "Male"))
if (!is.null(male_correlations)) {
  print(male_correlations$data)
}
cat("\n")

# Example 5: Get body fat data for normal BMI category
cat("5. Normal BMI Body Fat Data\n")
cat("==========================\n")
normal_bmi <- api_request("/api/bodyfat/bmi/Normal")
if (!is.null(normal_bmi)) {
  print(head(normal_bmi$data))
  cat("Total records:", normal_bmi$count, "\n")
}
cat("\n")

# Example 6: Get population data for females
cat("6. Female Population Data\n")
cat("========================\n")
female_pop <- api_request("/api/population", list(sex = "Female"))
if (!is.null(female_pop)) {
  print(head(female_pop$data))
  cat("Total records:", female_pop$count, "\n")
}
cat("\n")

# Example 7: Get summary statistics
cat("7. Summary Statistics\n")
cat("====================\n")
stats <- api_request("/api/statistics")
if (!is.null(stats)) {
  print(stats$data_summary)
}
cat("\n")

cat("API Examples Complete!\n")
cat("=====================\n")
cat("The API provides programmatic access to all NHANES BMI-Body Fat analysis results.\n")
cat("Start the API server with: make api\n")
cat("View interactive documentation at: http://localhost:8000/__docs__/\n")
