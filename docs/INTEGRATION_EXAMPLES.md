# Integration Examples and Use Cases

This guide provides practical examples of how to integrate the NHANES BMI Body Fat Analysis platform into various research workflows, from basic epidemiological studies to advanced statistical modeling and production deployment.

## 🎯 Basic Research Workflow

### Example 1: Standard Epidemiological Analysis

```r
# Complete research workflow using parallel pipeline
library(nhanesbmi)  # If packaged

# 1. Setup and configuration
make config-wizard  # Interactive configuration
# or edit config/config.yml directly

# 2. Run complete analysis
results <- run_parallel_pipeline()

# 3. Access results
correlation_data <- results$correlation_results
bmi_stats <- results$bmi_class_results

# 4. Generate custom visualizations
ggplot(correlation_data, aes(x = group, y = correlation)) +
  geom_bar(stat = "identity") +
  labs(title = "BMI-Body Fat Correlations by Sex")

# 5. Export for publication
write.csv(results$correlation_results, "my_study_correlations.csv")
```

### Example 2: Interactive Learning Session

```r
# Perfect for teaching epidemiological methods
# Launch interactive tutorial for students
make tutorial

# Students learn through:
# - Interactive data exploration
# - Hands-on statistical exercises
# - Quiz-based knowledge assessment
# - Real-time feedback and guidance
```

## 📊 Advanced Statistical Integration

### Example 3: Custom Statistical Models

```r
# Extend platform with custom statistical methods
library(brms)  # Bayesian regression models
library(lme4)  # Mixed effects models

# Load platform results
results <- run_parallel_pipeline()
data <- results$cleaned_data

# Custom Bayesian analysis
bayesian_model <- brm(
  bodyfat_pct ~ BMXBMI + sex + (1|SDMVSTRA),
  data = data,
  family = gaussian(),
  chains = 4, cores = 4  # Uses parallel processing
)

# Compare with platform results
summary(bayesian_model)
```

### Example 4: Machine Learning Integration

```r
# Machine learning prediction of body fat from BMI
library(caret)
library(randomForest)

# Prepare training data
train_data <- data %>%
  select(BMXBMI, bodyfat_pct, sex, RIDAGEYR) %>%
  na.omit()

# Train model
set.seed(123)
model <- train(
  bodyfat_pct ~ .,
  data = train_data,
  method = "rf",
  ntree = 100
)

# Compare predictions with actual correlations
predictions <- predict(model, train_data)
correlation <- cor(predictions, train_data$bodyfat_pct)
print(paste("Model correlation:", round(correlation, 3)))
```

## 🔬 Research Applications

### Example 5: Public Health Policy Analysis

```r
# Analyze BMI-body fat relationships for policy recommendations
# 1. Load platform results
results <- run_parallel_pipeline()

# 2. Stratify by demographic factors
bmi_analysis <- results$bmi_class_results %>%
  group_by(bmi_cat, sex) %>%
  summarize(
    mean_fat = mean(mean_bodyfat),
    ci_lower = mean(mean_ci_lower),
    ci_upper = mean(mean_ci_upper)
  )

# 3. Generate policy recommendations
policy_recommendations <- function(analysis_results) {
  # Identify high-risk groups
  high_risk <- analysis_results %>%
    filter(mean_fat > 25) %>%  # High body fat threshold
    arrange(desc(mean_fat))

  return(high_risk)
}

# 4. Create policy brief
high_risk_groups <- policy_recommendations(bmi_analysis)
write.csv(high_risk_groups, "policy_brief_high_risk_groups.csv")
```

### Example 6: Longitudinal Trend Analysis

```r
# Compare across NHANES cycles (when multiple cycles available)
# 1. Load data from multiple cycles
cycles <- c("2017-2018", "2015-2016", "2013-2014")

results_list <- list()
for (cycle in cycles) {
  # Update config for each cycle
  config$nhanes$cycle <- cycle
  write_yaml(config, "config/config.yml")

  # Run analysis for this cycle
  results_list[[cycle]] <- run_parallel_pipeline()
}

# 2. Compare trends over time
trend_analysis <- do.call(rbind, lapply(names(results_list), function(cycle) {
  data.frame(
    cycle = cycle,
    correlation = results_list[[cycle]]$correlation_results$correlation[1],
    sample_size = results_list[[cycle]]$bmi_class_results$pop_total[1]
  )
}))

# 3. Visualize trends
ggplot(trend_analysis, aes(x = cycle, y = correlation)) +
  geom_line() +
  geom_point() +
  labs(title = "BMI-Body Fat Correlation Trends Over Time")
```

## 🛠️ Software Integration

### Example 7: R Markdown Integration

```r
---
title: "NHANES BMI Analysis Report"
author: "Research Team"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
# Load platform functions
source("../R/data_versioning.R")
source("../parallel_pipeline.R")

# Run analysis
results <- run_parallel_pipeline()
```

## Data Quality Check

```{r data-quality}
# Display data quality information
display_quality_report()
```

## Analysis Results

### Correlation Analysis
```{r correlations}
# Display correlation results
kable(results$correlation_results, digits = 3)
```

### BMI Class Analysis
```{r bmi-classes}
# Display BMI class statistics
kable(results$bmi_class_results, digits = 2)
```

## Visualizations

```{r plots, fig.width=10, fig.height=6}
# Generate custom plots
ggplot(results$bmi_class_results,
       aes(x = bmi_cat, y = mean_bodyfat, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Body Fat by BMI Class and Sex")
```
```

### Example 8: Shiny Dashboard Integration

```r
# Create interactive dashboard using platform results
library(shiny)
library(plotly)
library(DT)

# Load platform results
results <- run_parallel_pipeline()

ui <- fluidPage(
  titlePanel("NHANES BMI Body Fat Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("bmi_category", "BMI Category:",
                 choices = unique(results$bmi_class_results$bmi_cat)),
      selectInput("statistic", "Statistic:",
                 choices = c("mean_bodyfat", "q05", "q50", "q95"))
    ),

    mainPanel(
      plotlyOutput("bmi_plot"),
      DTOutput("results_table")
    )
  )
)

server <- function(input, output) {
  output$bmi_plot <- renderPlotly({
    filtered_data <- results$bmi_class_results %>%
      filter(bmi_cat == input$bmi_category)

    plot_ly(filtered_data, x = ~sex, y = ~get(input$statistic),
            type = "bar", color = ~sex) %>%
      layout(title = paste(input$statistic, "by Sex for", input$bmi_category))
  })

  output$results_table <- renderDT({
    datatable(results$correlation_results,
              options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
```

### Example 9: API Integration

```r
# Create REST API for platform results
library(plumber)

# Load platform results
results <- run_parallel_pipeline()

#* @apiTitle NHANES BMI Analysis API
#* @apiDescription API for accessing NHANES analysis results

#* Get correlation results
#* @get /correlations
function() {
  return(results$correlation_results)
}

#* Get BMI class statistics
#* @get /bmi-stats
function() {
  return(results$bmi_class_results)
}

#* Get data quality report
#* @get /quality-report
function() {
  source("R/data_versioning.R")
  return(display_quality_report())
}

# Launch API server
make api-launch  # Or: R -e "pr <- plumb('api.R'); pr$run(port=8000)"
```

## 🎓 Educational Integration

### Example 10: Classroom Teaching

```r
# Use platform for epidemiology coursework

# 1. Pre-class preparation
make tutorial  # Students complete interactive tutorial

# 2. In-class exercises
# Students run analyses and discuss results

# 3. Homework assignments
# Customize parameters and compare results

# 4. Project work
# Students extend platform with custom analyses

# Example classroom workflow:
class_exercise <- function() {
  # Load platform results
  results <- run_parallel_pipeline()

  # Students analyze different subgroups
  male_data <- results$bmi_class_results %>% filter(sex == "Male")
  female_data <- results$bmi_class_results %>% filter(sex == "Female")

  # Compare male vs female body fat distributions
  t_test_result <- t.test(male_data$mean_bodyfat, female_data$mean_bodyfat)

  return(list(
    male_mean = mean(male_data$mean_bodyfat),
    female_mean = mean(female_data$mean_bodyfat),
    p_value = t_test_result$p.value
  ))
}
```

### Example 11: Student Research Projects

```r
# Template for student research projects

# 1. Define research question
research_question <- "
Does BMI predict body fat percentage differently
in different age groups or ethnicities?
"

# 2. Customize analysis parameters
custom_config <- list(
  analysis = list(
    age_range = c(20, 40),  # Focus on younger adults
    # Add custom stratification variables
  )
)

# 3. Run custom analysis
custom_results <- run_custom_analysis(custom_config)

# 4. Generate research report
generate_student_report(custom_results, research_question)
```

## 🔧 Production Deployment

### Example 12: Automated Reporting Pipeline

```bash
#!/bin/bash
# Automated weekly report generation

# 1. Update data if needed
make data-updates

# 2. Run analysis
make parallel-pipeline

# 3. Generate report
make report

# 4. Email results to stakeholders
echo "Weekly NHANES analysis complete. Results in outputs/report/" |
  mail -s "NHANES Analysis Report" stakeholders@organization.org

# 5. Archive results
cp -r outputs/ archives/weekly_$(date +%Y%m%d)/
```

### Example 13: Docker Container Integration

```dockerfile
# Dockerfile for reproducible deployment
FROM rocker/r-base:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libgit2-dev \
    pandoc

# Copy project files
COPY . /nhanes-analysis

# Set working directory
WORKDIR /nhanes-analysis

# Install R dependencies
RUN R -e "renv::restore()"

# Expose port for Shiny apps
EXPOSE 3838

# Default command
CMD ["R", "-e", "shiny::runApp('app.R', host='0.0.0.0', port=3838)"]
```

### Example 14: CI/CD Integration

```yaml
# GitHub Actions workflow for automated testing
name: NHANES Analysis CI/CD

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - uses: r-lib/actions/setup-r@v2

    - name: Install dependencies
      run: R -e "renv::restore()"

    - name: Run tests
      run: make test

    - name: Check code quality
      run: make quality

    - name: Build documentation
      run: R -e "devtools::build()"

  deploy:
    needs: test
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest
    steps:
    - name: Deploy to production
      run: make deploy
```

## 📈 Performance Integration

### Example 15: Batch Processing

```r
# Process multiple analysis configurations
configurations <- list(
  list(age_range = c(20, 39), name = "young_adults"),
  list(age_range = c(40, 59), name = "middle_aged"),
  list(age_range = c(60, 80), name = "older_adults")
)

results_list <- list()
for (config in configurations) {
  # Update configuration
  current_config <- read_yaml("config/config.yml")
  current_config$analysis$age_range <- config$age_range
  write_yaml(current_config, "config/config.yml")

  # Run analysis
  results_list[[config$name]] <- run_parallel_pipeline()
}

# Compare results across age groups
comparison_plot <- ggplot() +
  # Add comparison visualizations
```

### Example 16: Real-time Monitoring

```r
# Monitor analysis progress and performance
monitor_analysis <- function() {
  # Start monitoring
  start_time <- Sys.time()

  # Run analysis
  results <- run_parallel_pipeline()

  # Calculate metrics
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "secs")

  # Log performance metrics
  performance_log <- data.frame(
    timestamp = Sys.time(),
    execution_time = execution_time,
    cache_hits = calculate_cache_hits(),
    memory_usage = memory.size(),
    cpu_cores = availableCores()
  )

  write.csv(performance_log, "outputs/logs/performance_metrics.csv", append = TRUE)

  return(results)
}
```

## 🌐 Web Integration

### Example 17: Website Embedding

```html
<!-- Embed analysis results in website -->
<!DOCTYPE html>
<html>
<head>
    <title>NHANES BMI Analysis Results</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
</head>
<body>
    <h1>BMI vs Body Fat Analysis</h1>

    <!-- Embed interactive plot -->
    <div id="correlation-plot"></div>

    <script>
        // Load data from API
        fetch('/api/correlations')
        .then(response => response.json())
        .then(data => {
            // Create interactive visualization
            const trace = {
                x: data.group,
                y: data.correlation,
                type: 'bar'
            };

            Plotly.newPlot('correlation-plot', [trace]);
        });
    </script>

    <!-- Link to full analysis -->
    <p><a href="/report">View Complete Analysis Report</a></p>
</body>
</html>
```

### Example 18: Data Portal Integration

```r
# Integrate with research data portal
library(jsonlite)

# Export results in portal-compatible format
export_for_portal <- function(results) {
  portal_data <- list(
    metadata = list(
      analysis_date = Sys.Date(),
      data_source = "NHANES 2017-2018",
      analysis_type = "BMI-Body Fat Correlation",
      methods = "Survey-weighted linear regression"
    ),
    results = list(
      correlations = results$correlation_results,
      statistics = results$bmi_class_results,
      visualizations = results$plot_files
    ),
    reproducibility = list(
      data_registry = "data/registry/data_registry.json",
      config_file = "config/config.yml",
      code_version = system("git rev-parse HEAD", intern = TRUE)
    )
  )

  # Export as JSON for portal ingestion
  write_json(portal_data, "outputs/portal_export.json", pretty = TRUE)

  return(portal_data)
}
```

## 📚 Documentation Integration

### Example 19: Academic Paper Workflow

```r
# Generate supplementary materials for publication
generate_paper_supplements <- function(results) {

  # 1. Data quality report for supplementary materials
  quality_report <- generate_quality_report()

  # 2. Reproducibility manifest
  manifest <- generate_data_manifest()

  # 3. Statistical analysis details
  analysis_details <- list(
    sample_size = nrow(results$cleaned_data),
    survey_design = "Complex survey with MEC weights",
    statistical_method = "Survey-weighted correlation analysis",
    software_version = R.version.string,
    package_versions = installed.packages()[c("survey", "dplyr", "ggplot2"), "Version"]
  )

  # 4. Create supplementary files
  write_json(quality_report, "supplements/data_quality.json")
  write_json(manifest, "supplements/data_manifest.json")
  write_json(analysis_details, "supplements/analysis_details.json")

  # 5. Generate README for supplements
  readme_content <- "
# Supplementary Materials for NHANES BMI Analysis

## Files Included:
- data_quality.json: Data integrity and quality report
- data_manifest.json: Reproducibility manifest with file hashes
- analysis_details.json: Complete analysis methodology and software versions

## Reproducing Results:
1. Clone repository: git clone <repo_url>
2. Run analysis: make parallel-pipeline
3. Verify integrity: make data-integrity
4. Compare results with provided outputs

## Data Sources:
- NHANES 2017-2018 datasets (CDC)
- File integrity verified via SHA256 hashing
- Registry available at: data/registry/data_registry.json
"

  writeLines(readme_content, "supplements/README.md")

  return(list(
    quality_report = quality_report,
    manifest = manifest,
    analysis_details = analysis_details
  ))
}
```

---

**These integration examples demonstrate the versatility and extensibility of the NHANES BMI Body Fat Analysis platform, making it suitable for a wide range of research applications from basic epidemiological studies to advanced statistical modeling and production deployment scenarios.**
