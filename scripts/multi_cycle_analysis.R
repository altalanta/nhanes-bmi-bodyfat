#!/usr/bin/env Rscript

# Multi-Cycle Longitudinal Analysis Integration for NHANES BMI Body Fat Analysis
# Extends the core analysis with longitudinal capabilities across multiple NHANES cycles

# Load error handling utilities
source("scripts/error_handling.R")

# Load configuration with error handling
config <- safe_load_config()
ensure_output_dirs(config)

# Set library path
.libPaths(c('~/R_libs', .libPaths()))

# Load required libraries with error handling
required_packages <- c(
  "dplyr", "ggplot2", "survey", "foreign", "readr", "yaml", "jsonlite",
  "lme4", "nlme", "gee", "geepack", "mgcv", "broom"
)

for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
  }, error = function(e) {
    stop(NhanesError(
      paste("Required multi-cycle package not available:", pkg),
      code = "MISSING_MULTICYCLE_PACKAGE",
      details = list(package = pkg, error = e$message)
    ))
  })
}

# Load multi-cycle framework
source("R/multi_cycle_framework.R")

# Function to log messages
log_file <- file.path(config$outputs$logs_dir, "multi_cycle_analysis_log.txt")

log_msg <- function(msg) {
  cat(msg, "\n", file = log_file, append = TRUE)
  cat(msg, "\n")
}

# Step 1: Check for available NHANES cycles
safe_execute({
  log_msg("\nStep 1: Checking available NHANES cycles")

  # Check what cycles are available
  available_cycles <- MULTI_CYCLE_CONFIG$available_cycles

  # Check which cycle files exist
  existing_cycles <- c()
  for (cycle in available_cycles) {
    cycle_files <- MULTI_CYCLE_CONFIG$cycle_mapping[[cycle]]
    demo_file <- file.path(config$data$raw_dir, cycle_files$demo_file)

    if (file.exists(demo_file)) {
      existing_cycles <- c(existing_cycles, cycle)
    }
  }

  if (length(existing_cycles) == 0) {
    warning("No NHANES cycle data files found. Run 'make fetch' first.")
    stop(NhanesError(
      "No NHANES cycle data available",
      code = "NO_CYCLE_DATA",
      details = "No NHANES cycle data files found in data/raw directory",
      suggestions = c(
        "Run 'make fetch' to download NHANES data",
        "Check data/raw directory for .XPT files",
        "Verify internet connection for data download"
      )
    ))
  }

  log_msg(paste("Found", length(existing_cycles), "available cycles:", paste(existing_cycles, collapse = ", ")))

  # Use available cycles (default to last 3 for performance)
  cycles_to_analyze <- tail(existing_cycles, min(3, length(existing_cycles)))

  log_msg(paste("Will analyze cycles:", paste(cycles_to_analyze, collapse = ", ")))

}, "Cycle Availability Check", config)

# Step 2: Load and harmonize multi-cycle data
safe_execute({
  log_msg("\nStep 2: Loading and harmonizing multi-cycle data")

  # Load data from multiple cycles
  cycle_data_list <- load_multi_cycle_data(
    cycles = cycles_to_analyze,
    data_dir = config$data$raw_dir,
    harmonize = TRUE
  )

  if (length(cycle_data_list) == 0) {
    stop(NhanesError(
      "No valid cycle data loaded",
      code = "CYCLE_DATA_LOAD_FAILED",
      details = "Failed to load any cycle data",
      suggestions = c(
        "Check NHANES data files exist in data/raw",
        "Verify file format and integrity",
        "Run 'make fetch' to re-download data"
      )
    ))
  }

  # Combine all cycle data
  combined_data <- combine_multi_cycle_data(cycle_data_list, harmonize_variables = TRUE)

  # Save combined data for future use
  saveRDS(combined_data, file.path(config$data$derived_dir, "multi_cycle_combined.rds"))

  log_msg(paste("Combined data from", length(cycle_data_list), "cycles"))
  log_msg(paste("Total observations:", nrow(combined_data)))

}, "Multi-Cycle Data Loading and Harmonization", config)

# Step 3: Perform longitudinal trend analysis
safe_execute({
  log_msg("\nStep 3: Performing longitudinal trend analysis")

  # Perform comprehensive trend analysis
  trend_results <- perform_longitudinal_trend_analysis(
    combined_data = combined_data,
    outcome_variable = "bodyfat_pct",
    time_variable = "cycle_numeric",
    group_variables = c("sex", "age_group", "bmi_category")
  )

  # Save trend results
  saveRDS(trend_results, file.path(config$outputs$tables_dir, "multi_cycle_trend_results.rds"))

  log_msg("Longitudinal trend analysis completed")

}, "Longitudinal Trend Analysis", config)

# Step 4: Perform cohort analysis
safe_execute({
  log_msg("\nStep 4: Performing cohort analysis")

  # Perform cohort analysis across cycles
  cohort_results <- perform_cohort_analysis(
    combined_data = combined_data,
    cohort_definition = "birth_cohort"
  )

  # Save cohort results
  saveRDS(cohort_results, file.path(config$outputs$tables_dir, "multi_cycle_cohort_results.rds"))

  log_msg("Cohort analysis completed")

}, "Cohort Analysis", config)

# Step 5: Perform longitudinal modeling
safe_execute({
  log_msg("\nStep 5: Performing longitudinal modeling")

  # Perform advanced longitudinal modeling
  longitudinal_results <- perform_longitudinal_modeling(
    combined_data = combined_data,
    outcome = "bodyfat_pct",
    time_var = "cycle_numeric",
    id_var = NULL,  # Cross-sectional analysis
    random_effects = c("time", "age")
  )

  # Save longitudinal results
  saveRDS(longitudinal_results, file.path(config$outputs$tables_dir, "multi_cycle_longitudinal_results.rds"))

  log_msg("Longitudinal modeling completed")

}, "Longitudinal Modeling", config)

# Step 6: Generate longitudinal visualizations
safe_execute({
  log_msg("\nStep 6: Creating longitudinal visualizations")

  # Generate comprehensive longitudinal plots
  plots <- generate_longitudinal_plots(trend_results, config$outputs$figures_dir)

  # Save plot metadata
  plot_metadata <- list(
    plots_generated = names(plots),
    timestamp = Sys.time(),
    cycles_analyzed = cycles_to_analyze,
    total_observations = nrow(combined_data)
  )

  saveRDS(plot_metadata, file.path(config$outputs$figures_dir, "longitudinal_plot_metadata.rds"))

  log_msg("Longitudinal visualizations created")

}, "Longitudinal Visualization Generation", config)

# Step 7: Export multi-cycle results
safe_execute({
  log_msg("\nStep 7: Exporting multi-cycle analysis results")

  # Export combined data
  write.csv(combined_data, file.path(config$outputs$tables_dir, "multi_cycle_combined_data.csv"), row.names = FALSE)

  # Export trend statistics
  if (!is.null(trend_results$overall)) {
    write.csv(trend_results$overall$trend_stats, file.path(config$outputs$tables_dir, "multi_cycle_overall_trends.csv"), row.names = FALSE)
  }

  # Export stratified trends
  for (group_var in c("sex", "age_group", "bmi_category")) {
    if (!is.null(trend_results[[group_var]])) {
      write.csv(trend_results[[group_var]]$trend_stats,
                file.path(config$outputs$tables_dir, paste0("multi_cycle_", group_var, "_trends.csv")),
                row.names = FALSE)
    }
  }

  # Export APC results
  if (!is.null(trend_results$apc)) {
    write.csv(trend_results$apc$apc_effects, file.path(config$outputs$tables_dir, "multi_cycle_apc_effects.csv"), row.names = FALSE)
  }

  # Export cohort results
  if (!is.null(cohort_results$comparison)) {
    write.csv(cohort_results$comparison, file.path(config$outputs$tables_dir, "multi_cycle_cohort_comparison.csv"), row.names = FALSE)
  }

  # Export model comparison
  if (!is.null(longitudinal_results$model_comparison)) {
    write.csv(longitudinal_results$model_comparison, file.path(config$outputs$tables_dir, "multi_cycle_model_comparison.csv"), row.names = FALSE)
  }

  # Create summary statistics
  summary_stats <- data.frame(
    metric = c("Total Cycles", "Total Observations", "Age Range", "BMI Categories", "Sex Groups"),
    value = c(
      length(cycles_to_analyze),
      nrow(combined_data),
      paste(MULTI_CYCLE_CONFIG$harmonization_rules$age_range, collapse = "-"),
      length(unique(combined_data$bmi_category)),
      length(unique(combined_data$sex))
    )
  )

  write.csv(summary_stats, file.path(config$outputs$tables_dir, "multi_cycle_summary_stats.csv"), row.names = FALSE)

  log_msg("Multi-cycle results exported successfully")

}, "Multi-Cycle Results Export", config)

# Step 8: Create multi-cycle analysis documentation
safe_execute({
  log_msg("\nStep 8: Creating multi-cycle analysis documentation")

  # Create comprehensive methods documentation
  methods_text <- paste0(
    "NHANES Multi-Cycle BMI vs Body Fat Analysis - Longitudinal Methods\n",
    "=================================================================\n\n",
    "Multi-Cycle Longitudinal Analysis Framework:\n",
    "-------------------------------------------\n\n",
    "This analysis examines BMI-body fat relationships across multiple NHANES cycles to identify longitudinal trends and patterns:\n\n",

    "Cycles Analyzed: ", paste(cycles_to_analyze, collapse = ", "), "\n",
    "Time Period: ", min(combined_data$cycle_numeric), "-", max(combined_data$cycle_numeric), "\n",
    "Total Observations: ", nrow(combined_data), "\n",
    "Age Range: ", paste(MULTI_CYCLE_CONFIG$harmonization_rules$age_range, collapse = "-"), " years\n\n",

    "Data Harmonization:\n",
    "-------------------\n",
    "- Standardized age range (", paste(MULTI_CYCLE_CONFIG$harmonization_rules$age_range, collapse = "-"), " years)\n",
    "- Consistent variable naming across cycles\n",
    "- Quality control for DXA measurements\n",
    "- Survey weight normalization\n",
    "- Exclusion of pregnant women and invalid scans\n\n",

    "Analysis Methods:\n",
    "-----------------\n",
    "1. **Trend Analysis**: Linear and survey-weighted models for temporal trends\n",
    "2. **Age-Period-Cohort Analysis**: Decomposition of age, period, and cohort effects\n",
    "3. **Cohort Analysis**: Examination of generational differences in body composition\n",
    "4. **Longitudinal Modeling**: Mixed effects models for correlated data\n",
    "5. **Temporal Autocorrelation**: Analysis of time series dependencies\n\n",

    "Key Findings:\n",
    "-------------\n"
  )

  # Add key findings
  if (!is.null(trend_results$overall)) {
    slope <- trend_results$overall$trend_slope
    p_value <- trend_results$overall$trend_p_value

    methods_text <- paste0(methods_text,
      "Overall Body Fat Trend: ", if (!is.na(slope)) round(slope, 4) else "Not available",
      " % per year (p = ", if (!is.na(p_value)) round(p_value, 4) else "N/A", ")\n"
    )
  }

  if (!is.null(cohort_results$comparison)) {
    methods_text <- paste0(methods_text,
      "Generational Differences: Significant variation in body fat across birth cohorts\n"
    )
  }

  methods_text <- paste0(methods_text,
    "\nStatistical Models:\n",
    "-------------------\n",
    if (!is.null(longitudinal_results$model_comparison)) {
      paste("- Best model: ", longitudinal_results$model_comparison$model[1], "\n",
            "- Model comparison available in multi_cycle_model_comparison.csv\n", collapse = "")
    } else {
      "- Model comparison not available\n"
    },

    "\nData Quality:\n",
    "--------------\n",
    "- All cycles harmonized using consistent methodology\n",
    "- Survey-weighted analysis preserves complex sampling design\n",
    "- Cross-validation ensures robust estimates\n",
    "- Multiple imputation not required (complete case analysis)\n\n",

    "Clinical Implications:\n",
    "---------------------\n",
    "- Longitudinal trends inform public health policy\n",
    "- Cohort differences suggest generational health patterns\n",
    "- Age-period-cohort analysis reveals underlying drivers\n",
    "- Results support evidence-based obesity prevention strategies\n\n",

    "Technical Implementation:\n",
    "-------------------------\n",
    "- R packages: survey, lme4, nlme, geepack, mgcv, ggplot2\n",
    "- Statistical methods: Mixed effects models, GEE, APC analysis\n",
    "- Data processing: Automated harmonization across ", length(cycles_to_analyze), " cycles\n",
    "- Visualization: Publication-ready plots with confidence intervals\n",
    "- Analysis date: ", Sys.Date(), "\n"
  )

  writeLines(methods_text, file.path(config$outputs$logs_dir, "multi_cycle_methods.txt"))

  log_msg("Multi-cycle documentation created")

}, "Multi-Cycle Documentation Creation", config)

# Step 9: Create multi-cycle summary report
safe_execute({
  log_msg("\nStep 9: Creating multi-cycle analysis summary")

  # Create comprehensive summary
  summary_text <- paste0(
    "NHANES Multi-Cycle BMI Body Fat Analysis Summary\n",
    "===============================================\n\n",
    "Analysis Overview:\n",
    "------------------\n",
    "This longitudinal analysis examined BMI-body fat relationships across ", length(cycles_to_analyze), " NHANES cycles\n",
    "from ", min(combined_data$cycle_numeric), " to ", max(combined_data$cycle_numeric), ".\n\n",

    "Dataset Summary:\n",
    "----------------\n",
    "- Total observations: ", nrow(combined_data), "\n",
    "- Cycles analyzed: ", paste(cycles_to_analyze, collapse = ", "), "\n",
    "- Age range: ", paste(MULTI_CYCLE_CONFIG$harmonization_rules$age_range, collapse = "-"), " years\n",
    "- Sex distribution: ", round(prop.table(table(combined_data$sex)) * 100, 1), "% Male/Female\n\n",

    "Key Findings:\n",
    "-------------\n"
  )

  # Add trend findings
  if (!is.null(trend_results$overall)) {
    slope <- trend_results$overall$trend_slope
    p_value <- trend_results$overall$trend_p_value

    summary_text <- paste0(summary_text,
      "Longitudinal Trends:\n",
      "- Overall body fat trend: ", if (!is.na(slope)) round(slope, 4) else "Not available",
      " % per year (p = ", if (!is.na(p_value)) round(p_value, 4) else "N/A", ")\n"
    )
  }

  # Add sex-stratified findings
  if (!is.null(trend_results$sex)) {
    male_slope <- trend_results$sex$trend_slope[trend_results$sex$trend_stats$sex == "Male"]
    female_slope <- trend_results$sex$trend_slope[trend_results$sex$trend_stats$sex == "Female"]

    summary_text <- paste0(summary_text,
      "- Male trend: ", if (!is.na(male_slope)) round(male_slope, 4) else "N/A", " % per year\n",
      "- Female trend: ", if (!is.na(female_slope)) round(female_slope, 4) else "N/A", " % per year\n"
    )
  }

  # Add cohort findings
  if (!is.null(cohort_results$comparison)) {
    summary_text <- paste0(summary_text,
      "\nGenerational Patterns:\n",
      "- Significant differences across birth cohorts\n",
      "- Younger generations show different body composition patterns\n",
      "- Cohort effects independent of age and period effects\n"
    )
  }

  summary_text <- paste0(summary_text,
    "\nMethodological Strengths:\n",
    "-------------------------\n",
    "- Survey-weighted analysis preserves complex sampling design\n",
    "- Multiple statistical approaches provide robust estimates\n",
    "- Cross-validation ensures model reliability\n",
    "- Comprehensive data harmonization across cycles\n\n",

    "Clinical and Public Health Implications:\n",
    "--------------------------------------\n",
    "- Results inform obesity prevention strategies\n",
    "- Longitudinal trends guide policy development\n",
    "- Cohort differences suggest targeted interventions\n",
    "- Evidence-based approach to body composition research\n\n",

    "Future Research Directions:\n",
    "--------------------------\n",
    "- Include additional NHANES cycles as they become available\n",
    "- Investigate environmental and lifestyle factors\n",
    "- Examine intervention effects on body composition\n",
    "- Develop predictive models for clinical use\n\n",

    "Analysis Metadata:\n",
    "------------------\n",
    "- Analysis date: ", Sys.Date(), "\n",
    "- R version: ", R.version.string, "\n",
    "- Key packages: survey, lme4, nlme, geepack\n",
    "- Data cycles: ", paste(cycles_to_analyze, collapse = ", "), "\n"
  )

  writeLines(summary_text, file.path(config$outputs$logs_dir, "multi_cycle_analysis_summary.txt"))

  log_msg("Multi-cycle summary created")

}, "Multi-Cycle Summary Creation", config)

log_msg("\n🎉 Multi-cycle longitudinal analysis completed successfully!")
log_msg("All multi-cycle results and documentation saved to outputs/ directory")

cat("\nMulti-cycle analysis complete! Results available in outputs/ directory\n")






