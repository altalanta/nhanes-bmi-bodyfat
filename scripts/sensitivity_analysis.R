#!/usr/bin/env Rscript

# Sensitivity analyses for NHANES BMI vs body fat analysis with Performance Benchmarking
# Additional models and comparisons with comprehensive performance tracking

suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(here)
  library(jsonlite)
})

# Load performance utilities
source("R/performance.R")

# Initialize performance tracking for sensitivity analysis
initialize_performance_tracking("nhanes_sensitivity_analysis")

# Set up paths
repo_root <- here::here()
outputs_dir <- file.path(repo_root, "outputs", "tables")
logs_dir <- file.path(repo_root, "outputs", "logs")

dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

# Enhanced logging for sensitivity analysis
sens_log <- function(msg) {
  log_performance(msg, "INFO")
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", msg, "\n")
}

sens_log("NHANES BMI vs Body Fat Sensitivity Analyses with Performance Benchmarking")
sens_log("Starting comprehensive sensitivity analysis...")

# Load analytic dataset with performance tracking
data_loading <- benchmark_operation({
  sens_log("Loading analytic dataset...")

  # Validate dataset exists
  dataset_path <- here("data", "derived", "analytic.rds")
  if (!file.exists(dataset_path)) {
    stop(NhanesError(
      paste("Analytic dataset not found:", dataset_path),
      code = "MISSING_ANALYTIC_DATASET",
      details = list(dataset_path = dataset_path)
    ))
  }

  analytic <- readRDS(dataset_path)

  if (is.null(analytic) || nrow(analytic) == 0) {
    stop(NhanesError(
      "Analytic dataset is empty",
      code = "EMPTY_ANALYTIC_DATASET",
      details = list(n_rows = nrow(analytic), n_cols = ncol(analytic))
    ))
  }

  return(analytic)
}, "data_loading", metadata = list(dataset_path = dataset_path))

analytic <- data_loading

# Create survey design with performance tracking
survey_design_creation <- benchmark_operation({
  sens_log("Creating survey design...")

  # Validate required survey variables
  required_vars <- c("psu", "strata", "survey_weight")
  missing_vars <- setdiff(required_vars, names(analytic))

  if (length(missing_vars) > 0) {
    stop(NhanesError(
      paste("Missing required survey variables:", paste(missing_vars, collapse = ", ")),
      code = "MISSING_SURVEY_VARS",
      details = list(missing = missing_vars, available = names(analytic))
    ))
  }

  svy_design <- svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~survey_weight,
    nest = TRUE,
    data = analytic
  )

  return(svy_design)
}, "survey_design_creation", metadata = list(n_observations = nrow(analytic)))

svy_design <- survey_design_creation

options(survey.lonely.psu = "adjust")

# Sensitivity Analysis 1: Linear vs Non-linear Models with Performance Tracking
sens_log("1. Model Comparison: Linear vs Quadratic vs Cubic")
sens_log("=================================================")

# Fit various models with performance tracking
model_fitting <- benchmark_operation({
  sens_log("Fitting model comparison models...")

  # Basic polynomial models
  model_linear <- svyglm(bodyfat_pct ~ bmi, design = svy_design)
  model_quad <- svyglm(bodyfat_pct ~ bmi + I(bmi^2), design = svy_design)
  model_cubic <- svyglm(bodyfat_pct ~ bmi + I(bmi^2) + I(bmi^3), design = svy_design)

  # Models with sex interaction
  model_sex_int <- svyglm(bodyfat_pct ~ bmi * sex, design = svy_design)
  model_quad_sex <- svyglm(bodyfat_pct ~ (bmi + I(bmi^2)) * sex, design = svy_design)

  return(list(
    linear = model_linear,
    quadratic = model_quad,
    cubic = model_cubic,
    linear_sex = model_sex_int,
    quadratic_sex = model_quad_sex
  ))
}, "model_fitting", metadata = list(n_models = 5, model_types = c("linear", "quadratic", "cubic", "linear_sex", "quadratic_sex")))

models <- model_fitting

# Create model comparison table with performance metrics
model_comparison_creation <- benchmark_operation({
  sens_log("Creating model comparison table...")

  model_comparison <- data.frame(
    Model = c("Linear", "Quadratic", "Cubic", "Linear + Sex", "Quadratic + Sex"),
    AIC = c(AIC(models$linear), AIC(models$quadratic), AIC(models$cubic),
            AIC(models$linear_sex), AIC(models$quadratic_sex)),
    BIC = c(BIC(models$linear), BIC(models$quadratic), BIC(models$cubic),
            BIC(models$linear_sex), BIC(models$quadratic_sex)),
    R_squared = c(
      summary(models$linear)$r.squared,
      summary(models$quadratic)$r.squared,
      summary(models$cubic)$r.squared,
      summary(models$linear_sex)$r.squared,
      summary(models$quadratic_sex)$r.squared
    ),
    RMSE = c(
      sqrt(mean(residuals(models$linear)^2)),
      sqrt(mean(residuals(models$quadratic)^2)),
      sqrt(mean(residuals(models$cubic)^2)),
      sqrt(mean(residuals(models$linear_sex)^2)),
      sqrt(mean(residuals(models$quadratic_sex)^2))
    )
  )

  return(model_comparison)
}, "model_comparison_creation", metadata = list(n_models = 5))

model_comparison <- model_comparison_creation

# Save model comparison with performance tracking
model_comparison_saving <- benchmark_operation({
  output_file <- file.path(outputs_dir, "model_comparison.csv")
  write.csv(model_comparison, output_file, row.names = FALSE)

  sens_log(paste("Model comparison saved to:", output_file))
  return(output_file)
}, "model_comparison_saving", metadata = list(output_file = "model_comparison.csv"))

print(model_comparison)

# Sensitivity Analysis 2: Age-adjusted Models with Performance Tracking
sens_log("2. Age-Adjusted Models")
sens_log("======================")

# Models with age adjustment with performance tracking
age_models_fitting <- benchmark_operation({
  sens_log("Fitting age-adjusted models...")

  model_age_linear <- svyglm(bodyfat_pct ~ bmi + age_centered, design = svy_design)
  model_age_quad <- svyglm(bodyfat_pct ~ bmi + I(bmi^2) + age_centered, design = svy_design)
  model_age_sex <- svyglm(bodyfat_pct ~ bmi * sex + age_centered, design = svy_design)

  return(list(
    age_linear = model_age_linear,
    age_quadratic = model_age_quad,
    age_sex = model_age_sex
  ))
}, "age_models_fitting", metadata = list(n_models = 3, includes_age = TRUE))

age_models <- age_models_fitting

# Create age-adjusted model comparison with performance tracking
age_comparison_creation <- benchmark_operation({
  sens_log("Creating age-adjusted model comparison...")

  age_adj_comparison <- data.frame(
    Model = c("BMI", "BMI + Age", "BMI² + Age", "BMI × Sex + Age"),
    AIC = c(AIC(models$linear), AIC(age_models$age_linear), AIC(age_models$age_quadratic), AIC(age_models$age_sex)),
    R_squared = c(
      summary(models$linear)$r.squared,
      summary(age_models$age_linear)$r.squared,
      summary(age_models$age_quadratic)$r.squared,
      summary(age_models$age_sex)$r.squared
    )
  )

  return(age_adj_comparison)
}, "age_comparison_creation", metadata = list(n_models = 4))

age_adj_comparison <- age_comparison_creation

# Save age-adjusted model comparison with performance tracking
age_comparison_saving <- benchmark_operation({
  output_file <- file.path(outputs_dir, "age_adjusted_models.csv")
  write.csv(age_adj_comparison, output_file, row.names = FALSE)

  sens_log(paste("Age-adjusted model comparison saved to:", output_file))
  return(output_file)
}, "age_comparison_saving", metadata = list(output_file = "age_adjusted_models.csv"))

cat("Age-adjusted model comparison:\n")
print(age_adj_comparison)

# Sensitivity Analysis 3: Race/Ethnicity Adjustment with Performance Tracking
sens_log("3. Race/Ethnicity-Adjusted Models")
sens_log("==================================")

# Check race/ethnicity distribution with performance tracking
race_analysis <- benchmark_operation({
  sens_log("Analyzing race/ethnicity distribution...")

  race_dist <- table(analytic$race_ethnicity, useNA = "always")
  cat("Race/ethnicity distribution:\n")
  print(race_dist)

  # Determine if sufficient variation for race/ethnicity adjustment
  unique_races <- length(unique(analytic$race_ethnicity[!is.na(analytic$race_ethnicity)]))
  sufficient_variation <- unique_races >= 3

  sens_log(paste("Race/ethnicity categories:", unique_races, "- Sufficient variation:", sufficient_variation))

  return(list(race_dist = race_dist, unique_races = unique_races, sufficient_variation = sufficient_variation))
}, "race_analysis", metadata = list(unique_races = length(unique(analytic$race_ethnicity[!is.na(analytic$race_ethnicity)]))))

# Models with race/ethnicity adjustment (if sufficient variation)
if (race_analysis$sufficient_variation) {
  race_models_fitting <- benchmark_operation({
    sens_log("Fitting race/ethnicity-adjusted models...")

    model_race <- svyglm(bodyfat_pct ~ bmi + race_ethnicity, design = svy_design)
    model_race_sex <- svyglm(bodyfat_pct ~ bmi * sex + race_ethnicity, design = svy_design)

    return(list(race = model_race, race_sex = model_race_sex))
  }, "race_models_fitting", metadata = list(n_models = 2, includes_race = TRUE))

  race_models <- race_models_fitting

  # Create race-adjusted model comparison with performance tracking
  race_comparison_creation <- benchmark_operation({
    sens_log("Creating race-adjusted model comparison...")

    race_adj_comparison <- data.frame(
      Model = c("BMI", "BMI + Race", "BMI × Sex + Race"),
      AIC = c(AIC(models$linear), AIC(race_models$race), AIC(race_models$race_sex)),
      R_squared = c(
        summary(models$linear)$r.squared,
        summary(race_models$race)$r.squared,
        summary(race_models$race_sex)$r.squared
      )
    )

    return(race_adj_comparison)
  }, "race_comparison_creation", metadata = list(n_models = 3))

  race_adj_comparison <- race_comparison_creation

  # Save race-adjusted model comparison with performance tracking
  race_comparison_saving <- benchmark_operation({
    output_file <- file.path(outputs_dir, "race_adjusted_models.csv")
    write.csv(race_adj_comparison, output_file, row.names = FALSE)

    sens_log(paste("Race-adjusted model comparison saved to:", output_file))
    return(output_file)
  }, "race_comparison_saving", metadata = list(output_file = "race_adjusted_models.csv"))

  cat("Race-adjusted model comparison:\n")
  print(race_adj_comparison)
} else {
  sens_log("Insufficient race/ethnicity variation for adjustment analysis")
  cat("Insufficient race/ethnicity variation for adjustment analysis\n")
}

# Sensitivity Analysis 4: Correlation by BMI Strata
cat("\n4. Correlations by BMI Strata\n")
cat("==============================\n")

# Define BMI strata
bmi_strata <- list(
  "Normal/Overweight" = c(18.5, 30),
  "Obesity" = c(30, Inf),
  "Severe Obesity" = c(35, Inf)
)

stratum_correlations <- data.frame()

for (stratum_name in names(bmi_strata)) {
  bmi_range <- bmi_strata[[stratum_name]]
  
  # Subset data
  stratum_data <- analytic %>%
    filter(bmi >= bmi_range[1] & bmi < bmi_range[2])
  
  if (nrow(stratum_data) >= 100) {  # Minimum sample size
    stratum_design <- svydesign(
      ids = ~psu,
      strata = ~strata,
      weights = ~survey_weight,
      nest = TRUE,
      data = stratum_data
    )
    
    # Compute correlation
    corr_matrix <- svyvar(~bmi + bodyfat_pct, stratum_design)
    correlation <- corr_matrix[1,2] / sqrt(corr_matrix[1,1] * corr_matrix[2,2])
    
    stratum_correlations <- rbind(stratum_correlations, data.frame(
      Stratum = stratum_name,
      BMI_Range = paste(bmi_range[1], "to", ifelse(is.infinite(bmi_range[2]), "max", bmi_range[2])),
      N = nrow(stratum_data),
      Correlation = correlation
    ))
  }
}

if (nrow(stratum_correlations) > 0) {
  write.csv(stratum_correlations, file.path(outputs_dir, "correlations_by_bmi_strata.csv"), row.names = FALSE)
  cat("BMI stratum correlations:\n")
  print(stratum_correlations)
}

# Sensitivity Analysis 5: Exclusion Impact
cat("\n5. Impact of Exclusion Criteria\n")
cat("================================\n")

# Load raw data to assess exclusion impact
demo_raw <- foreign::read.xport(here("data", "raw", "DEMO_J.XPT"))
bmx_raw <- foreign::read.xport(here("data", "raw", "BMX_J.XPT"))
dxx_raw <- foreign::read.xport(here("data", "raw", "DXX_J.XPT"))

# Merge raw data
raw_merged <- demo_raw %>%
  select(SEQN, RIDAGEYR, RIAGENDR, WTMEC2YR, SDMVSTRA, SDMVPSU) %>%
  left_join(bmx_raw %>% select(SEQN, BMXBMI), by = "SEQN") %>%
  left_join(dxx_raw %>% select(SEQN, DXDTOFAT), by = "SEQN")

# Calculate exclusion impacts
exclusion_impact <- data.frame(
  Step = c("Initial", "Age 20-59", "Valid BMI", "Valid Body Fat", "Valid Survey Vars"),
  N = c(
    nrow(raw_merged),
    sum(raw_merged$RIDAGEYR >= 20 & raw_merged$RIDAGEYR <= 59, na.rm = TRUE),
    sum(raw_merged$RIDAGEYR >= 20 & raw_merged$RIDAGEYR <= 59 & 
        !is.na(raw_merged$BMXBMI) & raw_merged$BMXBMI > 0, na.rm = TRUE),
    sum(raw_merged$RIDAGEYR >= 20 & raw_merged$RIDAGEYR <= 59 & 
        !is.na(raw_merged$BMXBMI) & raw_merged$BMXBMI > 0 &
        !is.na(raw_merged$DXDTOFAT) & raw_merged$DXDTOFAT > 0, na.rm = TRUE),
    nrow(analytic)
  )
) %>%
  mutate(
    Excluded = lag(N, default = first(N)) - N,
    Percent_Remaining = round(100 * N / first(N), 1)
  )

write.csv(exclusion_impact, file.path(outputs_dir, "exclusion_impact.csv"), row.names = FALSE)

cat("Exclusion impact:\n")
print(exclusion_impact)

# Sensitivity Analysis 6: Weighted vs Unweighted Estimates
cat("\n6. Weighted vs Unweighted Comparison\n")
cat("====================================\n")

# Compute key estimates with and without weights
weighted_stats <- list(
  overall_corr = {
    corr_matrix <- svyvar(~bmi + bodyfat_pct, svy_design)
    corr_matrix[1,2] / sqrt(corr_matrix[1,1] * corr_matrix[2,2])
  },
  mean_bmi = as.numeric(svymean(~bmi, svy_design)),
  mean_bodyfat = as.numeric(svymean(~bodyfat_pct, svy_design))
)

unweighted_stats <- list(
  overall_corr = cor(analytic$bmi, analytic$bodyfat_pct),
  mean_bmi = mean(analytic$bmi),
  mean_bodyfat = mean(analytic$bodyfat_pct)
)

weight_comparison <- data.frame(
  Statistic = c("BMI-Body Fat Correlation", "Mean BMI", "Mean Body Fat"),
  Weighted = c(weighted_stats$overall_corr, weighted_stats$mean_bmi, weighted_stats$mean_bodyfat),
  Unweighted = c(unweighted_stats$overall_corr, unweighted_stats$mean_bmi, unweighted_stats$mean_bodyfat)
) %>%
  mutate(
    Difference = Weighted - Unweighted,
    Percent_Diff = round(100 * (Weighted - Unweighted) / Unweighted, 2)
  )

write.csv(weight_comparison, file.path(outputs_dir, "weighted_vs_unweighted.csv"), row.names = FALSE)

cat("Weighted vs unweighted comparison:\n")
print(weight_comparison)

# Generate comprehensive sensitivity analysis performance summary
sens_log("Generating sensitivity analysis performance summary...")

sensitivity_performance_summary <- benchmark_operation({
  # Calculate comprehensive sensitivity analysis metrics
  sensitivity_summary <- list(
    timestamp = Sys.time(),
    analyses_performed = 4,  # Model comparison, age adjustment, race adjustment, correlation strata
    models_fitted = length(performance_tracker$benchmarks) - 5,  # Subtract non-model operations
    total_benchmarks = length(performance_tracker$benchmarks),
    computational_efficiency = list(
      avg_model_fit_time = mean(sapply(performance_tracker$benchmarks[grepl("model", names(performance_tracker$benchmarks))], function(b) b$duration)),
      total_analysis_time = sum(sapply(performance_tracker$benchmarks, function(b) b$duration)),
      peak_memory_usage = max(sapply(performance_tracker$benchmarks, function(b) b$memory_used))
    ),
    model_types_evaluated = c(
      "linear", "quadratic", "cubic",
      "linear_sex_interaction", "quadratic_sex_interaction",
      "age_adjusted", "race_adjusted"
    ),
    statistical_measures = list(
      best_model_aic = min(model_comparison$AIC),
      best_model_r2 = max(model_comparison$R_squared),
      models_compared = nrow(model_comparison)
    )
  )

  # Save sensitivity analysis performance summary
  sens_perf_file <- file.path(logs_dir, "sensitivity_performance_summary.json")
  jsonlite::write_json(sensitivity_summary, sens_perf_file, pretty = TRUE, auto_unbox = TRUE)

  return(sens_perf_file)
}, "sensitivity_performance_summary", metadata = list(n_benchmarks = length(performance_tracker$benchmarks)))

# Create enhanced summary log with performance details
sensitivity_log_file <- file.path(logs_dir, "sensitivity_analysis.txt")
sensitivity_log_creation <- benchmark_operation({
  cat("NHANES BMI vs Body Fat Sensitivity Analysis with Performance Benchmarking\n", file = sensitivity_log_file)
  cat("========================================================================\n", file = sensitivity_log_file, append = TRUE)
  cat("Analysis date:", as.character(Sys.time()), "\n\n", file = sensitivity_log_file, append = TRUE)

  cat("Performance Summary:\n", file = sensitivity_log_file, append = TRUE)
  cat(sprintf("- Total benchmarks: %d\n", length(performance_tracker$benchmarks)), file = sensitivity_log_file, append = TRUE)
  cat(sprintf("- Total analysis time: %.2f seconds\n", sum(sapply(performance_tracker$benchmarks, function(b) b$duration))), file = sensitivity_log_file, append = TRUE)
  cat(sprintf("- Peak memory usage: %.2f MB\n", max(sapply(performance_tracker$benchmarks, function(b) b$memory_used))), file = sensitivity_log_file, append = TRUE)

  cat("\nFiles generated:\n", file = sensitivity_log_file, append = TRUE)
  cat("- model_comparison.csv\n", file = sensitivity_log_file, append = TRUE)
  cat("- age_adjusted_models.csv\n", file = sensitivity_log_file, append = TRUE)
  if (file.exists(file.path(outputs_dir, "race_adjusted_models.csv"))) {
    cat("- race_adjusted_models.csv\n", file = sensitivity_log_file, append = TRUE)
  }
  if (file.exists(file.path(outputs_dir, "correlations_by_bmi_strata.csv"))) {
    cat("- correlations_by_bmi_strata.csv\n", file = sensitivity_log_file, append = TRUE)
  }
  cat("- exclusion_impact.csv\n", file = sensitivity_log_file, append = TRUE)
  cat("- weighted_vs_unweighted.csv\n", file = sensitivity_log_file, append = TRUE)

  cat("\n✓ Sensitivity analyses completed successfully with comprehensive performance tracking\n")
  cat("Results saved to outputs/tables/\n")
  cat("Performance report saved to outputs/logs/performance_report.html\n")
  cat("Log saved to:", sensitivity_log_file, "\n")

  return(sensitivity_log_file)
}, "sensitivity_log_creation", metadata = list(log_file = sensitivity_log_file))

# Cleanup performance tracking
cleanup_performance_tracking()

sens_log("Sensitivity analysis completed successfully!")
sens_log(paste("Total benchmarks:", length(performance_tracker$benchmarks)))
sens_log("All performance data saved and tracked")