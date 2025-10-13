#!/usr/bin/env Rscript

# Sensitivity analyses for NHANES BMI vs body fat analysis
# Additional models and comparisons

suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(here)
  library(jsonlite)
})

# Set up paths
repo_root <- here::here()
outputs_dir <- file.path(repo_root, "outputs", "tables")
logs_dir <- file.path(repo_root, "outputs", "logs")

dir.create(outputs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

cat("NHANES BMI vs Body Fat Sensitivity Analyses\n")
cat("===========================================\n\n")

# Load analytic dataset
analytic <- readRDS(here("data", "derived", "analytic.rds"))

# Create survey design
svy_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~survey_weight,
  nest = TRUE,
  data = analytic
)

options(survey.lonely.psu = "adjust")

# Sensitivity Analysis 1: Linear vs Non-linear Models
cat("1. Model Comparison: Linear vs Quadratic vs Cubic\n")
cat("=================================================\n")

# Fit various models
model_linear <- svyglm(bodyfat_pct ~ bmi, design = svy_design)
model_quad <- svyglm(bodyfat_pct ~ bmi + I(bmi^2), design = svy_design)
model_cubic <- svyglm(bodyfat_pct ~ bmi + I(bmi^2) + I(bmi^3), design = svy_design)

# Model with sex interaction
model_sex_int <- svyglm(bodyfat_pct ~ bmi * sex, design = svy_design)
model_quad_sex <- svyglm(bodyfat_pct ~ (bmi + I(bmi^2)) * sex, design = svy_design)

# Create model comparison table
model_comparison <- data.frame(
  Model = c("Linear", "Quadratic", "Cubic", "Linear + Sex", "Quadratic + Sex"),
  AIC = c(AIC(model_linear), AIC(model_quad), AIC(model_cubic), 
          AIC(model_sex_int), AIC(model_quad_sex)),
  BIC = c(BIC(model_linear), BIC(model_quad), BIC(model_cubic),
          BIC(model_sex_int), BIC(model_quad_sex)),
  R_squared = c(
    summary(model_linear)$r.squared,
    summary(model_quad)$r.squared, 
    summary(model_cubic)$r.squared,
    summary(model_sex_int)$r.squared,
    summary(model_quad_sex)$r.squared
  ),
  RMSE = c(
    sqrt(mean(residuals(model_linear)^2)),
    sqrt(mean(residuals(model_quad)^2)),
    sqrt(mean(residuals(model_cubic)^2)),
    sqrt(mean(residuals(model_sex_int)^2)),
    sqrt(mean(residuals(model_quad_sex)^2))
  )
)

write.csv(model_comparison, file.path(outputs_dir, "model_comparison.csv"), row.names = FALSE)

cat("Model comparison saved to outputs/tables/model_comparison.csv\n")
print(model_comparison)

# Sensitivity Analysis 2: Age-adjusted Models
cat("\n2. Age-Adjusted Models\n")
cat("======================\n")

# Models with age adjustment
model_age_linear <- svyglm(bodyfat_pct ~ bmi + age_centered, design = svy_design)
model_age_quad <- svyglm(bodyfat_pct ~ bmi + I(bmi^2) + age_centered, design = svy_design)
model_age_sex <- svyglm(bodyfat_pct ~ bmi * sex + age_centered, design = svy_design)

age_adj_comparison <- data.frame(
  Model = c("BMI", "BMI + Age", "BMI² + Age", "BMI × Sex + Age"),
  AIC = c(AIC(model_linear), AIC(model_age_linear), AIC(model_age_quad), AIC(model_age_sex)),
  R_squared = c(
    summary(model_linear)$r.squared,
    summary(model_age_linear)$r.squared,
    summary(model_age_quad)$r.squared,
    summary(model_age_sex)$r.squared
  )
)

write.csv(age_adj_comparison, file.path(outputs_dir, "age_adjusted_models.csv"), row.names = FALSE)

cat("Age-adjusted model comparison:\n")
print(age_adj_comparison)

# Sensitivity Analysis 3: Race/Ethnicity Adjustment
cat("\n3. Race/Ethnicity-Adjusted Models\n")
cat("==================================\n")

# Check race/ethnicity distribution
race_dist <- table(analytic$race_ethnicity, useNA = "always")
cat("Race/ethnicity distribution:\n")
print(race_dist)

# Models with race/ethnicity adjustment (if sufficient variation)
if (length(unique(analytic$race_ethnicity[!is.na(analytic$race_ethnicity)])) >= 3) {
  model_race <- svyglm(bodyfat_pct ~ bmi + race_ethnicity, design = svy_design)
  model_race_sex <- svyglm(bodyfat_pct ~ bmi * sex + race_ethnicity, design = svy_design)
  
  race_adj_comparison <- data.frame(
    Model = c("BMI", "BMI + Race", "BMI × Sex + Race"),
    AIC = c(AIC(model_linear), AIC(model_race), AIC(model_race_sex)),
    R_squared = c(
      summary(model_linear)$r.squared,
      summary(model_race)$r.squared,
      summary(model_race_sex)$r.squared
    )
  )
  
  write.csv(race_adj_comparison, file.path(outputs_dir, "race_adjusted_models.csv"), row.names = FALSE)
  cat("Race/ethnicity-adjusted models saved\n")
} else {
  cat("Insufficient race/ethnicity variation for adjustment\n")
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

# Create summary log
sensitivity_log <- file.path(logs_dir, "sensitivity_analysis.txt")
cat("NHANES BMI vs Body Fat Sensitivity Analysis\n", file = sensitivity_log)
cat("===========================================\n", file = sensitivity_log, append = TRUE)
cat("Analysis date:", as.character(Sys.Time()), "\n\n", file = sensitivity_log, append = TRUE)

cat("Files generated:\n", file = sensitivity_log, append = TRUE)
cat("- model_comparison.csv\n", file = sensitivity_log, append = TRUE)
cat("- age_adjusted_models.csv\n", file = sensitivity_log, append = TRUE)
if (file.exists(file.path(outputs_dir, "race_adjusted_models.csv"))) {
  cat("- race_adjusted_models.csv\n", file = sensitivity_log, append = TRUE)
}
if (file.exists(file.path(outputs_dir, "correlations_by_bmi_strata.csv"))) {
  cat("- correlations_by_bmi_strata.csv\n", file = sensitivity_log, append = TRUE)
}
cat("- exclusion_impact.csv\n", file = sensitivity_log, append = TRUE)
cat("- weighted_vs_unweighted.csv\n", file = sensitivity_log, append = TRUE)

cat("\n✓ Sensitivity analyses completed successfully\n")
cat("Results saved to outputs/tables/\n")
cat("Log saved to:", sensitivity_log, "\n")