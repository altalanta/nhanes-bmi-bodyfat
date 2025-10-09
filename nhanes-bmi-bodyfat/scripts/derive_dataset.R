#!/usr/bin/env Rscript

# Derive analytic dataset with exclusions and derived variables
# Creates cleaned dataset for analysis with logging

suppressPackageStartupMessages({
  library(dplyr)
  library(foreign)
  library(jsonlite)
  library(arrow)
})

# Set up paths
repo_root <- here::here()
data_raw_dir <- file.path(repo_root, "data", "raw")
data_derived_dir <- file.path(repo_root, "data", "derived")
logs_dir <- file.path(repo_root, "outputs", "logs")

dir.create(data_derived_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

# Set up logging
flow_log <- file.path(logs_dir, "flow.txt")
cat("NHANES 2017-2018 Dataset Derivation Flow\n", file = flow_log)
cat("=========================================\n", file = flow_log, append = TRUE)
cat("Start time:", as.character(Sys.time()), "\n\n", file = flow_log, append = TRUE)

log_step <- function(step, count, description) {
  msg <- sprintf("Step %s: %s (N = %d)\n", step, description, count)
  cat(msg, file = flow_log, append = TRUE)
  cat(msg)
}

cat("Loading NHANES 2017-2018 data files...\n")

# Load datasets
demo <- read.xport(file.path(data_raw_dir, "DEMO_J.XPT"))
bmx <- read.xport(file.path(data_raw_dir, "BMX_J.XPT"))
dxx <- read.xport(file.path(data_raw_dir, "DXX_J.XPT"))
dxxag <- read.xport(file.path(data_raw_dir, "DXXAG_J.XPT"))

log_step("0", nrow(demo), "Initial DEMO_J records")

# Step 1: Merge datasets
nhanes <- demo %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, RIDRETH3, WTMEC2YR, SDMVSTRA, SDMVPSU) %>%
  left_join(bmx %>% select(SEQN, BMXBMI, BMXWT, BMXHT), by = "SEQN") %>%
  left_join(dxx %>% select(SEQN, DXDTOFAT, DXDTOPF, DXXOSTAT), by = "SEQN") %>%
  left_join(dxxag %>% select(SEQN, DXAAGFAT, DXDAGFAT), by = "SEQN")

log_step("1", nrow(nhanes), "After merging all datasets")

# Step 2: Age restriction (20-59 years)
nhanes_adults <- nhanes %>%
  filter(RIDAGEYR >= 20 & RIDAGEYR <= 59)

log_step("2", nrow(nhanes_adults), "Adults aged 20-59 years")

# Step 3: Exclude pregnant women (if applicable - NHANES codes this in DEMO)
# Note: RIDEXPRG variable may not be in DEMO_J for this cycle
if ("RIDEXPRG" %in% names(nhanes_adults)) {
  nhanes_nonpreg <- nhanes_adults %>%
    filter(is.na(RIDEXPRG) | RIDEXPRG != 1)  # Exclude pregnant
  log_step("3a", nrow(nhanes_nonpreg), "After excluding pregnant women")
} else {
  nhanes_nonpreg <- nhanes_adults
  log_step("3a", nrow(nhanes_nonpreg), "No pregnancy variable found, skipping exclusion")
}

# Step 4: Valid survey design variables
nhanes_survey <- nhanes_nonpreg %>%
  filter(!is.na(WTMEC2YR) & WTMEC2YR > 0 &
         !is.na(SDMVSTRA) & !is.na(SDMVPSU))

log_step("4", nrow(nhanes_survey), "With valid survey design variables")

# Step 5: Valid BMI
nhanes_bmi <- nhanes_survey %>%
  filter(!is.na(BMXBMI) & BMXBMI > 0 & BMXBMI < 100)  # Reasonable BMI range

log_step("5", nrow(nhanes_bmi), "With valid BMI")

# Step 6: Valid DXA body fat and scan status
nhanes_dxa <- nhanes_bmi %>%
  filter(!is.na(DXDTOFAT) & DXDTOFAT > 0 & DXDTOFAT < 100 &  # Valid % body fat
         (is.na(DXXOSTAT) | DXXOSTAT == 1))  # Complete/valid DXA scan

log_step("6", nrow(nhanes_dxa), "With valid DXA body fat measures")

# Step 7: Create derived variables
nhanes_final <- nhanes_dxa %>%
  mutate(
    # Demographics
    age_group = case_when(
      RIDAGEYR >= 20 & RIDAGEYR <= 39 ~ "20-39",
      RIDAGEYR >= 40 & RIDAGEYR <= 59 ~ "40-59"
    ),
    sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
    race_ethnicity = case_when(
      RIDRETH3 == 1 ~ "Mexican American",
      RIDRETH3 == 2 ~ "Other Hispanic", 
      RIDRETH3 == 3 ~ "Non-Hispanic White",
      RIDRETH3 == 4 ~ "Non-Hispanic Black",
      RIDRETH3 == 6 ~ "Non-Hispanic Asian",
      RIDRETH3 == 7 ~ "Other Race/Multi-racial"
    ),
    
    # BMI categories (WHO/CDC)
    bmi_cat = case_when(
      BMXBMI < 18.5 ~ "Underweight",
      BMXBMI >= 18.5 & BMXBMI < 25 ~ "Normal",
      BMXBMI >= 25 & BMXBMI < 30 ~ "Overweight",
      BMXBMI >= 30 & BMXBMI < 35 ~ "Obesity I",
      BMXBMI >= 35 & BMXBMI < 40 ~ "Obesity II", 
      BMXBMI >= 40 ~ "Obesity III"
    ),
    bmi_cat = factor(bmi_cat, levels = c("Underweight", "Normal", "Overweight",
                                         "Obesity I", "Obesity II", "Obesity III")),
    
    # Body fat percentage (use total body fat)
    bodyfat_pct = DXDTOFAT,
    
    # Derived indicators
    obesity = BMXBMI >= 30,
    high_bodyfat = case_when(
      sex == "Male" & bodyfat_pct >= 25 ~ TRUE,
      sex == "Female" & bodyfat_pct >= 32 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Continuous age for modeling
    age_centered = RIDAGEYR - 40  # Center at age 40
  ) %>%
  
  # Rename variables for clarity
  rename(
    seqn = SEQN,
    age = RIDAGEYR,
    bmi = BMXBMI,
    weight_kg = BMXWT,
    height_cm = BMXHT,
    survey_weight = WTMEC2YR,
    strata = SDMVSTRA,
    psu = SDMVPSU
  ) %>%
  
  # Select final variables
  select(
    # Identifiers
    seqn,
    
    # Demographics  
    age, age_group, age_centered, sex, race_ethnicity,
    
    # Anthropometrics
    bmi, bmi_cat, weight_kg, height_cm, obesity,
    
    # Body composition
    bodyfat_pct, high_bodyfat,
    
    # Survey design
    survey_weight, strata, psu
  )

log_step("7", nrow(nhanes_final), "Final analytic dataset")

# Calculate exclusion counts
initial_n <- nrow(demo)
final_n <- nrow(nhanes_final)
excluded_n <- initial_n - final_n

cat("\n", file = flow_log, append = TRUE)
cat("EXCLUSION SUMMARY\n", file = flow_log, append = TRUE)
cat("=================\n", file = flow_log, append = TRUE)
cat(sprintf("Initial records: %d\n", initial_n), file = flow_log, append = TRUE)
cat(sprintf("Final analytic sample: %d\n", final_n), file = flow_log, append = TRUE)
cat(sprintf("Total excluded: %d (%.1f%%)\n", excluded_n, 100 * excluded_n / initial_n), file = flow_log, append = TRUE)

# Save analytic dataset in multiple formats
analytic_rds <- file.path(data_derived_dir, "analytic.rds")
analytic_parquet <- file.path(data_derived_dir, "analytic.parquet")
analytic_csv <- file.path(data_derived_dir, "analytic.csv")

saveRDS(nhanes_final, analytic_rds)
write_parquet(nhanes_final, analytic_parquet)
write.csv(nhanes_final, analytic_csv, row.names = FALSE)

cat(sprintf("\nAnalytic dataset saved:\n"))
cat(sprintf("- RDS: %s\n", analytic_rds))
cat(sprintf("- Parquet: %s\n", analytic_parquet))
cat(sprintf("- CSV: %s\n", analytic_csv))

# Summary statistics
cat("\n", file = flow_log, append = TRUE)
cat("ANALYTIC DATASET SUMMARY\n", file = flow_log, append = TRUE)
cat("========================\n", file = flow_log, append = TRUE)

summary_stats <- nhanes_final %>%
  summarise(
    n = n(),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    bmi_mean = mean(bmi, na.rm = TRUE),
    bmi_sd = sd(bmi, na.rm = TRUE),
    bodyfat_mean = mean(bodyfat_pct, na.rm = TRUE),
    bodyfat_sd = sd(bodyfat_pct, na.rm = TRUE),
    pct_female = mean(sex == "Female") * 100,
    pct_obesity = mean(obesity) * 100
  )

for (var in names(summary_stats)) {
  cat(sprintf("%s: %.2f\n", var, summary_stats[[var]]), file = flow_log, append = TRUE)
}

# By sex summary
cat("\n", file = flow_log, append = TRUE)
cat("BY SEX\n", file = flow_log, append = TRUE)
cat("------\n", file = flow_log, append = TRUE)

sex_summary <- nhanes_final %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    bmi_mean = mean(bmi, na.rm = TRUE),
    bodyfat_mean = mean(bodyfat_pct, na.rm = TRUE),
    .groups = "drop"
  )

for (i in 1:nrow(sex_summary)) {
  cat(sprintf("%s: n=%d, BMI=%.1f, BodyFat=%.1f\n", 
              sex_summary$sex[i], sex_summary$n[i], 
              sex_summary$bmi_mean[i], sex_summary$bodyfat_mean[i]), 
      file = flow_log, append = TRUE)
}

cat(sprintf("\nEnd time: %s\n", Sys.time()), file = flow_log, append = TRUE)

cat("\n✓ Dataset derivation completed successfully\n")
cat(sprintf("Final sample size: %d participants\n", nrow(nhanes_final)))
cat(sprintf("Flow log: %s\n", flow_log))