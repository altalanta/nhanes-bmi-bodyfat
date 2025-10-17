#!/usr/bin/env Rscript

# Derive analytic dataset with comprehensive validation and derived variables
# Creates cleaned dataset for analysis with enhanced logging and validation

suppressPackageStartupMessages({
  library(dplyr)
  library(foreign)
  library(jsonlite)
  library(arrow)
  library(here)
})

# Source validation utilities
source(file.path(here::here(), "R", "data_validation.R"))
source(file.path(here::here(), "R", "performance.R"))

# Set up paths
repo_root <- here::here()
data_raw_dir <- file.path(repo_root, "data", "raw")
data_derived_dir <- file.path(repo_root, "data", "derived")
logs_dir <- file.path(repo_root, "outputs", "logs")

dir.create(data_derived_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

# Initialize performance tracking
initialize_performance_tracking("nhanes_dataset_derivation")

# Set up enhanced logging
flow_log <- file.path(logs_dir, "derivation_flow.txt")
validation_log <- file.path(logs_dir, "validation_log.txt")

cat("NHANES 2017-2018 Dataset Derivation with Comprehensive Validation\n", file = flow_log)
cat("================================================================\n", file = flow_log, append = TRUE)
cat("Start time:", as.character(Sys.time()), "\n\n", file = flow_log, append = TRUE)

# Enhanced logging function with validation details
log_step <- function(step, count, description, validation_details = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  msg <- sprintf("[%s] Step %s: %s (N = %d)", timestamp, step, description, count)

  # Add validation details if provided
  if (!is.null(validation_details)) {
    msg <- paste0(msg, " | Validation: ", validation_details)
  }
  msg <- paste0(msg, "\n")

  cat(msg, file = flow_log, append = TRUE)
  cat(msg)
}

# Validation logging function
log_validation <- function(level, message, details = NULL) {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  log_msg <- sprintf("[%s] %s: %s", timestamp, level, message)

  if (!is.null(details)) {
    log_msg <- paste0(log_msg, " | Details: ", paste(details, collapse = ", "))
  }
  log_msg <- paste0(log_msg, "\n")

  cat(log_msg, file = validation_log, append = TRUE)

  # Also log to main flow log for critical issues
  if (level %in% c("ERROR", "CRITICAL")) {
    cat(log_msg, file = flow_log, append = TRUE)
  }
}

# Enhanced data loading with validation
load_and_validate_dataset <- function(dataset_name, filepath, required_vars = NULL) {
  cat("Loading and validating:", dataset_name, "...\n")

  tryCatch({
    # Validate file exists and is readable
    if (!file.exists(filepath)) {
      stop(NhanesError(
        paste("Dataset file not found:", filepath),
        code = "DATASET_FILE_NOT_FOUND",
        details = list(dataset = dataset_name, filepath = filepath)
      ))
    }

    # Load the dataset
    data <- read.xport(filepath)

    if (is.null(data) || nrow(data) == 0) {
      stop(NhanesError(
        paste("Dataset is empty or invalid:", dataset_name),
        code = "EMPTY_DATASET",
        details = list(dataset = dataset_name, filepath = filepath)
      ))
    }

    # Validate required variables if specified
    if (!is.null(required_vars)) {
      missing_vars <- setdiff(required_vars, names(data))
      if (length(missing_vars) > 0) {
        warning(NhanesWarning(
          paste("Missing expected variables in", dataset_name, ":",
                paste(missing_vars, collapse = ", ")),
          code = "MISSING_VARIABLES",
          details = list(dataset = dataset_name, missing_vars = missing_vars)
        ))
      }
    }

    # Basic structure validation
    validation_result <- list(
      dataset = dataset_name,
      n_rows = nrow(data),
      n_cols = ncol(data),
      file_size = file.size(filepath),
      has_seqn = "SEQN" %in% names(data),
      missing_vars = if (!is.null(required_vars)) setdiff(required_vars, names(data)) else NULL
    )

    log_validation("INFO", paste("Successfully loaded", dataset_name),
                   c(paste(validation_result$n_rows, "rows"), paste(validation_result$n_cols, "columns")))

    return(list(data = data, validation = validation_result))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      log_validation("ERROR", paste("Failed to load", dataset_name), e$message)
      stop(e)
    } else {
      log_validation("ERROR", paste("Unexpected error loading", dataset_name), e$message)
      stop(NhanesError(
        paste("Dataset loading failed:", e$message),
        code = "DATASET_LOAD_ERROR",
        details = list(dataset = dataset_name, filepath = filepath, original_error = e$message)
      ))
    }
  })
}

cat("Loading and validating NHANES 2017-2018 data files with comprehensive validation...\n")

# Load and validate datasets with enhanced validation
datasets_validation <- list()

# DEMO_J dataset
demo_result <- benchmark_operation(
  load_and_validate_dataset(
    "DEMO_J",
    file.path(data_raw_dir, "DEMO_J.XPT"),
    required_vars = c("SEQN", "RIDAGEYR", "RIAGENDR", "WTMEC2YR", "SDMVSTRA", "SDMVPSU")
  ),
  "demo_loading",
  metadata = list(dataset = "DEMO_J", expected_vars = 6)
)
demo <- demo_result$data
datasets_validation$demo <- demo_result$validation
log_step("0", nrow(demo), "Initial DEMO_J records", paste("validated, size:", round(datasets_validation$demo$file_size / 1024, 1), "KB"))

# BMX_J dataset
bmx_result <- benchmark_operation(
  load_and_validate_dataset(
    "BMX_J",
    file.path(data_raw_dir, "BMX_J.XPT"),
    required_vars = c("SEQN", "BMXBMI")
  ),
  "bmx_loading",
  metadata = list(dataset = "BMX_J", expected_vars = 2)
)
bmx <- bmx_result$data
datasets_validation$bmx <- bmx_result$validation

# DXX_J dataset
dxx_result <- benchmark_operation(
  load_and_validate_dataset(
    "DXX_J",
    file.path(data_raw_dir, "DXX_J.XPT"),
    required_vars = c("SEQN")
  ),
  "dxx_loading",
  metadata = list(dataset = "DXX_J", expected_vars = 1)
)
dxx <- dxx_result$data
datasets_validation$dxx <- dxx_result$validation

# DXXAG_J dataset
dxxag_result <- benchmark_operation(
  load_and_validate_dataset(
    "DXXAG_J",
    file.path(data_raw_dir, "DXXAG_J.XPT"),
    required_vars = c("SEQN")
  ),
  "dxxag_loading",
  metadata = list(dataset = "DXXAG_J", expected_vars = 1)
)
dxxag <- dxxag_result$data
datasets_validation$dxxag <- dxxag_result$validation

# Enhanced dataset merging with validation
merge_datasets_with_validation <- function(demo, bmx, dxx, dxxag) {
  cat("Merging datasets with validation...\n")

  tryCatch({
    # Start with demographics as base
    nhanes <- demo %>%
      select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, RIDRETH3, WTMEC2YR, SDMVSTRA, SDMVPSU)

    # Validate base dataset has SEQN
    if (!"SEQN" %in% names(nhanes)) {
      stop(NhanesError(
        "Base dataset missing SEQN column",
        code = "MERGE_BASE_MISSING_SEQN",
        details = list(base_dataset = "demo", columns = names(nhanes))
      ))
    }

    # Merge BMX data
    if ("BMXBMI" %in% names(bmx)) {
      nhanes <- nhanes %>%
        left_join(bmx %>% select(SEQN, BMXBMI, BMXWT, BMXHT), by = "SEQN")

      # Validate merge didn't lose records unexpectedly
      original_count <- nrow(nhanes)
      bmx_joined <- sum(!is.na(nhanes$BMXBMI))
      log_validation("INFO", "BMX merge completed",
                     c(paste("BMI records added:", bmx_joined), paste("total records:", original_count)))
    }

    # Merge DXX data
    dxx_vars <- intersect(c("DXDTOFAT", "DXDTOPF", "DXXOSTAT"), names(dxx))
    if (length(dxx_vars) > 0) {
      nhanes <- nhanes %>%
        left_join(dxx %>% select_at(c("SEQN", dxx_vars)), by = "SEQN")

      dxx_joined <- sum(!is.na(nhanes[[dxx_vars[1]]]))
      log_validation("INFO", "DXX merge completed",
                     c(paste("DXX records added:", dxx_joined), paste("variables:", paste(dxx_vars, collapse = ","))))
    }

    # Merge DXXAG data
    dxxag_vars <- intersect(c("DXAAGFAT", "DXDAGFAT"), names(dxxag))
    if (length(dxxag_vars) > 0) {
      nhanes <- nhanes %>%
        left_join(dxxag %>% select_at(c("SEQN", dxxag_vars)), by = "SEQN")

      dxxag_joined <- sum(!is.na(nhanes[[dxxag_vars[1]]]))
      log_validation("INFO", "DXXAG merge completed",
                     c(paste("DXXAG records added:", dxxag_joined), paste("variables:", paste(dxxag_vars, collapse = ","))))
    }

    # Validate merged dataset
    final_count <- nrow(nhanes)

    if (final_count == 0) {
      stop(NhanesError(
        "Merged dataset is empty",
        code = "EMPTY_MERGED_DATASET",
        details = list(original_demo = nrow(demo), final_count = final_count)
      ))
    }

    # Check for data loss
    if (final_count < nrow(demo) * 0.5) {
      warning(NhanesWarning(
        paste("Significant data loss during merge:", round((1 - final_count/nrow(demo)) * 100, 1), "% loss"),
        code = "SIGNIFICANT_DATA_LOSS",
        details = list(original = nrow(demo), final = final_count, loss_pct = (1 - final_count/nrow(demo)) * 100)
      ))
    }

    return(nhanes)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      log_validation("ERROR", "Dataset merge failed", e$message)
      stop(e)
    } else {
      log_validation("ERROR", "Unexpected merge error", e$message)
      stop(NhanesError(
        paste("Dataset merge failed:", e$message),
        code = "MERGE_ERROR",
        details = list(original_error = e$message)
      ))
    }
  })
}

# Step 1: Merge datasets with validation
nhanes <- benchmark_operation(
  merge_datasets_with_validation(demo, bmx, dxx, dxxag),
  "dataset_merging",
  metadata = list(
    demo_rows = nrow(demo),
    bmx_rows = nrow(bmx),
    dxx_rows = nrow(dxx),
    dxxag_rows = nrow(dxxag)
  )
)

log_step("1", nrow(nhanes), "After merging all datasets with validation")

# Enhanced age restriction with validation
apply_age_restriction <- function(data, age_range = c(20, 59)) {
  cat("Applying age restriction with validation...\n")

  tryCatch({
    if (!"RIDAGEYR" %in% names(data)) {
      stop(NhanesError(
        "Age variable RIDAGEYR not found in dataset",
        code = "MISSING_AGE_VARIABLE",
        details = list(columns = names(data), required = "RIDAGEYR")
      ))
    }

    original_count <- nrow(data)

    # Apply age restriction
    nhanes_adults <- data %>%
      filter(RIDAGEYR >= age_range[1] & RIDAGEYR <= age_range[2])

    final_count <- nrow(nhanes_adults)
    excluded_count <- original_count - final_count

    # Validate age restriction results
    if (final_count == 0) {
      stop(NhanesError(
        paste("No records remain after age restriction to", age_range[1], "-", age_range[2], "years"),
        code = "NO_RECORDS_AFTER_AGE_RESTRICTION",
        details = list(original = original_count, final = final_count, age_range = age_range)
      ))
    }

    # Check for reasonable exclusion rate
    exclusion_rate <- excluded_count / original_count
    if (exclusion_rate > 0.8) {
      warning(NhanesWarning(
        paste("High exclusion rate after age restriction:", round(exclusion_rate * 100, 1), "%"),
        code = "HIGH_EXCLUSION_RATE",
        details = list(exclusion_rate = exclusion_rate * 100, original = original_count, final = final_count)
      ))
    }

    log_validation("INFO", "Age restriction applied",
                   c(paste("age range:", paste(age_range, collapse = "-")),
                     paste("excluded:", excluded_count),
                     paste("remaining:", final_count)))

    return(nhanes_adults)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      log_validation("ERROR", "Age restriction failed", e$message)
      stop(e)
    } else {
      log_validation("ERROR", "Unexpected age restriction error", e$message)
      stop(NhanesError(
        paste("Age restriction failed:", e$message),
        code = "AGE_RESTRICTION_ERROR",
        details = list(original_error = e$message)
      ))
    }
  })
}

# Step 2: Age restriction with validation
nhanes_adults <- benchmark_operation(
  apply_age_restriction(nhanes, c(20, 59)),
  "age_restriction",
  metadata = list(original_count = nrow(nhanes), age_range = "20-59")
)

log_step("2", nrow(nhanes_adults), "Adults aged 20-59 years with validation")

# Step 3: Enhanced pregnancy exclusion with validation
apply_pregnancy_exclusion <- function(data) {
  cat("Checking pregnancy exclusion with validation...\n")

  tryCatch({
    original_count <- nrow(data)

    # Check if pregnancy variable exists
    if ("RIDEXPRG" %in% names(data)) {
      nhanes_nonpreg <- data %>%
        filter(is.na(RIDEXPRG) | RIDEXPRG != 1)  # Exclude pregnant

      excluded_count <- original_count - nrow(nhanes_nonpreg)

      log_validation("INFO", "Pregnancy exclusion applied",
                     c(paste("pregnant excluded:", excluded_count),
                       paste("remaining:", nrow(nhanes_nonpreg))))

      return(nhanes_nonpreg)
    } else {
      log_validation("INFO", "No pregnancy variable found, skipping exclusion", "RIDEXPRG not in dataset")
      return(data)
    }

  }, error = function(e) {
    log_validation("ERROR", "Pregnancy exclusion failed", e$message)
    stop(NhanesError(
      paste("Pregnancy exclusion failed:", e$message),
      code = "PREGNANCY_EXCLUSION_ERROR",
      details = list(original_error = e$message)
    ))
  })
}

nhanes_nonpreg <- benchmark_operation(
  apply_pregnancy_exclusion(nhanes_adults),
  "pregnancy_exclusion",
  metadata = list(has_pregnancy_var = "RIDEXPRG" %in% names(nhanes_adults))
)

if ("RIDEXPRG" %in% names(nhanes_adults)) {
  log_step("3a", nrow(nhanes_nonpreg), "After excluding pregnant women with validation")
} else {
  log_step("3a", nrow(nhanes_nonpreg), "No pregnancy variable found, skipping exclusion")
}

# Enhanced survey design variable validation
apply_survey_design_validation <- function(data) {
  cat("Validating survey design variables...\n")

  tryCatch({
    required_vars <- c("WTMEC2YR", "SDMVSTRA", "SDMVPSU")
    missing_vars <- setdiff(required_vars, names(data))

    if (length(missing_vars) > 0) {
      stop(NhanesError(
        paste("Missing survey design variables:", paste(missing_vars, collapse = ", ")),
        code = "MISSING_SURVEY_VARS",
        details = list(missing = missing_vars, available = setdiff(names(data), missing_vars))
      ))
    }

    original_count <- nrow(data)

    # Apply survey design validation
    nhanes_survey <- data %>%
      filter(!is.na(WTMEC2YR) & WTMEC2YR > 0 &
             !is.na(SDMVSTRA) & !is.na(SDMVPSU))

    final_count <- nrow(nhanes_survey)
    excluded_count <- original_count - final_count

    # Validate survey design results
    if (final_count == 0) {
      stop(NhanesError(
        "No records remain after survey design validation",
        code = "NO_RECORDS_AFTER_SURVEY_VALIDATION",
        details = list(original = original_count, final = final_count)
      ))
    }

    # Check for reasonable exclusion rate
    exclusion_rate <- excluded_count / original_count
    if (exclusion_rate > 0.5) {
      warning(NhanesWarning(
        paste("High exclusion rate for survey design variables:", round(exclusion_rate * 100, 1), "%"),
        code = "HIGH_SURVEY_EXCLUSION_RATE",
        details = list(exclusion_rate = exclusion_rate * 100, original = original_count, final = final_count)
      ))
    }

    # Validate survey design variable distributions
    weight_summary <- summary(nhanes_survey$WTMEC2YR)
    strata_summary <- length(unique(nhanes_survey$SDMVSTRA))
    psu_summary <- length(unique(nhanes_survey$SDMVPSU))

    log_validation("INFO", "Survey design validation completed",
                   c(paste("weights valid:", sum(!is.na(nhanes_survey$WTMEC2YR))),
                     paste("strata count:", strata_summary),
                     paste("PSU count:", psu_summary),
                     paste("excluded:", excluded_count)))

    return(nhanes_survey)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      log_validation("ERROR", "Survey design validation failed", e$message)
      stop(e)
    } else {
      log_validation("ERROR", "Unexpected survey validation error", e$message)
      stop(NhanesError(
        paste("Survey design validation failed:", e$message),
        code = "SURVEY_VALIDATION_ERROR",
        details = list(original_error = e$message)
      ))
    }
  })
}

# Step 4: Survey design validation
nhanes_survey <- benchmark_operation(
  apply_survey_design_validation(nhanes_nonpreg),
  "survey_design_validation",
  metadata = list(
    original_count = nrow(nhanes_nonpreg),
    required_vars = c("WTMEC2YR", "SDMVSTRA", "SDMVPSU")
  )
)

log_step("4", nrow(nhanes_survey), "Valid survey design variables with validation")

# Enhanced BMI validation
apply_bmi_validation <- function(data, bmi_range = c(10, 80)) {
  cat("Validating BMI values...\n")

  tryCatch({
    if (!"BMXBMI" %in% names(data)) {
      stop(NhanesError(
        "BMI variable BMXBMI not found in dataset",
        code = "MISSING_BMI_VARIABLE",
        details = list(columns = names(data), required = "BMXBMI")
      ))
    }

    original_count <- nrow(data)

    # Apply BMI validation
    nhanes_bmi <- data %>%
      filter(!is.na(BMXBMI) & BMXBMI >= bmi_range[1] & BMXBMI <= bmi_range[2])

    final_count <- nrow(nhanes_bmi)
    excluded_count <- original_count - final_count

    # Validate BMI results
    if (final_count == 0) {
      stop(NhanesError(
        paste("No records remain after BMI validation (range:", bmi_range[1], "-", bmi_range[2], ")"),
        code = "NO_RECORDS_AFTER_BMI_VALIDATION",
        details = list(original = original_count, final = final_count, bmi_range = bmi_range)
      ))
    }

    # Check for reasonable exclusion rate
    exclusion_rate <- excluded_count / original_count
    if (exclusion_rate > 0.3) {
      warning(NhanesWarning(
        paste("High exclusion rate for BMI validation:", round(exclusion_rate * 100, 1), "%"),
        code = "HIGH_BMI_EXCLUSION_RATE",
        details = list(exclusion_rate = exclusion_rate * 100, original = original_count, final = final_count)
      ))
    }

    # Validate BMI distribution
    bmi_summary <- summary(nhanes_bmi$BMXBMI)

    log_validation("INFO", "BMI validation completed",
                   c(paste("BMI range:", paste(bmi_range, collapse = "-")),
                     paste("excluded:", excluded_count),
                     paste("remaining:", final_count),
                     paste("mean BMI:", round(bmi_summary["Mean"], 2))))

    return(nhanes_bmi)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      log_validation("ERROR", "BMI validation failed", e$message)
      stop(e)
    } else {
      log_validation("ERROR", "Unexpected BMI validation error", e$message)
      stop(NhanesError(
        paste("BMI validation failed:", e$message),
        code = "BMI_VALIDATION_ERROR",
        details = list(original_error = e$message)
      ))
    }
  })
}

# Step 5: BMI validation
nhanes_bmi <- benchmark_operation(
  apply_bmi_validation(nhanes_survey, c(10, 80)),
  "bmi_validation",
  metadata = list(
    original_count = nrow(nhanes_survey),
    bmi_range = "10-80"
  )
)

log_step("5", nrow(nhanes_bmi), "With valid BMI and validation")

# Enhanced DXA validation
apply_dxa_validation <- function(data) {
  cat("Validating DXA body fat measures...\n")

  tryCatch({
    # Check for DXA variables
    dxa_vars <- intersect(c("DXDTOFAT", "DXDTOPF", "DXXOSTAT"), names(data))
    if (length(dxa_vars) == 0) {
      stop(NhanesError(
        "No DXA body fat variables found in dataset",
        code = "MISSING_DXA_VARIABLES",
        details = list(available = names(data), expected = c("DXDTOFAT", "DXDTOPF", "DXXOSTAT"))
      ))
    }

    original_count <- nrow(data)

    # Determine which body fat variable to use
    bodyfat_var <- if ("DXDTOFAT" %in% dxa_vars) "DXDTOFAT" else "DXDTOPF"

    # Apply DXA validation
    nhanes_dxa <- data %>%
      filter(!is.na(!!sym(bodyfat_var)) &
             !!sym(bodyfat_var) > 0 &
             !!sym(bodyfat_var) < 100 &
             (is.na(DXXOSTAT) | DXXOSTAT == 1))

    final_count <- nrow(nhanes_dxa)
    excluded_count <- original_count - final_count

    # Validate DXA results
    if (final_count == 0) {
      stop(NhanesError(
        paste("No records remain after DXA validation using", bodyfat_var),
        code = "NO_RECORDS_AFTER_DXA_VALIDATION",
        details = list(original = original_count, final = final_count, bodyfat_var = bodyfat_var)
      ))
    }

    # Check for reasonable exclusion rate
    exclusion_rate <- excluded_count / original_count
    if (exclusion_rate > 0.5) {
      warning(NhanesWarning(
        paste("High exclusion rate for DXA validation:", round(exclusion_rate * 100, 1), "%"),
        code = "HIGH_DXA_EXCLUSION_RATE",
        details = list(exclusion_rate = exclusion_rate * 100, original = original_count, final = final_count)
      ))
    }

    log_validation("INFO", "DXA validation completed",
                   c(paste("body fat var:", bodyfat_var),
                     paste("excluded:", excluded_count),
                     paste("remaining:", final_count)))

    return(nhanes_dxa)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      log_validation("ERROR", "DXA validation failed", e$message)
      stop(e)
    } else {
      log_validation("ERROR", "Unexpected DXA validation error", e$message)
      stop(NhanesError(
        paste("DXA validation failed:", e$message),
        code = "DXA_VALIDATION_ERROR",
        details = list(original_error = e$message)
      ))
    }
  })
}

# Step 6: DXA validation
nhanes_dxa <- benchmark_operation(
  apply_dxa_validation(nhanes_bmi),
  "dxa_validation",
  metadata = list(
    original_count = nrow(nhanes_bmi),
    expected_vars = c("DXDTOFAT", "DXDTOPF", "DXXOSTAT")
  )
)

log_step("6", nrow(nhanes_dxa), "With valid DXA body fat measures and validation")

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

# Final dataset validation
validate_final_dataset <- function(data, datasets_validation) {
  cat("Performing comprehensive final dataset validation...\n")

  tryCatch({
    # Basic structure validation
    if (nrow(data) == 0) {
      stop(NhanesError(
        "Final dataset is empty",
        code = "EMPTY_FINAL_DATASET",
        details = list(n_rows = nrow(data), n_cols = ncol(data))
      ))
    }

    # Validate required columns exist
    required_cols <- c("seqn", "age", "sex", "bmi", "bodyfat_pct", "survey_weight", "strata", "psu")
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
      stop(NhanesError(
        paste("Final dataset missing required columns:", paste(missing_cols, collapse = ", ")),
        code = "MISSING_FINAL_COLUMNS",
        details = list(missing = missing_cols, available = names(data))
      ))
    }

    # Validate data types
    if (!is.numeric(data$seqn)) {
      warning(NhanesWarning(
        "SEQN should be numeric",
        code = "INVALID_SEQN_TYPE",
        details = list(actual_type = class(data$seqn))
      ))
    }

    if (!is.numeric(data$bmi) || !is.numeric(data$bodyfat_pct)) {
      stop(NhanesError(
        "BMI and body fat percentage must be numeric",
        code = "INVALID_NUMERIC_VARS",
        details = list(
          bmi_class = class(data$bmi),
          bodyfat_class = class(data$bodyfat_pct)
        )
      ))
    }

    # Validate ranges
    if (any(data$bmi <= 0 | data$bmi > 100, na.rm = TRUE)) {
      warning(NhanesWarning(
        "BMI values outside expected range (0-100)",
        code = "BMI_OUT_OF_RANGE",
        details = list(
          min_bmi = min(data$bmi, na.rm = TRUE),
          max_bmi = max(data$bmi, na.rm = TRUE)
        )
      ))
    }

    if (any(data$bodyfat_pct <= 0 | data$bodyfat_pct > 100, na.rm = TRUE)) {
      warning(NhanesWarning(
        "Body fat percentage outside expected range (0-100)",
        code = "BODYFAT_OUT_OF_RANGE",
        details = list(
          min_bodyfat = min(data$bodyfat_pct, na.rm = TRUE),
          max_bodyfat = max(data$bodyfat_pct, na.rm = TRUE)
        )
      ))
    }

    # Validate survey design variables
    if (any(data$survey_weight <= 0, na.rm = TRUE)) {
      stop(NhanesError(
        "Invalid survey weights (≤0) in final dataset",
        code = "INVALID_SURVEY_WEIGHTS",
        details = list(
          min_weight = min(data$survey_weight, na.rm = TRUE),
          negative_weights = sum(data$survey_weight <= 0, na.rm = TRUE)
        )
      ))
    }

    # Validate categorical variables
    if (!all(data$sex %in% c("Male", "Female"))) {
      stop(NhanesError(
        "Invalid sex categories in final dataset",
        code = "INVALID_SEX_CATEGORIES",
        details = list(unique_sex = unique(data$sex))
      ))
    }

    # Generate comprehensive validation report
    validation_report <- list(
      timestamp = Sys.time(),
      n_rows = nrow(data),
      n_cols = ncol(data),
      column_validation = list(
        required_present = length(missing_cols) == 0,
        seqn_numeric = is.numeric(data$seqn),
        bmi_numeric = is.numeric(data$bmi),
        bodyfat_numeric = is.numeric(data$bodyfat_pct),
        survey_weight_positive = all(data$survey_weight > 0, na.rm = TRUE)
      ),
      range_validation = list(
        bmi_in_range = all(data$bmi >= 10 & data$bmi <= 80, na.rm = TRUE),
        bodyfat_in_range = all(data$bodyfat_pct >= 5 & data$bodyfat_pct <= 50, na.rm = TRUE),
        age_in_range = all(data$age >= 20 & data$age <= 59, na.rm = TRUE)
      ),
      distribution_summary = list(
        mean_bmi = mean(data$bmi, na.rm = TRUE),
        sd_bmi = sd(data$bmi, na.rm = TRUE),
        mean_bodyfat = mean(data$bodyfat_pct, na.rm = TRUE),
        sd_bodyfat = sd(data$bodyfat_pct, na.rm = TRUE),
        pct_female = mean(data$sex == "Female", na.rm = TRUE) * 100,
        pct_obese = mean(data$bmi >= 30, na.rm = TRUE) * 100
      )
    )

    log_validation("INFO", "Final dataset validation completed",
                   c(paste("rows:", nrow(data)),
                     paste("columns:", ncol(data)),
                     paste("valid structure:", validation_report$column_validation$required_present)))

    return(validation_report)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      log_validation("ERROR", "Final dataset validation failed", e$message)
      stop(e)
    } else {
      log_validation("ERROR", "Unexpected final validation error", e$message)
      stop(NhanesError(
        paste("Final dataset validation failed:", e$message),
        code = "FINAL_VALIDATION_ERROR",
        details = list(original_error = e$message)
      ))
    }
  })
}

# Step 7: Final dataset creation with validation
nhanes_final <- benchmark_operation({
  nhanes_dxa %>%
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
}, "final_dataset_creation", metadata = list(original_rows = nrow(nhanes_dxa)))

log_step("7", nrow(nhanes_final), "Final analytic dataset with derived variables")

# Validate final dataset
final_validation <- benchmark_operation(
  validate_final_dataset(nhanes_final, datasets_validation),
  "final_dataset_validation",
  metadata = list(n_rows = nrow(nhanes_final), n_cols = ncol(nhanes_final))
)

# Save validation report
validation_report_file <- file.path(logs_dir, "final_dataset_validation.json")
write_json(final_validation, validation_report_file, pretty = TRUE, auto_unbox = TRUE)

# Calculate exclusion counts with enhanced tracking
initial_n <- nrow(demo)
final_n <- nrow(nhanes_final)
excluded_n <- initial_n - final_n

cat("\n", file = flow_log, append = TRUE)
cat("COMPREHENSIVE EXCLUSION SUMMARY\n", file = flow_log, append = TRUE)
cat("==============================\n", file = flow_log, append = TRUE)
cat(sprintf("Initial records: %d\n", initial_n), file = flow_log, append = TRUE)
cat(sprintf("Final analytic sample: %d\n", final_n), file = flow_log, append = TRUE)
cat(sprintf("Total excluded: %d (%.1f%%)\n", excluded_n, 100 * excluded_n / initial_n), file = flow_log, append = TRUE)

# Add validation summary to log
cat("\n", file = flow_log, append = TRUE)
cat("VALIDATION SUMMARY\n", file = flow_log, append = TRUE)
cat("=================\n", file = flow_log, append = TRUE)
cat(sprintf("Final dataset validation: %s\n", if (final_validation$column_validation$required_present) "PASSED" else "FAILED"), file = flow_log, append = TRUE)
cat(sprintf("BMI range validation: %s\n", if (final_validation$range_validation$bmi_in_range) "PASSED" else "WARNING"), file = flow_log, append = TRUE)
cat(sprintf("Body fat range validation: %s\n", if (final_validation$range_validation$bodyfat_in_range) "PASSED" else "WARNING"), file = flow_log, append = TRUE)

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

# Enhanced summary statistics with validation details
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

# Log summary statistics
for (var in names(summary_stats)) {
  cat(sprintf("%s: %.2f\n", var, summary_stats[[var]]), file = flow_log, append = TRUE)
}

# By sex summary with validation
cat("\n", file = flow_log, append = TRUE)
cat("BY SEX SUMMARY\n", file = flow_log, append = TRUE)
cat("==============\n", file = flow_log, append = TRUE)

sex_summary <- nhanes_final %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    bmi_mean = mean(bmi, na.rm = TRUE),
    bmi_sd = sd(bmi, na.rm = TRUE),
    bodyfat_mean = mean(bodyfat_pct, na.rm = TRUE),
    bodyfat_sd = sd(bodyfat_pct, na.rm = TRUE),
    pct_obesity = mean(obesity) * 100,
    .groups = "drop"
  )

for (i in 1:nrow(sex_summary)) {
  cat(sprintf("%s: n=%d, BMI=%.1f±%.1f, BodyFat=%.1f±%.1f, Obesity=%.1f%%\n",
              sex_summary$sex[i], sex_summary$n[i],
              sex_summary$bmi_mean[i], sex_summary$bmi_sd[i],
              sex_summary$bodyfat_mean[i], sex_summary$bodyfat_sd[i],
              sex_summary$pct_obesity[i]),
      file = flow_log, append = TRUE)
}

# BMI category distribution
cat("\n", file = flow_log, append = TRUE)
cat("BMI CATEGORY DISTRIBUTION\n", file = flow_log, append = TRUE)
cat("========================\n", file = flow_log, append = TRUE)

bmi_cat_summary <- nhanes_final %>%
  group_by(bmi_cat) %>%
  summarise(
    n = n(),
    pct = n() / nrow(nhanes_final) * 100,
    .groups = "drop"
  ) %>%
  arrange(bmi_cat)

for (i in 1:nrow(bmi_cat_summary)) {
  cat(sprintf("%s: n=%d (%.1f%%)\n",
              bmi_cat_summary$bmi_cat[i], bmi_cat_summary$n[i], bmi_cat_summary$pct[i]),
      file = flow_log, append = TRUE)
}

# Generate performance report
perf_report <- generate_performance_report()
cat("\n", file = flow_log, append = TRUE)
cat("PERFORMANCE SUMMARY\n", file = flow_log, append = TRUE)
cat("==================\n", file = flow_log, append = TRUE)
cat(sprintf("Total operations benchmarked: %d\n", length(performance_tracker$benchmarks)), file = flow_log, append = TRUE)
cat(sprintf("Total processing time: %.2f seconds\n", sum(sapply(performance_tracker$benchmarks, function(b) b$duration))), file = flow_log, append = TRUE)
cat(sprintf("Peak memory usage: %.2f MB\n", max(sapply(performance_tracker$benchmarks, function(b) b$memory_used))), file = flow_log, append = TRUE)

cat(sprintf("\nEnd time: %s\n", Sys.time()), file = flow_log, append = TRUE)

# Cleanup performance tracking
cleanup_performance_tracking()

cat("\n✓ Dataset derivation completed successfully with comprehensive validation!\n")
cat(sprintf("Final sample size: %d participants\n", nrow(nhanes_final)))
cat(sprintf("Validation status: %s\n", if (final_validation$column_validation$required_present) "PASSED" else "FAILED"))
cat(sprintf("Performance tracking: %s\n", if (length(performance_tracker$benchmarks) > 0) "COMPLETED" else "NOT USED"))
cat(sprintf("Flow log: %s\n", flow_log))
cat(sprintf("Validation log: %s\n", validation_log))
cat(sprintf("Validation report: %s\n", validation_report_file))
cat(sprintf("Performance report: %s\n", file.path(logs_dir, "performance_report.html")))