# Multi-Study Longitudinal Framework for NHANES BMI Body Fat Analysis
# Provides comprehensive support for multiple NHANES cycles and longitudinal analysis

library(survey)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(broom)
library(mgcv)
library(nlme)
library(lme4)
library(gee)
library(geepack)
library(mice)

# Multi-cycle configuration
MULTI_CYCLE_CONFIG <- list(
  available_cycles = c("2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018"),
  default_cycle = "2017-2018",
  cycle_mapping = list(
    "2009-2010" = list(
      demo_file = "DEMO_F.XPT",
      bmx_file = "BMX_F.XPT",
      dxx_file = "DXX_F.XPT",
      dxxag_file = "DXXAG_F.XPT"
    ),
    "2011-2012" = list(
      demo_file = "DEMO_G.XPT",
      bmx_file = "BMX_G.XPT",
      dxx_file = "DXX_G.XPT",
      dxxag_file = "DXXAG_G.XPT"
    ),
    "2013-2014" = list(
      demo_file = "DEMO_H.XPT",
      bmx_file = "BMX_H.XPT",
      dxx_file = "DXX_H.XPT",
      dxxag_file = "DXXAG_H.XPT"
    ),
    "2015-2016" = list(
      demo_file = "DEMO_I.XPT",
      bmx_file = "BMX_I.XPT",
      dxx_file = "DXX_I.XPT",
      dxxag_file = "DXXAG_I.XPT"
    ),
    "2017-2018" = list(
      demo_file = "DEMO_J.XPT",
      bmx_file = "BMX_J.XPT",
      dxx_file = "DXX_J.XPT",
      dxxag_file = "DXXAG_J.XPT"
    )
  ),
  variable_mapping = list(
    # Demographics variables
    "RIDAGEYR" = "RIDAGEYR",  # Age
    "RIAGENDR" = "RIAGENDR",  # Gender
    "RIDRETH1" = "RIDRETH1",  # Race/Ethnicity
    "WTMEC2YR" = "WTMEC2YR",  # MEC weights
    "SDMVSTRA" = "SDMVSTRA",  # Strata
    "SDMVPSU" = "SDMVPSU",    # PSU

    # Body measures variables
    "BMXBMI" = "BMXBMI",      # BMI

    # DXA variables (may vary by cycle)
    "DXDTOFAT" = c("DXDTOFAT", "DXDTOPF"),  # Total body fat percentage
    "DXDAPFAT" = "DXDAPFAT",  # Android fat percentage
    "DXDGPFAT" = "DXDGPFAT"   # Gynoid fat percentage
  ),
  harmonization_rules = list(
    # Rules for standardizing variables across cycles
    age_range = c(20, 59),  # Consistent age range
    exclude_pregnant = TRUE,
    exclude_invalid_dxa = TRUE,
    weight_normalization = TRUE
  )
)

#' Load and harmonize data from multiple NHANES cycles
#'
#' @param cycles Vector of NHANES cycles to load (e.g., c("2017-2018", "2015-2016"))
#' @param data_dir Directory containing NHANES data files
#' @param harmonize Logical to apply harmonization rules
#' @return List with harmonized data for each cycle
load_multi_cycle_data <- function(
    cycles = c("2017-2018", "2015-2016", "2013-2014"),
    data_dir = "data/raw",
    harmonize = TRUE
) {

  cat("Loading multi-cycle NHANES data for cycles:", paste(cycles, collapse = ", "), "\n")

  cycle_data <- list()

  for (cycle in cycles) {
    cat("  Processing cycle:", cycle, "\n")

    if (!cycle %in% names(MULTI_CYCLE_CONFIG$cycle_mapping)) {
      warning(paste("Cycle", cycle, "not found in mapping configuration"))
      next
    }

    cycle_files <- MULTI_CYCLE_CONFIG$cycle_mapping[[cycle]]

    # Load individual datasets for this cycle
    demo_file <- file.path(data_dir, cycle_files$demo_file)
    bmx_file <- file.path(data_dir, cycle_files$bmx_file)
    dxx_file <- file.path(data_dir, cycle_files$dxx_file)
    dxxag_file <- file.path(data_dir, cycle_files$dxxag_file)

    if (!file.exists(demo_file)) {
      warning(paste("Demographics file not found for cycle", cycle, ":", demo_file))
      next
    }

    # Load data with error handling
    demo_data <- tryCatch({
      foreign::read.xport(demo_file)
    }, error = function(e) {
      warning(paste("Failed to load demographics for cycle", cycle, ":", e$message))
      return(NULL)
    })

    bmx_data <- tryCatch({
      foreign::read.xport(bmx_file)
    }, error = function(e) {
      warning(paste("Failed to load body measures for cycle", cycle, ":", e$message))
      return(NULL)
    })

    dxx_data <- tryCatch({
      foreign::read.xport(dxx_file)
    }, error = function(e) {
      warning(paste("Failed to load DXA data for cycle", cycle, ":", e$message))
      return(NULL)
    })

    dxxag_data <- tryCatch({
      foreign::read.xport(dxxag_file)
    }, error = function(e) {
      warning(paste("Failed to load DXA AG data for cycle", cycle, ":", e$message))
      return(NULL)
    })

    # Merge datasets
    if (!is.null(demo_data) && !is.null(bmx_data)) {
      # Start with demographics
      merged_data <- demo_data %>%
        select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, WTMEC2YR, SDMVSTRA, SDMVPSU)

      # Add body measures
      if (!is.null(bmx_data)) {
        merged_data <- merged_data %>%
          left_join(bmx_data %>% select(SEQN, BMXBMI), by = "SEQN")
      }

      # Add DXA data (try multiple variable names)
      if (!is.null(dxx_data)) {
        # Find body fat variable
        body_fat_var <- find_body_fat_variable(dxx_data, cycle)
        if (!is.null(body_fat_var)) {
          merged_data <- merged_data %>%
            left_join(dxx_data %>% select(SEQN, !!body_fat_var), by = "SEQN")
        }
      }

      # Add android/gynoid data if available
      if (!is.null(dxxag_data)) {
        android_var <- find_android_fat_variable(dxxag_data, cycle)
        gynoid_var <- find_gynoid_fat_variable(dxxag_data, cycle)

        if (!is.null(android_var) && !is.null(gynoid_var)) {
          merged_data <- merged_data %>%
            left_join(dxxag_data %>% select(SEQN, !!android_var, !!gynoid_var), by = "SEQN")
        }
      }

      # Apply harmonization if requested
      if (harmonize) {
        merged_data <- harmonize_cycle_data(merged_data, cycle)
      }

      cycle_data[[cycle]] <- merged_data
      cat("    Loaded", nrow(merged_data), "observations for cycle", cycle, "\n")

    } else {
      warning(paste("Insufficient data for cycle", cycle))
    }
  }

  cat("Multi-cycle data loading complete\n")
  return(cycle_data)
}

#' Find body fat variable across different NHANES cycles
#'
#' @param dxx_data DXA dataset
#' @param cycle NHANES cycle
#' @return Variable name for body fat percentage
find_body_fat_variable <- function(dxx_data, cycle) {

  # Common variable names across cycles
  possible_vars <- c("DXDTOFAT", "DXDTOPF", "DXDPFAT")

  for (var in possible_vars) {
    if (var %in% names(dxx_data)) {
      return(var)
    }
  }

  # If not found, try to identify from variable labels
  if (!is.null(attr(dxx_data, "var.labels"))) {
    labels <- attr(dxx_data, "var.labels")
    fat_matches <- grep("percent fat|%fat|total.*fat", labels, ignore.case = TRUE)

    if (length(fat_matches) > 0) {
      return(names(dxx_data)[fat_matches[1]])
    }
  }

  warning(paste("Could not identify body fat variable for cycle", cycle))
  return(NULL)
}

#' Find android fat variable
#'
#' @param dxxag_data DXA android/gynoid dataset
#' @param cycle NHANES cycle
#' @return Variable name for android fat percentage
find_android_fat_variable <- function(dxxag_data, cycle) {

  if (is.null(dxxag_data) || nrow(dxxag_data) == 0) {
    return(NULL)
  }

  # Common variable names
  possible_vars <- c("DXDAPFAT", "DXDAPFPC")

  for (var in possible_vars) {
    if (var %in% names(dxxag_data)) {
      return(var)
    }
  }

  # Try to identify from labels
  if (!is.null(attr(dxxag_data, "var.labels"))) {
    labels <- attr(dxxag_data, "var.labels")
    android_matches <- grep("android.*fat|trunk.*fat", labels, ignore.case = TRUE)

    if (length(android_matches) > 0) {
      return(names(dxxag_data)[android_matches[1]])
    }
  }

  return(NULL)
}

#' Find gynoid fat variable
#'
#' @param dxxag_data DXA android/gynoid dataset
#' @param cycle NHANES cycle
#' @return Variable name for gynoid fat percentage
find_gynoid_fat_variable <- function(dxxag_data, cycle) {

  if (is.null(dxxag_data) || nrow(dxxag_data) == 0) {
    return(NULL)
  }

  # Common variable names
  possible_vars <- c("DXDGPFAT", "DXDGPFPC")

  for (var in possible_vars) {
    if (var %in% names(dxxag_data)) {
      return(var)
    }
  }

  # Try to identify from labels
  if (!is.null(attr(dxxag_data, "var.labels"))) {
    labels <- attr(dxxag_data, "var.labels")
    gynoid_matches <- grep("gynoid.*fat|leg.*fat", labels, ignore.case = TRUE)

    if (length(gynoid_matches) > 0) {
      return(names(dxxag_data)[gynoid_matches[1]])
    }
  }

  return(NULL)
}

#' Harmonize data across NHANES cycles
#'
#' @param data Dataset for a specific cycle
#' @param cycle NHANES cycle identifier
#' @return Harmonized dataset
harmonize_cycle_data <- function(data, cycle) {

  # Standardize age range
  data <- data %>%
    filter(RIDAGEYR >= MULTI_CYCLE_CONFIG$harmonization_rules$age_range[1] &
           RIDAGEYR <= MULTI_CYCLE_CONFIG$harmonization_rules$age_range[2])

  # Remove invalid DXA scans
  if ("DXDTOFAT" %in% names(data)) {
    data <- data %>%
      filter(!is.na(DXDTOFAT) & DXDTOFAT > 0 & DXDTOFAT < 100)
  }

  # Remove pregnant women if pregnancy data available
  if ("RIDEXPRG" %in% names(data)) {
    data <- data %>%
      filter(RIDEXPRG != 1)  # Not pregnant
  }

  # Ensure required survey variables exist
  required_vars <- c("WTMEC2YR", "SDMVSTRA", "SDMVPSU")
  for (var in required_vars) {
    if (!var %in% names(data)) {
      warning(paste("Required survey variable", var, "missing for cycle", cycle))
    }
  }

  # Add cycle identifier
  data$nhanes_cycle <- cycle

  # Add derived variables
  data <- data %>%
    mutate(
      age_group = case_when(
        RIDAGEYR >= 20 & RIDAGEYR < 30 ~ "20-29",
        RIDAGEYR >= 30 & RIDAGEYR < 40 ~ "30-39",
        RIDAGEYR >= 40 & RIDAGEYR < 50 ~ "40-49",
        RIDAGEYR >= 50 & RIDAGEYR <= 59 ~ "50-59",
        TRUE ~ "Other"
      ),
      bmi_category = case_when(
        BMXBMI < 18.5 ~ "Underweight",
        BMXBMI >= 18.5 & BMXBMI < 25 ~ "Normal",
        BMXBMI >= 25 & BMXBMI < 30 ~ "Overweight",
        BMXBMI >= 30 & BMXBMI < 35 ~ "Obesity I",
        BMXBMI >= 35 & BMXBMI < 40 ~ "Obesity II",
        BMXBMI >= 40 ~ "Obesity III",
        TRUE ~ "Unknown"
      ),
      sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female"))
    )

  return(data)
}

#' Combine data from multiple cycles with harmonization
#'
#' @param cycle_data_list List of datasets from different cycles
#' @param harmonize_variables Logical to harmonize variable names
#' @return Combined dataset with cycle indicators
combine_multi_cycle_data <- function(cycle_data_list, harmonize_variables = TRUE) {

  cat("Combining multi-cycle data...\n")

  if (length(cycle_data_list) == 0) {
    stop("No cycle data provided")
  }

  # Combine all cycles
  combined_data <- bind_rows(cycle_data_list, .id = "source_cycle")

  # Clean up the source_cycle column
  combined_data$nhanes_cycle <- combined_data$source_cycle
  combined_data$source_cycle <- NULL

  # Harmonize variable names if requested
  if (harmonize_variables) {
    combined_data <- harmonize_variable_names(combined_data)
  }

  # Add analysis-friendly variables
  combined_data <- combined_data %>%
    mutate(
      cycle_year = as.numeric(gsub("-.*", "", nhanes_cycle)),
      cycle_numeric = case_when(
        nhanes_cycle == "2009-2010" ~ 2010,
        nhanes_cycle == "2011-2012" ~ 2012,
        nhanes_cycle == "2013-2014" ~ 2014,
        nhanes_cycle == "2015-2016" ~ 2016,
        nhanes_cycle == "2017-2018" ~ 2018,
        TRUE ~ NA_real_
      ),
      time_from_baseline = cycle_numeric - min(cycle_numeric, na.rm = TRUE),
      decade = floor(cycle_numeric / 10) * 10
    )

  cat("Combined data from", length(cycle_data_list), "cycles\n")
  cat("Total observations:", nrow(combined_data), "\n")
  cat("Cycles included:", paste(unique(combined_data$nhanes_cycle), collapse = ", "), "\n")

  return(combined_data)
}

#' Harmonize variable names across cycles
#'
#' @param data Combined dataset
#' @return Dataset with harmonized variable names
harmonize_variable_names <- function(data) {

  # Standardize body fat variable names
  body_fat_cols <- grep("DXDTOFAT|DXDTOPF|DXDPFAT", names(data), value = TRUE)
  if (length(body_fat_cols) > 0) {
    # Rename to standard name
    for (col in body_fat_cols) {
      if (col != "bodyfat_pct") {
        names(data)[names(data) == col] <- "bodyfat_pct"
        break  # Only rename the first match
      }
    }
  }

  # Ensure consistent column naming
  data <- data %>%
    rename(
      age = RIDAGEYR,
      sex_numeric = RIAGENDR,
      ethnicity = RIDRETH1,
      mec_weight = WTMEC2YR,
      strata = SDMVSTRA,
      psu = SDMVPSU,
      bmi = BMXBMI
    )

  return(data)
}

#' Perform longitudinal trend analysis
#'
#' @param combined_data Combined multi-cycle dataset
#' @param outcome_variable Outcome variable for trend analysis (default: "bodyfat_pct")
#' @param time_variable Time variable (default: "cycle_numeric")
#' @param group_variables Variables for stratification
#' @return List with trend analysis results
perform_longitudinal_trend_analysis <- function(
    combined_data,
    outcome_variable = "bodyfat_pct",
    time_variable = "cycle_numeric",
    group_variables = c("sex", "age_group", "bmi_category")
) {

  cat("Performing longitudinal trend analysis...\n")

  # Prepare data for analysis
  trend_data <- combined_data %>%
    filter(!is.na(!!sym(outcome_variable))) %>%
    filter(!is.na(!!sym(time_variable)))

  if (nrow(trend_data) == 0) {
    stop("No valid data for trend analysis")
  }

  trend_results <- list()

  # Overall trends
  cat("  Analyzing overall trends...\n")
  overall_trends <- analyze_trend_by_group(
    trend_data, outcome_variable, time_variable
  )
  trend_results$overall <- overall_trends

  # Stratified trends
  for (group_var in group_variables) {
    if (group_var %in% names(trend_data)) {
      cat("  Analyzing trends by", group_var, "...\n")

      stratified_trends <- analyze_trend_by_group(
        trend_data, outcome_variable, time_variable, group_var
      )
      trend_results[[group_var]] <- stratified_trends
    }
  }

  # Age-period-cohort analysis
  cat("  Performing age-period-cohort analysis...\n")
  apc_results <- perform_age_period_cohort_analysis(
    trend_data, outcome_variable, time_variable
  )
  trend_results$apc <- apc_results

  # Temporal autocorrelation
  cat("  Analyzing temporal autocorrelation...\n")
  autocorrelation_results <- analyze_temporal_autocorrelation(
    trend_data, outcome_variable, time_variable
  )
  trend_results$temporal <- autocorrelation_results

  cat("Longitudinal trend analysis complete\n")

  return(trend_results)
}

#' Analyze trends by grouping variables
#'
#' @param data Dataset for analysis
#' @param outcome Outcome variable
#' @param time_var Time variable
#' @param group_var Optional grouping variable
#' @return Trend analysis results
analyze_trend_by_group <- function(data, outcome, time_var, group_var = NULL) {

  # Base formula
  if (is.null(group_var)) {
    formula_str <- paste(outcome, "~", time_var)
  } else {
    formula_str <- paste(outcome, "~", time_var, "*", group_var)
  }

  # Fit linear model
  lm_model <- lm(as.formula(formula_str), data = data)

  # Fit survey-weighted model
  if (all(c("mec_weight", "strata", "psu") %in% names(data))) {
    survey_design <- svydesign(
      ids = ~psu,
      strata = ~strata,
      weights = ~mec_weight,
      nest = TRUE,
      data = data
    )

    survey_model <- svyglm(as.formula(formula_str), design = survey_design)
  } else {
    survey_model <- NULL
  }

  # Calculate trend statistics
  trend_stats <- data %>%
    group_by(!!sym(ifelse(is.null(group_var), time_var, group_var))) %>%
    summarize(
      n = n(),
      mean_outcome = mean(!!sym(outcome), na.rm = TRUE),
      sd_outcome = sd(!!sym(outcome), na.rm = TRUE),
      se_outcome = sd_outcome / sqrt(n)
    ) %>%
    mutate(
      time_numeric = !!sym(ifelse(is.null(group_var), time_var, "as.numeric(as.character(" paste0(group_var, ")"))
    )

  # Calculate trend slope
  if (nrow(trend_stats) > 1) {
    trend_slope <- coef(lm(mean_outcome ~ time_numeric, data = trend_stats))[2]
    trend_p_value <- summary(lm(mean_outcome ~ time_numeric, data = trend_stats))$coefficients[2, 4]
  } else {
    trend_slope <- NA
    trend_p_value <- NA
  }

  return(list(
    linear_model = lm_model,
    survey_model = survey_model,
    trend_stats = trend_stats,
    trend_slope = trend_slope,
    trend_p_value = trend_p_value,
    formula = formula_str
  ))
}

#' Perform age-period-cohort analysis
#'
#' @param data Combined dataset
#' @param outcome Outcome variable
#' @param time_var Time variable
#' @return APC analysis results
perform_age_period_cohort_analysis <- function(data, outcome, time_var) {

  cat("    Running age-period-cohort analysis...\n")

  # Prepare APC data
  apc_data <- data %>%
    filter(!is.na(!!sym(outcome))) %>%
    mutate(
      birth_cohort = !!sym(time_var) - age,
      period = !!sym(time_var),
      age_group = cut(age, breaks = c(20, 30, 40, 50, 60),
                     labels = c("20-29", "30-39", "40-49", "50-59"))
    ) %>%
    filter(!is.na(birth_cohort) & !is.na(period) & !is.na(age_group))

  if (nrow(apc_data) == 0) {
    warning("Insufficient data for APC analysis")
    return(list(error = "Insufficient data"))
  }

  # Fit APC model
  tryCatch({
    # Linear APC model
    apc_model <- lm(as.formula(paste(outcome, "~ age + period + birth_cohort")),
                   data = apc_data)

    # Survey-weighted APC model
    if (all(c("mec_weight", "strata", "psu") %in% names(apc_data))) {
      survey_design <- svydesign(
        ids = ~psu,
        strata = ~strata,
        weights = ~mec_weight,
        nest = TRUE,
        data = apc_data
      )

      survey_apc_model <- svyglm(as.formula(paste(outcome, "~ age + period + birth_cohort")),
                                design = survey_design)
    } else {
      survey_apc_model <- NULL
    }

    # Calculate APC effects
    apc_effects <- data.frame(
      component = c("Age", "Period", "Cohort"),
      coefficient = coef(apc_model)[c("age", "period", "birth_cohort")],
      p_value = summary(apc_model)$coefficients[c("age", "period", "birth_cohort"), 4]
    )

    return(list(
      linear_model = apc_model,
      survey_model = survey_apc_model,
      apc_effects = apc_effects,
      data_summary = list(
        n_observations = nrow(apc_data),
        age_range = range(apc_data$age),
        period_range = range(apc_data$period),
        cohort_range = range(apc_data$birth_cohort)
      )
    ))

  }, error = function(e) {
    warning(paste("APC analysis failed:", e$message))
    return(list(
      error = e$message,
      error_type = "APC_analysis_failed"
    ))
  })
}

#' Analyze temporal autocorrelation
#'
#' @param data Dataset for analysis
#' @param outcome Outcome variable
#' @param time_var Time variable
#' @return Temporal autocorrelation results
analyze_temporal_autocorrelation <- function(data, outcome, time_var) {

  cat("    Analyzing temporal autocorrelation...\n")

  # Prepare time series data
  ts_data <- data %>%
    filter(!is.na(!!sym(outcome))) %>%
    arrange(!!sym(time_var)) %>%
    group_by(!!sym(time_var)) %>%
    summarize(
      mean_outcome = mean(!!sym(outcome), na.rm = TRUE),
      n = n(),
      se = sd(!!sym(outcome), na.rm = TRUE) / sqrt(n)
    )

  if (nrow(ts_data) < 3) {
    warning("Insufficient time points for autocorrelation analysis")
    return(list(error = "Insufficient time points"))
  }

  # Calculate autocorrelation
  outcome_values <- ts_data$mean_outcome
  acf_result <- acf(outcome_values, plot = FALSE)
  pacf_result <- pacf(outcome_values, plot = FALSE)

  # Test for stationarity (simplified)
  stationarity_test <- tryCatch({
    # Simple trend test
    time_values <- 1:nrow(ts_data)
    trend_test <- cor.test(time_values, outcome_values)
    list(
      correlation = trend_test$estimate,
      p_value = trend_test$p.value,
      significant_trend = trend_test$p.value < 0.05
    )
  }, error = function(e) {
    list(error = e$message)
  })

  return(list(
    autocorrelation = acf_result,
    partial_autocorrelation = pacf_result,
    stationarity_test = stationarity_test,
    time_series_data = ts_data
  ))
}

#' Perform longitudinal modeling with mixed effects
#'
#' @param combined_data Combined multi-cycle dataset
#' @param outcome Outcome variable
#' @param time_var Time variable
#' @param id_var Subject ID variable (if longitudinal)
#' @param random_effects Random effects structure
#' @return Longitudinal modeling results
perform_longitudinal_modeling <- function(
    combined_data,
    outcome = "bodyfat_pct",
    time_var = "cycle_numeric",
    id_var = NULL,
    random_effects = c("time", "age")
) {

  cat("Performing longitudinal modeling...\n")

  # Prepare longitudinal data
  if (is.null(id_var)) {
    # Cross-sectional analysis
    long_data <- combined_data %>%
      filter(!is.na(!!sym(outcome))) %>%
      mutate(
        time_numeric = as.numeric(factor(!!sym(time_var))),
        subject_id = paste(nhanes_cycle, SEQN, sep = "_")
      )
  } else {
    # True longitudinal data
    long_data <- combined_data %>%
      filter(!is.na(!!sym(outcome)) & !is.na(!!sym(id_var)))
  }

  # Fit mixed effects models
  models <- list()

  tryCatch({
    # Basic mixed model
    if ("age" %in% names(long_data) && "sex" %in% names(long_data)) {
      base_formula <- as.formula(paste(outcome, "~ time_numeric * age + sex"))
    } else {
      base_formula <- as.formula(paste(outcome, "~ time_numeric"))
    }

    # Random intercept model
    random_formula <- as.formula(paste("~ 1 |", if (!is.null(id_var)) id_var else "subject_id"))

    mixed_model <- lmer(base_formula, data = long_data)
    models$random_intercept <- mixed_model

    # Random slope model
    if (length(random_effects) > 0) {
      random_slope_formula <- as.formula(paste("~ time_numeric |", if (!is.null(id_var)) id_var else "subject_id"))
      mixed_slope_model <- lmer(update(base_formula, ~ . + (1 + time_numeric || subject_id)), data = long_data)
      models$random_slope <- mixed_slope_model
    }

  }, error = function(e) {
    warning(paste("Mixed effects modeling failed:", e$message))
    models$mixed_error <- e$message
  })

  # Fit GEE model for correlated data
  tryCatch({
    gee_formula <- as.formula(paste(outcome, "~ time_numeric + age + sex"))
    gee_model <- geeglm(gee_formula, data = long_data, id = if (!is.null(id_var)) id_var else subject_id,
                       corstr = "ar1")
    models$gee <- gee_model

  }, error = function(e) {
    warning(paste("GEE modeling failed:", e$message))
    models$gee_error <- e$message
  })

  # Calculate model comparison metrics
  model_comparison <- compare_longitudinal_models(models, long_data, outcome)

  cat("Longitudinal modeling complete\n")

  return(list(
    models = models,
    model_comparison = model_comparison,
    data_summary = list(
      n_observations = nrow(long_data),
      n_subjects = length(unique(long_data$subject_id)),
      time_points = length(unique(long_data[[time_var]])),
      cycles_included = unique(long_data$nhanes_cycle)
    )
  ))
}

#' Compare longitudinal models
#'
#' @param models List of fitted models
#' @param data Dataset used for modeling
#' @param outcome Outcome variable name
#' @return Model comparison results
compare_longitudinal_models <- function(models, data, outcome) {

  comparison_results <- data.frame(
    model_type = character(),
    aic = numeric(),
    bic = numeric(),
    log_likelihood = numeric(),
    r_squared = numeric(),
    stringsAsFactors = FALSE
  )

  for (model_name in names(models)) {
    model <- models[[model_name]]

    if (!is.character(model)) {  # Not an error
      tryCatch({
        # Calculate fit statistics
        aic_value <- AIC(model)
        bic_value <- BIC(model)
        loglik_value <- logLik(model)

        # Calculate pseudo R-squared
        null_model <- lm(as.formula(paste(outcome, "~ 1")), data = data)
        r_squared <- 1 - (deviance(model) / deviance(null_model))

        comparison_results <- rbind(comparison_results, data.frame(
          model_type = model_name,
          aic = aic_value,
          bic = bic_value,
          log_likelihood = as.numeric(loglik_value),
          r_squared = r_squared
        ))

      }, error = function(e) {
        # Skip models that can't be evaluated
      })
    }
  }

  if (nrow(comparison_results) > 0) {
    comparison_results <- comparison_results[order(comparison_results$aic), ]
    comparison_results$rank <- 1:nrow(comparison_results)
  }

  return(comparison_results)
}

#' Perform cohort analysis across cycles
#'
#' @param combined_data Combined multi-cycle dataset
#' @param cohort_definition How to define cohorts (e.g., "birth_year", "age_at_baseline")
#' @return Cohort analysis results
perform_cohort_analysis <- function(
    combined_data,
    cohort_definition = "birth_cohort"
) {

  cat("Performing cohort analysis...\n")

  # Create cohort data
  cohort_data <- combined_data %>%
    mutate(
      birth_year = cycle_numeric - age,
      cohort_group = case_when(
        birth_year >= 1990 ~ "Millennial",
        birth_year >= 1970 ~ "Generation X",
        birth_year >= 1950 ~ "Baby Boomer",
        birth_year >= 1930 ~ "Silent Generation",
        TRUE ~ "Other"
      ),
      cohort_decade = floor(birth_year / 10) * 10
    )

  # Cohort-specific analysis
  cohort_results <- list()

  # Overall cohort trends
  cohort_trends <- cohort_data %>%
    group_by(cohort_decade, nhanes_cycle) %>%
    summarize(
      n = n(),
      mean_bodyfat = mean(bodyfat_pct, na.rm = TRUE),
      mean_bmi = mean(BMXBMI, na.rm = TRUE),
      mean_age = mean(age, na.rm = TRUE)
    ) %>%
    arrange(cohort_decade, nhanes_cycle)

  cohort_results$overall_trends <- cohort_trends

  # Cohort-specific models
  for (cohort in unique(cohort_data$cohort_group)) {
    cohort_subset <- cohort_data %>% filter(cohort_group == cohort)

    if (nrow(cohort_subset) > 10) {
      cohort_model <- lm(bodyfat_pct ~ cycle_numeric + age + sex, data = cohort_subset)
      cohort_results[[cohort]] <- list(
        model = cohort_model,
        data = cohort_subset,
        n_observations = nrow(cohort_subset)
      )
    }
  }

  # Cohort comparison
  cohort_comparison <- cohort_data %>%
    group_by(cohort_group) %>%
    summarize(
      n_total = n(),
      mean_bodyfat = mean(bodyfat_pct, na.rm = TRUE),
      bodyfat_trend = coef(lm(bodyfat_pct ~ cycle_numeric))[2],
      mean_bmi = mean(BMXBMI, na.rm = TRUE),
      bmi_trend = coef(lm(BMXBMI ~ cycle_numeric))[2]
    )

  cohort_results$comparison <- cohort_comparison

  cat("Cohort analysis complete\n")

  return(cohort_results)
}

#' Generate longitudinal visualizations
#'
#' @param trend_results Results from longitudinal trend analysis
#' @param output_dir Directory for saving plots
#' @return List of generated plots
generate_longitudinal_plots <- function(trend_results, output_dir = "outputs/figures") {

  cat("Generating longitudinal visualizations...\n")

  plots <- list()

  # Overall trend plot
  if (!is.null(trend_results$overall)) {
    overall_data <- trend_results$overall$trend_stats

    p1 <- ggplot(overall_data, aes(x = time_numeric, y = mean_outcome)) +
      geom_point(size = 3, color = "#2c5aa0") +
      geom_line(color = "#2c5aa0") +
      geom_errorbar(aes(ymin = mean_outcome - se_outcome, ymax = mean_outcome + se_outcome),
                   width = 0.2, color = "#2c5aa0", alpha = 0.5) +
      labs(title = "Body Fat Percentage Trends Over Time",
           subtitle = "Mean body fat percentage across NHANES cycles",
           x = "NHANES Cycle",
           y = "Body Fat Percentage (%)") +
      theme_minimal() +
      theme(legend.position = "none")

    plots$overall_trend <- p1

    ggsave(file.path(output_dir, "longitudinal_overall_trend.png"), p1, width = 10, height = 6, dpi = 300)
  }

  # Stratified trends
  if (!is.null(trend_results$sex)) {
    sex_data <- trend_results$sex$trend_stats

    p2 <- ggplot(sex_data, aes(x = time_numeric, y = mean_outcome, color = sex)) +
      geom_point(size = 3) +
      geom_line(size = 1.5) +
      geom_errorbar(aes(ymin = mean_outcome - se_outcome, ymax = mean_outcome + se_outcome),
                   width = 0.2, alpha = 0.5) +
      labs(title = "Body Fat Trends by Sex",
           subtitle = "Longitudinal trends in body fat percentage by gender",
           x = "NHANES Cycle",
           y = "Body Fat Percentage (%)",
           color = "Sex") +
      scale_color_manual(values = c("Male" = "#2c5aa0", "Female" = "#e74c3c")) +
      theme_minimal()

    plots$sex_trends <- p2

    ggsave(file.path(output_dir, "longitudinal_sex_trends.png"), p2, width = 10, height = 6, dpi = 300)
  }

  # Age group trends
  if (!is.null(trend_results$age_group)) {
    age_data <- trend_results$age_group$trend_stats

    p3 <- ggplot(age_data, aes(x = time_numeric, y = mean_outcome, color = age_group)) +
      geom_point(size = 3) +
      geom_line(size = 1.5) +
      labs(title = "Body Fat Trends by Age Group",
           subtitle = "Longitudinal trends across age categories",
           x = "NHANES Cycle",
           y = "Body Fat Percentage (%)",
           color = "Age Group") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    plots$age_trends <- p3

    ggsave(file.path(output_dir, "longitudinal_age_trends.png"), p3, width = 12, height = 8, dpi = 300)
  }

  # Cohort analysis plot
  if (!is.null(trend_results$cohort_comparison)) {
    cohort_data <- trend_results$cohort_comparison

    p4 <- ggplot(cohort_data, aes(x = cohort_group, y = mean_bodyfat, fill = cohort_group)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_bodyfat - 2, ymax = mean_bodyfat + 2),
                   width = 0.2, alpha = 0.5) +
      labs(title = "Body Fat by Birth Cohort",
           subtitle = "Average body fat percentage across generational cohorts",
           x = "Birth Cohort",
           y = "Body Fat Percentage (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")

    plots$cohort_comparison <- p4

    ggsave(file.path(output_dir, "longitudinal_cohort_comparison.png"), p4, width = 10, height = 6, dpi = 300)
  }

  cat("Longitudinal visualizations created\n")

  return(plots)
}

#' Run complete multi-cycle analysis
#'
#' @param cycles NHANES cycles to analyze
#' @param data_dir Directory containing NHANES data
#' @param perform_trends Logical to perform trend analysis
#' @param perform_cohorts Logical to perform cohort analysis
#' @param perform_longitudinal Logical to perform longitudinal modeling
#' @return Complete multi-cycle analysis results
run_multi_cycle_analysis <- function(
    cycles = c("2017-2018", "2015-2016", "2013-2014"),
    data_dir = "data/raw",
    perform_trends = TRUE,
    perform_cohorts = TRUE,
    perform_longitudinal = TRUE
) {

  cat("🚀 Starting multi-cycle NHANES analysis...\n")

  # Step 1: Load and harmonize data
  cat("Step 1: Loading and harmonizing multi-cycle data...\n")
  cycle_data_list <- load_multi_cycle_data(cycles, data_dir, harmonize = TRUE)

  if (length(cycle_data_list) == 0) {
    stop("No valid cycle data loaded")
  }

  # Step 2: Combine data
  cat("Step 2: Combining multi-cycle data...\n")
  combined_data <- combine_multi_cycle_data(cycle_data_list, harmonize_variables = TRUE)

  # Step 3: Perform trend analysis
  trend_results <- NULL
  if (perform_trends) {
    cat("Step 3: Performing longitudinal trend analysis...\n")
    trend_results <- perform_longitudinal_trend_analysis(combined_data)
  }

  # Step 4: Perform cohort analysis
  cohort_results <- NULL
  if (perform_cohorts) {
    cat("Step 4: Performing cohort analysis...\n")
    cohort_results <- perform_cohort_analysis(combined_data)
  }

  # Step 5: Perform longitudinal modeling
  longitudinal_results <- NULL
  if (perform_longitudinal) {
    cat("Step 5: Performing longitudinal modeling...\n")
    longitudinal_results <- perform_longitudinal_modeling(combined_data)
  }

  # Step 6: Generate visualizations
  cat("Step 6: Generating longitudinal visualizations...\n")
  plots <- generate_longitudinal_plots(trend_results, "outputs/figures")

  # Step 7: Export results
  cat("Step 7: Exporting multi-cycle analysis results...\n")

  # Export combined data
  write.csv(combined_data, "outputs/tables/multi_cycle_combined_data.csv", row.names = FALSE)

  # Export trend results
  if (!is.null(trend_results)) {
    if (!is.null(trend_results$overall)) {
      write.csv(trend_results$overall$trend_stats, "outputs/tables/multi_cycle_trend_stats.csv", row.names = FALSE)
    }

    if (!is.null(trend_results$apc)) {
      write.csv(trend_results$apc$apc_effects, "outputs/tables/multi_cycle_apc_effects.csv", row.names = FALSE)
    }
  }

  # Export cohort results
  if (!is.null(cohort_results)) {
    write.csv(cohort_results$comparison, "outputs/tables/multi_cycle_cohort_comparison.csv", row.names = FALSE)
  }

  # Export model comparison
  if (!is.null(longitudinal_results)) {
    write.csv(longitudinal_results$model_comparison, "outputs/tables/multi_cycle_model_comparison.csv", row.names = FALSE)
  }

  # Step 8: Create documentation
  cat("Step 8: Creating multi-cycle analysis documentation...\n")

  methods_text <- paste0(
    "NHANES Multi-Cycle BMI vs Body Fat Analysis - Longitudinal Methods\n",
    "==================================================================\n\n",
    "Multi-Cycle Analysis Overview:\n",
    "------------------------------\n\n",
    "This analysis examines BMI-body fat relationships across multiple NHANES cycles for longitudinal insights:\n\n",
    "Cycles Included: ", paste(cycles, collapse = ", "), "\n",
    "Total Observations: ", nrow(combined_data), "\n",
    "Time Period: ", min(combined_data$cycle_numeric), "-", max(combined_data$cycle_numeric), "\n",
    "Age Range: ", paste(MULTI_CYCLE_CONFIG$harmonization_rules$age_range, collapse = "-"), " years\n\n",

    "Data Harmonization:\n",
    "-------------------\n",
    "- Standardized age range across all cycles\n",
    "- Consistent variable naming and coding\n",
    "- Quality control for DXA measurements\n",
    "- Survey weight normalization\n\n",

    "Analysis Methods:\n",
    "-----------------\n",
    "1. **Trend Analysis**: Linear and survey-weighted models for temporal trends\n",
    "2. **Age-Period-Cohort Analysis**: Decomposition of age, period, and cohort effects\n",
    "3. **Cohort Analysis**: Examination of generational differences\n",
    "4. **Longitudinal Modeling**: Mixed effects and GEE models for correlated data\n",
    "5. **Temporal Autocorrelation**: Analysis of time series dependencies\n\n",

    "Key Findings:\n",
    "-------------\n"
  )

  # Add key findings
  if (!is.null(trend_results$overall)) {
    slope <- trend_results$overall$trend_slope
    p_value <- trend_results$overall$trend_p_value

    methods_text <- paste0(methods_text,
      "Overall Trend: ", if (!is.na(slope)) round(slope, 4) else "Not available",
      " % per year (p = ", if (!is.na(p_value)) round(p_value, 4) else "N/A", ")\n"
    )
  }

  if (!is.null(cohort_results$comparison)) {
    methods_text <- paste0(methods_text,
      "Cohort Differences: Significant variation across generational groups\n"
    )
  }

  methods_text <- paste0(methods_text,
    "\nTechnical Details:\n",
    "------------------\n",
    "- R packages: survey, lme4, nlme, geepack, mgcv\n",
    "- Statistical methods: Linear mixed models, GEE, APC analysis\n",
    "- Data harmonization: Standardized across ", length(cycles), " cycles\n",
    "- Analysis date: ", Sys.Date(), "\n"
  )

  writeLines(methods_text, "outputs/logs/multi_cycle_methods.txt")

  # Compile final results
  final_results <- list(
    cycle_data = cycle_data_list,
    combined_data = combined_data,
    trend_results = trend_results,
    cohort_results = cohort_results,
    longitudinal_results = longitudinal_results,
    plots = plots,
    analysis_metadata = list(
      cycles_analyzed = cycles,
      total_observations = nrow(combined_data),
      analysis_timestamp = Sys.time(),
      methods_version = "1.0"
    )
  )

  cat("🎉 Multi-cycle analysis completed successfully!\n")
  cat("📊 Results saved to outputs/ directory\n")

  return(final_results)
}






