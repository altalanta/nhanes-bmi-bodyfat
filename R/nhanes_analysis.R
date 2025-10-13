# Main NHANES BMI vs Body Fat Analysis Functions

#' Load NHANES datasets with validation
#'
#' Loads and validates all required NHANES datasets for BMI-body fat analysis.
#'
#' @param config Configuration list from load_config()
#' @return List containing all loaded datasets
#' @export
load_nhanes_datasets <- function(config) {
  # Load datasets using safe functions
  demo <- safe_read_xpt(config$nhanes_demo_path, "DEMO_J dataset")
  bmx <- safe_read_xpt(config$nhanes_bmx_path, "BMX_J dataset")
  dxx <- safe_read_xpt(config$nhanes_dxx_path, "DXX_J dataset")
  dxxag <- safe_read_xpt(config$nhanes_dxxag_path, "DXXAG_J dataset")

  # Validate datasets
  validate_nhanes_data(demo, "DEMO_J", c("SEQN", "RIDAGEYR", "RIAGENDR", "WTMEC2YR", "SDMVSTRA", "SDMVPSU"))
  validate_nhanes_data(bmx, "BMX_J", c("SEQN", "BMXBMI"))
  validate_nhanes_data(dxx, "DXX_J", c("SEQN"))
  validate_nhanes_data(dxxag, "DXXAG_J", c("SEQN"))

  # Return as named list
  list(
    demo = demo,
    bmx = bmx,
    dxx = dxx,
    dxxag = dxxag
  )
}

#' Identify body fat percentage variable
#'
#' Searches for and identifies the body fat percentage variable in DXA data.
#'
#' @param dxx DXA whole body dataset
#' @param dxxag DXA android/gynoid dataset (optional)
#' @return Name of the body fat percentage variable
#' @export
identify_bodyfat_variable <- function(dxx, dxxag = NULL) {
  # Get variable labels
  dxx_labels <- attr(dxx, "var.labels")
  if (is.null(dxx_labels)) {
    dxx_labels <- sapply(names(dxx), function(x) {
      label <- attr(dxx[[x]], "label")
      if (is.null(label)) return("")
      return(label)
    })
  }

  # Search for % body fat variable
  fat_patterns <- c("percent fat", "%fat", "total % fat", "% fat")
  bodyfat_vars <- c()

  for (pattern in fat_patterns) {
    matches <- grep(pattern, dxx_labels, ignore.case = TRUE)
    if (length(matches) > 0) {
      bodyfat_vars <- c(bodyfat_vars, names(dxx)[matches])
    }
  }

  # Also check for common NHANES body fat variable names
  common_names <- c("DXDTOFAT", "DXDTOPF", "DXDPFAT")
  for (name in common_names) {
    if (name %in% names(dxx)) {
      bodyfat_vars <- c(bodyfat_vars, name)
    }
  }

  bodyfat_vars <- unique(bodyfat_vars)

  if (length(bodyfat_vars) == 0) {
    stop(NhanesError(
      "Could not identify % body fat variable in DXX_J",
      code = "BODYFAT_VAR_NOT_FOUND"
    ))
  }

  # Use the first match (usually DXDTOPF for total percent fat)
  bodyfat_var <- bodyfat_vars[1]
  safe_log(paste("Resolved % body fat variable:", bodyfat_var))

  if (!is.null(dxx_labels[bodyfat_var])) {
    safe_log(paste("Variable label:", dxx_labels[bodyfat_var]))
  }

  return(bodyfat_var)
}

#' Create analytic dataset with exclusions
#'
#' Merges datasets and applies study exclusions for the target population.
#'
#' @param datasets List of NHANES datasets
#' @param config Configuration list
#' @return Analytic dataset ready for survey analysis
#' @export
create_analytic_dataset <- function(datasets, config) {
  safe_log("Creating analytic dataset", "INFO")

  # Merge datasets
  nhanes_data <- datasets$demo %>%
    left_join(datasets$bmx, by = "SEQN") %>%
    left_join(datasets$dxx, by = "SEQN")

  # Apply exclusions
  # Age 20-59
  nhanes_data <- nhanes_data %>%
    filter(RIDAGEYR >= config$age_min & RIDAGEYR <= config$age_max)

  # Non-missing BMI and body fat
  bodyfat_var <- identify_bodyfat_variable(datasets$dxx, datasets$dxxag)
  nhanes_data <- nhanes_data %>%
    filter(!is.na(BMXBMI) & !is.na(!!sym(bodyfat_var)))

  # Valid survey design variables
  nhanes_data <- nhanes_data %>%
    filter(!is.na(!!sym(config$survey_weights_col)) &
           !is.na(!!sym(config$strata_col)) &
           !is.na(!!sym(config$psu_col)))

  # Rename body fat variable for consistency
  names(nhanes_data)[names(nhanes_data) == bodyfat_var] <- "bodyfat_pct"

  safe_log(paste("Analytic dataset created with", nrow(nhanes_data), "observations"))

  return(nhanes_data)
}

#' Create survey design object
#'
#' Creates a survey design object with proper weighting and stratification.
#'
#' @param data Analytic dataset
#' @param config Configuration list
#' @return Survey design object
#' @export
create_survey_design <- function(data, config) {
  # Validate survey design parameters
  validate_survey_design(
    data,
    config$survey_weights_col,
    config$strata_col,
    config$psu_col
  )

  # Create survey design object
  svy_design <- svydesign(
    ids = as.formula(paste("~", config$psu_col)),
    strata = as.formula(paste("~", config$strata_col)),
    weights = as.formula(paste("~", config$survey_weights_col)),
    nest = TRUE,
    data = data
  )

  safe_log("Survey design created successfully", "INFO")
  return(svy_design)
}

#' Compute survey-weighted correlations
#'
#' Calculates BMI-body fat correlations with confidence intervals.
#'
#' @param svy_design Survey design object
#' @return List containing correlation estimates and confidence intervals
#' @export
compute_correlations <- function(svy_design) {
  # Overall correlation
  overall_corr <- svyvar(~BMXBMI + bodyfat_pct, svy_design)
  correlation <- overall_corr[1,2] / sqrt(overall_corr[1,1] * overall_corr[2,2])

  # Standard error using delta method
  corr_se <- sqrt((1 - correlation^2)^2 / (4 * correlation^2) *
                  (overall_corr[1,1]/overall_corr[1,2]^2 +
                   overall_corr[2,2]/overall_corr[1,2]^2 -
                   2/(overall_corr[1,1] * overall_corr[2,2])))

  # Confidence intervals
  corr_ci_lower <- correlation - 1.96 * corr_se
  corr_ci_upper <- correlation + 1.96 * corr_se

  # By sex
  male_design <- subset(svy_design, RIAGENDR == 1)  # Male
  female_design <- subset(svy_design, RIAGENDR == 2)  # Female

  male_corr <- svyvar(~BMXBMI + bodyfat_pct, male_design)
  male_correlation <- male_corr[1,2] / sqrt(male_corr[1,1] * male_corr[2,2])

  female_corr <- svyvar(~BMXBMI + bodyfat_pct, female_design)
  female_correlation <- female_corr[1,2] / sqrt(female_corr[1,1] * female_corr[2,2])

  # Return comprehensive results
  list(
    overall = list(
      correlation = correlation,
      ci_lower = corr_ci_lower,
      ci_upper = corr_ci_upper,
      se = corr_se
    ),
    male = list(
      correlation = male_correlation,
      n_obs = nrow(male_design$variables)
    ),
    female = list(
      correlation = female_correlation,
      n_obs = nrow(female_design$variables)
    ),
    sample_size = nrow(svy_design$variables)
  )
}

#' Run complete NHANES BMI-body fat analysis
#'
#' Orchestrates the complete analysis pipeline from data loading to results.
#'
#' @param config_file Path to configuration file (default: "config/config.yml")
#' @return List containing all analysis results
#' @export
run_nhanes_analysis <- function(config_file = "config/config.yml") {
  safe_log("Starting complete NHANES BMI-body fat analysis", "INFO")

  # Load configuration
  config <- safe_load_config(config_file)
  ensure_output_dirs(config)

  # Load and validate datasets
  datasets <- load_nhanes_datasets(config)

  # Create analytic dataset
  analytic_data <- create_analytic_dataset(datasets, config)

  # Create survey design
  svy_design <- create_survey_design(analytic_data, config)

  # Compute correlations
  correlations <- compute_correlations(svy_design)

  # Save results
  results_file <- file.path(config$outputs_tables_path, "nhanes_analysis_results.rds")
  safe_save_data(list(
    correlations = correlations,
    config = config,
    timestamp = Sys.time(),
    sample_size = correlations$sample_size
  ), results_file)

  safe_log("Analysis completed successfully", "INFO")
  return(list(
    correlations = correlations,
    config = config,
    analytic_data = analytic_data,
    svy_design = svy_design
  ))
}

