#' @title NHANES BMI vs Body Fat Analysis - Modular Architecture
#' @description Modular, reusable functions for NHANES BMI-body fat analysis
#' @docType package
#' @name nhanesbmi

#' Load and validate NHANES datasets
#'
#' @param config Configuration object with file paths
#' @return List containing merged NHANES datasets
#' @export
load_nhanes_data <- function(config) {
  # Load datasets with error handling
  datasets <- list(
    demo = safe_read_xpt(config$nhanes_demo_path, "DEMO_J dataset"),
    bmx = safe_read_xpt(config$nhanes_bmx_path, "BMX_J dataset"),
    dxx = safe_read_xpt(config$nhanes_dxx_path, "DXX_J dataset"),
    dxxag = safe_read_xpt(config$nhanes_dxxag_path, "DXXAG_J dataset")
  )

  # Validate datasets
  validate_nhanes_data(datasets$demo, "DEMO_J",
                      c("SEQN", "RIDAGEYR", "RIAGENDR", "WTMEC2YR", "SDMVSTRA", "SDMVPSU"))
  validate_nhanes_data(datasets$bmx, "BMX_J", c("SEQN", "BMXBMI"))
  validate_nhanes_data(datasets$dxx, "DXX_J", c("SEQN"))
  validate_nhanes_data(datasets$dxxag, "DXXAG_J", c("SEQN"))

  return(datasets)
}

#' Identify body fat percentage variable in DXA dataset
#'
#' @param dxx DXA dataset
#' @param dxxag Optional android/gynoid dataset
#' @return List with bodyfat_var, android_var, gynoid_var
#' @export
identify_bodyfat_variables <- function(dxx, dxxag = NULL) {
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

  # Check for common NHANES body fat variable names
  common_names <- c("DXDTOFAT", "DXDTOPF", "DXDPFAT")
  for (name in common_names) {
    if (name %in% names(dxx)) {
      bodyfat_vars <- c(bodyfat_vars, name)
    }
  }

  if (length(bodyfat_vars) == 0) {
    stop("Could not identify % body fat variable in DXX_J")
  }

  bodyfat_var <- bodyfat_vars[1]

  # Identify android and gynoid variables if available
  android_var <- gynoid_var <- NULL

  if (!is.null(dxxag) && nrow(dxxag) > 0) {
    dxxag_labels <- attr(dxxag, "var.labels")
    if (is.null(dxxag_labels)) {
      dxxag_labels <- sapply(names(dxxag), function(x) {
        label <- attr(dxxag[[x]], "label")
        if (is.null(label)) return("")
        return(label)
      })
    }

    # Look for android fat
    android_matches <- grep("android.*fat", dxxag_labels, ignore.case = TRUE)
    if (length(android_matches) > 0) {
      android_var <- names(dxxag)[android_matches[1]]
    }

    # Look for gynoid fat
    gynoid_matches <- grep("gynoid.*fat", dxxag_labels, ignore.case = TRUE)
    if (length(gynoid_matches) > 0) {
      gynoid_var <- names(dxxag)[gynoid_matches[1]]
    }
  }

  return(list(
    bodyfat_var = bodyfat_var,
    android_var = android_var,
    gynoid_var = gynoid_var
  ))
}

#' Merge NHANES datasets and apply inclusion criteria
#'
#' @param datasets List of NHANES datasets from load_nhanes_data()
#' @param bodyfat_vars List from identify_bodyfat_variables()
#' @param config Configuration object
#' @return Processed dataset ready for analysis
#' @export
prepare_analytic_dataset <- function(datasets, bodyfat_vars, config) {
  # Merge datasets
  nhanes <- datasets$demo %>%
    select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, WTMEC2YR, SDMVSTRA, SDMVPSU) %>%
    left_join(datasets$bmx %>% select(SEQN, BMXBMI), by = "SEQN") %>%
    left_join(datasets$dxx %>% select_at(c("SEQN", bodyfat_vars$bodyfat_var)), by = "SEQN")

  # Add android/gynoid if available
  if (!is.null(bodyfat_vars$android_var) && !is.null(bodyfat_vars$gynoid_var)) {
    nhanes <- nhanes %>%
      left_join(datasets$dxxag %>% select_at(c("SEQN", bodyfat_vars$android_var, bodyfat_vars$gynoid_var)), by = "SEQN")
  }

  # Apply inclusion criteria
  nhanes_adults <- nhanes %>%
    filter(RIDAGEYR >= config$analysis$age_range[1] &
           RIDAGEYR <= config$analysis$age_range[2])

  nhanes_complete <- nhanes_adults %>%
    filter(!is.na(BMXBMI) &
           !is.na(!!sym(bodyfat_vars$bodyfat_var)) &
           !is.na(WTMEC2YR) &
           !is.na(SDMVSTRA) &
           !is.na(SDMVPSU))

  # Create BMI categories and sex factor
  nhanes_complete <- nhanes_complete %>%
    mutate(
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
      sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female"))
    )

  # Rename body fat variable
  names(nhanes_complete)[names(nhanes_complete) == bodyfat_vars$bodyfat_var] <- "bodyfat_pct"

  return(nhanes_complete)
}

#' Create survey design object
#'
#' @param data Analytic dataset
#' @param config Configuration object
#' @return Survey design object
#' @export
create_survey_design <- function(data, config) {
  # Validate survey design parameters
  validate_survey_design(
    data,
    config$analysis$survey_weights_col,
    config$analysis$strata_col,
    config$analysis$psu_col
  )

  # Create survey design object
  svy_design <- svydesign(
    ids = as.formula(paste("~", config$analysis$psu_col)),
    strata = as.formula(paste("~", config$analysis$strata_col)),
    weights = as.formula(paste("~", config$analysis$survey_weights_col)),
    nest = TRUE,
    data = data
  )

  return(svy_design)
}

#' Compute survey-weighted correlation between BMI and body fat
#'
#' @param svy_design Survey design object
#' @param by_sex Logical: compute sex-stratified correlations
#' @return Data frame with correlation results
#' @export
compute_bmi_bodyfat_correlation <- function(svy_design, by_sex = TRUE) {
  # Overall correlation
  overall_corr <- svyvar(~BMXBMI + bodyfat_pct, svy_design)
  bmi_var <- overall_corr[1,1]
  bf_var <- overall_corr[2,2]
  covar <- overall_corr[1,2]
  correlation <- covar / sqrt(bmi_var * bf_var)

  # Standard error using delta method
  corr_se <- sqrt((1 - correlation^2)^2 / (4 * correlation^2) *
                  (bmi_var/covar^2 + bf_var/covar^2 - 2/(bmi_var * bf_var)))
  corr_ci_lower <- correlation - 1.96 * corr_se
  corr_ci_upper <- correlation + 1.96 * corr_se

  results <- data.frame(
    group = "Overall",
    correlation = correlation,
    std_error = corr_se,
    ci_lower = corr_ci_lower,
    ci_upper = corr_ci_upper
  )

  if (by_sex) {
    # By sex correlations
    male_design <- subset(svy_design, sex == "Male")
    female_design <- subset(svy_design, sex == "Female")

    # Male correlation
    male_corr <- svyvar(~BMXBMI + bodyfat_pct, male_design)
    male_correlation <- male_corr[1,2] / sqrt(male_corr[1,1] * male_corr[2,2])
    male_corr_se <- sqrt((1 - male_correlation^2)^2 / (4 * male_correlation^2) *
                         (male_corr[1,1]/male_corr[1,2]^2 + male_corr[2,2]/male_corr[1,2]^2 -
                          2/(male_corr[1,1] * male_corr[2,2])))
    male_ci_lower <- male_correlation - 1.96 * male_corr_se
    male_ci_upper <- male_correlation + 1.96 * male_corr_se

    # Female correlation
    female_corr <- svyvar(~BMXBMI + bodyfat_pct, female_design)
    female_correlation <- female_corr[1,2] / sqrt(female_corr[1,1] * female_corr[2,2])
    female_corr_se <- sqrt((1 - female_correlation^2)^2 / (4 * female_correlation^2) *
                           (female_corr[1,1]/female_corr[1,2]^2 + female_corr[2,2]/female_corr[1,2]^2 -
                            2/(female_corr[1,1] * female_corr[2,2])))
    female_ci_lower <- female_correlation - 1.96 * female_corr_se
    female_ci_upper <- female_correlation + 1.96 * female_corr_se

    sex_results <- data.frame(
      group = c("Male", "Female"),
      correlation = c(male_correlation, female_correlation),
      std_error = c(male_corr_se, female_corr_se),
      ci_lower = c(male_ci_lower, female_ci_lower),
      ci_upper = c(male_ci_upper, female_ci_upper)
    )

    results <- bind_rows(results, sex_results)
  }

  return(results)
}

#' Compute body fat statistics by BMI class and sex
#'
#' @param svy_design Survey design object
#' @return Data frame with body fat statistics by BMI class and sex
#' @export
compute_bmi_class_stats <- function(svy_design) {
  compute_group_stats <- function(design_obj) {
    n_unweighted <- nrow(design_obj$variables)
    pop_total <- sum(weights(design_obj))

    mean_bf <- svymean(~bodyfat_pct, design_obj)
    mean_est <- as.numeric(mean_bf)
    mean_se <- as.numeric(SE(mean_bf))
    mean_ci <- confint(mean_bf)

    # Quantiles
    q05 <- as.numeric(svyquantile(~bodyfat_pct, design_obj, quantiles = 0.05)[[1]])
    q50 <- as.numeric(svyquantile(~bodyfat_pct, design_obj, quantiles = 0.50)[[1]])
    q95 <- as.numeric(svyquantile(~bodyfat_pct, design_obj, quantiles = 0.95)[[1]])

    return(data.frame(
      n_unweighted = n_unweighted,
      pop_total = pop_total,
      mean_bodyfat = mean_est,
      mean_se = mean_se,
      mean_ci_lower = as.numeric(mean_ci[1]),
      mean_ci_upper = as.numeric(mean_ci[2]),
      q05 = q05,
      q50 = q50,
      q95 = q95
    ))
  }

  # Compute stats by BMI class and sex
  bmi_sex_results <- expand.grid(
    bmi_cat = levels(svy_design$variables$bmi_cat),
    sex = levels(svy_design$variables$sex),
    stringsAsFactors = FALSE
  ) %>%
    rowwise() %>%
    do({
      subset_design <- subset(svy_design, bmi_cat == .$bmi_cat & sex == .$sex)
      if (nrow(subset_design$variables) > 0) {
        cbind(., compute_group_stats(subset_design))
      } else {
        cbind(., data.frame(n_unweighted = 0, pop_total = 0,
                           mean_bodyfat = NA, mean_se = NA,
                           mean_ci_lower = NA, mean_ci_upper = NA,
                           q05 = NA, q50 = NA, q95 = NA))
      }
    }) %>%
    ungroup()

  return(bmi_sex_results)
}

#' Test linearity assumption between BMI and body fat
#'
#' @param svy_design Survey design object
#' @return List with model comparison results
#' @export
test_linearity_assumption <- function(svy_design) {
  # Linear model
  linear_model <- svyglm(bodyfat_pct ~ BMXBMI, design = svy_design)

  # Quadratic model
  quad_model <- svyglm(bodyfat_pct ~ BMXBMI + I(BMXBMI^2), design = svy_design)

  # Compare models
  linear_aic <- AIC(linear_model)
  quad_aic <- AIC(quad_model)

  quad_p <- summary(quad_model)$coefficients["I(BMXBMI^2)", "Pr(>|t|)"]

  results <- list(
    linear_aic = linear_aic,
    quad_aic = quad_aic,
    quad_p_value = quad_p,
    is_nonlinear = quad_p < 0.05,
    linear_model = linear_model,
    quad_model = quad_model
  )

  return(results)
}

#' Run complete NHANES BMI-body fat analysis
#'
#' @param config Configuration object
#' @param output_dir Directory for output files
#' @return List with all analysis results
#' @export
run_complete_analysis <- function(config, output_dir = "outputs/tables") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Load and prepare data
  datasets <- load_nhanes_data(config)
  bodyfat_vars <- identify_bodyfat_variables(datasets$dxx, datasets$dxxag)
  analytic_data <- prepare_analytic_dataset(datasets, bodyfat_vars, config)
  svy_design <- create_survey_design(analytic_data, config)

  # Run analyses
  correlation_results <- compute_bmi_bodyfat_correlation(svy_design)
  bmi_class_results <- compute_bmi_class_stats(svy_design)
  linearity_results <- test_linearity_assumption(svy_design)

  # Save results
  write.csv(correlation_results,
            file.path(output_dir, "corr_bmi_bodyfat_overall_and_by_sex.csv"),
            row.names = FALSE)
  write.csv(bmi_class_results,
            file.path(output_dir, "bodyfat_by_bmi_class_by_sex.csv"),
            row.names = FALSE)

  # Population counts
  pop_counts <- bmi_class_results %>%
    select(bmi_cat, sex, n_unweighted, pop_total)
  write.csv(pop_counts,
            file.path(output_dir, "population_counts_by_group.csv"),
            row.names = FALSE)

  # Create methods documentation
  methods_text <- create_methods_documentation(analytic_data, bodyfat_vars, config, datasets)

  return(list(
    correlation_results = correlation_results,
    bmi_class_results = bmi_class_results,
    linearity_results = linearity_results,
    analytic_data = analytic_data,
    svy_design = svy_design,
    methods_text = methods_text
  ))
}

#' Create methods documentation for the analysis
#'
#' @param analytic_data Processed analytic dataset
#' @param bodyfat_vars List of identified body fat variables
#' @param config Configuration object
#' @param datasets List of NHANES datasets (optional, for exclusion calculation)
#' @return Character string with methods documentation
#' @export
create_methods_documentation <- function(analytic_data, bodyfat_vars, config, datasets = NULL) {
  excluded_count <- nrow(analytic_data)  # Default to final sample size if datasets not provided

  if (!is.null(datasets)) {
    excluded_count <- nrow(datasets$demo %>%
      filter(RIDAGEYR >= config$analysis$age_range[1] &
             RIDAGEYR <= config$analysis$age_range[2]) %>%
      left_join(datasets$bmx %>% select(SEQN, BMXBMI), by = "SEQN") %>%
      left_join(datasets$dxx %>% select_at(c("SEQN", bodyfat_vars$bodyfat_var)), by = "SEQN") %>%
      filter(!is.na(BMXBMI) & !is.na(!!sym(bodyfat_vars$bodyfat_var)))) - nrow(analytic_data)
  }

  methods_text <- paste0(
    "NHANES 2017-2018 BMI vs % Body Fat Analysis Methods\n",
    "==================================================\n\n",
    "Data Source:\n",
    "- Cycles: 2017-2018 only\n",
    "- Files: DEMO_J.XPT, BMX_J.XPT, DXX_J.XPT, DXXAG_J.XPT\n\n",
    "Sample:\n",
    "- Target population: U.S. civilian non-institutionalized adults (",
    config$analysis$age_range[1], "-", config$analysis$age_range[2], " years)\n",
    "- Inclusion: Valid BMI, DXA % body fat, and survey design variables\n",
    "- Final sample size: ", nrow(analytic_data), " participants\n",
    "- Exclusions: ", excluded_count, " due to missing data\n\n",
    "Variables:\n",
    "- BMI: BMXBMI from BMX_J (kg/m²)\n",
    "- Whole-body % body fat: ", bodyfat_vars$bodyfat_var, " from DXX_J\n",
    "- BMI classes: <18.5, [18.5,25), [25,30), [30,35), [35,40), ≥40 kg/m²\n\n",
    "Survey Design:\n",
    "- Weights: ", config$analysis$survey_weights_col, " (2-year MEC examination weights)\n",
    "- Strata: ", config$analysis$strata_col, "\n",
    "- Primary sampling units: ", config$analysis$psu_col, "\n",
    "- Variance estimation: Taylor linearization\n",
    "- Lonely PSU adjustment: applied\n\n",
    "Statistical Methods:\n",
    "- All estimates are survey-weighted with design-based standard errors\n",
    "- Correlations computed from survey-weighted covariance matrix\n",
    "- 95% confidence intervals use design-based standard errors\n",
    "- Quantiles computed using survey methods\n\n",
    "Missing Data:\n",
    "- Complete-case analysis (listwise deletion)\n",
    "- No multiple imputation applied\n",
    "- DXA coverage in 2017-2018 cycle is generally good\n\n",
    "Software:\n",
    "- R version: ", R.version.string, "\n",
    "- survey package version: ", packageVersion("survey"), "\n",
    "- dplyr package version: ", packageVersion("dplyr"), "\n",
    "- Analysis date: ", Sys.Date(), "\n"
  )

  return(methods_text)
}
