# Advanced Statistical Methods for NHANES BMI-Body Fat Analysis

#' Run longitudinal analysis for NHANES data
#'
#' Performs longitudinal analysis if multiple NHANES cycles are available.
#' Currently designed for cross-sectional analysis but extensible for panel data.
#'
#' @param data_list List of datasets from different NHANES cycles
#' @param time_var Variable indicating time/wave (if available)
#' @param config Configuration list
#' @return List containing longitudinal analysis results
#' @export
run_longitudinal_analysis <- function(data_list, time_var = NULL, config = NULL) {
  safe_log("Starting longitudinal analysis", "INFO")

  if (is.null(time_var)) {
    safe_log("No time variable provided - performing cross-sectional trend analysis", "INFO")

    # Analyze trends across cycles if multiple datasets provided
    if (length(data_list) > 1) {
      trend_results <- analyze_cross_sectional_trends(data_list, config)
      return(trend_results)
    } else {
      stop("Longitudinal analysis requires multiple datasets or time variable")
    }
  }

  # If time variable is available, perform true longitudinal analysis
  # This would require linking individual participants across waves
  # For now, implement cross-sectional trend analysis

  trend_results <- analyze_cross_sectional_trends(data_list, config)
  return(trend_results)
}

#' Analyze trends across NHANES cycles
#'
#' Compares BMI-body fat relationships across different NHANES survey cycles.
#'
#' @param data_list List of datasets from different cycles
#' @param config Configuration list
#' @return List containing trend analysis results
#' @export
analyze_cross_sectional_trends <- function(data_list, config) {
  safe_log("Analyzing cross-sectional trends across cycles", "INFO")

  cycle_results <- list()

  for (cycle_name in names(data_list)) {
    cat(sprintf("Analyzing cycle: %s\n", cycle_name))

    data <- data_list[[cycle_name]]

    # Create survey design for this cycle
    svy_design <- create_survey_design(data, config)

    # Compute correlations for this cycle
    correlations <- compute_correlations(svy_design)

    # Store results
    cycle_results[[cycle_name]] <- list(
      correlations = correlations,
      sample_size = nrow(data),
      mean_age = mean(data$RIDAGEYR, na.rm = TRUE),
      mean_bmi = mean(data$BMXBMI, na.rm = TRUE),
      mean_bodyfat = mean(data$bodyfat_pct, na.rm = TRUE)
    )
  }

  # Compare trends across cycles
  trend_comparison <- compare_cycle_trends(cycle_results)

  return(list(
    cycle_results = cycle_results,
    trend_comparison = trend_comparison,
    analysis_type = "cross_sectional_trends",
    timestamp = Sys.time()
  ))
}

#' Compare trends across NHANES cycles
#'
#' Statistical comparison of correlation trends across survey cycles.
#'
#' @param cycle_results Results from analyze_cross_sectional_trends
#' @return List containing trend comparison results
#' @export
compare_cycle_trends <- function(cycle_results) {
  safe_log("Comparing trends across cycles", "INFO")

  cycles <- names(cycle_results)
  correlations <- sapply(cycle_results, function(x) x$correlations$overall$correlation)

  # Simple trend analysis
  trend_lm <- lm(correlations ~ seq_along(correlations))
  trend_summary <- summary(trend_lm)

  # Calculate trend statistics
  trend_direction <- if (trend_summary$coefficients[2, 1] > 0) "increasing" else "decreasing"
  trend_significance <- if (trend_summary$coefficients[2, 4] < 0.05) "significant" else "not significant"

  return(list(
    cycles = cycles,
    correlations = correlations,
    trend_slope = trend_summary$coefficients[2, 1],
    trend_p_value = trend_summary$coefficients[2, 4],
    trend_direction = trend_direction,
    trend_significance = trend_significance,
    trend_r_squared = trend_summary$r.squared
  ))
}

#' Run Bayesian correlation analysis
#'
#' Performs Bayesian analysis of BMI-body fat correlation for uncertainty quantification.
#'
#' @param data Analytic dataset
#' @param config Configuration list
#' @param priors Prior distribution specifications (default: "default")
#' @return List containing Bayesian correlation results
#' @export
run_bayesian_correlation <- function(data, config, priors = "default") {
  safe_log("Starting Bayesian correlation analysis", "INFO")

  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("rstanarm package required for Bayesian analysis")
  }

  if (!requireNamespace("bayesplot", quietly = TRUE)) {
    warning("bayesplot package not available for diagnostic plots")
  }

  # Prepare data
  analysis_data <- data %>%
    select(BMXBMI, bodyfat_pct) %>%
    filter(!is.na(BMXBMI) & !is.na(bodyfat_pct)) %>%
    mutate(
      bmi_scaled = scale(BMXBMI),
      bodyfat_scaled = scale(bodyfat_pct)
    )

  # Set up priors based on specification
  if (priors == "default") {
    # Default weakly informative priors
    prior_correlation <- rstanarm::normal(0.8, 0.2)  # Prior belief around 0.8 correlation
    prior_intercept <- rstanarm::normal(0, 1)
    prior_slope <- rstanarm::normal(1, 0.5)
  } else if (priors == "skeptical") {
    # More skeptical priors
    prior_correlation <- rstanarm::normal(0.5, 0.1)
    prior_intercept <- rstanarm::normal(0, 0.5)
    prior_slope <- rstanarm::normal(1, 0.2)
  } else {
    stop("Unknown prior specification")
  }

  # Fit Bayesian linear regression
  bayesian_model <- rstanarm::stan_glm(
    bodyfat_pct ~ BMXBMI,
    data = analysis_data,
    family = gaussian(),
    prior = rstanarm::normal(0, 1),  # Weakly informative priors
    prior_intercept = prior_intercept,
    prior_aux = rstanarm::exponential(1),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = parallel::detectCores() - 1,
    refresh = 0  # Suppress sampling output
  )

  # Extract correlation from model
  posterior_samples <- as.data.frame(bayesian_model)

  # Calculate implied correlation from regression parameters
  # For standardized variables, correlation ≈ slope coefficient
  correlation_posterior <- posterior_samples$bmi_scaled

  # Bayesian credible intervals
  correlation_ci <- quantile(correlation_posterior, c(0.025, 0.5, 0.975))

  # Model diagnostics
  model_summary <- summary(bayesian_model)

  return(list(
    model = bayesian_model,
    correlation_posterior = correlation_posterior,
    correlation_mean = mean(correlation_posterior),
    correlation_ci = correlation_ci,
    model_summary = model_summary,
    priors_used = priors,
    convergence_diagnostics = list(
      rhat = max(bayesian_model$stanfit@stan_args[[1]]$Rhat),
      neff_ratio = min(bayesian_model$stanfit@stan_args[[1]]$n_eff / bayesian_model$stanfit@stan_args[[1]]$n_iter)
    )
  ))
}

#' Run machine learning analysis for predictive modeling
#'
#' Applies machine learning methods to predict body fat from anthropometric measures.
#'
#' @param data Analytic dataset
#' @param methods Vector of ML methods to apply (default: c("random_forest", "xgboost"))
#' @param config Configuration list
#' @return List containing ML analysis results
#' @export
run_ml_analysis <- function(data, methods = c("random_forest", "xgboost"), config = NULL) {
  safe_log("Starting machine learning analysis", "INFO")

  # Check for required packages
  required_ml_packages <- c("randomForest", "xgboost", "caret", "glmnet")
  missing_packages <- setdiff(required_ml_packages,
                             installed.packages()[, "Package"])

  if (length(missing_packages) > 0) {
    stop(paste("Required ML packages not installed:", paste(missing_packages, collapse = ", ")))
  }

  # Prepare data for ML
  ml_data <- data %>%
    select(BMXBMI, BMXWAIST, RIAGENDR, RIDAGEYR, bodyfat_pct) %>%
    filter(complete.cases(.)) %>%
    mutate(sex = as.factor(RIAGENDR))

  if (nrow(ml_data) < 100) {
    warning("Small sample size for ML analysis. Results may be unreliable.")
  }

  # Split data for training and testing
  set.seed(42)
  train_index <- caret::createDataPartition(ml_data$bodyfat_pct, p = 0.8, list = FALSE)
  train_data <- ml_data[train_index, ]
  test_data <- ml_data[-train_index, ]

  ml_results <- list()

  # Random Forest
  if ("random_forest" %in% methods) {
    safe_log("Training Random Forest model", "INFO")

    rf_model <- randomForest::randomForest(
      bodyfat_pct ~ BMXBMI + BMXWAIST + sex + RIDAGEYR,
      data = train_data,
      ntree = 500,
      importance = TRUE,
      do.trace = FALSE
    )

    # Predictions and performance
    rf_predictions <- predict(rf_model, test_data)
    rf_rmse <- sqrt(mean((test_data$bodyfat_pct - rf_predictions)^2))
    rf_r_squared <- 1 - sum((test_data$bodyfat_pct - rf_predictions)^2) /
                       sum((test_data$bodyfat_pct - mean(test_data$bodyfat_pct))^2)

    ml_results$random_forest <- list(
      model = rf_model,
      predictions = rf_predictions,
      rmse = rf_rmse,
      r_squared = rf_r_squared,
      variable_importance = randomForest::importance(rf_model)
    )
  }

  # XGBoost
  if ("xgboost" %in% methods) {
    safe_log("Training XGBoost model", "INFO")

    # Prepare data for XGBoost
    train_matrix <- xgboost::xgb.DMatrix(
      data = as.matrix(train_data[, c("BMXBMI", "BMXWAIST", "RIAGENDR", "RIDAGEYR")]),
      label = train_data$bodyfat_pct
    )

    test_matrix <- xgboost::xgb.DMatrix(
      data = as.matrix(test_data[, c("BMXBMI", "BMXWAIST", "RIAGENDR", "RIDAGEYR")]),
      label = test_data$bodyfat_pct
    )

    # Train XGBoost model
    xgb_model <- xgboost::xgboost(
      data = train_matrix,
      nrounds = 100,
      max_depth = 6,
      eta = 0.3,
      objective = "reg:squarederror",
      verbose = 0
    )

    # Predictions and performance
    xgb_predictions <- predict(xgb_model, test_matrix)
    xgb_rmse <- sqrt(mean((test_data$bodyfat_pct - xgb_predictions)^2))
    xgb_r_squared <- 1 - sum((test_data$bodyfat_pct - xgb_predictions)^2) /
                        sum((test_data$bodyfat_pct - mean(test_data$bodyfat_pct))^2)

    ml_results$xgboost <- list(
      model = xgb_model,
      predictions = xgb_predictions,
      rmse = xgb_rmse,
      r_squared = xgb_r_squared,
      feature_importance = xgboost::xgb.importance(model = xgb_model)
    )
  }

  # Model comparison
  if (length(ml_results) > 1) {
    model_comparison <- compare_ml_models(ml_results, test_data)
    ml_results$model_comparison <- model_comparison
  }

  return(list(
    ml_results = ml_results,
    train_size = nrow(train_data),
    test_size = nrow(test_data),
    methods_applied = methods,
    timestamp = Sys.time()
  ))
}

#' Compare machine learning models
#'
#' Statistical comparison of different ML model performances.
#'
#' @param ml_results Results from run_ml_analysis
#' @param test_data Test dataset for evaluation
#' @return List containing model comparison results
#' @export
compare_ml_models <- function(ml_results, test_data) {
  safe_log("Comparing ML model performances", "INFO")

  # Extract performance metrics
  model_names <- names(ml_results)
  rmse_values <- numeric(length(model_names))
  r_squared_values <- numeric(length(model_names))

  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model_result <- ml_results[[model_name]]

    rmse_values[i] <- model_result$rmse
    r_squared_values[i] <- model_result$r_squared
  }

  # Determine best performing model
  best_rmse_model <- model_names[which.min(rmse_values)]
  best_rsq_model <- model_names[which.max(r_squared_values)]

  # Statistical comparison (paired t-test for RMSE)
  if (length(model_names) == 2) {
    rmse_comparison <- t.test(rmse_values[1], rmse_values[2], paired = TRUE)
    rsq_comparison <- t.test(r_squared_values[1], r_squared_values[2], paired = TRUE)
  } else {
    rmse_comparison <- NULL
    rsq_comparison <- NULL
  }

  return(list(
    model_names = model_names,
    rmse_values = rmse_values,
    r_squared_values = r_squared_values,
    best_rmse_model = best_rmse_model,
    best_rsq_model = best_rsq_model,
    rmse_comparison = rmse_comparison,
    rsq_comparison = rsq_comparison,
    performance_summary = data.frame(
      Model = model_names,
      RMSE = rmse_values,
      R_Squared = r_squared_values
    )
  ))
}

#' Run advanced regression analysis
#'
#' Performs comprehensive regression analysis including linear, logistic, and quantile regression.
#'
#' @param data Analytic dataset
#' @param outcome_var Outcome variable name
#' @param predictor_vars Vector of predictor variable names
#' @param config Configuration list
#' @return List containing regression analysis results
#' @export
run_advanced_regression <- function(data, outcome_var, predictor_vars, config) {
  safe_log("Starting advanced regression analysis", "INFO")

  # Check for required packages
  required_packages <- c("quantreg", "lme4", "nlme")
  missing_packages <- setdiff(required_packages,
                             installed.packages()[, "Package"])

  if (length(missing_packages) > 0) {
    stop(paste("Required regression packages not installed:", paste(missing_packages, collapse = ", ")))
  }

  regression_results <- list()

  # 1. Linear Regression (Ordinary Least Squares)
  safe_log("Fitting linear regression model", "INFO")
  lm_formula <- as.formula(paste(outcome_var, "~",
                                paste(predictor_vars, collapse = " + ")))

  lm_model <- lm(lm_formula, data = data)
  lm_summary <- summary(lm_model)

  regression_results$linear <- list(
    model = lm_model,
    summary = lm_summary,
    coefficients = lm_summary$coefficients,
    r_squared = lm_summary$r.squared,
    adj_r_squared = lm_summary$adj.r.squared,
    aic = AIC(lm_model),
    bic = BIC(lm_model)
  )

  # 2. Survey-weighted Linear Regression
  safe_log("Fitting survey-weighted linear regression", "INFO")
  svy_design <- create_survey_design(data, config)
  svy_formula <- as.formula(paste(outcome_var, "~",
                                 paste(predictor_vars, collapse = " + ")))

  svy_lm_model <- survey::svyglm(svy_formula, design = svy_design)
  svy_lm_summary <- summary(svy_lm_model)

  regression_results$survey_weighted <- list(
    model = svy_lm_model,
    summary = svy_lm_summary,
    coefficients = svy_lm_summary$coefficients
  )

  # 3. Quantile Regression (for robust estimates)
  safe_log("Fitting quantile regression models", "INFO")
  quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  rq_models <- list()

  for (tau in quantiles) {
    rq_model <- quantreg::rq(lm_formula, data = data, tau = tau)
    rq_models[[paste0("tau_", tau)]] <- rq_model
  }

  regression_results$quantile <- list(
    models = rq_models,
    quantiles = quantiles
  )

  # 4. Model Comparison
  model_comparison <- compare_regression_models(regression_results)

  return(list(
    regression_results = regression_results,
    model_comparison = model_comparison,
    outcome_variable = outcome_var,
    predictor_variables = predictor_vars,
    sample_size = nrow(data),
    timestamp = Sys.time()
  ))
}

#' Compare regression models
#'
#' Statistical comparison of different regression model types.
#'
#' @param regression_results Results from run_advanced_regression
#' @return List containing model comparison results
#' @export
compare_regression_models <- function(regression_results) {
  safe_log("Comparing regression model performance", "INFO")

  # Extract AIC and BIC values
  model_names <- names(regression_results)
  aic_values <- numeric(length(model_names))
  bic_values <- numeric(length(model_names))

  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    if (model_name == "linear") {
      aic_values[i] <- regression_results[[model_name]]$aic
      bic_values[i] <- regression_results[[model_name]]$bic
    } else if (model_name == "quantile") {
      # For quantile regression, use average AIC across quantiles
      quantile_aics <- sapply(regression_results[[model_name]]$models, AIC)
      aic_values[i] <- mean(quantile_aics)
      bic_values[i] <- mean(quantile_aics) + 2 * log(nrow(regression_results[[model_name]]$models[[1]]$model))
    }
  }

  # Determine best models by AIC and BIC
  best_aic_model <- model_names[which.min(aic_values)]
  best_bic_model <- model_names[which.min(bic_values)]

  return(list(
    model_names = model_names,
    aic_values = aic_values,
    bic_values = bic_values,
    best_aic_model = best_aic_model,
    best_bic_model = best_bic_model,
    model_comparison = data.frame(
      Model = model_names,
      AIC = aic_values,
      BIC = bic_values,
      AIC_Delta = aic_values - min(aic_values),
      BIC_Delta = bic_values - min(bic_values)
    )
  ))
}

#' Run sensitivity analysis for different methodological choices
#'
#' Tests robustness of results to different analytical decisions.
#'
#' @param data Analytic dataset
#' @param config Configuration list
#' @param sensitivity_tests Vector of sensitivity tests to run
#' @return List containing sensitivity analysis results
#' @export
run_sensitivity_analysis <- function(data, config, sensitivity_tests = c("age_groups", "bmi_categories", "exclusions")) {
  safe_log("Starting sensitivity analysis", "INFO")

  sensitivity_results <- list()

  # 1. Age group sensitivity
  if ("age_groups" %in% sensitivity_tests) {
    safe_log("Testing sensitivity to age group definitions", "INFO")

    age_sensitivity <- list()

    # Different age groupings
    age_groups <- list(
      "20-39_vs_40-59" = c(20, 39, 40, 59),
      "20-49_vs_50-59" = c(20, 49, 50, 59),
      "18-64" = c(18, 64)  # Standard working age
    )

    for (group_name in names(age_groups)) {
      group_breaks <- age_groups[[group_name]]

      if (length(group_breaks) == 2) {
        # Single age range
        group_data <- data %>%
          filter(RIDAGEYR >= group_breaks[1] & RIDAGEYR <= group_breaks[2])
      } else {
        # Two age groups for comparison
        young_data <- data %>%
          filter(RIDAGEYR >= group_breaks[1] & RIDAGEYR <= group_breaks[2])
        old_data <- data %>%
          filter(RIDAGEYR >= group_breaks[3] & RIDAGEYR <= group_breaks[4])

        # Create designs and compute correlations
        if (nrow(young_data) > 50 && nrow(old_data) > 50) {
          young_design <- create_survey_design(young_data, config)
          old_design <- create_survey_design(old_data, config)

          young_corr <- compute_correlations(young_design)
          old_corr <- compute_correlations(old_design)

          age_sensitivity[[group_name]] <- list(
            young_correlation = young_corr$overall$correlation,
            old_correlation = old_corr$overall$correlation,
            correlation_difference = young_corr$overall$correlation - old_corr$overall$correlation
          )
        }
      }
    }

    sensitivity_results$age_groups <- age_sensitivity
  }

  # 2. BMI category sensitivity
  if ("bmi_categories" %in% sensitivity_tests) {
    safe_log("Testing sensitivity to BMI category definitions", "INFO")

    bmi_categories <- list(
      "WHO_standard" = c(18.5, 25, 30),  # Standard WHO categories
      "CDC_extended" = c(18.5, 25, 30, 35, 40),  # Extended categories
      "Clinical" = c(20, 25, 30, 35)  # Clinical cutpoints
    )

    bmi_sensitivity <- list()

    for (cat_name in names(bmi_categories)) {
      cutpoints <- bmi_categories[[cat_name]]

      # Create BMI categories
      bmi_cats <- cut(data$BMXBMI,
                     breaks = c(-Inf, cutpoints, Inf),
                     labels = c("Underweight", paste("Category", seq_along(cutpoints)), "Very Obese"))

      # Analyze by BMI category
      category_results <- list()

      for (cat in levels(bmi_cats)) {
        cat_data <- data[bmi_cats == cat, ]
        if (nrow(cat_data) > 20) {  # Minimum sample size
          cat_design <- create_survey_design(cat_data, config)
          cat_correlations <- compute_correlations(cat_design)
          category_results[[cat]] <- cat_correlations$overall$correlation
        }
      }

      bmi_sensitivity[[cat_name]] <- category_results
    }

    sensitivity_results$bmi_categories <- bmi_sensitivity
  }

  # 3. Exclusion criteria sensitivity
  if ("exclusions" %in% sensitivity_tests) {
    safe_log("Testing sensitivity to exclusion criteria", "INFO")

    exclusion_scenarios <- list(
      "base_case" = list(),  # No additional exclusions
      "strict_age" = list(age_range = c(25, 55)),  # Narrower age range
      "complete_cases_only" = list(require_complete = TRUE),  # Only complete cases
      "no_extremes" = list(remove_extremes = TRUE)  # Remove extreme BMI values
    )

    exclusion_results <- list()

    for (scenario_name in names(exclusion_scenarios)) {
      scenario_params <- exclusion_scenarios[[scenario_name]]

      # Apply scenario-specific exclusions
      scenario_data <- data

      if (!is.null(scenario_params$age_range)) {
        scenario_data <- scenario_data %>%
          filter(RIDAGEYR >= scenario_params$age_range[1] &
                 RIDAGEYR <= scenario_params$age_range[2])
      }

      if (!is.null(scenario_params$require_complete)) {
        scenario_data <- scenario_data %>%
          filter(complete.cases(.))
      }

      if (!is.null(scenario_params$remove_extremes)) {
        # Remove extreme BMI values (outside 2.5-97.5 percentiles)
        bmi_limits <- quantile(scenario_data$BMXBMI, c(0.025, 0.975), na.rm = TRUE)
        scenario_data <- scenario_data %>%
          filter(BMXBMI >= bmi_limits[1] & BMXBMI <= bmi_limits[2])
      }

      # Compute correlations for this scenario
      if (nrow(scenario_data) > 50) {
        scenario_design <- create_survey_design(scenario_data, config)
        scenario_correlations <- compute_correlations(scenario_design)

        exclusion_results[[scenario_name]] <- list(
          correlation = scenario_correlations$overall$correlation,
          sample_size = nrow(scenario_data),
          mean_bmi = mean(scenario_data$BMXBMI, na.rm = TRUE),
          mean_bodyfat = mean(scenario_data$bodyfat_pct, na.rm = TRUE)
        )
      }
    }

    sensitivity_results$exclusions <- exclusion_results
  }

  return(list(
    sensitivity_results = sensitivity_results,
    tests_performed = sensitivity_tests,
    timestamp = Sys.time()
  ))
}

#' Run complete advanced analysis pipeline
#'
#' Orchestrates all advanced statistical methods for comprehensive analysis.
#'
#' @param data Analytic dataset
#' @param config Configuration list
#' @param analysis_types Vector of analysis types to run
#' @return List containing all advanced analysis results
#' @export
run_complete_advanced_analysis <- function(data, config,
                                         analysis_types = c("longitudinal", "bayesian", "ml", "regression", "sensitivity")) {

  safe_log("Starting complete advanced analysis pipeline", "INFO")

  advanced_results <- list()

  # 1. Longitudinal/Cross-sectional trend analysis
  if ("longitudinal" %in% analysis_types) {
    safe_log("Running longitudinal analysis", "INFO")
    # For now, use cross-sectional trend analysis
    # In future, this could be extended to true longitudinal analysis
    trend_data <- list("current_cycle" = data)
    advanced_results$longitudinal <- run_longitudinal_analysis(trend_data, config = config)
  }

  # 2. Bayesian analysis
  if ("bayesian" %in% analysis_types) {
    safe_log("Running Bayesian correlation analysis", "INFO")
    advanced_results$bayesian <- run_bayesian_correlation(data, config)
  }

  # 3. Machine learning analysis
  if ("ml" %in% analysis_types) {
    safe_log("Running machine learning analysis", "INFO")
    advanced_results$ml <- run_ml_analysis(data, config = config)
  }

  # 4. Advanced regression analysis
  if ("regression" %in% analysis_types) {
    safe_log("Running advanced regression analysis", "INFO")
    predictor_vars <- c("BMXBMI", "RIAGENDR", "RIDAGEYR")
    if ("BMXWAIST" %in% names(data)) {
      predictor_vars <- c(predictor_vars, "BMXWAIST")
    }
    advanced_results$regression <- run_advanced_regression(data, "bodyfat_pct", predictor_vars, config)
  }

  # 5. Sensitivity analysis
  if ("sensitivity" %in% analysis_types) {
    safe_log("Running sensitivity analysis", "INFO")
    advanced_results$sensitivity <- run_sensitivity_analysis(data, config)
  }

  # Create comprehensive summary
  summary <- create_advanced_analysis_summary(advanced_results)

  return(list(
    advanced_results = advanced_results,
    summary = summary,
    analysis_types = analysis_types,
    timestamp = Sys.time(),
    sample_size = nrow(data)
  ))
}

#' Create summary of advanced analysis results
#'
#' Generates comprehensive summary of all advanced analysis results.
#'
#' @param advanced_results Results from run_complete_advanced_analysis
#' @return List containing analysis summary
#' @export
create_advanced_analysis_summary <- function(advanced_results) {
  safe_log("Creating advanced analysis summary", "INFO")

  summary <- list()

  # Basic correlation from main analysis (for comparison)
  if (!is.null(advanced_results$longitudinal$cycle_results$current_cycle)) {
    basic_corr <- advanced_results$longitudinal$cycle_results$current_cycle$correlations$overall$correlation
  } else {
    basic_corr <- NA
  }

  # Bayesian results
  if (!is.null(advanced_results$bayesian)) {
    bayesian_corr <- advanced_results$bayesian$correlation_mean
    bayesian_ci <- advanced_results$bayesian$correlation_ci
  } else {
    bayesian_corr <- NA
    bayesian_ci <- c(NA, NA, NA)
  }

  # ML results
  if (!is.null(advanced_results$ml)) {
    ml_performance <- advanced_results$ml$ml_results$model_comparison$performance_summary
    best_ml_model <- ml_performance$Model[which.max(ml_performance$R_Squared)]
    best_ml_rsq <- max(ml_performance$R_Squared)
  } else {
    best_ml_model <- NA
    best_ml_rsq <- NA
  }

  # Regression results
  if (!is.null(advanced_results$regression)) {
    regression_comparison <- advanced_results$regression$model_comparison$model_comparison
    best_regression_model <- regression_comparison$Model[which.min(regression_comparison$AIC)]
  } else {
    best_regression_model <- NA
  }

  summary <- list(
    basic_correlation = basic_corr,
    bayesian_correlation = bayesian_corr,
    bayesian_ci = bayesian_ci,
    best_ml_model = best_ml_model,
    best_ml_r_squared = best_ml_rsq,
    best_regression_model = best_regression_model,
    analysis_completeness = list(
      longitudinal = !is.null(advanced_results$longitudinal),
      bayesian = !is.null(advanced_results$bayesian),
      ml = !is.null(advanced_results$ml),
      regression = !is.null(advanced_results$regression),
      sensitivity = !is.null(advanced_results$sensitivity)
    )
  )

  return(summary)
}
