#' @title Causal Inference Framework for NHANES BMI-Body Fat Data
#' @description Advanced causal inference methods for estimating treatment effects and causal relationships
#' @import dplyr
#' @import ggplot2
#' @import MatchIt
#' @import WeightIt
#' @import cobalt
#' @import estimatr
#' @import survey
#' @export

# Load configuration and utilities
source("scripts/load_config.R")
source("scripts/error_handling.R")
source("R/performance.R")

#' Propensity Score Matching for Causal Inference
#'
#' @param data Analytic dataset
#' @param treatment Treatment variable (e.g., "obesity")
#' @param covariates Vector of covariate names for matching
#' @param method Matching method ("nearest", "optimal", "genetic")
#' @param ratio Matching ratio (1:1, 1:2, etc.)
#' @return Matching results and balance diagnostics
#' @export
propensity_score_matching <- function(data, treatment = "obesity",
                                     covariates = c("age_centered", "sex", "race_ethnicity"),
                                     method = "nearest", ratio = 1) {

  tryCatch({
    # Validate inputs
    if (!treatment %in% names(data)) {
      stop(NhanesError(
        paste("Treatment variable not found:", treatment),
        code = "MISSING_TREATMENT_VAR",
        details = list(treatment = treatment, available = names(data))
      ))
    }

    missing_covariates <- setdiff(covariates, names(data))
    if (length(missing_covariates) > 0) {
      stop(NhanesError(
        paste("Missing covariates:", paste(missing_covariates, collapse = ", ")),
        code = "MISSING_COVARIATES",
        details = list(missing = missing_covariates, available = names(data))
      ))
    }

    # Create propensity score model formula
    formula_str <- paste(treatment, "~", paste(covariates, collapse = " + "))
    formula <- as.formula(formula_str)

    # Fit propensity score model
    ps_model <- glm(formula, data = data, family = binomial())

    # Add propensity scores to data
    data$propensity_score <- predict(ps_model, type = "response")

    # Perform matching
    match_formula <- as.formula(paste(treatment, "~ propensity_score"))

    match_result <- matchit(
      match_formula,
      data = data,
      method = method,
      ratio = ratio,
      caliper = 0.2,  # 0.2 standard deviation caliper
      replace = FALSE,
      discard = "both"  # Discard treated units outside common support
    )

    # Get matched data
    matched_data <- match.data(match_result)

    # Assess balance
    balance <- bal.tab(match_result, thresholds = c(m = 0.1), un = TRUE)

    # Calculate treatment effect
    treatment_effect <- lm_robust(
      bodyfat_pct ~ obesity,
      data = matched_data,
      weights = matched_data$weights
    )

    cat("Propensity score matching completed\n")
    cat(sprintf("Matched sample size: %d (originally %d)\n", nrow(matched_data), nrow(data)))
    cat(sprintf("Standardized mean difference (SMD) max: %.3f\n", max(abs(balance$Balance$Diff.Un))))

    return(list(
      match_result = match_result,
      matched_data = matched_data,
      propensity_model = ps_model,
      balance = balance,
      treatment_effect = treatment_effect,
      original_n = nrow(data),
      matched_n = nrow(matched_data)
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Propensity score matching failed:", e$message),
        code = "PSM_ERROR",
        details = list(treatment = treatment, covariates = covariates, original_error = e$message)
      ))
    }
  })
}

#' Inverse Probability Weighting (IPW) for Causal Inference
#'
#' @param data Analytic dataset
#' @param treatment Treatment variable
#' @param covariates Vector of covariate names for weighting
#' @param method Weighting method ("ps" for propensity score, "cbps" for covariate balancing)
#' @return IPW results and diagnostics
#' @export
inverse_probability_weighting <- function(data, treatment = "obesity",
                                         covariates = c("age_centered", "sex", "race_ethnicity"),
                                         method = "ps") {

  tryCatch({
    # Validate inputs
    if (!treatment %in% names(data)) {
      stop(NhanesError(
        paste("Treatment variable not found:", treatment),
        code = "MISSING_TREATMENT_VAR"
      ))
    }

    # Create propensity score model formula
    formula_str <- paste(treatment, "~", paste(covariates, collapse = " + "))
    formula <- as.formula(formula_str)

    # Estimate weights
    if (method == "ps") {
      weights_result <- weightit(
        formula,
        data = data,
        method = "ps",  # Propensity score weighting
        estimand = "ATE",  # Average treatment effect
        stabilize = TRUE
      )
    } else if (method == "cbps") {
      weights_result <- weightit(
        formula,
        data = data,
        method = "cbps",  # Covariate balancing propensity score
        estimand = "ATE"
      )
    } else {
      stop(NhanesError("Method must be 'ps' or 'cbps'",
                      code = "INVALID_WEIGHTING_METHOD"))
    }

    # Add weights to data
    weighted_data <- data
    weighted_data$ipw_weights <- weights_result$weights

    # Assess balance
    balance <- bal.tab(weights_result, thresholds = c(m = 0.1), un = TRUE)

    # Calculate weighted treatment effect
    treatment_effect <- lm_robust(
      bodyfat_pct ~ obesity,
      data = weighted_data,
      weights = weighted_data$ipw_weights
    )

    # Calculate effective sample size
    ess_treated <- sum(weighted_data$ipw_weights[weighted_data$obesity == 1])
    ess_control <- sum(weighted_data$ipw_weights[weighted_data$obesity == 0])
    ess_total <- ess_treated + ess_control

    cat("Inverse probability weighting completed\n")
    cat(sprintf("Effective sample size: %.0f (originally %d)\n", ess_total, nrow(data)))
    cat(sprintf("SMD max: %.3f\n", max(abs(balance$Balance$Diff.Un))))

    return(list(
      weights_result = weights_result,
      weighted_data = weighted_data,
      balance = balance,
      treatment_effect = treatment_effect,
      ess_treated = ess_treated,
      ess_control = ess_control,
      ess_total = ess_total,
      method = method
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Inverse probability weighting failed:", e$message),
        code = "IPW_ERROR",
        details = list(treatment = treatment, method = method, original_error = e$message)
      ))
    }
  })
}

#' Instrumental Variable Analysis
#'
#' @param data Analytic dataset
#' @param outcome Outcome variable (bodyfat_pct)
#' @param treatment Treatment variable (bmi)
#' @param instrument Instrumental variable (e.g., genetic factors if available)
#' @param covariates Vector of covariate names
#' @return IV analysis results
#' @export
instrumental_variable_analysis <- function(data, outcome = "bodyfat_pct",
                                          treatment = "bmi",
                                          instrument = NULL,
                                          covariates = c("age_centered", "sex", "race_ethnicity")) {

  tryCatch({
    # For demonstration, we'll use age as a potential instrument
    # In practice, you'd want a true instrumental variable
    if (is.null(instrument)) {
      instrument <- "age_centered"
      cat("Using age_centered as instrumental variable for demonstration\n")
    }

    # Validate variables exist
    required_vars <- c(outcome, treatment, instrument, covariates)
    missing_vars <- setdiff(required_vars, names(data))

    if (length(missing_vars) > 0) {
      stop(NhanesError(
        paste("Missing required variables:", paste(missing_vars, collapse = ", ")),
        code = "MISSING_IV_VARS",
        details = list(missing = missing_vars, required = required_vars)
      ))
    }

    # First stage: Treatment ~ Instrument + Covariates
    first_stage_formula <- as.formula(paste(treatment, "~", instrument, "+", paste(covariates, collapse = " + ")))
    first_stage <- lm(first_stage_formula, data = data)

    # Add predicted treatment to data
    data$predicted_bmi <- predict(first_stage)

    # Second stage: Outcome ~ Predicted Treatment + Covariates
    second_stage_formula <- as.formula(paste(outcome, "~ predicted_bmi +", paste(covariates, collapse = " + ")))
    second_stage <- lm(second_stage_formula, data = data)

    # Calculate F-statistic for instrument strength
    f_stat <- summary(first_stage)$fstatistic[1]

    # Weak instrument test (F > 10 is typically required)
    weak_instrument <- f_stat < 10

    if (weak_instrument) {
      warning(NhanesWarning(
        paste("Weak instrument detected (F =", round(f_stat, 2), "< 10)"),
        code = "WEAK_INSTRUMENT",
        details = list(f_stat = f_stat, threshold = 10)
      ))
    }

    cat("Instrumental variable analysis completed\n")
    cat(sprintf("First stage F-statistic: %.2f\n", f_stat))
    cat(sprintf("Treatment effect estimate: %.3f\n", coef(second_stage)["predicted_bmi"]))

    return(list(
      first_stage = first_stage,
      second_stage = second_stage,
      f_statistic = f_stat,
      weak_instrument = weak_instrument,
      treatment_effect = coef(second_stage)["predicted_bmi"],
      se_treatment_effect = summary(second_stage)$coefficients["predicted_bmi", "Std. Error"]
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Instrumental variable analysis failed:", e$message),
        code = "IV_ERROR",
        details = list(outcome = outcome, treatment = treatment, instrument = instrument, original_error = e$message)
      ))
    }
  })
}

#' Sensitivity Analysis for Unobserved Confounding
#'
#' @param data Analytic dataset
#' @param treatment Treatment variable
#' @param outcome Outcome variable
#' @param covariates Vector of observed covariates
#' @param sensitivity_range Range of sensitivity parameters to test
#' @return Sensitivity analysis results
#' @export
sensitivity_analysis_unobserved <- function(data, treatment = "obesity", outcome = "bodyfat_pct",
                                           covariates = c("age_centered", "sex", "race_ethnicity"),
                                           sensitivity_range = seq(1, 3, by = 0.5)) {

  tryCatch({
    # Base model without unobserved confounding
    base_formula <- as.formula(paste(outcome, "~", treatment, "+", paste(covariates, collapse = " + ")))
    base_model <- lm(base_formula, data = data)
    base_effect <- coef(base_model)[treatment]

    # Sensitivity analysis for different levels of unobserved confounding
    sensitivity_results <- data.frame(
      sensitivity_param = sensitivity_range,
      adjusted_effect = numeric(length(sensitivity_range)),
      bias_factor = numeric(length(sensitivity_range)),
      still_significant = logical(length(sensitivity_range))
    )

    for (i in seq_along(sensitivity_range)) {
      gamma <- sensitivity_range[i]

      # Simple sensitivity analysis using Rosenbaum bounds approach
      # This is a simplified version - in practice, you'd use more sophisticated methods

      # Calculate bias factor
      # For binary treatment, the bias factor relates to the odds ratio of selection into treatment
      bias_factor <- gamma / (1 + gamma * (1 - gamma) / gamma)  # Simplified calculation

      # Adjust effect estimate
      adjusted_effect <- base_effect / bias_factor

      # Check if still statistically significant (simplified)
      se_adjusted <- summary(base_model)$coefficients[treatment, "Std. Error"] / sqrt(bias_factor)
      t_stat_adjusted <- abs(adjusted_effect) / se_adjusted
      still_significant <- t_stat_adjusted > 1.96

      sensitivity_results$adjusted_effect[i] <- adjusted_effect
      sensitivity_results$bias_factor[i] <- bias_factor
      sensitivity_results$still_significant[i] <- still_significant
    }

    cat("Sensitivity analysis for unobserved confounding completed\n")
    cat(sprintf("Base treatment effect: %.3f\n", base_effect))
    cat(sprintf("Effect remains significant up to gamma = %.1f\n",
                max(sensitivity_results$sensitivity_param[sensitivity_results$still_significant])))

    return(list(
      base_model = base_model,
      base_effect = base_effect,
      sensitivity_results = sensitivity_results,
      max_gamma_significant = max(sensitivity_results$sensitivity_param[sensitivity_results$still_significant])
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Sensitivity analysis failed:", e$message),
        code = "SENSITIVITY_ERROR",
        details = list(treatment = treatment, outcome = outcome, original_error = e$message)
      ))
    }
  })
}

#' Generate Causal Inference Report
#'
#' @param causal_results Results from causal inference functions
#' @param output_dir Output directory for plots and tables
#' @return Report generation results
#' @export
generate_causal_report <- function(causal_results, output_dir = "outputs") {

  tryCatch({
    # Create output directories
    plots_dir <- file.path(output_dir, "figures", "causal")
    tables_dir <- file.path(output_dir, "tables")
    logs_dir <- file.path(output_dir, "logs")
    dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

    # Generate plots for each analysis type
    plots <- list()

    # Balance plot (if propensity score matching was used)
    if ("balance" %in% names(causal_results)) {
      plots$balance <- love.plot(causal_results$balance, thresholds = c(m = 0.1)) +
        labs(title = "Covariate Balance After Matching")
    }

    # Sensitivity analysis plot (if sensitivity analysis was performed)
    if ("sensitivity_results" %in% names(causal_results)) {
      plots$sensitivity <- ggplot(causal_results$sensitivity_results,
                                  aes(x = sensitivity_param, y = adjusted_effect)) +
        geom_line() +
        geom_point() +
        geom_hline(yintercept = causal_results$base_effect, linetype = "dashed", color = "red") +
        labs(title = "Sensitivity Analysis: Treatment Effect vs Unobserved Confounding",
             x = "Sensitivity Parameter (Gamma)",
             y = "Adjusted Treatment Effect") +
        theme_minimal()
    }

    # Save plots
    for (plot_name in names(plots)) {
      plot_file <- file.path(plots_dir, paste0("causal_", plot_name, ".png"))
      ggsave(plot_file, plots[[plot_name]], width = 10, height = 6, dpi = 300)
    }

    # Generate summary tables
    if ("treatment_effect" %in% names(causal_results)) {
      # Treatment effect summary
      effect_summary <- data.frame(
        method = class(causal_results$treatment_effect)[1],
        estimate = coef(causal_results$treatment_effect)[2],
        std_error = summary(causal_results$treatment_effect)$coefficients[2, "Std. Error"],
        p_value = summary(causal_results$treatment_effect)$coefficients[2, "Pr(>|t|)"],
        conf_low = confint(causal_results$treatment_effect)[2, 1],
        conf_high = confint(causal_results$treatment_effect)[2, 2]
      )

      write.csv(effect_summary,
                file.path(tables_dir, "causal_treatment_effect.csv"),
                row.names = FALSE)
    }

    # Generate comprehensive report
    report_content <- paste0(
      "Causal Inference Analysis Report\n",
      "===============================\n\n",
      "Analysis Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
    )

    # Add method-specific results
    if ("treatment_effect" %in% names(causal_results)) {
      report_content <- paste0(report_content,
        "Treatment Effect Results:\n",
        "========================\n",
        sprintf("Method: %s\n", class(causal_results$treatment_effect)[1]),
        sprintf("Estimate: %.3f\n", coef(causal_results$treatment_effect)[2]),
        sprintf("95%% CI: [%.3f, %.3f]\n",
                confint(causal_results$treatment_effect)[2, 1],
                confint(causal_results$treatment_effect)[2, 2]),
        sprintf("P-value: %.3f\n\n", summary(causal_results$treatment_effect)$coefficients[2, "Pr(>|t|)"])
      )
    }

    if ("f_statistic" %in% names(causal_results)) {
      report_content <- paste0(report_content,
        "Instrumental Variable Analysis:\n",
        "==============================\n",
        sprintf("First stage F-statistic: %.2f\n", causal_results$f_statistic),
        sprintf("Instrument strength: %s\n", if(causal_results$weak_instrument) "Weak" else "Strong"),
        sprintf("Treatment effect: %.3f\n\n", causal_results$treatment_effect)
      )
    }

    if ("max_gamma_significant" %in% names(causal_results)) {
      report_content <- paste0(report_content,
        "Sensitivity Analysis:\n",
        "====================\n",
        sprintf("Effect remains significant up to gamma = %.1f\n", causal_results$max_gamma_significant),
        sprintf("Base treatment effect: %.3f\n\n", causal_results$base_effect)
      )
    }

    # Write report
    report_file <- file.path(logs_dir, "causal_inference_report.txt")
    writeLines(report_content, report_file)

    cat("Causal inference report generated\n")
    cat("Plots saved to:", plots_dir, "\n")
    cat("Tables saved to:", tables_dir, "\n")
    cat("Report saved to:", report_file, "\n")

    return(list(
      plots = plots,
      report_file = report_file,
      plots_dir = plots_dir,
      tables_dir = tables_dir,
      logs_dir = logs_dir
    ))

  }, error = function(e) {
    stop(NhanesError(
      paste("Causal inference report generation failed:", e$message),
      code = "CAUSAL_REPORT_ERROR",
      details = list(original_error = e$message)
    ))
  })
}

