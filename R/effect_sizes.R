#' @title Effect Size Calculations for NHANES BMI-Body Fat Data
#' @description Standardized effect size calculations and interpretation for statistical results
#' @import dplyr
#' @import ggplot2
#' @import pwr
#' @import MBESS
#' @import effectsize
#' @export

# Load configuration and utilities
source("scripts/load_config.R")
source("scripts/error_handling.R")
source("R/performance.R")

#' Calculate Cohen's d for continuous outcomes
#'
#' @param data Analytic dataset
#' @param outcome Outcome variable name
#' @param group Grouping variable name
#' @param paired Logical indicating if data are paired
#' @return Cohen's d effect size with confidence intervals
#' @export
calculate_cohens_d <- function(data, outcome = "bodyfat_pct", group = "obesity",
                              paired = FALSE) {

  tryCatch({
    # Validate inputs
    if (!outcome %in% names(data) || !group %in% names(data)) {
      stop(NhanesError(
        paste("Variables not found in data:", outcome, group),
        code = "MISSING_EFFECT_VARS",
        details = list(outcome = outcome, group = group, available = names(data))
      ))
    }

    # Extract outcome values by group
    outcome_treated <- data[[outcome]][data[[group]] == 1]
    outcome_control <- data[[outcome]][data[[group]] == 0]

    # Remove missing values
    outcome_treated <- outcome_treated[!is.na(outcome_treated)]
    outcome_control <- outcome_control[!is.na(outcome_control)]

    if (length(outcome_treated) == 0 || length(outcome_control) == 0) {
      stop(NhanesError(
        "No valid outcome data for one or both groups",
        code = "INSUFFICIENT_DATA",
        details = list(treated_n = length(outcome_treated), control_n = length(outcome_control))
      ))
    }

    # Calculate Cohen's d
    if (paired) {
      # For paired data
      cohens_d <- MBESS::smd(
        Mean.1 = mean(outcome_treated),
        Mean.2 = mean(outcome_control),
        s.1 = sd(outcome_treated),
        s.2 = sd(outcome_control),
        n.1 = length(outcome_treated),
        n.2 = length(outcome_control),
        Unbiased = TRUE
      )
    } else {
      # For independent samples
      cohens_d <- effectsize::cohens_d(
        outcome_treated,
        outcome_control,
        pooled_sd = TRUE,
        ci = 0.95
      )
    }

    # Calculate pooled standard deviation
    pooled_sd <- sqrt(
      ((length(outcome_treated) - 1) * var(outcome_treated) +
       (length(outcome_control) - 1) * var(outcome_control)) /
      (length(outcome_treated) + length(outcome_control) - 2)
    )

    # Effect size interpretation
    d_interpretation <- case_when(
      abs(cohens_d$Cohens_d) < 0.2 ~ "Negligible",
      abs(cohens_d$Cohens_d) < 0.5 ~ "Small",
      abs(cohens_d$Cohens_d) < 0.8 ~ "Medium",
      TRUE ~ "Large"
    )

    cat("Cohen's d calculated successfully\n")
    cat(sprintf("Effect size: %.3f (%s)\n", cohens_d$Cohens_d, d_interpretation))
    cat(sprintf("95%% CI: [%.3f, %.3f]\n", cohens_d$CI_low, cohens_d$CI_high))

    return(list(
      cohens_d = cohens_d,
      interpretation = d_interpretation,
      pooled_sd = pooled_sd,
      group_stats = list(
        treated = list(mean = mean(outcome_treated), sd = sd(outcome_treated), n = length(outcome_treated)),
        control = list(mean = mean(outcome_control), sd = sd(outcome_control), n = length(outcome_control))
      ),
      method = if(paired) "paired" else "independent"
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Cohen's d calculation failed:", e$message),
        code = "COHENS_D_ERROR",
        details = list(outcome = outcome, group = group, original_error = e$message)
      ))
    }
  })
}

#' Calculate Odds Ratios for binary outcomes
#'
#' @param data Analytic dataset
#' @param outcome Binary outcome variable
#' @param exposure Exposure variable
#' @param covariates Vector of covariate names for adjustment
#' @return Odds ratio with confidence intervals
#' @export
calculate_odds_ratio <- function(data, outcome = "high_bodyfat", exposure = "obesity",
                                covariates = c("age_centered", "sex")) {

  tryCatch({
    # Validate inputs
    if (!outcome %in% names(data) || !exposure %in% names(data)) {
      stop(NhanesError(
        paste("Variables not found in data:", outcome, exposure),
        code = "MISSING_OR_VARS"
      ))
    }

    # Convert to factors if needed
    data[[outcome]] <- as.factor(data[[outcome]])
    data[[exposure]] <- as.factor(data[[exposure]])

    # Create formula for logistic regression
    if (length(covariates) > 0) {
      formula_str <- paste(outcome, "~", exposure, "+", paste(covariates, collapse = " + "))
    } else {
      formula_str <- paste(outcome, "~", exposure)
    }
    formula <- as.formula(formula_str)

    # Fit logistic regression model
    logit_model <- glm(formula, data = data, family = binomial())

    # Extract odds ratio for exposure
    exposure_coef <- summary(logit_model)$coefficients[exposure, ]
    odds_ratio <- exp(exposure_coef["Estimate"])
    se_log_or <- exposure_coef["Std. Error"]
    ci_log_or <- confint(logit_model, exposure)
    ci_or <- exp(ci_log_or)

    # Calculate p-value
    z_stat <- exposure_coef["Estimate"] / exposure_coef["Std. Error"]
    p_value <- 2 * (1 - pnorm(abs(z_stat)))

    # Odds ratio interpretation
    or_interpretation <- case_when(
      odds_ratio < 0.5 ~ "Strong negative association",
      odds_ratio < 0.8 ~ "Moderate negative association",
      odds_ratio < 1.2 ~ "Weak/no association",
      odds_ratio < 2.0 ~ "Moderate positive association",
      TRUE ~ "Strong positive association"
    )

    cat("Odds ratio calculated successfully\n")
    cat(sprintf("Odds ratio: %.3f (%s)\n", odds_ratio, or_interpretation))
    cat(sprintf("95%% CI: [%.3f, %.3f]\n", ci_or[1], ci_or[2]))
    cat(sprintf("P-value: %.3f\n", p_value))

    return(list(
      odds_ratio = odds_ratio,
      confidence_interval = ci_or,
      p_value = p_value,
      interpretation = or_interpretation,
      model = logit_model,
      exposure = exposure,
      outcome = outcome,
      covariates = covariates
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Odds ratio calculation failed:", e$message),
        code = "OR_ERROR",
        details = list(outcome = outcome, exposure = exposure, original_error = e$message)
      ))
    }
  })
}

#' Calculate Correlation Effect Sizes
#'
#' @param data Analytic dataset
#' @param x_var First variable for correlation
#' @param y_var Second variable for correlation
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @return Correlation coefficient with confidence intervals
#' @export
calculate_correlation_effect <- function(data, x_var = "bmi", y_var = "bodyfat_pct",
                                        method = "pearson") {

  tryCatch({
    # Validate inputs
    if (!x_var %in% names(data) || !y_var %in% names(data)) {
      stop(NhanesError(
        paste("Variables not found in data:", x_var, y_var),
        code = "MISSING_CORR_VARS"
      ))
    }

    # Remove missing values
    complete_data <- data[complete.cases(data[, c(x_var, y_var)]), ]

    if (nrow(complete_data) < 10) {
      warning(NhanesWarning(
        paste("Small sample size for correlation:", nrow(complete_data), "observations"),
        code = "SMALL_SAMPLE_CORR"
      ))
    }

    # Calculate correlation
    if (method == "pearson") {
      corr_result <- cor.test(complete_data[[x_var]], complete_data[[y_var]],
                             method = "pearson", conf.level = 0.95)
    } else if (method == "spearman") {
      corr_result <- cor.test(complete_data[[x_var]], complete_data[[y_var]],
                             method = "spearman", conf.level = 0.95)
    } else if (method == "kendall") {
      corr_result <- cor.test(complete_data[[x_var]], complete_data[[y_var]],
                             method = "kendall", conf.level = 0.95)
    } else {
      stop(NhanesError("Method must be 'pearson', 'spearman', or 'kendall'",
                      code = "INVALID_CORR_METHOD"))
    }

    # Effect size interpretation for correlations
    r_interpretation <- case_when(
      abs(corr_result$estimate) < 0.1 ~ "Negligible",
      abs(corr_result$estimate) < 0.3 ~ "Small",
      abs(corr_result$estimate) < 0.5 ~ "Medium",
      TRUE ~ "Large"
    )

    cat("Correlation effect size calculated successfully\n")
    cat(sprintf("Correlation coefficient: %.3f (%s)\n", corr_result$estimate, r_interpretation))
    cat(sprintf("95%% CI: [%.3f, %.3f]\n", corr_result$conf.int[1], corr_result$conf.int[2]))
    cat(sprintf("P-value: %.3f\n", corr_result$p.value))

    return(list(
      correlation = corr_result$estimate,
      confidence_interval = corr_result$conf.int,
      p_value = corr_result$p.value,
      interpretation = r_interpretation,
      method = method,
      n_obs = nrow(complete_data),
      variable_1 = x_var,
      variable_2 = y_var
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Correlation calculation failed:", e$message),
        code = "CORR_ERROR",
        details = list(x_var = x_var, y_var = y_var, method = method, original_error = e$message)
      ))
    }
  })
}

#' Power Analysis for Effect Size Planning
#'
#' @param effect_size Expected effect size (Cohen's d)
#' @param n_sample Sample size per group
#' @param alpha Significance level
#' @param power Desired power
#' @param test_type Type of test ("t.test" or "correlation")
#' @return Power analysis results
#' @export
power_analysis <- function(effect_size = 0.5, n_sample = 100,
                          alpha = 0.05, power = 0.8, test_type = "t.test") {

  tryCatch({
    if (test_type == "t.test") {
      # Power for two-sample t-test
      power_result <- pwr::pwr.t2n.test(
        n1 = n_sample,
        n2 = n_sample,
        d = effect_size,
        sig.level = alpha,
        power = power,
        alternative = "two.sided"
      )
    } else if (test_type == "correlation") {
      # Power for correlation test
      power_result <- pwr::pwr.r.test(
        n = n_sample * 2,  # Total sample size
        r = effect_size,
        sig.level = alpha,
        power = power,
        alternative = "two.sided"
      )
    } else {
      stop(NhanesError("Test type must be 't.test' or 'correlation'",
                      code = "INVALID_POWER_TEST"))
    }

    # Calculate required sample size for desired power
    if (test_type == "t.test") {
      n_required <- pwr::pwr.t2n.test(
        d = effect_size,
        sig.level = alpha,
        power = power,
        alternative = "two.sided"
      )$n
    } else {
      n_required <- pwr::pwr.r.test(
        r = effect_size,
        sig.level = alpha,
        power = power,
        alternative = "two.sided"
      )$n
    }

    cat("Power analysis completed\n")
    cat(sprintf("Achieved power: %.3f (target: %.3f)\n", power_result$power, power))
    cat(sprintf("Required sample size per group: %.0f\n", ceiling(n_required)))

    return(list(
      power_analysis = power_result,
      effect_size = effect_size,
      required_n = ceiling(n_required),
      achieved_power = power_result$power,
      test_type = test_type,
      alpha = alpha,
      power = power
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Power analysis failed:", e$message),
        code = "POWER_ERROR",
        details = list(effect_size = effect_size, test_type = test_type, original_error = e$message)
      ))
    }
  })
}

#' Comprehensive Effect Size Report
#'
#' @param effect_results List of effect size calculation results
#' @param output_dir Output directory for tables and plots
#' @return Report generation results
#' @export
generate_effect_size_report <- function(effect_results, output_dir = "outputs") {

  tryCatch({
    # Create output directories
    tables_dir <- file.path(output_dir, "tables")
    logs_dir <- file.path(output_dir, "logs")
    dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

    # Generate effect size summary table
    effect_summary <- data.frame(
      analysis_type = character(),
      effect_size = numeric(),
      interpretation = character(),
      confidence_interval = character(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )

    # Add Cohen's d results if available
    if ("cohens_d" %in% names(effect_results)) {
      effect_summary <- rbind(effect_summary, data.frame(
        analysis_type = "Cohen's d (Standardized Mean Difference)",
        effect_size = effect_results$cohens_d$Cohens_d,
        interpretation = effect_results$interpretation,
        confidence_interval = sprintf("[%.3f, %.3f]", effect_results$cohens_d$CI_low, effect_results$cohens_d$CI_high),
        p_value = NA,
        stringsAsFactors = FALSE
      ))
    }

    # Add odds ratio results if available
    if ("odds_ratio" %in% names(effect_results)) {
      effect_summary <- rbind(effect_summary, data.frame(
        analysis_type = "Odds Ratio",
        effect_size = effect_results$odds_ratio,
        interpretation = effect_results$interpretation,
        confidence_interval = sprintf("[%.3f, %.3f]", effect_results$confidence_interval[1], effect_results$confidence_interval[2]),
        p_value = effect_results$p_value,
        stringsAsFactors = FALSE
      ))
    }

    # Add correlation results if available
    if ("correlation" %in% names(effect_results)) {
      effect_summary <- rbind(effect_summary, data.frame(
        analysis_type = paste("Correlation (", effect_results$method, ")"),
        effect_size = effect_results$correlation,
        interpretation = effect_results$interpretation,
        confidence_interval = sprintf("[%.3f, %.3f]", effect_results$confidence_interval[1], effect_results$confidence_interval[2]),
        p_value = effect_results$p_value,
        stringsAsFactors = FALSE
      ))
    }

    # Save effect size summary
    write.csv(effect_summary,
              file.path(tables_dir, "effect_size_summary.csv"),
              row.names = FALSE)

    # Generate comprehensive report
    report_content <- paste0(
      "Effect Size Analysis Report\n",
      "==========================\n\n",
      "Analysis Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
      "Summary of Effect Sizes:\n",
      "=======================\n"
    )

    for (i in 1:nrow(effect_summary)) {
      report_content <- paste0(report_content,
        sprintf("%s:\n", effect_summary$analysis_type[i]),
        sprintf("  Effect size: %.3f\n", effect_summary$effect_size[i]),
        sprintf("  Interpretation: %s\n", effect_summary$interpretation[i]),
        sprintf("  95%% CI: %s\n", effect_summary$confidence_interval[i])
      )

      if (!is.na(effect_summary$p_value[i])) {
        report_content <- paste0(report_content,
          sprintf("  P-value: %.3f\n", effect_summary$p_value[i])
        )
      }

      report_content <- paste0(report_content, "\n")
    }

    # Add interpretation guidelines
    report_content <- paste0(report_content,
      "Interpretation Guidelines:\n",
      "=========================\n",
      "Cohen's d:\n",
      "  < 0.2: Negligible effect\n",
      "  0.2-0.5: Small effect\n",
      "  0.5-0.8: Medium effect\n",
      "  > 0.8: Large effect\n\n",
      "Odds Ratio:\n",
      "  < 0.5: Strong negative association\n",
      "  0.5-0.8: Moderate negative association\n",
      "  0.8-1.2: Weak/no association\n",
      "  1.2-2.0: Moderate positive association\n",
      "  > 2.0: Strong positive association\n\n",
      "Correlation:\n",
      "  < 0.1: Negligible\n",
      "  0.1-0.3: Small\n",
      "  0.3-0.5: Medium\n",
      "  > 0.5: Large\n"
    )

    # Write report
    report_file <- file.path(logs_dir, "effect_size_analysis_report.txt")
    writeLines(report_content, report_file)

    cat("Effect size analysis report generated\n")
    cat("Tables saved to:", tables_dir, "\n")
    cat("Report saved to:", report_file, "\n")

    return(list(
      effect_summary = effect_summary,
      report_file = report_file,
      tables_dir = tables_dir,
      logs_dir = logs_dir
    ))

  }, error = function(e) {
    stop(NhanesError(
      paste("Effect size report generation failed:", e$message),
      code = "EFFECT_REPORT_ERROR",
      details = list(original_error = e$message)
    ))
  })
}






