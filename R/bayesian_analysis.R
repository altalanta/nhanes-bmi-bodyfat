#' @title Bayesian Statistical Analysis Framework for NHANES BMI-Body Fat Data
#' @description Advanced Bayesian methods for uncertainty quantification and robust inference
#' @import dplyr
#' @import ggplot2
#' @import rstanarm
#' @import bayesplot
#' @import loo
#' @import posterior
#' @import brms
#' @export

# Load configuration and utilities
source("scripts/load_config.R")
source("scripts/error_handling.R")
source("R/performance.R")

#' Bayesian Linear Regression with Stan
#'
#' @param data Analytic dataset
#' @param formula Model formula
#' @param prior Prior distribution specification
#' @param chains Number of MCMC chains
#' @param iter Number of iterations per chain
#' @param warmup Warmup iterations
#' @return Stan model fit object
#' @export
fit_bayesian_linear <- function(data, formula = bodyfat_pct ~ bmi + age_centered + sex + race_ethnicity,
                               prior = rstanarm::normal(scale = 1),
                               chains = 4, iter = 2000, warmup = 1000) {

  tryCatch({
    # Validate inputs
    if (!"bodyfat_pct" %in% names(data)) {
      stop(NhanesError("Body fat percentage variable not found in data",
                      code = "MISSING_BODYFAT_VAR"))
    }

    # Fit Bayesian model
    model <- stan_glm(
      formula = formula,
      data = data,
      family = gaussian(),
      prior = prior,
      chains = chains,
      iter = iter,
      warmup = warmup,
      cores = min(chains, parallel::detectCores() - 1),
      refresh = 0,
      seed = 42
    )

    # Validate model convergence
    rhat_values <- rhat(model)
    if (any(rhat_values > 1.1, na.rm = TRUE)) {
      warning(NhanesWarning("Some parameters may not have converged (R-hat > 1.1)",
                           code = "BAYESIAN_CONVERGENCE_WARNING"))
    }

    # Calculate model diagnostics
    diagnostics <- list(
      rhat_max = max(rhat_values, na.rm = TRUE),
      n_eff_min = min(neff_ratio(model), na.rm = TRUE),
      n_divergent = sum(get_divergent_iterations(model))
    )

    cat("Bayesian linear model fitted successfully\n")
    cat(sprintf("R-hat max: %.3f, N_eff min: %.0f, Divergences: %d\n",
                diagnostics$rhat_max, diagnostics$n_eff_min, diagnostics$n_divergent))

    return(list(
      model = model,
      diagnostics = diagnostics,
      formula = formula,
      n_obs = nrow(data)
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Bayesian model fitting failed:", e$message),
        code = "BAYESIAN_FIT_ERROR",
        details = list(formula = deparse(formula), original_error = e$message)
      ))
    }
  })
}

#' Bayesian Model Comparison using LOO-CV
#'
#' @param models List of fitted Bayesian models
#' @return Model comparison results with LOO weights
#' @export
compare_bayesian_models <- function(models) {

  tryCatch({
    if (length(models) < 2) {
      stop(NhanesError("Need at least 2 models for comparison",
                      code = "INSUFFICIENT_MODELS"))
    }

    # Extract log-likelihood matrices
    log_lik_list <- lapply(models, function(m) loo::extract_log_lik(m$model, merge_chains = FALSE))

    # Compute LOO-CV for each model
    loo_results <- list()
    for (i in seq_along(models)) {
      loo_results[[i]] <- loo::loo(log_lik_list[[i]])
    }

    # Compare models using LOO
    loo_comparison <- loo::loo_compare(loo_results)

    # Calculate model weights
    loo_weights <- loo::loo_model_weights(loo_results)

    # Create comparison summary
    comparison_summary <- data.frame(
      model = names(models),
      looic = sapply(loo_results, function(x) x$estimates["looic", "Estimate"]),
      se_looic = sapply(loo_results, function(x) x$estimates["looic", "SE"]),
      elpd_loo = sapply(loo_results, function(x) x$estimates["elpd_loo", "Estimate"]),
      se_elpd_loo = sapply(loo_results, function(x) x$estimates["elpd_loo", "SE"]),
      loo_weight = as.numeric(loo_weights)
    )

    # Order by LOOIC (lower is better)
    comparison_summary <- comparison_summary[order(comparison_summary$looic), ]

    cat("Bayesian model comparison completed\n")
    cat("Best model by LOOIC:", comparison_summary$model[1], "\n")

    return(list(
      comparison = comparison_summary,
      loo_results = loo_results,
      loo_weights = loo_weights,
      best_model = comparison_summary$model[1]
    ))

  }, error = function(e) {
    stop(NhanesError(
      paste("Bayesian model comparison failed:", e$message),
      code = "BAYESIAN_COMPARISON_ERROR",
      details = list(n_models = length(models), original_error = e$message)
    ))
  })
}

#' Posterior Predictive Checks
#'
#' @param model Fitted Bayesian model
#' @param data Original data
#' @param n_draws Number of posterior draws for PPC
#' @return PPC results and plots
#' @export
posterior_predictive_checks <- function(model, data, n_draws = 100) {

  tryCatch({
    # Generate posterior predictive samples
    y_rep <- posterior_predict(model$model, draws = n_draws, newdata = data)

    # Calculate test statistics for observed and replicated data
    test_stats <- function(y) {
      c(
        mean = mean(y),
        sd = sd(y),
        min = min(y),
        max = max(y),
        skewness = moments::skewness(y),
        kurtosis = moments::kurtosis(y)
      )
    }

    obs_stats <- test_stats(data$bodyfat_pct)
    rep_stats <- apply(y_rep, 1, test_stats)

    # Create PPC summary
    ppc_summary <- data.frame(
      statistic = names(obs_stats),
      observed = obs_stats,
      mean_replicated = rowMeans(rep_stats),
      sd_replicated = apply(rep_stats, 1, sd),
      p_value = apply(rep_stats, 1, function(x) mean(x >= obs_stats))
    )

    # Generate PPC plots
    ppc_plots <- list()

    # Density overlay plot
    ppc_plots$density <- ppc_dens_overlay(data$bodyfat_pct, y_rep[1:50, ]) +
      labs(title = "Posterior Predictive Check: Density Overlay")

    # Test statistics plot
    ppc_plots$test_stats <- ppc_stat(data$bodyfat_pct, y_rep, stat = "mean") +
      labs(title = "Posterior Predictive Check: Test Statistics")

    cat("Posterior predictive checks completed\n")

    return(list(
      summary = ppc_summary,
      plots = ppc_plots,
      y_rep = y_rep,
      n_draws = n_draws
    ))

  }, error = function(e) {
    stop(NhanesError(
      paste("Posterior predictive checks failed:", e$message),
      code = "PPC_ERROR",
      details = list(original_error = e$message)
    ))
  })
}

#' Bayesian Variable Selection
#'
#' @param data Analytic dataset
#' @param response Response variable
#' @param predictors Vector of predictor variables
#' @param method Variable selection method ("horseshoe" or "lasso")
#' @return Variable selection results
#' @export
bayesian_variable_selection <- function(data, response = "bodyfat_pct",
                                       predictors = c("bmi", "age_centered", "sex", "race_ethnicity"),
                                       method = "horseshoe") {

  tryCatch({
    # Create formula with all predictors
    formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
    formula <- as.formula(formula_str)

    # Fit model with specified prior
    if (method == "horseshoe") {
      prior <- hs(global_scale = 1, df = 1, df_global = 1, df_slab = 4)
    } else if (method == "lasso") {
      prior <- lasso(df = 1, scale = 1)
    } else {
      stop(NhanesError("Method must be 'horseshoe' or 'lasso'",
                      code = "INVALID_VS_METHOD"))
    }

    model <- stan_glm(
      formula = formula,
      data = data,
      family = gaussian(),
      prior = prior,
      chains = 4,
      iter = 2000,
      warmup = 1000,
      cores = min(4, parallel::detectCores() - 1),
      refresh = 0,
      seed = 42
    )

    # Extract posterior samples
    posterior_samples <- as.matrix(model)

    # Calculate inclusion probabilities
    inclusion_probs <- colMeans(posterior_samples != 0)

    # Variables with high inclusion probability (>0.5)
    selected_vars <- names(inclusion_probs[inclusion_probs > 0.5])

    cat("Bayesian variable selection completed\n")
    cat("Selected variables:", paste(selected_vars, collapse = ", "), "\n")

    return(list(
      model = model,
      inclusion_probabilities = inclusion_probs,
      selected_variables = selected_vars,
      method = method
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Bayesian variable selection failed:", e$message),
        code = "BAYESIAN_VS_ERROR",
        details = list(method = method, original_error = e$message)
      ))
    }
  })
}

#' Generate Bayesian Analysis Report
#'
#' @param bayesian_results Results from Bayesian analysis functions
#' @param output_dir Output directory for plots and tables
#' @return Report generation results
#' @export
generate_bayesian_report <- function(bayesian_results, output_dir = "outputs") {

  tryCatch({
    # Create output directories
    plots_dir <- file.path(output_dir, "figures", "bayesian")
    tables_dir <- file.path(output_dir, "tables")
    dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

    # Generate plots for each analysis
    plots <- list()

    # Model diagnostics plot
    if ("model" %in% names(bayesian_results)) {
      plots$diagnostics <- mcmc_trace(bayesian_results$model) +
        labs(title = "Bayesian Model Diagnostics: Trace Plots")
    }

    # Posterior distributions
    if ("model" %in% names(bayesian_results)) {
      plots$posteriors <- mcmc_areas(bayesian_results$model,
                                    pars = vars(-`(Intercept)`),
                                    prob = 0.95) +
        labs(title = "Posterior Distributions (95% Credible Intervals)")
    }

    # Save plots
    for (plot_name in names(plots)) {
      plot_file <- file.path(plots_dir, paste0("bayesian_", plot_name, ".png"))
      ggsave(plot_file, plots[[plot_name]], width = 10, height = 6, dpi = 300)
    }

    # Generate summary table
    if ("inclusion_probabilities" %in% names(bayesian_results)) {
      inclusion_summary <- data.frame(
        variable = names(bayesian_results$inclusion_probabilities),
        inclusion_probability = bayesian_results$inclusion_probabilities,
        selected = bayesian_results$inclusion_probabilities > 0.5
      )

      write.csv(inclusion_summary,
                file.path(tables_dir, "bayesian_variable_selection.csv"),
                row.names = FALSE)
    }

    # Generate comprehensive report
    report_content <- paste0(
      "Bayesian Analysis Report\n",
      "=======================\n\n",
      "Analysis Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
    )

    # Add model diagnostics if available
    if ("diagnostics" %in% names(bayesian_results)) {
      report_content <- paste0(report_content,
        "Model Diagnostics:\n",
        "=================\n",
        sprintf("R-hat max: %.3f\n", bayesian_results$diagnostics$rhat_max),
        sprintf("N_eff min: %.0f\n", bayesian_results$diagnostics$n_eff_min),
        sprintf("Divergences: %d\n\n", bayesian_results$diagnostics$n_divergent)
      )
    }

    # Add variable selection results if available
    if ("selected_variables" %in% names(bayesian_results)) {
      report_content <- paste0(report_content,
        "Variable Selection Results:\n",
        "=========================\n",
        "Method: ", bayesian_results$method, "\n",
        "Selected variables: ", paste(bayesian_results$selected_variables, collapse = ", "), "\n\n"
      )
    }

    # Write report
    report_file <- file.path(output_dir, "logs", "bayesian_analysis_report.txt")
    writeLines(report_content, report_file)

    cat("Bayesian analysis report generated\n")
    cat("Plots saved to:", plots_dir, "\n")
    cat("Tables saved to:", tables_dir, "\n")
    cat("Report saved to:", report_file, "\n")

    return(list(
      plots = plots,
      report_file = report_file,
      plots_dir = plots_dir,
      tables_dir = tables_dir,
      logs_dir = file.path(output_dir, "logs")
    ))

  }, error = function(e) {
    stop(NhanesError(
      paste("Bayesian report generation failed:", e$message),
      code = "BAYESIAN_REPORT_ERROR",
      details = list(original_error = e$message)
    ))
  })
}
