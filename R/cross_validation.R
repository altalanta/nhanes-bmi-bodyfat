#' @title Cross-Validation Framework for NHANES BMI-Body Fat Data
#' @description Systematic cross-validation and model validation framework for robust statistical inference
#' @import dplyr
#' @import ggplot2
#' @import caret
#' @import boot
#' @import rsample
#' @import yardstick
#' @export

# Load configuration and utilities
source("scripts/load_config.R")
source("scripts/error_handling.R")
source("R/performance.R")

#' K-Fold Cross-Validation for Regression Models
#'
#' @param data Analytic dataset
#' @param formula Model formula
#' @param k Number of folds
#' @param method Model fitting method ("lm", "glm", "stan_glm")
#' @param family Family for GLM models
#' @return Cross-validation results
#' @export
k_fold_cross_validation <- function(data, formula = bodyfat_pct ~ bmi + age_centered + sex + race_ethnicity,
                                   k = 5, method = "lm", family = gaussian()) {

  tryCatch({
    # Validate inputs
    if (!all(all.vars(formula) %in% names(data))) {
      missing_vars <- setdiff(all.vars(formula), names(data))
      stop(NhanesError(
        paste("Variables not found in data:", paste(missing_vars, collapse = ", ")),
        code = "MISSING_CV_VARS",
        details = list(formula = deparse(formula), missing = missing_vars)
      ))
    }

    # Create k-fold splits
    set.seed(42)  # For reproducibility
    folds <- rsample::vfold_cv(data, v = k)

    # Initialize results storage
    cv_results <- data.frame(
      fold = integer(k),
      train_rmse = numeric(k),
      test_rmse = numeric(k),
      train_r2 = numeric(k),
      test_r2 = numeric(k),
      n_train = integer(k),
      n_test = integer(k)
    )

    # Perform k-fold CV
    for (i in 1:k) {
      # Split data
      train_data <- folds$splits[[i]] %>% rsample::analysis()
      test_data <- folds$splits[[i]] %>% rsample::assessment()

      # Fit model
      if (method == "lm") {
        model <- lm(formula, data = train_data)
      } else if (method == "glm") {
        model <- glm(formula, data = train_data, family = family)
      } else if (method == "stan_glm") {
        # Use rstanarm for Bayesian models
        model <- stan_glm(formula, data = train_data, family = family,
                         chains = 2, iter = 1000, warmup = 500, cores = 2, refresh = 0)
      } else {
        stop(NhanesError("Method must be 'lm', 'glm', or 'stan_glm'",
                        code = "INVALID_CV_METHOD"))
      }

      # Predictions
      train_pred <- predict(model, train_data)
      test_pred <- predict(model, test_data)

      # Calculate metrics (handle different model types)
      if (method == "stan_glm") {
        # For Bayesian models, use posterior predictive means
        train_pred <- colMeans(posterior_predict(model, newdata = train_data))
        test_pred <- colMeans(posterior_predict(model, newdata = test_data))
      }

      # Calculate performance metrics
      train_rmse <- sqrt(mean((train_data$bodyfat_pct - train_pred)^2, na.rm = TRUE))
      test_rmse <- sqrt(mean((test_data$bodyfat_pct - test_pred)^2, na.rm = TRUE))

      train_r2 <- cor(train_data$bodyfat_pct, train_pred, use = "complete.obs")^2
      test_r2 <- cor(test_data$bodyfat_pct, test_pred, use = "complete.obs")^2

      # Store results
      cv_results[i, ] <- c(
        i, train_rmse, test_rmse, train_r2, test_r2,
        nrow(train_data), nrow(test_data)
      )

      cat(sprintf("Fold %d/%d: Train RMSE=%.3f, Test RMSE=%.3f\n", i, k, train_rmse, test_rmse))
    }

    # Calculate summary statistics
    cv_summary <- data.frame(
      metric = c("Train_RMSE", "Test_RMSE", "Train_R2", "Test_R2"),
      mean = c(mean(cv_results$train_rmse), mean(cv_results$test_rmse),
               mean(cv_results$train_r2), mean(cv_results$test_r2)),
      sd = c(sd(cv_results$train_rmse), sd(cv_results$test_rmse),
             sd(cv_results$train_r2), sd(cv_results$test_r2)),
      min = c(min(cv_results$train_rmse), min(cv_results$test_rmse),
              min(cv_results$train_r2), min(cv_results$test_r2)),
      max = c(max(cv_results$train_rmse), max(cv_results$test_rmse),
              max(cv_results$train_r2), max(cv_results$test_r2))
    )

    cat("K-fold cross-validation completed\n")
    cat(sprintf("Average Test RMSE: %.3f ± %.3f\n", mean(cv_results$test_rmse), sd(cv_results$test_rmse)))
    cat(sprintf("Average Test R²: %.3f ± %.3f\n", mean(cv_results$test_r2), sd(cv_results$test_r2)))

    return(list(
      cv_results = cv_results,
      cv_summary = cv_summary,
      method = method,
      k = k,
      formula = formula,
      n_total = nrow(data)
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("K-fold cross-validation failed:", e$message),
        code = "KFOLD_CV_ERROR",
        details = list(k = k, method = method, original_error = e$message)
      ))
    }
  })
}

#' Bootstrap Validation for Model Stability
#'
#' @param data Analytic dataset
#' @param formula Model formula
#' @param n_bootstrap Number of bootstrap samples
#' @param method Model fitting method
#' @return Bootstrap validation results
#' @export
bootstrap_validation <- function(data, formula = bodyfat_pct ~ bmi + age_centered + sex + race_ethnicity,
                                n_bootstrap = 100, method = "lm") {

  tryCatch({
    # Validate inputs
    if (!all(all.vars(formula) %in% names(data))) {
      missing_vars <- setdiff(all.vars(formula), names(data))
      stop(NhanesError(
        paste("Variables not found in data:", paste(missing_vars, collapse = ", ")),
        code = "MISSING_BOOTSTRAP_VARS"
      ))
    }

    # Initialize results storage
    bootstrap_results <- data.frame(
      bootstrap = 1:n_bootstrap,
      rmse = numeric(n_bootstrap),
      r2 = numeric(n_bootstrap),
      coef_bmi = numeric(n_bootstrap),
      coef_age = numeric(n_bootstrap),
      coef_sex_female = numeric(n_bootstrap)
    )

    # Perform bootstrap
    set.seed(42)
    for (i in 1:n_bootstrap) {
      # Sample with replacement
      boot_indices <- sample(1:nrow(data), replace = TRUE)
      boot_data <- data[boot_indices, ]

      # Fit model
      if (method == "lm") {
        model <- lm(formula, data = boot_data)
      } else if (method == "glm") {
        model <- glm(formula, data = boot_data, family = gaussian())
      } else {
        stop(NhanesError("Method must be 'lm' or 'glm'",
                        code = "INVALID_BOOTSTRAP_METHOD"))
      }

      # Predictions
      predictions <- predict(model, boot_data)

      # Calculate metrics
      rmse <- sqrt(mean((boot_data$bodyfat_pct - predictions)^2, na.rm = TRUE))
      r2 <- cor(boot_data$bodyfat_pct, predictions, use = "complete.obs")^2

      # Extract coefficients
      coefs <- coef(model)
      coef_bmi <- coefs["bmi"]
      coef_age <- coefs["age_centered"]
      coef_sex_female <- if ("sexFemale" %in% names(coefs)) coefs["sexFemale"] else NA

      # Store results
      bootstrap_results[i, ] <- c(i, rmse, r2, coef_bmi, coef_age, coef_sex_female)

      if (i %% 10 == 0) {
        cat(sprintf("Bootstrap %d/%d completed\n", i, n_bootstrap))
      }
    }

    # Calculate summary statistics
    bootstrap_summary <- data.frame(
      metric = c("RMSE", "R2", "BMI_Coefficient", "Age_Coefficient", "Sex_Coefficient"),
      mean = c(mean(bootstrap_results$rmse), mean(bootstrap_results$r2),
               mean(bootstrap_results$coef_bmi, na.rm = TRUE),
               mean(bootstrap_results$coef_age, na.rm = TRUE),
               mean(bootstrap_results$coef_sex_female, na.rm = TRUE)),
      sd = c(sd(bootstrap_results$rmse), sd(bootstrap_results$r2),
             sd(bootstrap_results$coef_bmi, na.rm = TRUE),
             sd(bootstrap_results$coef_age, na.rm = TRUE),
             sd(bootstrap_results$coef_sex_female, na.rm = TRUE)),
      ci_2.5 = c(quantile(bootstrap_results$rmse, 0.025), quantile(bootstrap_results$r2, 0.025),
                 quantile(bootstrap_results$coef_bmi, 0.025, na.rm = TRUE),
                 quantile(bootstrap_results$coef_age, 0.025, na.rm = TRUE),
                 quantile(bootstrap_results$coef_sex_female, 0.025, na.rm = TRUE)),
      ci_97.5 = c(quantile(bootstrap_results$rmse, 0.975), quantile(bootstrap_results$r2, 0.975),
                  quantile(bootstrap_results$coef_bmi, 0.975, na.rm = TRUE),
                  quantile(bootstrap_results$coef_age, 0.975, na.rm = TRUE),
                  quantile(bootstrap_results$coef_sex_female, 0.975, na.rm = TRUE))
    )

    cat("Bootstrap validation completed\n")
    cat(sprintf("Bootstrap RMSE: %.3f [%.3f, %.3f]\n",
                bootstrap_summary$mean[1], bootstrap_summary$ci_2.5[1], bootstrap_summary$ci_97.5[1]))
    cat(sprintf("Bootstrap R²: %.3f [%.3f, %.3f]\n",
                bootstrap_summary$mean[2], bootstrap_summary$ci_2.5[2], bootstrap_summary$ci_97.5[2]))

    return(list(
      bootstrap_results = bootstrap_results,
      bootstrap_summary = bootstrap_summary,
      method = method,
      n_bootstrap = n_bootstrap,
      formula = formula,
      n_total = nrow(data)
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Bootstrap validation failed:", e$message),
        code = "BOOTSTRAP_ERROR",
        details = list(n_bootstrap = n_bootstrap, method = method, original_error = e$message)
      ))
    }
  })
}

#' Model Performance Comparison Across Methods
#'
#' @param data Analytic dataset
#' @param methods Vector of model methods to compare
#' @param k_cv Number of CV folds
#' @param n_bootstrap Number of bootstrap samples
#' @return Comprehensive model comparison results
#' @export
compare_model_performance <- function(data, methods = c("lm", "glm", "stan_glm"),
                                    k_cv = 5, n_bootstrap = 50) {

  tryCatch({
    # Initialize results storage
    comparison_results <- list()

    # Test each method
    for (method in methods) {
      cat(sprintf("Testing method: %s\n", method))

      # K-fold cross-validation
      cv_results <- tryCatch({
        k_fold_cross_validation(data, k = k_cv, method = method)
      }, error = function(e) {
        cat(sprintf("CV failed for %s: %s\n", method, e$message))
        return(NULL)
      })

      # Bootstrap validation
      bootstrap_results <- tryCatch({
        bootstrap_validation(data, n_bootstrap = n_bootstrap, method = method)
      }, error = function(e) {
        cat(sprintf("Bootstrap failed for %s: %s\n", method, e$message))
        return(NULL)
      })

      comparison_results[[method]] <- list(
        cv_results = cv_results,
        bootstrap_results = bootstrap_results
      )
    }

    # Create comparison summary
    comparison_summary <- data.frame(
      method = character(),
      cv_rmse_mean = numeric(),
      cv_rmse_sd = numeric(),
      cv_r2_mean = numeric(),
      cv_r2_sd = numeric(),
      bootstrap_rmse_mean = numeric(),
      bootstrap_rmse_sd = numeric(),
      bootstrap_r2_mean = numeric(),
      bootstrap_r2_sd = numeric(),
      stringsAsFactors = FALSE
    )

    for (method in methods) {
      cv_res <- comparison_results[[method]]$cv_results
      boot_res <- comparison_results[[method]]$bootstrap_results

      if (!is.null(cv_res)) {
        cv_rmse <- mean(cv_res$cv_results$test_rmse)
        cv_rmse_sd <- sd(cv_res$cv_results$test_rmse)
        cv_r2 <- mean(cv_res$cv_results$test_r2)
        cv_r2_sd <- sd(cv_res$cv_results$test_r2)
      } else {
        cv_rmse <- cv_rmse_sd <- cv_r2 <- cv_r2_sd <- NA
      }

      if (!is.null(boot_res)) {
        boot_rmse <- mean(boot_res$bootstrap_results$rmse)
        boot_rmse_sd <- sd(boot_res$bootstrap_results$rmse)
        boot_r2 <- mean(boot_res$bootstrap_results$r2)
        boot_r2_sd <- sd(boot_res$bootstrap_results$r2)
      } else {
        boot_rmse <- boot_rmse_sd <- boot_r2 <- boot_r2_sd <- NA
      }

      comparison_summary <- rbind(comparison_summary, data.frame(
        method = method,
        cv_rmse_mean = cv_rmse,
        cv_rmse_sd = cv_rmse_sd,
        cv_r2_mean = cv_r2,
        cv_r2_sd = cv_r2_sd,
        bootstrap_rmse_mean = boot_rmse,
        bootstrap_rmse_sd = boot_rmse_sd,
        bootstrap_r2_mean = boot_r2,
        bootstrap_r2_sd = boot_r2_sd,
        stringsAsFactors = FALSE
      ))
    }

    # Determine best performing method
    valid_methods <- comparison_summary$method[!is.na(comparison_summary$cv_rmse_mean)]
    if (length(valid_methods) > 0) {
      best_method_cv <- valid_methods[which.min(comparison_summary$cv_rmse_mean[!is.na(comparison_summary$cv_rmse_mean)])]
      best_method_bootstrap <- valid_methods[which.min(comparison_summary$bootstrap_rmse_mean[!is.na(comparison_summary$bootstrap_rmse_mean)])]
    } else {
      best_method_cv <- best_method_bootstrap <- NA
    }

    cat("Model performance comparison completed\n")
    cat("Best method by CV RMSE:", best_method_cv, "\n")
    cat("Best method by Bootstrap RMSE:", best_method_bootstrap, "\n")

    return(list(
      comparison_results = comparison_results,
      comparison_summary = comparison_summary,
      best_method_cv = best_method_cv,
      best_method_bootstrap = best_method_bootstrap,
      methods_tested = methods,
      k_cv = k_cv,
      n_bootstrap = n_bootstrap
    ))

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Model performance comparison failed:", e$message),
        code = "MODEL_COMPARISON_ERROR",
        details = list(methods = methods, original_error = e$message)
      ))
    }
  })
}

#' Generate Cross-Validation Report
#'
#' @param cv_results Results from cross-validation functions
#' @param output_dir Output directory for plots and tables
#' @return Report generation results
#' @export
generate_cv_report <- function(cv_results, output_dir = "outputs") {

  tryCatch({
    # Create output directories
    plots_dir <- file.path(output_dir, "figures", "cv")
    tables_dir <- file.path(output_dir, "tables")
    logs_dir <- file.path(output_dir, "logs")
    dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

    # Generate CV performance plots
    plots <- list()

    # RMSE comparison across folds
    if ("cv_results" %in% names(cv_results)) {
      cv_data <- cv_results$cv_results$cv_results

      plots$rmse_folds <- ggplot(cv_data, aes(x = factor(fold), y = test_rmse)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_errorbar(aes(ymin = test_rmse - sd(test_rmse), ymax = test_rmse + sd(test_rmse)),
                     width = 0.2) +
        labs(title = "Cross-Validation RMSE by Fold",
             x = "Fold", y = "Test RMSE") +
        theme_minimal()

      plots$r2_folds <- ggplot(cv_data, aes(x = factor(fold), y = test_r2)) +
        geom_bar(stat = "identity", fill = "darkgreen") +
        geom_errorbar(aes(ymin = test_r2 - sd(test_r2), ymax = test_r2 + sd(test_r2)),
                     width = 0.2) +
        labs(title = "Cross-Validation R² by Fold",
             x = "Fold", y = "Test R²") +
        theme_minimal()
    }

    # Bootstrap distribution plots
    if ("bootstrap_results" %in% names(cv_results)) {
      boot_data <- cv_results$bootstrap_results$bootstrap_results

      plots$rmse_bootstrap <- ggplot(boot_data, aes(x = rmse)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        geom_vline(xintercept = mean(boot_data$rmse), color = "red", linetype = "dashed", size = 1) +
        labs(title = "Bootstrap Distribution of RMSE",
             x = "RMSE", y = "Frequency") +
        theme_minimal()

      plots$r2_bootstrap <- ggplot(boot_data, aes(x = r2)) +
        geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
        geom_vline(xintercept = mean(boot_data$r2), color = "red", linetype = "dashed", size = 1) +
        labs(title = "Bootstrap Distribution of R²",
             x = "R²", y = "Frequency") +
        theme_minimal()
    }

    # Save plots
    for (plot_name in names(plots)) {
      plot_file <- file.path(plots_dir, paste0("cv_", plot_name, ".png"))
      ggsave(plot_file, plots[[plot_name]], width = 8, height = 6, dpi = 300)
    }

    # Generate comprehensive report
    report_content <- paste0(
      "Cross-Validation Analysis Report\n",
      "===============================\n\n",
      "Analysis Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n"
    )

    # Add CV results if available
    if ("cv_results" %in% names(cv_results)) {
      cv_sum <- cv_results$cv_results$cv_summary

      report_content <- paste0(report_content,
        "K-Fold Cross-Validation Results:\n",
        "===============================\n",
        sprintf("Number of folds: %d\n", cv_results$cv_results$k),
        sprintf("Method: %s\n", cv_results$cv_results$method),
        sprintf("Total observations: %d\n\n", cv_results$cv_results$n_total),
        "Performance Summary:\n"
      )

      for (i in 1:nrow(cv_sum)) {
        report_content <- paste0(report_content,
          sprintf("  %s: %.3f ± %.3f [%.3f, %.3f]\n",
                  cv_sum$metric[i], cv_sum$mean[i], cv_sum$sd[i],
                  cv_sum$min[i], cv_sum$max[i])
        )
      }
      report_content <- paste0(report_content, "\n")
    }

    # Add bootstrap results if available
    if ("bootstrap_results" %in% names(cv_results)) {
      boot_sum <- cv_results$bootstrap_results$bootstrap_summary

      report_content <- paste0(report_content,
        "Bootstrap Validation Results:\n",
        "===========================\n",
        sprintf("Number of bootstrap samples: %d\n", cv_results$bootstrap_results$n_bootstrap),
        sprintf("Method: %s\n", cv_results$bootstrap_results$method),
        sprintf("Total observations: %d\n\n", cv_results$bootstrap_results$n_total),
        "Bootstrap Summary (95% CI):\n"
      )

      for (i in 1:nrow(boot_sum)) {
        report_content <- paste0(report_content,
          sprintf("  %s: %.3f [%.3f, %.3f]\n",
                  boot_sum$metric[i], boot_sum$mean[i],
                  boot_sum$ci_2.5[i], boot_sum$ci_97.5[i])
        )
      }
      report_content <- paste0(report_content, "\n")
    }

    # Write report
    report_file <- file.path(logs_dir, "cross_validation_report.txt")
    writeLines(report_content, report_file)

    cat("Cross-validation report generated\n")
    cat("Plots saved to:", plots_dir, "\n")
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
      paste("Cross-validation report generation failed:", e$message),
      code = "CV_REPORT_ERROR",
      details = list(original_error = e$message)
    ))
  })
}







