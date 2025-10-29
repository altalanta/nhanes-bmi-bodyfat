#!/usr/bin/env Rscript

# Machine Learning Analysis Integration for NHANES BMI Body Fat Analysis
# Extends the core analysis with advanced ML capabilities
#' Run Machine Learning Analysis
#'
#' @param cleaned_data The cleaned NHANES dataset.
#' @param config The project configuration list.
#' @return A list of machine learning analysis results.
run_ml_analysis <- function(cleaned_data, config) {
  # Load required libraries with error handling
  required_packages <- c(
    "dplyr", "ggplot2", "caret", "randomForest", "xgboost", "neuralnet",
    "cluster", "factoextra", "recipes", "tidymodels", "digest", "yaml"
  )

  for (pkg in required_packages) {
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      stop(NhanesError(
        paste("Required ML package not available:", pkg),
        code = "MISSING_ML_PACKAGE",
        details = list(package = pkg, error = e$message)
      ))
    })
  }

  # Load ML framework
  source("R/ml_framework.R")

  # Function to log messages
  log_file <- file.path(config$outputs$logs_dir, "ml_analysis_log.txt")

  log_msg <- function(msg) {
    cat(msg, "\n", file = log_file, append = TRUE)
    cat(msg, "\n")
  }

  # Step 1: Run comprehensive ML analysis
  safe_execute({
    log_msg("\nStep 1: Running comprehensive ML analysis")

    # Run complete ML analysis
    ml_results <- run_complete_ml_analysis(
      data = cleaned_data,
      target_variable = "bodyfat_pct",
      models_to_train = c("rf", "xgb", "lm"),  # Can be extended
      feature_selection = TRUE,
      clustering_analysis = TRUE,
      interpretability = TRUE
    )

    safe_log("ML analysis completed successfully", "INFO")

  }, "Machine Learning Analysis", config)

  # Step 2: Generate ML-specific visualizations
  safe_execute({
    log_msg("\nStep 2: Creating ML-specific visualizations")

    # Model comparison plot
    if (!is.null(ml_results$model_comparison)) {
      model_comp_plot <- ggplot(ml_results$model_comparison,
                               aes(x = reorder(model, -test_rmse), y = test_rmse, fill = model)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_text(aes(label = round(test_rmse, 3)), vjust = -0.5) +
        labs(title = "Model Performance Comparison",
             subtitle = "Test Set RMSE by Model Type",
             x = "Model",
             y = "Root Mean Square Error") +
        theme_minimal() +
        theme(legend.position = "none")

      ggsave(file.path(config$outputs$figures_dir, "ml_model_comparison.png"),
             model_comp_plot, width = 10, height = 6, dpi = 300)
    }

    # Feature importance plot
    if (!is.null(ml_results$feature_selection)) {
      # Get feature importance from best model
      best_model_name <- ml_results$model_comparison$model[1]
      best_model <- ml_results$model_results$models[[best_model_name]]

      if (!is.null(best_model$finalModel)) {
        importance_data <- extract_feature_importance(best_model, best_model_name)

        if (nrow(importance_data) > 0) {
          top_features <- head(importance_data, 10)

          importance_plot <- ggplot(top_features,
                                   aes(x = reorder(feature, importance), y = importance)) +
            geom_bar(stat = "identity", fill = "#2c5aa0", alpha = 0.8) +
            coord_flip() +
            labs(title = "Top Feature Importance",
                 subtitle = paste("From", best_model_name, "model"),
                 x = "Feature",
                 y = "Importance Score") +
            theme_minimal()

          ggsave(file.path(config$outputs$figures_dir, "ml_feature_importance.png"),
                 importance_plot, width = 10, height = 8, dpi = 300)
        }
      }
    }

    # Clustering visualization
    if (!is.null(ml_results$clustering_results)) {
      clustering_plot <- ml_results$clustering_results$comparison_data %>%
        ggplot(aes(x = method, y = mean_bodyfat, fill = factor(cluster))) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~method) +
        labs(title = "Body Fat Distribution by Cluster and Method",
             x = "Clustering Method",
             y = "Mean Body Fat (%)",
             fill = "Cluster") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggsave(file.path(config$outputs$figures_dir, "ml_clustering_comparison.png"),
             clustering_plot, width = 12, height = 8, dpi = 300)
    }

    safe_log("ML visualizations created", "INFO")

  }, "ML Visualization Generation", config)

  # Step 3: Export ML results
  safe_execute({
    log_msg("\nStep 3: Exporting ML analysis results")

    # Export model comparison
    if (!is.null(ml_results$model_comparison)) {
      write.csv(ml_results$model_comparison,
                file.path(config$outputs$tables_dir, "ml_model_comparison.csv"),
                row.names = FALSE)
    }

    # Export feature selection results
    if (!is.null(ml_results$feature_selection)) {
      # Export consensus features
      consensus_features <- data.frame(
        feature = ml_results$feature_selection$consensus_features,
        selected_by_methods = ml_results$feature_selection$feature_counts[ml_results$feature_selection$consensus_features]
      )
      write.csv(consensus_features,
                file.path(config$outputs$tables_dir, "ml_selected_features.csv"),
                row.names = FALSE)

      # Export feature selection method comparison
      method_comparison <- data.frame(
        method = names(ml_results$feature_selection$selection_results),
        features_selected = sapply(ml_results$feature_selection$selection_results, function(x) x$n_selected)
      )
      write.csv(method_comparison,
                file.path(config$outputs$tables_dir, "ml_feature_selection_methods.csv"),
                row.names = FALSE)
    }

    # Export clustering results
    if (!is.null(ml_results$clustering_results)) {
      write.csv(ml_results$clustering_results$comparison_data,
                file.path(config$outputs$tables_dir, "ml_clustering_results.csv"),
                row.names = FALSE)
    }

    # Export analysis metadata
    analysis_metadata <- ml_results$analysis_metadata
    write.csv(data.frame(
      metric = names(analysis_metadata),
      value = unlist(analysis_metadata)
    ), file.path(config$outputs$tables_dir, "ml_analysis_metadata.csv"), row.names = FALSE)

    safe_log("ML results exported successfully", "INFO")

  }, "ML Results Export", config)

  return(ml_results)
}

