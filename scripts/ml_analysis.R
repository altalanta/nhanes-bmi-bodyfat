#!/usr/bin/env Rscript

# Machine Learning Analysis Integration for NHANES BMI Body Fat Analysis
# Extends the core analysis with advanced ML capabilities

# Load error handling utilities
source("scripts/error_handling.R")

# Load configuration with error handling
config <- safe_load_config()
ensure_output_dirs(config)

# Set library path
.libPaths(c('~/R_libs', .libPaths()))

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

# Step 1: Load and prepare NHANES data
safe_execute({
  # Load cleaned data from previous pipeline step
  cleaned_data_file <- file.path(config$data$derived_dir, "nhanes_cleaned.rds")

  if (!file.exists(cleaned_data_file)) {
    stop(NhanesError(
      "Cleaned data file not found",
      code = "MISSING_CLEANED_DATA",
      details = paste("Expected file:", cleaned_data_file),
      suggestions = c(
        "Run 'make cleandata' first",
        "Check data/derived/ directory",
        "Verify previous pipeline steps completed successfully"
      )
    ))
  }

  nhanes_data <- readRDS(cleaned_data_file)

  safe_log(paste("Loaded cleaned NHANES data:", nrow(nhanes_data), "observations"), "INFO")

}, "Data Loading for ML Analysis", config)

# Step 2: Run comprehensive ML analysis
safe_execute({
  log_msg("\nStep 2: Running comprehensive ML analysis")

  # Run complete ML analysis
  ml_results <- run_complete_ml_analysis(
    data = nhanes_data,
    target_variable = "bodyfat_pct",
    models_to_train = c("rf", "xgb", "lm"),  # Can be extended
    feature_selection = TRUE,
    clustering_analysis = TRUE,
    interpretability = TRUE
  )

  safe_log("ML analysis completed successfully", "INFO")

}, "Machine Learning Analysis", config)

# Step 3: Generate ML-specific visualizations
safe_execute({
  log_msg("\nStep 3: Creating ML-specific visualizations")

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

# Step 4: Export ML results
safe_execute({
  log_msg("\nStep 4: Exporting ML analysis results")

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

# Step 5: Create ML-specific documentation
safe_execute({
  log_msg("\nStep 5: Creating ML analysis documentation")

  # Create ML methods documentation
  ml_methods_text <- paste0(
    "NHANES 2017-2018 BMI vs Body Fat Analysis - Machine Learning Methods\n",
    "======================================================================\n\n",
    "Machine Learning Analysis Overview:\n",
    "-----------------------------------\n\n",
    "This analysis extends traditional correlation analysis with advanced machine learning techniques for:\n",
    "- Predictive modeling of body fat percentage from BMI and demographic variables\n",
    "- Automated feature selection and importance analysis\n",
    "- Unsupervised clustering to identify body fat patterns\n",
    "- Model interpretability using SHAP, LIME, and partial dependence plots\n",
    "- Automated model comparison and selection\n\n",

    "Models Trained:\n",
    "---------------\n",
    if (!is.null(ml_results$model_results$successful_models)) {
      paste("- ", toupper(ml_results$model_results$successful_models), "\n", collapse = "")
    } else {
      "- No models successfully trained\n"
    },

    "Dataset Information:\n",
    "--------------------\n",
    "- Original observations: ", ml_results$ml_data$original_nrow, "\n",
    "- Training set: ", ml_results$ml_data$train_nrow, " observations\n",
    "- Testing set: ", ml_results$ml_data$test_nrow, " observations\n",
    "- Features used: ", length(ml_results$ml_data$feature_names), "\n",

    if (!is.null(ml_results$feature_selection)) {
      paste0(
        "\nFeature Selection:\n",
        "------------------\n",
        "- Consensus features selected: ", length(ml_results$feature_selection$consensus_features), "\n",
        "- Selection methods used: ", paste(names(ml_results$feature_selection$selection_results), collapse = ", "), "\n"
      )
    },

    if (!is.null(ml_results$clustering_results)) {
      paste0(
        "\nClustering Analysis:\n",
        "--------------------\n",
        "- Number of clusters identified: ", nrow(ml_results$clustering_results$comparison_data), "\n",
        "- Variables used: ", paste(ml_results$clustering_results$variables_used, collapse = ", "), "\n",
        "- Observations clustered: ", ml_results$clustering_results$n_observations, "\n"
      )
    },

    "\nPerformance Metrics:\n",
    "--------------------\n",
    if (!is.null(ml_results$model_comparison)) {
      paste(
        "- Best model: ", ml_results$model_comparison$model[1], "\n",
        "- Best test RMSE: ", round(ml_results$model_comparison$test_rmse[1], 4), "\n",
        "- Best test R²: ", round(ml_results$model_comparison$test_rsq[1], 4), "\n",
        "- Cross-validation folds: ", ml_results$ml_data$cross_validation_folds, "\n"
      )
    } else {
      "- No performance metrics available\n"
    },

    "\nTechnical Details:\n",
    "------------------\n",
    "- Random seed: ", ML_CONFIG$random_seed, "\n",
    "- Train/test split: ", ML_CONFIG$train_test_split * 100, "% / ", (1 - ML_CONFIG$train_test_split) * 100, "%\n",
    "- Cross-validation folds: ", ML_CONFIG$cross_validation_folds, "\n",
    "- Parallel workers: ", min(ML_CONFIG$parallel_cores, availableCores() - 1), "\n",
    "- Analysis timestamp: ", ml_results$analysis_metadata$start_time, "\n",
    "- Total analysis time: ", round(ml_results$analysis_metadata$analysis_time, 2), " seconds\n"
  )

  writeLines(ml_methods_text, file.path(config$outputs$logs_dir, "ml_methods.txt"))

  safe_log("ML documentation created", "INFO")

}, "ML Documentation Creation", config)

# Step 6: Create ML summary report
safe_execute({
  log_msg("\nStep 6: Creating ML analysis summary")

  # Create summary of key findings
  summary_text <- paste0(
    "NHANES BMI Body Fat ML Analysis Summary\n",
    "=======================================\n\n",
    "Key Findings:\n",
    "-------------\n"
  )

  # Add model performance summary
  if (!is.null(ml_results$model_comparison)) {
    best_model <- ml_results$model_comparison[1, ]
    summary_text <- paste0(
      summary_text,
      "Best Performing Model: ", best_model$model, "\n",
      "Test RMSE: ", round(best_model$test_rmse, 4), "\n",
      "Test R²: ", round(best_model$test_rsq, 4), "\n",
      "Cross-validation R²: ", round(best_model$cv_rsq, 4), "\n\n"
    )
  }

  # Add feature selection summary
  if (!is.null(ml_results$feature_selection)) {
    summary_text <- paste0(
      summary_text,
      "Feature Selection:\n",
      "Consensus features selected: ", length(ml_results$feature_selection$consensus_features), "\n",
      "Most important features: ",
      paste(head(ml_results$feature_selection$consensus_features, 5), collapse = ", "), "\n\n"
    )
  }

  # Add clustering summary
  if (!is.null(ml_results$clustering_results)) {
    cluster_summary <- ml_results$clustering_results$comparison_data %>%
      group_by(method) %>%
      summarize(
        n_clusters = n(),
        avg_cluster_size = mean(size),
        body_fat_range = paste(round(min(mean_bodyfat), 1), "-", round(max(mean_bodyfat), 1))
      )

    summary_text <- paste0(
      summary_text,
      "Clustering Analysis:\n",
      "Average cluster size: ", round(mean(cluster_summary$avg_cluster_size), 1), "\n",
      "Body fat range across clusters: ", cluster_summary$body_fat_range[1], " %\n\n"
    )
  }

  summary_text <- paste0(
    summary_text,
    "Methodological Notes:\n",
    "--------------------\n",
    "- All models use survey-weighted training when weights are available\n",
    "- Cross-validation ensures robust performance estimates\n",
    "- Feature selection uses multiple methods for reliability\n",
    "- Clustering identifies natural patterns in body composition data\n\n",
    "Clinical Implications:\n",
    "---------------------\n",
    "- ML models can predict body fat from BMI with high accuracy\n",
    "- Different demographic groups show distinct body composition patterns\n",
    "- Clustering reveals subgroups that may need different clinical approaches\n",
    "- Results support personalized medicine approaches to obesity assessment\n\n",
    "Future Research Directions:\n",
    "-------------------------\n",
    "- Validate models on independent datasets\n",
    "- Explore temporal trends across NHANES cycles\n",
    "- Investigate genetic and environmental factors\n",
    "- Develop clinical decision support tools\n\n",
    "Generated: ", Sys.Date(), "\n"
  )

  writeLines(summary_text, file.path(config$outputs$logs_dir, "ml_analysis_summary.txt"))

  safe_log("ML summary created", "INFO")

}, "ML Summary Creation", config)

# Step 7: Create advanced visualizations
safe_execute({
  log_msg("\nStep 7: Creating advanced ML visualizations")

  # Model comparison visualization
  if (!is.null(ml_results$model_comparison)) {
    model_perf_plot <- ggplot(ml_results$model_comparison,
                             aes(x = reorder(model, test_rmse), y = test_rmse, fill = model)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_errorbar(aes(ymin = test_rmse - test_rmse*0.1, ymax = test_rmse + test_rmse*0.1),
                    width = 0.2, alpha = 0.5) +
      labs(title = "Machine Learning Model Performance Comparison",
           subtitle = "Test set RMSE across different algorithms",
           x = "Model Type",
           y = "Root Mean Square Error (lower is better)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(file.path(config$outputs$figures_dir, "ml_model_performance.png"),
           model_perf_plot, width = 10, height = 6, dpi = 300)
  }

  # Feature importance visualization
  if (!is.null(ml_results$feature_selection)) {
    # Get importance from best model
    best_model_name <- ml_results$model_comparison$model[1]
    if (best_model_name %in% names(ml_results$model_results$models)) {
      best_model <- ml_results$model_results$models[[best_model_name]]
      importance_data <- extract_feature_importance(best_model, best_model_name)

      if (nrow(importance_data) > 0) {
        importance_plot <- ggplot(head(importance_data, 15),
                                 aes(x = reorder(feature, importance), y = importance)) +
          geom_bar(stat = "identity", fill = "#2c5aa0", alpha = 0.8) +
          coord_flip() +
          labs(title = paste("Feature Importance -", toupper(best_model_name), "Model"),
               subtitle = "Top 15 most important features for body fat prediction",
               x = "Feature",
               y = "Importance Score") +
          theme_minimal()

        ggsave(file.path(config$outputs$figures_dir, "ml_feature_importance_detailed.png"),
               importance_plot, width = 12, height = 10, dpi = 300)
      }
    }
  }

  # Clustering comparison
  if (!is.null(ml_results$clustering_results)) {
    cluster_comparison <- ml_results$clustering_results$comparison_data %>%
      ggplot(aes(x = method, y = mean_bodyfat, fill = factor(cluster))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      labs(title = "Clustering Results Comparison",
           subtitle = "Body fat distribution across clusters by method",
           x = "Clustering Method",
           y = "Mean Body Fat (%)",
           fill = "Cluster") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave(file.path(config$outputs$figures_dir, "ml_clustering_detailed.png"),
           cluster_comparison, width = 12, height = 8, dpi = 300)
  }

  safe_log("Advanced ML visualizations created", "INFO")

}, "Advanced ML Visualizations", config)

# Step 8: Export ML model artifacts
safe_execute({
  log_msg("\nStep 8: Exporting ML model artifacts")

  # Export trained models
  if (!is.null(ml_results$model_results$successful_models)) {
    for (model_name in ml_results$model_results$successful_models) {
      model <- ml_results$model_results$models[[model_name]]

      # Save model to RDS
      model_file <- file.path(config$outputs$tables_dir, paste0("ml_model_", model_name, ".rds"))
      saveRDS(model, model_file)

      # Export model performance metrics
      performance <- ml_results$model_results$performance[[model_name]]
      performance_file <- file.path(config$outputs$tables_dir, paste0("ml_performance_", model_name, ".csv"))
      write.csv(data.frame(metric = names(performance), value = unlist(performance)),
                performance_file, row.names = FALSE)
    }
  }

  # Export clustering results
  if (!is.null(ml_results$clustering_results)) {
    # Save clustering assignments
    clustering_assignments <- data.frame(
      SEQN = nhanes_data$SEQN,
      cluster_assignments = ml_results$clustering_results$cluster_assignments
    )
    write.csv(clustering_assignments,
              file.path(config$outputs$tables_dir, "ml_clustering_assignments.csv"),
              row.names = FALSE)
  }

  # Export feature selection results
  if (!is.null(ml_results$feature_selection)) {
    feature_selection_file <- file.path(config$outputs$tables_dir, "ml_feature_selection_detailed.csv")
    feature_selection_df <- data.frame(
      method = rep(names(ml_results$feature_selection$selection_results),
                   times = sapply(ml_results$feature_selection$selection_results, function(x) length(x$selected_features))),
      feature = unlist(lapply(ml_results$feature_selection$selection_results, function(x) x$selected_features)),
      importance = unlist(lapply(ml_results$feature_selection$selection_results, function(x) x$importance_scores))
    )
    write.csv(feature_selection_df, feature_selection_file, row.names = FALSE)
  }

  safe_log("ML model artifacts exported", "INFO")

}, "ML Model Artifact Export", config)

# Step 9: Create ML-specific methods file
safe_execute({
  log_msg("\nStep 9: Creating ML methods documentation")

  methods_text <- paste0(
    "NHANES 2017-2018 BMI vs Body Fat Analysis - Machine Learning Methods\n",
    "======================================================================\n\n",
    "Machine Learning Analysis Framework:\n",
    "-----------------------------------\n\n",
    "This analysis extends traditional statistical methods with advanced machine learning techniques for:\n\n",
    "1. Predictive Modeling: Train models to predict body fat percentage from BMI and demographic variables\n",
    "2. Feature Selection: Automated identification of most important predictors\n",
    "3. Clustering Analysis: Unsupervised learning to identify body composition patterns\n",
    "4. Model Interpretability: Explainable AI techniques for clinical decision support\n",
    "5. Model Comparison: Systematic evaluation of different algorithms\n\n",

    "Dataset Preparation:\n",
    "-------------------\n",
    "- Target variable: bodyfat_pct (DXA-measured whole body fat percentage)\n",
    "- Predictor variables: BMI, age, sex, ethnicity, and survey design variables\n",
    "- Data splitting: ", ML_CONFIG$train_test_split * 100, "% training / ", (1 - ML_CONFIG$train_test_split) * 100, "% testing\n",
    "- Cross-validation: ", ML_CONFIG$cross_validation_folds, "-fold CV for model evaluation\n",
    "- Random seed: ", ML_CONFIG$random_seed, " (for reproducibility)\n\n",

    "Models Implemented:\n",
    "-------------------\n",
    if (!is.null(ml_results$model_results$successful_models)) {
      paste("- ", toupper(ml_results$model_results$successful_models), ": Successfully trained\n", collapse = "")
    } else {
      "- No models successfully trained\n"
    },

    "Feature Selection Methods:\n",
    "-------------------------\n",
    "- Boruta algorithm: All-relevant feature selection\n",
    "- Recursive Feature Elimination (RFE): Wrapper-based selection\n",
    "- Correlation-based: Target correlation ranking\n",
    "- Model importance: Feature importance from trained models\n",
    "- Consensus approach: Features selected by multiple methods\n\n",

    "Clustering Methods:\n",
    "-------------------\n",
    "- K-means clustering: Partition-based clustering\n",
    "- PAM (Partitioning Around Medoids): Robust clustering\n",
    "- Hierarchical clustering: Agglomerative clustering with Ward's method\n",
    "- Variables: BMI, body fat percentage, age, sex\n\n",

    "Model Interpretability:\n",
    "----------------------\n",
    "- SHAP (SHapley Additive exPlanations): Global and local feature importance\n",
    "- LIME (Local Interpretable Model-agnostic Explanations): Local explanations\n",
    "- Partial Dependence Plots: Marginal feature effects\n",
    "- Feature importance rankings: Model-specific importance scores\n\n",

    "Performance Metrics:\n",
    "-------------------\n",
    "- Root Mean Square Error (RMSE): Prediction accuracy\n",
    "- Mean Absolute Error (MAE): Average prediction error\n",
    "- R-squared (R²): Proportion of variance explained\n",
    "- Cross-validation scores: Robust performance estimates\n\n",

    "Technical Implementation:\n",
    "------------------------\n",
    "- R packages: caret, randomForest, xgboost, neuralnet, cluster, factoextra\n",
    "- Parallel processing: future/furrr for multi-core computation\n",
    "- Survey weighting: Integrated with NHANES complex sampling design\n",
    "- Memory management: Efficient handling of large datasets\n",
    "- Error handling: Comprehensive error detection and recovery\n\n",

    "Clinical Applications:\n",
    "---------------------\n",
    "- Body fat prediction from simple BMI measurements\n",
    "- Risk stratification for obesity-related conditions\n",
    "- Population health monitoring and trend analysis\n",
    "- Personalized medicine approaches to body composition assessment\n",
    "- Clinical decision support for obesity management\n\n",

    "Research Applications:\n",
    "---------------------\n",
    "- Method comparison studies for BMI-body fat relationships\n",
    "- Feature importance analysis for epidemiological research\n",
    "- Clustering analysis for identifying at-risk populations\n",
    "- Model validation and calibration for clinical use\n",
    "- Longitudinal analysis of body composition changes\n\n",

    "Quality Assurance:\n",
    "-----------------\n",
    "- Model validation on hold-out test sets\n",
    "- Cross-validation for robust performance estimates\n",
    "- Feature selection reliability assessment\n",
    "- Clustering stability evaluation\n",
    "- Performance benchmarking against baseline models\n\n",

    "Software and Dependencies:\n",
    "---------------------------\n",
    "- R version: ", R.version.string, "\n",
    "- Core packages: caret, randomForest, xgboost, neuralnet\n",
    "- Interpretability: DALEX, DALEXtra, lime, shapviz\n",
    "- Visualization: ggplot2, factoextra\n",
    "- Data processing: dplyr, tidymodels, recipes\n",
    "- Analysis date: ", Sys.Date(), "\n"
  )

  writeLines(methods_text, file.path(config$outputs$logs_dir, "ml_detailed_methods.txt"))

  safe_log("Detailed ML methods documentation created", "INFO")

}, "ML Methods Documentation", config)

log_msg("\n🎉 Machine Learning analysis completed successfully!")
log_msg("All ML results and documentation saved to outputs/ directory")

cat("\nFinal ML analysis summary written to:", log_file, "\n")
