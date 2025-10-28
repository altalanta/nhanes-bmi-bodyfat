# Machine Learning Integration Framework for NHANES BMI Body Fat Analysis
# Provides comprehensive ML capabilities for predictive modeling and pattern discovery

library(caret)
library(randomForest)
library(xgboost)
library(neuralnet)
library(cluster)
library(factoextra)
library(DALEX)
library(DALEXtra)
library(lime)
library(shapviz)
library(recipes)
library(themis)
library(tidymodels)
library(workflows)
library(tune)
library(finetune)
library(stacks)

# ML configuration
ML_CONFIG <- list(
  random_seed = 123,
  cross_validation_folds = 5,
  train_test_split = 0.8,
  parallel_cores = availableCores() - 1,
  model_comparison_metrics = c("RMSE", "MAE", "R2"),
  feature_selection_methods = c("boruta", "rfe", "correlation", "importance"),
  resampling_methods = c("cv", "repeatedcv", "boot", "lgocv")
)

# Set random seed for reproducibility
set.seed(ML_CONFIG$random_seed)

# Core ML Functions

#' Prepare data for machine learning analysis
#'
#' @param data NHANES dataset
#' @param target_variable Target variable for prediction (default: "bodyfat_pct")
#' @param feature_variables Predictor variables (default: all except target and metadata)
#' @param remove_missing Logical to remove rows with missing values
#' @param handle_categorical Method for handling categorical variables
#' @return List with training and testing data
prepare_ml_data <- function(
    data,
    target_variable = "bodyfat_pct",
    feature_variables = NULL,
    remove_missing = TRUE,
    handle_categorical = "dummy"
) {

  # Validate target variable exists
  if (!target_variable %in% names(data)) {
    stop(paste("Target variable", target_variable, "not found in dataset"))
  }

  # Select feature variables
  if (is.null(feature_variables)) {
    # Exclude target and metadata columns
    exclude_cols <- c(target_variable, "SEQN", "WTMEC2YR", "SDMVSTRA", "SDMVPSU")
    feature_variables <- setdiff(names(data), exclude_cols)
  }

  # Ensure all feature variables exist
  missing_features <- setdiff(feature_variables, names(data))
  if (length(missing_features) > 0) {
    warning(paste("Missing feature variables:", paste(missing_features, collapse = ", ")))
    feature_variables <- intersect(feature_variables, names(data))
  }

  # Prepare dataset
  ml_data <- data[, c(feature_variables, target_variable)]

  # Handle missing values
  if (remove_missing) {
    ml_data <- ml_data[complete.cases(ml_data), ]
    cat("Removed", nrow(data) - nrow(ml_data), "rows with missing values\n")
  }

  # Handle categorical variables
  categorical_cols <- names(ml_data)[sapply(ml_data, is.factor)]
  if (length(categorical_cols) > 0 && handle_categorical == "dummy") {
    # Convert categorical to dummy variables
    ml_data <- fastDummies::dummy_cols(ml_data, select_columns = categorical_cols,
                                      remove_first_dummy = TRUE, remove_selected_columns = TRUE)
  }

  # Split into training and testing sets
  set.seed(ML_CONFIG$random_seed)
  train_index <- createDataPartition(ml_data[[target_variable]], p = ML_CONFIG$train_test_split, list = FALSE)
  train_data <- ml_data[train_index, ]
  test_data <- ml_data[-train_index, ]

  # Prepare survey weights for training (if available)
  weights <- NULL
  if ("WTMEC2YR" %in% names(data)) {
    weights <- data$WTMEC2YR[train_index]
  }

  result <- list(
    train_data = train_data,
    test_data = test_data,
    feature_names = feature_variables,
    target_name = target_variable,
    weights = weights,
    original_nrow = nrow(data),
    train_nrow = nrow(train_data),
    test_nrow = nrow(test_data)
  )

  cat("ML data prepared:\n")
  cat("  Original dataset:", result$original_nrow, "observations\n")
  cat("  Training set:", result$train_nrow, "observations\n")
  cat("  Testing set:", result$test_nrow, "observations\n")
  cat("  Features:", length(result$feature_names), "\n")

  return(result)
}

#' Train multiple ML models for comparison
#'
#' @param ml_data Prepared ML data from prepare_ml_data()
#' @param models_to_train Vector of model types to train
#' @param tune_models Logical to perform hyperparameter tuning
#' @param cross_validation_folds Number of CV folds
#' @return List of trained models with performance metrics
train_ml_models <- function(
    ml_data,
    models_to_train = c("rf", "xgb", "lm", "nn"),
    tune_models = TRUE,
    cross_validation_folds = ML_CONFIG$cross_validation_folds
) {

  cat("Training ML models:", paste(models_to_train, collapse = ", "), "\n")

  # Setup parallel processing
  original_plan <- plan()
  plan(multisession, workers = min(ML_CONFIG$parallel_cores, availableCores() - 1))

  models <- list()
  performance_results <- list()

  for (model_type in models_to_train) {
    cat("  Training", toupper(model_type), "model...\n")

    tryCatch({
      model_result <- train_single_model(
        ml_data, model_type, tune_models, cross_validation_folds
      )

      models[[model_type]] <- model_result$model
      performance_results[[model_type]] <- model_result$performance

      cat("    ✅", toupper(model_type), "model trained successfully\n")
      cat("      Best parameters:", paste(names(model_result$model$bestTune), collapse = ", "), "\n")
      cat("      CV RMSE:", round(model_result$performance$cv_rmse, 4), "\n")

    }, error = function(e) {
      warning(paste("Failed to train", toupper(model_type), "model:", e$message))
      models[[model_type]] <- NULL
      performance_results[[model_type]] <- list(
        error = e$message,
        status = "failed"
      )
    })
  }

  # Restore original plan
  plan(original_plan)

  # Compile results
  successful_models <- names(models)[!sapply(models, is.null)]
  cat("Successfully trained", length(successful_models), "out of", length(models_to_train), "models\n")

  result <- list(
    models = models,
    performance = performance_results,
    successful_models = successful_models,
    training_data = ml_data,
    config = list(
      models_trained = models_to_train,
      tuning_enabled = tune_models,
      cv_folds = cross_validation_folds,
      timestamp = Sys.time()
    )
  )

  return(result)
}

#' Train a single ML model with hyperparameter tuning
#'
#' @param ml_data Prepared ML data
#' @param model_type Type of model ("rf", "xgb", "lm", "nn")
#' @param tune_model Logical to perform tuning
#' @param cv_folds Number of cross-validation folds
#' @return List with model and performance metrics
train_single_model <- function(ml_data, model_type, tune_model, cv_folds) {

  # Define model specifications
  model_specs <- switch(model_type,
    "rf" = list(
      method = "rf",
      tuneGrid = if (tune_model) {
        expand.grid(mtry = c(2, 4, 6, 8, 10))
      } else {
        expand.grid(mtry = 4)
      }
    ),
    "xgb" = list(
      method = "xgbTree",
      tuneGrid = if (tune_model) {
        expand.grid(
          nrounds = c(50, 100, 150),
          max_depth = c(3, 6, 9),
          eta = c(0.1, 0.3),
          gamma = c(0, 1),
          colsample_bytree = c(0.6, 0.8, 1.0),
          min_child_weight = c(1, 5),
          subsample = c(0.8, 1.0)
        )
      } else {
        expand.grid(
          nrounds = 100,
          max_depth = 6,
          eta = 0.3,
          gamma = 0,
          colsample_bytree = 1.0,
          min_child_weight = 1,
          subsample = 1.0
        )
      }
    ),
    "lm" = list(
      method = "lm",
      tuneGrid = expand.grid(intercept = TRUE)  # No tuning needed for linear model
    ),
    "nn" = list(
      method = "neuralnet",
      tuneGrid = if (tune_model) {
        expand.grid(
          layer1 = c(5, 10, 15),
          layer2 = c(0, 5, 10),
          layer3 = c(0, 5)
        )
      } else {
        expand.grid(
          layer1 = 10,
          layer2 = 5,
          layer3 = 0
        )
      }
    )
  )

  if (is.null(model_specs)) {
    stop(paste("Unknown model type:", model_type))
  }

  # Prepare training control
  train_control <- trainControl(
    method = "cv",
    number = cv_folds,
    verboseIter = FALSE,
    allowParallel = TRUE
  )

  # Train model
  model <- train(
    as.formula(paste(ml_data$target_name, "~ .")),
    data = ml_data$train_data,
    method = model_specs$method,
    tuneGrid = model_specs$tuneGrid,
    trControl = train_control,
    weights = ml_data$weights
  )

  # Calculate performance metrics
  predictions <- predict(model, ml_data$test_data)
  actual <- ml_data$test_data[[ml_data$target_name]]

  performance <- list(
    model_type = model_type,
    best_params = model$bestTune,
    cv_rmse = min(model$results$RMSE),
    cv_rsq = max(model$results$Rsquared),
    test_rmse = sqrt(mean((predictions - actual)^2)),
    test_mae = mean(abs(predictions - actual)),
    test_rsq = cor(predictions, actual)^2,
    feature_importance = extract_feature_importance(model, model_type)
  )

  return(list(
    model = model,
    performance = performance
  ))
}

#' Extract feature importance from trained model
#'
#' @param model Trained model object
#' @param model_type Type of model
#' @return Data frame with feature importance scores
extract_feature_importance <- function(model, model_type) {

  tryCatch({
    switch(model_type,
      "rf" = {
        importance <- randomForest::importance(model$finalModel)
        if (is.matrix(importance)) {
          importance_df <- data.frame(
            feature = rownames(importance),
            importance = importance[, 1],  # Use %IncMSE
            importance_type = "%IncMSE"
          )
        } else {
          importance_df <- data.frame(
            feature = names(importance),
            importance = importance,
            importance_type = "importance"
          )
        }
      },
      "xgb" = {
        importance <- xgb.importance(model = model$finalModel)
        importance_df <- data.frame(
          feature = importance$Feature,
          importance = importance$Gain,
          importance_type = "Gain"
        )
      },
      "lm" = {
        # For linear models, use coefficient magnitudes
        coefs <- coef(model$finalModel)
        importance_df <- data.frame(
          feature = names(coefs)[-1],  # Exclude intercept
          importance = abs(coefs)[-1],
          importance_type = "coefficient_magnitude"
        )
      },
      "nn" = {
        # Neural networks don't have traditional feature importance
        importance_df <- data.frame(
          feature = names(model$finalModel$weights[[1]][[1]])[-1],
          importance = rep(1, length(names(model$finalModel$weights[[1]][[1]])[-1])),
          importance_type = "neural_network"
        )
      }
    )

    importance_df <- importance_df[order(-importance_df$importance), ]
    return(importance_df)

  }, error = function(e) {
    warning(paste("Could not extract feature importance for", model_type, "model:", e$message))
    return(data.frame(
      feature = "unknown",
      importance = 0,
      importance_type = "error"
    ))
  })
}

#' Perform automated feature selection
#'
#' @param ml_data Prepared ML data
#' @param methods Feature selection methods to use
#' @param top_features Number of top features to select
#' @return List with selected features and performance metrics
perform_feature_selection <- function(
    ml_data,
    methods = ML_CONFIG$feature_selection_methods,
    top_features = 20
) {

  cat("Performing automated feature selection...\n")

  selection_results <- list()

  for (method in methods) {
    cat("  Using", method, "method...\n")

    tryCatch({
      switch(method,
        "boruta" = {
          # Boruta algorithm for feature selection
          if (requireNamespace("Boruta", quietly = TRUE)) {
            boruta_result <- Boruta::Boruta(
              as.formula(paste(ml_data$target_name, "~ .")),
              data = ml_data$train_data,
              doTrace = 0
            )

            selected_features <- Boruta::getSelectedAttributes(boruta_result)
            importance_scores <- Boruta::attStats(boruta_result)
          } else {
            warning("Boruta package not available")
            selected_features <- character(0)
            importance_scores <- NULL
          }
        },
        "rfe" = {
          # Recursive Feature Elimination
          control <- rfeControl(
            functions = rfFuncs,
            method = "cv",
            number = 5
          )

          rfe_result <- rfe(
            x = ml_data$train_data[, ml_data$feature_names],
            y = ml_data$train_data[[ml_data$target_name]],
            sizes = c(5, 10, 15, top_features),
            rfeControl = control
          )

          selected_features <- predictors(rfe_result)
          importance_scores <- rfe_result$variables$Overall
        },
        "correlation" = {
          # Correlation-based feature selection
          cor_matrix <- cor(ml_data$train_data[, ml_data$feature_names])
          target_cor <- cor(ml_data$train_data[, ml_data$feature_names],
                          ml_data$train_data[[ml_data$target_name]])

          # Select features with highest absolute correlation
          selected_idx <- order(abs(target_cor), decreasing = TRUE)[1:top_features]
          selected_features <- ml_data$feature_names[selected_idx]
          importance_scores <- abs(target_cor[selected_idx])
        },
        "importance" = {
          # Feature importance from a quick model
          quick_model <- randomForest(
            as.formula(paste(ml_data$target_name, "~ .")),
            data = ml_data$train_data,
            ntree = 50,
            importance = TRUE
          )

          importance_scores <- randomForest::importance(quick_model)[, 1]
          selected_idx <- order(importance_scores, decreasing = TRUE)[1:top_features]
          selected_features <- ml_data$feature_names[selected_idx]
          importance_scores <- importance_scores[selected_idx]
        }
      )

      selection_results[[method]] <- list(
        method = method,
        selected_features = selected_features,
        importance_scores = importance_scores,
        n_selected = length(selected_features)
      )

      cat("    Selected", length(selected_features), "features using", method, "\n")

    }, error = function(e) {
      warning(paste("Feature selection failed for", method, ":", e$message))
      selection_results[[method]] <- list(
        method = method,
        error = e$message,
        selected_features = character(0)
      )
    })
  }

  # Find consensus features (appear in multiple methods)
  all_selected <- unlist(lapply(selection_results, function(x) x$selected_features))
  feature_counts <- table(all_selected)
  consensus_features <- names(feature_counts)[feature_counts >= 2]  # Appear in at least 2 methods

  if (length(consensus_features) == 0) {
    # If no consensus, use features from the best performing method
    best_method <- names(selection_results)[which.max(sapply(selection_results, function(x) length(x$selected_features)))]
    consensus_features <- selection_results[[best_method]]$selected_features
  }

  cat("Feature selection complete:\n")
  cat("  Consensus features:", length(consensus_features), "\n")
  cat("  Methods used:", length(methods), "\n")

  return(list(
    selection_results = selection_results,
    consensus_features = consensus_features,
    all_selected = all_selected,
    feature_counts = feature_counts
  ))
}

#' Perform clustering analysis to identify body fat patterns
#'
#' @param data Dataset for clustering
#' @param variables Variables to use for clustering
#' @param n_clusters Number of clusters to identify
#' @param methods Clustering methods to use
#' @return List with clustering results and visualizations
perform_clustering_analysis <- function(
    data,
    variables = c("BMXBMI", "bodyfat_pct", "RIDAGEYR"),
    n_clusters = 4,
    methods = c("kmeans", "pam", "hclust")
) {

  cat("Performing clustering analysis...\n")

  # Prepare clustering data
  clustering_data <- data[, intersect(variables, names(data))]
  clustering_data <- clustering_data[complete.cases(clustering_data), ]

  if (nrow(clustering_data) == 0) {
    stop("No complete cases available for clustering")
  }

  # Scale data for clustering
  scaled_data <- scale(clustering_data)

  clustering_results <- list()

  for (method in methods) {
    cat("  Using", method, "clustering...\n")

    tryCatch({
      switch(method,
        "kmeans" = {
          cluster_result <- kmeans(scaled_data, centers = n_clusters, nstart = 25)
          cluster_assignments <- cluster_result$cluster
        },
        "pam" = {
          if (requireNamespace("cluster", quietly = TRUE)) {
            cluster_result <- cluster::pam(scaled_data, k = n_clusters)
            cluster_assignments <- cluster_result$clustering
          } else {
            warning("cluster package not available for PAM")
            next
          }
        },
        "hclust" = {
          distance_matrix <- dist(scaled_data)
          cluster_result <- hclust(distance_matrix, method = "ward.D2")
          cluster_assignments <- cutree(cluster_result, k = n_clusters)
        }
      )

      # Calculate cluster characteristics
      cluster_stats <- data.frame(
        cluster = 1:n_clusters,
        size = table(cluster_assignments),
        mean_bmi = tapply(data$BMXBMI, cluster_assignments, mean, na.rm = TRUE),
        mean_bodyfat = tapply(data$bodyfat_pct, cluster_assignments, mean, na.rm = TRUE),
        mean_age = tapply(data$RIDAGEYR, cluster_assignments, mean, na.rm = TRUE),
        pct_male = tapply(data$RIAGENDR - 1, cluster_assignments, mean, na.rm = TRUE) * 100
      )

      # Create visualization
      cluster_plot <- fviz_cluster(
        list(data = scaled_data, cluster = cluster_assignments),
        geom = "point",
        main = paste("Clustering Results -", toupper(method))
      )

      clustering_results[[method]] <- list(
        method = method,
        cluster_assignments = cluster_assignments,
        cluster_stats = cluster_stats,
        cluster_result = cluster_result,
        plot = cluster_plot
      )

      cat("    Found", n_clusters, "clusters with sizes:", paste(cluster_stats$size, collapse = ", "), "\n")

    }, error = function(e) {
      warning(paste("Clustering failed for", method, ":", e$message))
      clustering_results[[method]] <- list(
        method = method,
        error = e$message
      )
    })
  }

  # Create summary comparison
  comparison_data <- data.frame()
  for (method in names(clustering_results)) {
    if (!"error" %in% names(clustering_results[[method]])) {
      stats <- clustering_results[[method]]$cluster_stats
      stats$method <- method
      comparison_data <- rbind(comparison_data, stats)
    }
  }

  cat("Clustering analysis complete\n")

  return(list(
    clustering_results = clustering_results,
    comparison_data = comparison_data,
    variables_used = variables,
    n_observations = nrow(scaled_data)
  ))
}

#' Generate model interpretability explanations
#'
#' @param model Trained model
#' @param ml_data Prepared ML data
#' @param methods Interpretability methods to use
#' @param n_samples Number of samples for explanation
#' @return List with interpretability results
generate_model_explanations <- function(
    model,
    ml_data,
    methods = c("shap", "lime", "pdp"),
    n_samples = 100
) {

  cat("Generating model interpretability explanations...\n")

  explanations <- list()
  sample_data <- ml_data$test_data[1:min(n_samples, nrow(ml_data$test_data)), ]

  for (method in methods) {
    cat("  Using", method, "method...\n")

    tryCatch({
      switch(method,
        "shap" = {
          if (requireNamespace("shapviz", quietly = TRUE)) {
            # SHAP values for feature importance
            shap_explainer <- shapviz::shapviz(model$finalModel,
                                             X_pred = sample_data[, ml_data$feature_names],
                                             X_train = ml_data$train_data[, ml_data$feature_names])

            explanations[[method]] <- list(
              method = method,
              explainer = shap_explainer,
              global_importance = sv_importance(shap_explainer),
              summary_plot = sv_waterfall(shap_explainer, row_id = 1)
            )
          } else {
            warning("shapviz package not available")
          }
        },
        "lime" = {
          if (requireNamespace("lime", quietly = TRUE)) {
            # LIME for local explanations
            explainer <- lime(sample_data[, ml_data$feature_names], model)

            # Explain a few instances
            explanations_lime <- explain(
              sample_data[1:5, ml_data$feature_names],
              explainer,
              n_features = 5
            )

            explanations[[method]] <- list(
              method = method,
              explainer = explainer,
              explanations = explanations_lime
            )
          } else {
            warning("lime package not available")
          }
        },
        "pdp" = {
          # Partial dependence plots
          if (requireNamespace("pdp", quietly = TRUE)) {
            # Select top features for PDP
            feature_importance <- extract_feature_importance(model, model$method)
            top_features <- head(feature_importance$feature, 3)

            pdp_results <- list()
            for (feature in top_features) {
              if (feature %in% names(sample_data)) {
                pdp_result <- partial(model$finalModel, pred.var = feature,
                                    train = ml_data$train_data[, ml_data$feature_names])
                pdp_results[[feature]] <- pdp_result
              }
            }

            explanations[[method]] <- list(
              method = method,
              partial_plots = pdp_results
            )
          } else {
            warning("pdp package not available")
          }
        }
      )

      cat("    ✅", method, "explanations generated\n")

    }, error = function(e) {
      warning(paste("Failed to generate", method, "explanations:", e$message))
      explanations[[method]] <- list(
        method = method,
        error = e$message
      )
    })
  }

  cat("Model interpretability analysis complete\n")

  return(explanations)
}

#' Compare multiple ML models
#'
#' @param model_results List of trained models from train_ml_models()
#' @param ml_data Prepared ML data
#' @return Data frame with model comparison metrics
compare_ml_models <- function(model_results, ml_data) {

  cat("Comparing ML model performance...\n")

  comparison_results <- data.frame(
    model = character(),
    cv_rmse = numeric(),
    cv_rsq = numeric(),
    test_rmse = numeric(),
    test_mae = numeric(),
    test_rsq = numeric(),
    n_features = numeric(),
    training_time = numeric(),
    stringsAsFactors = FALSE
  )

  for (model_name in model_results$successful_models) {
    model <- model_results$models[[model_name]]
    performance <- model_results$performance[[model_name]]

    # Calculate test set predictions
    test_predictions <- predict(model, ml_data$test_data)
    test_actual <- ml_data$test_data[[ml_data$target_name]]

    test_rmse <- sqrt(mean((test_predictions - test_actual)^2))
    test_mae <- mean(abs(test_predictions - test_actual))
    test_rsq <- cor(test_predictions, test_actual)^2

    # Get number of features
    n_features <- length(ml_data$feature_names)

    comparison_results <- rbind(comparison_results, data.frame(
      model = model_name,
      cv_rmse = performance$cv_rmse,
      cv_rsq = performance$cv_rsq,
      test_rmse = test_rmse,
      test_mae = test_mae,
      test_rsq = test_rsq,
      n_features = n_features,
      training_time = NA  # Would need to track this during training
    ))
  }

  # Sort by test RMSE (lower is better)
  comparison_results <- comparison_results[order(comparison_results$test_rmse), ]

  # Add rank
  comparison_results$rank <- 1:nrow(comparison_results)

  cat("Model comparison complete:\n")
  print(comparison_results[, c("rank", "model", "test_rmse", "test_rsq", "cv_rsq")])

  return(comparison_results)
}

#' Run complete ML analysis pipeline
#'
#' @param data NHANES dataset
#' @param target_variable Target variable for prediction
#' @param models_to_train Models to train and compare
#' @param feature_selection Logical to perform feature selection
#' @param clustering_analysis Logical to perform clustering
#' @param interpretability Logical to generate model explanations
#' @return List with complete ML analysis results
run_complete_ml_analysis <- function(
    data,
    target_variable = "bodyfat_pct",
    models_to_train = c("rf", "xgb", "lm"),
    feature_selection = TRUE,
    clustering_analysis = TRUE,
    interpretability = TRUE
) {

  cat("🚀 Starting complete ML analysis pipeline...\n")

  start_time <- Sys.time()

  # Step 1: Prepare data
  cat("Step 1: Preparing data for ML analysis...\n")
  ml_data <- prepare_ml_data(data, target_variable)
  cat("✅ Data preparation complete\n\n")

  # Step 2: Feature selection (optional)
  selected_features <- NULL
  if (feature_selection) {
    cat("Step 2: Performing automated feature selection...\n")
    feature_selection_results <- perform_feature_selection(ml_data)
    selected_features <- feature_selection_results$consensus_features

    if (length(selected_features) > 0) {
      # Update ML data with selected features
      ml_data$train_data <- ml_data$train_data[, c(selected_features, target_variable)]
      ml_data$test_data <- ml_data$test_data[, c(selected_features, target_variable)]
      ml_data$feature_names <- selected_features

      cat("✅ Feature selection complete - using", length(selected_features), "features\n\n")
    } else {
      cat("⚠️ No features selected - using all available features\n\n")
    }
  }

  # Step 3: Train models
  cat("Step 3: Training and comparing ML models...\n")
  model_results <- train_ml_models(ml_data, models_to_train)
  cat("✅ Model training complete\n\n")

  # Step 4: Model comparison
  cat("Step 4: Comparing model performance...\n")
  model_comparison <- compare_ml_models(model_results, ml_data)
  cat("✅ Model comparison complete\n\n")

  # Step 5: Clustering analysis (optional)
  clustering_results <- NULL
  if (clustering_analysis) {
    cat("Step 5: Performing clustering analysis...\n")
    clustering_results <- perform_clustering_analysis(
      data,
      variables = c("BMXBMI", "bodyfat_pct", "RIDAGEYR", "RIAGENDR")
    )
    cat("✅ Clustering analysis complete\n\n")
  }

  # Step 6: Model interpretability (optional)
  explanations <- NULL
  if (interpretability && length(model_results$successful_models) > 0) {
    cat("Step 6: Generating model interpretability explanations...\n")
    best_model_name <- model_comparison$model[1]  # Use best performing model
    best_model <- model_results$models[[best_model_name]]

    explanations <- generate_model_explanations(best_model, ml_data)
    cat("✅ Model interpretability analysis complete\n\n")
  }

  # Compile final results
  end_time <- Sys.time()
  analysis_time <- difftime(end_time, start_time, units = "secs")

  final_results <- list(
    ml_data = ml_data,
    feature_selection = if (feature_selection) feature_selection_results else NULL,
    model_results = model_results,
    model_comparison = model_comparison,
    clustering_results = clustering_results,
    explanations = explanations,
    analysis_metadata = list(
      start_time = start_time,
      end_time = end_time,
      analysis_time = analysis_time,
      target_variable = target_variable,
      models_trained = models_to_train,
      features_used = length(ml_data$feature_names),
      sample_sizes = list(train = ml_data$train_nrow, test = ml_data$test_nrow)
    )
  )

  cat("🎉 Complete ML analysis pipeline finished in", round(analysis_time, 2), "seconds\n")
  cat("📊 Results include:", length(model_results$successful_models), "trained models\n")

  if (!is.null(clustering_results)) {
    cat("🔍 Clustering analysis identified", nrow(clustering_results$comparison_data), "cluster types\n")
  }

  return(final_results)
}



