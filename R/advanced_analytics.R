# Advanced Machine Learning Analytics for BMI-Body Fat Relationships
# Implements various ML approaches for prediction and modeling

#' @title Advanced BMI-Body Fat Analytics
#' @description Implements machine learning approaches for advanced BMI-body fat relationship modeling
#' @import dplyr
#' @import ggplot2
#' @import caret
#' @import randomForest
#' @import xgboost
#' @import glmnet
#' @import rstanarm
#' @import Metrics
#' @import pROC
#' @export

# Load configuration and utilities
source("scripts/load_config.R")
source("scripts/error_handling.R")

# Global ML configuration
ML_CONFIG <- list(
  random_seed = 42,
  train_test_split = 0.8,
  cv_folds = 5,
  tune_length = 10,
  parallel_cores = parallel::detectCores() - 1
)

#' Load and prepare data for machine learning
#' @param data_path Path to the derived dataset
#' @return Prepared data frame for ML modeling
load_ml_data <- function(data_path = "data/derived/nhanes_analytic.rds") {
  if (!file.exists(data_path)) {
    stop("Analytic dataset not found. Run 'make cleandata' first.")
  }

  data <- readRDS(data_path)

  # Prepare features for ML
  ml_data <- data %>%
    select(
      # Target variable
      bodyfat_pct,

      # Predictor variables
      bmi, age, age_centered, sex, race_ethnicity, bmi_cat,
      weight_kg, height_cm, obesity,

      # Survey design (for stratified sampling)
      strata, psu
    ) %>%
    mutate(
      # Convert categorical variables to factors
      sex = factor(sex),
      race_ethnicity = factor(race_ethnicity),
      bmi_cat = factor(bmi_cat),

      # Create interaction terms
      bmi_age = bmi * age_centered,
      bmi_sex = bmi * as.numeric(sex == "Female"),
      age_sex = age_centered * as.numeric(sex == "Female")
    ) %>%
    # Remove missing values
    na.omit()

  cat("ML dataset prepared with", nrow(ml_data), "observations and",
      ncol(ml_data) - 1, "predictors\n")

  return(ml_data)
}

#' Perform stratified train-test split accounting for survey design
#' @param data Prepared ML data
#' @param train_prop Proportion for training set
#' @return List with train and test sets
stratified_split <- function(data, train_prop = 0.8) {
  set.seed(ML_CONFIG$random_seed)

  # Use stratified sampling based on BMI categories and sex
  strata_vars <- data %>%
    group_by(bmi_cat, sex) %>%
    tally() %>%
    mutate(prop = n / sum(n))

  # Create training indices
  train_indices <- c()
  for (i in 1:nrow(strata_vars)) {
    cat <- strata_vars$bmi_cat[i]
    sex_val <- strata_vars$sex[i]
    n_cat_sex <- strata_vars$n[i]
    n_train <- round(n_cat_sex * train_prop)

    indices <- which(data$bmi_cat == cat & data$sex == sex_val)
    train_indices <- c(train_indices, sample(indices, n_train))
  }

  # Ensure no overlap and complete coverage
  test_indices <- setdiff(1:nrow(data), train_indices)

  list(
    train = data[train_indices, ],
    test = data[test_indices, ]
  )
}

#' Linear regression baseline model
#' @param train_data Training data
#' @param test_data Test data
#' @return Model results
linear_model <- function(train_data, test_data) {
  cat("Training linear regression model...\n")

  # Fit model
  model <- lm(bodyfat_pct ~ bmi + age_centered + sex + race_ethnicity +
              bmi:age_centered + bmi:sex, data = train_data)

  # Predictions
  train_pred <- predict(model, train_data)
  test_pred <- predict(model, test_data)

  # Performance metrics
  results <- list(
    model = model,
    method = "Linear Regression",
    train_rmse = rmse(train_data$bodyfat_pct, train_pred),
    test_rmse = rmse(test_data$bodyfat_pct, test_pred),
    train_r2 = cor(train_data$bodyfat_pct, train_pred)^2,
    test_r2 = cor(test_data$bodyfat_pct, test_pred)^2,
    predictions = data.frame(
      actual = test_data$bodyfat_pct,
      predicted = test_pred,
      residual = test_data$bodyfat_pct - test_pred
    )
  )

  cat("Linear model RMSE (test):", round(results$test_rmse, 3), "\n")
  cat("Linear model R² (test):", round(results$test_r2, 3), "\n")

  return(results)
}

#' Random Forest model
#' @param train_data Training data
#' @param test_data Test data
#' @return Model results
random_forest_model <- function(train_data, test_data) {
  cat("Training random forest model...\n")

  # Prepare data for randomForest (convert factors to numeric)
  train_rf <- train_data %>%
    mutate(
      sex_num = as.numeric(sex) - 1,
      race_num = as.numeric(race_ethnicity) - 1
    )

  test_rf <- test_data %>%
    mutate(
      sex_num = as.numeric(sex) - 1,
      race_num = as.numeric(race_ethnicity) - 1
    )

  # Fit model
  model <- randomForest(
    bodyfat_pct ~ bmi + age_centered + sex_num + race_num +
                  bmi_age + bmi_sex + age_sex,
    data = train_rf,
    ntree = 500,
    importance = TRUE,
    do.trace = FALSE
  )

  # Predictions
  train_pred <- predict(model, train_rf)
  test_pred <- predict(model, test_rf)

  # Performance metrics
  results <- list(
    model = model,
    method = "Random Forest",
    train_rmse = rmse(train_rf$bodyfat_pct, train_pred),
    test_rmse = rmse(test_rf$bodyfat_pct, test_pred),
    train_r2 = cor(train_rf$bodyfat_pct, train_pred)^2,
    test_r2 = cor(test_rf$bodyfat_pct, test_pred)^2,
    importance = importance(model),
    predictions = data.frame(
      actual = test_rf$bodyfat_pct,
      predicted = test_pred,
      residual = test_rf$bodyfat_pct - test_pred
    )
  )

  cat("Random Forest RMSE (test):", round(results$test_rmse, 3), "\n")
  cat("Random Forest R² (test):", round(results$test_r2, 3), "\n")

  return(results)
}

#' XGBoost model
#' @param train_data Training data
#' @param test_data Test data
#' @return Model results
xgboost_model <- function(train_data, test_data) {
  cat("Training XGBoost model...\n")

  # Prepare data for XGBoost
  train_xgb <- train_data %>%
    mutate(
      sex_num = as.numeric(sex) - 1,
      race_num = as.numeric(race_ethnicity) - 1
    ) %>%
    select(-sex, -race_ethnicity, -bmi_cat)

  test_xgb <- test_data %>%
    mutate(
      sex_num = as.numeric(sex) - 1,
      race_num = as.numeric(race_ethnicity) - 1
    ) %>%
    select(-sex, -race_ethnicity, -bmi_cat)

  # Create matrices
  train_matrix <- as.matrix(train_xgb %>% select(-bodyfat_pct))
  test_matrix <- as.matrix(test_xgb %>% select(-bodyfat_pct))

  # Fit model with cross-validation
  cv_model <- xgb.cv(
    data = train_matrix,
    label = train_xgb$bodyfat_pct,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = FALSE,
    params = list(
      objective = "reg:squarederror",
      eta = 0.1,
      max_depth = 6,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
  )

  best_rounds <- cv_model$best_iteration

  # Train final model
  model <- xgboost(
    data = train_matrix,
    label = train_xgb$bodyfat_pct,
    nrounds = best_rounds,
    verbose = FALSE,
    params = list(
      objective = "reg:squarederror",
      eta = 0.1,
      max_depth = 6,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
  )

  # Predictions
  train_pred <- predict(model, train_matrix)
  test_pred <- predict(model, test_matrix)

  # Performance metrics
  results <- list(
    model = model,
    method = "XGBoost",
    best_rounds = best_rounds,
    train_rmse = rmse(train_xgb$bodyfat_pct, train_pred),
    test_rmse = rmse(test_xgb$bodyfat_pct, test_pred),
    train_r2 = cor(train_xgb$bodyfat_pct, train_pred)^2,
    test_r2 = cor(test_xgb$bodyfat_pct, test_pred)^2,
    predictions = data.frame(
      actual = test_xgb$bodyfat_pct,
      predicted = test_pred,
      residual = test_xgb$bodyfat_pct - test_pred
    )
  )

  cat("XGBoost RMSE (test):", round(results$test_rmse, 3), "\n")
  cat("XGBoost R² (test):", round(results$test_r2, 3), "\n")

  return(results)
}

#' Bayesian regression model
#' @param train_data Training data
#' @param test_data Test data
#' @return Model results
bayesian_model <- function(train_data, test_data) {
  cat("Training Bayesian regression model...\n")

  # Fit Bayesian model (using rstanarm for efficiency)
  model <- stan_glm(
    bodyfat_pct ~ bmi + age_centered + sex + race_ethnicity +
                  bmi:age_centered + bmi:sex,
    data = train_data,
    family = gaussian(),
    chains = 2,
    iter = 2000,
    warmup = 1000,
    cores = 2,
    refresh = 0
  )

  # Predictions
  train_pred <- predict(model, train_data)
  test_pred <- predict(model, test_data)

  # Performance metrics
  results <- list(
    model = model,
    method = "Bayesian Regression",
    train_rmse = rmse(train_data$bodyfat_pct, train_pred),
    test_rmse = rmse(test_data$bodyfat_pct, test_pred),
    train_r2 = cor(train_data$bodyfat_pct, train_pred)^2,
    test_r2 = cor(test_data$bodyfat_pct, test_pred)^2,
    predictions = data.frame(
      actual = test_data$bodyfat_pct,
      predicted = test_pred,
      residual = test_data$bodyfat_pct - test_pred
    )
  )

  cat("Bayesian model RMSE (test):", round(results$test_rmse, 3), "\n")
  cat("Bayesian model R² (test):", round(results$test_r2, 3), "\n")

  return(results)
}

#' Compare all ML models
#' @param data Prepared ML data
#' @return Comparison results
compare_models <- function(data) {
  cat("Comparing machine learning models...\n")

  # Split data
  split_data <- stratified_split(data)

  # Train all models
  models <- list(
    linear = linear_model(split_data$train, split_data$test),
    random_forest = random_forest_model(split_data$train, split_data$test),
    xgboost = xgboost_model(split_data$train, split_data$test),
    bayesian = bayesian_model(split_data$train, split_data$test)
  )

  # Create comparison table
  comparison <- data.frame(
    Method = c("Linear Regression", "Random Forest", "XGBoost", "Bayesian Regression"),
    Train_RMSE = c(models$linear$train_rmse, models$random_forest$train_rmse,
                   models$xgboost$train_rmse, models$bayesian$train_rmse),
    Test_RMSE = c(models$linear$test_rmse, models$random_forest$test_rmse,
                  models$xgboost$test_rmse, models$bayesian$test_rmse),
    Train_R2 = c(models$linear$train_r2, models$random_forest$train_r2,
                 models$xgboost$train_r2, models$bayesian$train_r2),
    Test_R2 = c(models$linear$test_r2, models$random_forest$test_r2,
                models$xgboost$test_r2, models$bayesian$test_r2)
  )

  # Print comparison
  cat("\nModel Comparison:\n")
  cat("================\n")
  print(comparison)

  # Determine best model
  best_model <- comparison$Method[which.min(comparison$Test_RMSE)]

  cat("\nBest performing model:", best_model, "\n")

  return(list(
    models = models,
    comparison = comparison,
    best_model = best_model,
    split_data = split_data
  ))
}

#' Create prediction function for new data
#' @param model_results Results from compare_models()
#' @param new_data New data for prediction
#' @return Predictions
predict_bodyfat <- function(model_results, new_data) {
  # Use the best performing model for predictions
  best_method <- model_results$best_model

  if (best_method == "XGBoost") {
    # Prepare data for XGBoost
    pred_data <- new_data %>%
      mutate(
        sex_num = as.numeric(sex) - 1,
        race_num = as.numeric(race_ethnicity) - 1,
        bmi_age = bmi * (age - 40),
        bmi_sex = bmi * as.numeric(sex == "Female"),
        age_sex = (age - 40) * as.numeric(sex == "Female")
      ) %>%
      select(bmi, age, sex_num, race_num, bmi_age, bmi_sex, age_sex)

    predictions <- predict(model_results$models$xgboost$model, as.matrix(pred_data))

  } else if (best_method == "Random Forest") {
    # Prepare data for Random Forest
    pred_data <- new_data %>%
      mutate(
        sex_num = as.numeric(sex) - 1,
        race_num = as.numeric(race_ethnicity) - 1,
        bmi_age = bmi * (age - 40),
        bmi_sex = bmi * as.numeric(sex == "Female"),
        age_sex = (age - 40) * as.numeric(sex == "Female")
      )

    predictions <- predict(model_results$models$random_forest$model, pred_data)

  } else {
    # Linear or Bayesian model
    predictions <- predict(model_results$models[[tolower(gsub(" ", "_", best_method))]]$model, new_data)
  }

  return(predictions)
}

#' Generate comprehensive ML analysis report
#' @param ml_results Results from compare_models()
#' @param output_dir Output directory for plots and tables
generate_ml_report <- function(ml_results, output_dir = "outputs") {
  cat("Generating ML analysis report...\n")

  # Create plots directory if it doesn't exist
  plots_dir <- file.path(output_dir, "figures")
  dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

  # 1. Model comparison plot
  comparison_plot <- ggplot(ml_results$comparison, aes(x = Method, y = Test_RMSE, fill = Method)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Model Comparison - Test RMSE",
         y = "Root Mean Square Error", x = "Model") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(plots_dir, "ml_model_comparison.png"), comparison_plot,
         width = 10, height = 6, dpi = 300)

  # 2. Prediction accuracy plot (using best model)
  best_model <- ml_results$models[[tolower(gsub(" ", "_", ml_results$best_model))]]
  pred_plot <- ggplot(best_model$predictions, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("Predictions vs Actual -", ml_results$best_model),
         x = "Actual Body Fat %", y = "Predicted Body Fat %")

  ggsave(file.path(plots_dir, "ml_predictions.png"), pred_plot,
         width = 8, height = 6, dpi = 300)

  # 3. Residual plot
  residual_plot <- ggplot(best_model$predictions, aes(x = predicted, y = residual)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = paste("Residuals -", ml_results$best_model),
         x = "Predicted Body Fat %", y = "Residual")

  ggsave(file.path(plots_dir, "ml_residuals.png"), residual_plot,
         width = 8, height = 6, dpi = 300)

  # 4. Feature importance (for tree-based models)
  if (ml_results$best_model %in% c("Random Forest", "XGBoost")) {
    if (ml_results$best_model == "Random Forest") {
      importance_data <- data.frame(
        Feature = rownames(ml_results$models$random_forest$importance),
        Importance = ml_results$models$random_forest$importance[, 1]
      )
    } else {
      # XGBoost feature importance
      importance_data <- xgb.importance(model = ml_results$models$xgboost$model)
    }

    importance_plot <- ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Feature Importance -", ml_results$best_model),
           x = "Feature", y = "Importance")

    ggsave(file.path(plots_dir, "ml_feature_importance.png"), importance_plot,
           width = 10, height = 6, dpi = 300)
  }

  # Save model comparison table
  write.csv(ml_results$comparison,
            file.path(output_dir, "tables", "ml_model_comparison.csv"),
            row.names = FALSE)

  cat("ML report generated successfully!\n")
  cat("Plots saved to:", plots_dir, "\n")
  cat("Tables saved to:", file.path(output_dir, "tables"), "\n")
}

#' Run complete advanced ML analysis
#' @param config Project configuration
#' @return Complete ML analysis results
run_advanced_ml_analysis <- function(config = safe_load_config()) {
  cat("🚀 Starting Advanced ML Analysis for BMI-Body Fat Prediction\n")
  cat("==========================================================\n")

  # Load data
  ml_data <- load_ml_data()

  # Compare models
  ml_results <- compare_models(ml_data)

  # Generate report
  generate_ml_report(ml_results)

  # Save results
  results_path <- file.path(config$outputs$tables_dir, "ml_analysis_results.rds")
  saveRDS(ml_results, results_path)

  cat("\n✅ Advanced ML analysis complete!\n")
  cat("Results saved to:", results_path, "\n")

  return(ml_results)
}

# Export functions for use in other scripts
#' @export
if (getRversion() >= "2.15.1") {
  globalVariables(c("bodyfat_pct", "bmi", "age_centered", "sex", "race_ethnicity",
                    "bmi_age", "bmi_sex", "age_sex", "sex_num", "race_num",
                    "bmi_cat", "Method", "Test_RMSE", "Feature", "Importance"))
}