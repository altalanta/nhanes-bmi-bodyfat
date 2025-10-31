# R/ml_functions.R

# This file contains the granular functions for the machine learning pipeline.

#' Split data into training and testing sets
#' @param cleaned_data The cleaned NHANES dataset.
#' @return A list containing the training and testing data.
split_data <- function(cleaned_data) {
  set.seed(123)
  train_index <- createDataPartition(cleaned_data$bodyfat_pct, p = 0.8, list = FALSE)
  train_data <- cleaned_data[train_index, ]
  test_data <- cleaned_data[-train_index, ]
  list(train = train_data, test = test_data)
}

#' Define the feature engineering recipe
#' @param train_data The training dataset.
#' @return A tidymodels recipe.
create_recipe <- function(train_data) {
  recipe(bodyfat_pct ~ BMXBMI + RIDAGEYR + RIAGENDR, data = train_data) %>%
    step_dummy(all_nominal_predictors())
}

#' Train a Random Forest model
#' @param recipe A tidymodels recipe.
#' @param train_data The training dataset.
#' @return A trained workflow object.
train_rf_model <- function(recipe, train_data) {
  rf_spec <- rand_forest(trees = 100) %>%
    set_engine("randomForest") %>%
    set_mode("regression")

  workflow() %>%
    add_recipe(recipe) %>%
    add_model(rf_spec) %>%
    fit(data = train_data)
}

#' Train an XGBoost model
#' @param recipe A tidymodels recipe.
#' @param train_data The training dataset.
#' @return A trained workflow object.
train_xgb_model <- function(recipe, train_data) {
  xgb_spec <- boost_tree() %>%
    set_engine("xgboost") %>%
    set_mode("regression")

  workflow() %>%
    add_recipe(recipe) %>%
    add_model(xgb_spec) %>%
    fit(data = train_data)
}

#' Evaluate a trained model
#' @param model_workflow A trained workflow object.
#' @param test_data The testing dataset.
#' @param model_name A character string for the model name.
#' @return A data frame with the model's performance metrics.
evaluate_model <- function(model_workflow, test_data, model_name) {
  predictions <- predict(model_workflow, new_data = test_data) %>%
    bind_cols(test_data)
  
  metrics <- predictions %>%
    metrics(truth = bodyfat_pct, estimate = .pred)
  
  tibble(
    model = model_name,
    rmse = metrics$.estimate[metrics$.metric == "rmse"],
    rsq = metrics$.estimate[metrics$.metric == "rsq"]
  )
}

#' Compare model performance
#' @param ... A list of model performance data frames.
#' @return A combined data frame of model performance.
compare_models <- function(...) {
  bind_rows(...)
}

