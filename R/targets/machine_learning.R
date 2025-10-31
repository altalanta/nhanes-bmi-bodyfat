# R/targets/machine_learning.R

# This file defines the machine learning portion of the pipeline.

machine_learning_targets <- list(
  tar_target(
    ml_data_split,
    split_data(cleaned_data)
  ),
  tar_target(
    ml_recipe,
    create_recipe(ml_data_split$train)
  ),
  tar_target(
    rf_model,
    train_rf_model(ml_recipe, ml_data_split$train)
  ),
  tar_target(
    xgb_model,
    train_xgb_model(ml_recipe, ml_data_split$train)
  ),
  tar_target(
    rf_eval,
    evaluate_model(rf_model, ml_data_split$test, "Random Forest")
  ),
  tar_target(
    xgb_eval,
    evaluate_model(xgb_model, ml_data_split$test, "XGBoost")
  ),
  tar_target(
    model_comparison,
    compare_models(rf_eval, xgb_eval)
  )
)
