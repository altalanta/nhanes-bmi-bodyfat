# NHANES BMI Body Fat Analysis Pipeline using targets
# For reproducible, parallelized, and cached workflow

library(targets)
library(future)
library(furrr)
library(dplyr)
library(ggplot2)
library(survey)
library(foreign)
library(readr)
library(yaml)

# Set up parallel processing
plan(multisession, workers = availableCores() - 1)

# Load project configuration
config <- read_yaml("config/config.yml")

# Set targets options
tar_option_set(
  packages = c("dplyr", "ggplot2", "survey", "foreign", "readr", "yaml"),
  format = "rds", # Use RDS for faster serialization
  memory = "transient", # Memory management
  garbage_collection = TRUE, # Enable garbage collection
  storage = "worker", # Storage format
  retrieval = "worker" # Retrieval format
)

# Source utility functions
library(nhanesbmi)

# Pipeline targets
list(
  # 1. Data fetching and validation
  tar_target(
    nhanes_files,
    fetch_nhanes_data(config),
    format = "file"
  ),

  # 2. Load raw datasets
  tar_target(
    demo_data,
    safe_read_xpt(file.path(config$data$raw_dir, config$nhanes$demo_file), "DEMO_J dataset")
  ),

  tar_target(
    bmx_data,
    safe_read_xpt(file.path(config$data$raw_dir, config$nhanes$bmx_file), "BMX_J dataset")
  ),

  tar_target(
    dxx_data,
    safe_read_xpt(file.path(config$data$raw_dir, config$nhanes$dxx_file), "DXX_J dataset")
  ),

  tar_target(
    dxxag_data,
    safe_read_xpt(file.path(config$data$raw_dir, config$nhanes$dxxag_file), "DXXAG_J dataset")
  ),

  # 3. Data validation (parallel)
  tar_target(
    validation_results,
    validate_nhanes_datasets(list(demo_data, bmx_data, dxx_data, dxxag_data))
  ),

  # 4. Identify body fat variables
  tar_target(
    bodyfat_variable,
    identify_bodyfat_variable(dxx_data)
  ),

  tar_target(
    android_variable,
    identify_android_fat(dxxag_data)
  ),

  tar_target(
    gynoid_variable,
    identify_gynoid_fat(dxxag_data)
  ),

  # 5. Merge and clean datasets
  tar_target(
    merged_data,
    merge_nhanes_datasets(
      demo_data, bmx_data, dxx_data, dxxag_data,
      bodyfat_var = bodyfat_variable,
      android_var = android_variable,
      gynoid_var = gynoid_variable
    )
  ),

  tar_target(
    cleaned_data,
    clean_analytic_dataset(merged_data, config)
  ),

  # 6. Create survey design (cached)
  tar_target(
    survey_design,
    create_survey_design(cleaned_data, config)
  ),

  # 7. Correlation analysis (parallel by sex)
  tar_target(
    correlation_results,
    compute_correlations_parallel(survey_design)
  ),

  # 8. BMI class analysis (parallel processing)
  tar_target(
    bmi_class_results,
    compute_bmi_class_stats_parallel(survey_design, cleaned_data)
  ),

  # 9. Linearity assessment
  tar_target(
    linearity_results,
    assess_linearity(survey_design)
  ),

  # 10. Visualization (parallel rendering)
  tar_target(
    main_plot,
    create_bmi_bodyfat_plot(cleaned_data, config$outputs$main_plot_file),
    format = "file"
  ),

  tar_target(
    bmi_class_plot,
    create_bmi_class_plot(bmi_class_results, config$outputs$bmi_class_plot_file),
    format = "file"
  ),

  # 11. Advanced analyses (optional, parallel)
  tar_target(
    sensitivity_results,
    run_sensitivity_analyses(survey_design, cleaned_data),
    deployment = "worker" # Run in background
  ),

  tar_target(
    advanced_stats,
    run_advanced_statistics(survey_design, cleaned_data),
    deployment = "worker"
  ),

  # 12. ML Analysis (optional, parallel)
  tar_target(
    ml_results,
    run_ml_analysis(cleaned_data, config)
  ),

  # 13. Export results
  tar_target(
    results_export,
    export_all_results(
      correlation_results, bmi_class_results, linearity_results,
      sensitivity_results, advanced_stats,
      config$outputs$correlation_results_file,
      config$outputs$bmi_class_results_file,
      config$outputs$methods_file
    ),
    format = "file"
  ),

  # 14. Generate report
  tar_target(
    report_html,
    render_quarto_report(results_export, config$outputs$final_report_file),
    format = "file"
  ),

  # 15. Pipeline metadata
  tar_target(
    pipeline_metadata,
    list(
      targets_version = packageVersion("targets"),
      future_version = packageVersion("future"),
      furrr_version = packageVersion("furrr"),
      workers = availableCores() - 1,
      created_at = Sys.time()
    )
  )
)
