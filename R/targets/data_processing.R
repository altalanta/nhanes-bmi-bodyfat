# R/targets/data_processing.R

# This file defines the data processing portion of the pipeline
# which includes fetching, loading, cleaning, and merging the data.

data_processing_targets <- list(
  # 1. Data fetching and validation
  tar_target(
    nhanes_files,
    fetch_nhanes_data(config),
    format = "file"
  ),
  
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
  )
)

