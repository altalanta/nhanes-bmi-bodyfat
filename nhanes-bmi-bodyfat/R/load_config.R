#!/usr/bin/env Rscript

# Configuration loader for NHANES BMI Body Fat Analysis

load_config <- function(config_file = "config/config.yml") {
  # Check if config file exists
  if (!file.exists(config_file)) {
    stop(paste("Configuration file not found:", config_file))
  }

  # Load yaml package if not already loaded
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("yaml package is required but not installed.")
  }

  # Read configuration
  config <- yaml::read_yaml(config_file)

  # Convert to list with proper path construction
  config_list <- list()

  # Data directories (relative to project root)
  config_list$data_raw <- config$data$raw_dir
  config_list$data_derived <- config$data$derived_dir

  # Output directories
  config_list$outputs_tables <- config$outputs$tables_dir
  config_list$outputs_figures <- config$outputs$figures_dir
  config_list$outputs_logs <- config$outputs$logs_dir
  config_list$outputs_report <- config$outputs$report_dir

  # NHANES file names
  config_list$nhanes_demo_file <- config$nhanes$demo_file
  config_list$nhanes_bmx_file <- config$nhanes$bmx_file
  config_list$nhanes_dxx_file <- config$nhanes$dxx_file
  config_list$nhanes_dxxag_file <- config$nhanes$dxxag_file

  # Analysis parameters
  config_list$age_min <- config$analysis$age_range[1]
  config_list$age_max <- config$analysis$age_range[2]
  config_list$survey_weights_col <- config$analysis$survey_weights_col
  config_list$strata_col <- config$analysis$strata_col
  config_list$psu_col <- config$analysis$psu_col

  # Logging
  config_list$log_level <- config$logging$level
  config_list$log_file <- config$logging$file

  # Construct full paths
  project_root <- here::here()
  config_list$data_raw_path <- file.path(project_root, config_list$data_raw)
  config_list$data_derived_path <- file.path(project_root, config_list$data_derived)
  config_list$outputs_tables_path <- file.path(project_root, config_list$outputs_tables)
  config_list$outputs_figures_path <- file.path(project_root, config_list$outputs_figures)
  config_list$outputs_logs_path <- file.path(project_root, config_list$outputs_logs)
  config_list$outputs_report_path <- file.path(project_root, config_list$outputs_report)

  # Construct full file paths for NHANES data
  config_list$nhanes_demo_path <- file.path(config_list$data_raw_path, config_list$nhanes_demo_file)
  config_list$nhanes_bmx_path <- file.path(config_list$data_raw_path, config_list$nhanes_bmx_file)
  config_list$nhanes_dxx_path <- file.path(config_list$data_raw_path, config_list$nhanes_dxx_file)
  config_list$nhanes_dxxag_path <- file.path(config_list$data_raw_path, config_list$nhanes_dxxag_file)

  # Log file path
  config_list$log_file_path <- file.path(config_list$outputs_logs_path, config_list$log_file)

  return(config_list)
}

# Create directory structure if it doesn't exist
ensure_directories <- function(config) {
  dirs <- c(
    config$data_raw_path,
    config$data_derived_path,
    config$outputs_tables_path,
    config$outputs_figures_path,
    config$outputs_logs_path,
    config$outputs_report_path
  )

  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}
