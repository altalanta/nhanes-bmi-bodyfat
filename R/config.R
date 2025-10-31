#' Load and Validate Project Configuration
#'
#' This function loads the project's configuration from a YAML file and validates
#' its structure and contents. It checks for the presence of required sections,
#' parameters, and ensures that specified directories exist.
#'
#' @param file The path to the configuration YAML file.
#' @return A validated configuration list object.
#' @export
#' @examples
#' \dontrun{
#'   config <- load_and_validate_config("config/config.yml")
#' }
load_and_validate_config <- function(file = "config/config.yml") {
  # Check if the config file exists
  checkmate::assert_file_exists(file)

  # Load the configuration
  config <- yaml::read_yaml(file)

  # --- Validate Top-Level Sections ---
  checkmate::assert_names(
    names(config),
    must.include = c("data", "outputs", "nhanes", "analysis", "logging"),
    .var.name = "Top-level config sections"
  )

  # --- Validate 'data' Section ---
  checkmate::assert_names(
    names(config$data),
    must.include = c("raw_dir", "derived_dir"),
    .var.name = "config$data section"
  )
  checkmate::assert_directory_exists(config$data$raw_dir)
  checkmate::assert_directory_exists(config$data$derived_dir)

  # --- Validate 'outputs' Section ---
  checkmate::assert_names(
    names(config$outputs),
    must.include = c("tables_dir", "figures_dir", "logs_dir", "report_dir"),
    .var.name = "config$outputs section"
  )
  checkmate::assert_directory_exists(config$outputs$tables_dir)
  checkmate::assert_directory_exists(config$outputs$figures_dir)
  checkmate::assert_directory_exists(config$outputs$logs_dir)
  checkmate::assert_directory_exists(config$outputs$report_dir)

  # --- Validate 'analysis' Section ---
  checkmate::assert_names(
    names(config$analysis),
    must.include = c("age_range", "survey_weights_col", "strata_col", "psu_col"),
    .var.name = "config$analysis section"
  )
  checkmate::assert_numeric(config$analysis$age_range, len = 2, .var.name = "age_range")
  checkmate::assert_string(config$analysis$survey_weights_col, .var.name = "survey_weights_col")
  checkmate::assert_string(config$analysis$strata_col, .var.name = "strata_col")
  checkmate::assert_string(config$analysis$psu_col, .var.name = "psu_col")

  # If all checks pass, return the config object
  return(config)
}

