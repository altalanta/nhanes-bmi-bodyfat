# The "Dissemination Engine": Automated Publication & Interactive Reporting Suite
# This engine automates the "last mile" of the research process, transforming
# results directly into communication-ready artifacts.

# Required libraries
library(quarto)
library(shiny)
library(zip)
library(readr)
library(jsonlite)
library(knitr)
library(ggplot2)

# --- Configuration ---

#' @title Dissemination Engine Configuration
#' @description Defines settings for report generation, dashboards, and reproducibility packages.
DISSEMINATION_CONFIG <- list(
  # Manuscript settings
  manuscript = list(
    template_path = "templates/manuscript_template.qmd",
    default_format = "pdf",
    output_dir = "outputs/manuscripts",
    required_artifacts = c(
      "meta_analysis_results.rds",
      "integrated_harmonized_data.csv"
    )
  ),
  
  # Interactive dashboard settings
  dashboard = list(
    template_path = "templates/dashboard_app.R",
    output_dir = "outputs/dashboards",
    required_artifacts = c("meta_analysis_forest_plot.png")
  ),
  
  # Reproducibility package settings
  repro_package = list(
    output_dir = "outputs/repro_packages",
    files_to_include = c(
      "renv.lock",
      "scripts/",
      "R/",
      "config/",
      "main.R", # Assuming a main orchestrator script
      "Makefile"
    ),
    data_pointers = list(
      "outputs/derived_data/integrated_harmonized_data.csv"
    )
  ),
  
  # Author and project metadata
  project_metadata = list(
    title = "The Relationship Between BMI and Body Fat Percentage: A Multi-Study Meta-Analysis",
    authors = "NHANES BMI Body Fat Analysis Platform (Automated)",
    affiliation = "Institute for Computational Epidemiology",
    date = Sys.Date(),
    abstract = "This report presents an automated meta-analysis of the relationship between Body Mass Index (BMI) and body fat percentage across multiple demographic surveys. The analysis leverages a harmonized dataset from sources including the National Health and Nutrition Examination Survey (NHANES) and the UK Biobank."
  )
)

# --- Core Engine Functions ---

#' @title Initialize Dissemination Engine
#' @description Checks for required tools and directories.
#' @param config The engine configuration.
#' @return Invisible TRUE on success.
initialize_dissemination_engine <- function(config = DISSEMINATION_CONFIG) {
  message("Initializing Dissemination Engine...")
  
  # Check for Quarto CLI
  if (!quarto_is_available()) {
    stop("Quarto CLI not found. Please install it to use manuscript generation features.")
  }
  
  # Create output directories
  dir.create(config$manuscript$output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(config$dashboard$output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(config$repro_package$output_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Dissemination Engine initialized successfully.")
  return(invisible(TRUE))
}


#' @title Generate Manuscript
#' @description Renders a publication-ready manuscript from a Quarto template.
#' @param format The output format (e.g., "pdf", "docx").
#' @param output_filename The name of the output file.
#' @param config The engine configuration.
#' @return The path to the generated manuscript.
generate_manuscript <- function(format = DISSEMINATION_CONFIG$manuscript$default_format,
                                output_filename = paste0("manuscript_", Sys.Date()),
                                config = DISSEMINATION_CONFIG) {
  
  message("Starting manuscript generation...")
  
  # 1. Check for required artifacts
  # This requires the main project config, let's assume it's loaded.
  project_config <- safe_load_config()
  required_files <- c(
    file.path("derived_data", "meta_analysis_results.rds"),
    file.path("derived_data", "integrated_harmonized_data.csv"),
    file.path("figures", "meta_analysis_forest_plot.png"),
    file.path("tables", "integration_report_summary.txt")
  )
  check_for_artifacts(required_files, project_config)
  
  # 2. Define paths and parameters
  template_path <- config$manuscript$template_path
  output_dir <- config$manuscript$output_dir
  output_file <- paste0(output_filename, ".", format)
  full_output_path <- file.path(output_dir, output_file)
  
  # Define the parameters to be passed to the Quarto template
  render_params <- list(
    project_metadata = config$project_metadata,
    analysis_paths = list(
      meta_results = file.path(project_config$outputs$derived_data_dir, "meta_analysis_results.rds"),
      integrated_data = file.path(project_config$outputs$derived_data_dir, "integrated_harmonized_data.csv"),
      forest_plot = file.path(project_config$outputs$figures_dir, "meta_analysis_forest_plot.png"),
      integration_summary = file.path(project_config$outputs$tables_dir, "integration_report_summary.txt")
    )
  )
  
  message("Rendering Quarto template: '", template_path, "' to '", full_output_path, "'")
  
  # 3. Render the Quarto document
  quarto_render(
    input = template_path,
    output_file = output_file,
    output_dir = output_dir,
    execute_params = render_params,
    quiet = FALSE
  )
  
  if (file.exists(full_output_path)) {
    message("Manuscript generated successfully at: ", full_output_path)
    return(full_output_path)
  } else {
    stop("Manuscript generation failed. Output file not found.")
  }
}


#' @title Generate Interactive Dashboard
#' @description Creates a self-contained Shiny dashboard application.
#' @param dashboard_name The name for the dashboard directory.
#' @param config The engine configuration.
#' @return The path to the dashboard directory.
generate_interactive_dashboard <- function(dashboard_name = paste0("meta_analysis_dashboard_", Sys.Date()),
                                           config = DISSEMINATION_CONFIG) {
  
  message("Starting interactive dashboard generation...")
  
  # 1. Check for required artifacts
  project_config <- safe_load_config()
  required_files <- c(file.path("figures", "meta_analysis_forest_plot.png"))
  check_for_artifacts(required_files, project_config)
  
  # 2. Define paths
  dashboard_dir <- file.path(config$dashboard$output_dir, dashboard_name)
  template_path <- config$dashboard$template_path
  app_path <- file.path(dashboard_dir, "app.R")
  
  # 3. Create directory and copy files
  dir.create(dashboard_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Copy the Shiny app template
  if (!file.copy(template_path, app_path, overwrite = TRUE)) {
    stop("Failed to copy dashboard template to ", app_path)
  }
  
  # Copy the required plot into the dashboard directory
  source_plot_path <- file.path(project_config$outputs$figures_dir, "meta_analysis_forest_plot.png")
  dest_plot_path <- file.path(dashboard_dir, "meta_analysis_forest_plot.png")
  if (!file.copy(source_plot_path, dest_plot_path, overwrite = TRUE)) {
    stop("Failed to copy forest plot to dashboard directory.")
  }
  
  message("Interactive dashboard created successfully at: ", dashboard_dir)
  message("To run it, use the following R command:")
  message("shiny::runApp('", dashboard_dir, "')")
  
  return(dashboard_dir)
}


#' @title Create Reproducibility Package
#' @description Bundles all necessary code, data, and environment files into a zip archive.
#' @param package_name The name of the output zip file.
#' @param config The engine configuration.
#' @return The path to the created zip package.
create_reproducibility_package <- function(package_name = paste0("repro_package_", Sys.Date(), ".zip"),
                                           config = DISSEMINATION_CONFIG) {
  
  message("Starting reproducibility package creation...")
  
  # 1. Define paths
  output_dir <- config$repro_package$output_dir
  full_output_path <- file.path(output_dir, package_name)
  staging_dir <- file.path(tempdir(), "repro_package_staging")
  
  # Clean up any previous staging directory
  if (dir.exists(staging_dir)) {
    unlink(staging_dir, recursive = TRUE)
  }
  dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Staging files in: ", staging_dir)
  
  # 2. Copy project files to staging area
  files_to_copy <- config$repro_package$files_to_include
  for (file in files_to_copy) {
    if (file.exists(file)) {
      dest <- file.path(staging_dir, file)
      # Create parent directory if it doesn't exist
      dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
      file.copy(file, dest, recursive = TRUE)
    } else {
      warning("File or directory not found, skipping: ", file)
    }
  }
  
  # 3. Create a README for data and instructions
  readme_content <- c(
    "# Reproducibility Package",
    "",
    "This package contains the code and environment needed to reproduce the analysis.",
    "",
    "## Instructions",
    "",
    "1.  **Restore Environment**: Open R in this directory and run `renv::restore()` to install all required packages.",
    "2.  **Download Data**: The raw data is not included. Please download the necessary NHANES data files and place them in a `data/raw/` subdirectory.",
    "3.  **Run Analysis**: Open your terminal in this directory and run `make all` to execute the complete analysis pipeline.",
    "4.  **Generate Outputs**: Run `make manuscript` to generate the final report.",
    ""
  )
  writeLines(readme_content, file.path(staging_dir, "README.md"))
  
  # 4. Zip the staging directory
  message("Zipping the package to: ", full_output_path)
  # zip::zipr operates from the current working directory, so we need to be careful with paths
  # We'll zip the contents *of* the staging directory, not the directory itself.
  zip::zipr(
    zipfile = full_output_path,
    files = list.files(staging_dir, full.names = TRUE, recursive = TRUE),
    root = staging_dir
  )
  
  # 5. Clean up
  unlink(staging_dir, recursive = TRUE)
  
  if (file.exists(full_output_path)) {
    message("Reproducibility package created successfully: ", full_output_path)
    return(full_output_path)
  } else {
    stop("Failed to create reproducibility package.")
  }
}

# --- Helper Functions ---

#' @title Check for Required Artifacts
#' @description Verifies that the necessary analysis outputs exist before attempting dissemination.
#' @param required_files A character vector of file paths relative to the `outputs` dir.
#' @param config The main project config from config.yml
#' @return TRUE if all files are found, otherwise stops with an error.
check_for_artifacts <- function(required_files, config) {
  message("Checking for required analysis artifacts...")
  base_path <- config$outputs$base_dir
  
  missing_files <- c()
  for (file in required_files) {
    # This logic assumes paths are relative to various sub-dirs in outputs
    # A more robust implementation might use a manifest file
    full_path <- file.path(base_path, file) # Simplified for example
    if (!file.exists(full_path)) {
      missing_files <- c(missing_files, full_path)
    }
  }
  
  if (length(missing_files) > 0) {
    stop("Cannot proceed. The following required artifacts are missing:\n",
         paste(missing_files, collapse = "\n"),
         "\nPlease run the main analysis pipeline first (`make all`).")
  }
  
  message("All required artifacts found.")
  return(TRUE)
}
