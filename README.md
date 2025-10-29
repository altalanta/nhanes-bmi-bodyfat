# NHANES BMI vs Body Fat Analysis

[![R-CMD-check](https://github.com/altalanta/nhanes-bmi-bodyfat/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/altalanta/nhanes-bmi-bodyfat/actions/workflows/R-CMD-check.yml)

This repository contains the code and data for a reproducible analysis of the relationship between Body Mass Index (BMI) and body fat percentage, using data from the 2017-2018 National Health and Nutrition Examination Survey (NHANES).

## Features

*   **Reproducible Pipeline:** The entire analysis is managed by a `targets` pipeline, ensuring full reproducibility.
*   **R Package Structure:** The project is a self-contained R package, making dependencies and functions easy to manage.
*   **Machine Learning Integration:** Includes targets for running machine learning models to predict body fat percentage.
*   **Automated Testing:** Unit tests for core functions and continuous integration via GitHub Actions to ensure code quality.
*   **Configuration Driven:** All key parameters are managed in a central `config.yml` file.

## Getting Started

### Prerequisites

*   R (version 4.0.0 or higher)
*   RStudio (recommended)

### Installation

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/altalanta/nhanes-bmi-bodyfat.git
    cd nhanes-bmi-bodyfat
    ```

2.  **Open the RStudio Project:**
    Open the `nhanes-bmi-bodyfat.Rproj` file in RStudio.

3.  **Restore the Environment:**
    This project uses `renv` to manage dependencies. To install all required packages, run the following command in the R console:
    ```r
    renv::restore()
    ```

## Usage

The entire analysis pipeline can be run with a single command using the provided `Makefile`.

1.  **Run the full pipeline:**
    This will download the data, run the analysis, and generate all outputs, including tables, figures, and the final report.
    ```bash
    make pipeline
    ```

2.  **Visualize the pipeline:**
    To see a graph of the pipeline's dependencies, you can run:
    ```bash
    make pipeline-vis
    ```

3.  **View the final report:**
    Once the pipeline has completed, the final report can be found at `outputs/report/report.html`.

## Project Structure

```
nhanes-bmi-bodyfat/
├── R/                          # R package source code
│   ├── ml_analysis.R           # Machine learning functions
│   └── pipeline_steps.R        # Functions for the targets pipeline
├── config/
│   └── config.yml              # Project configuration
├── data/
│   ├── raw/                    # Raw NHANES files (auto-downloaded)
│   └── derived/                # Processed datasets
├── outputs/
│   ├── tables/                 # CSV results
│   ├── figures/                # PNG/PDF visualizations
│   └── report/                 # HTML report
├── tests/
│   └── testthat/               # testthat unit tests
│       └── test-pipeline_steps.R
├── .github/workflows/
│   └── R-CMD-check.yml         # GitHub Actions CI workflow
├── _targets.R                  # Main targets pipeline definition
├── DESCRIPTION                 # R package metadata
├── NAMESPACE                   # R package namespace
├── Makefile                    # High-level commands
├── renv.lock                   # R environment specification
└── README.md                   # This file
```