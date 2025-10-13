# NHANES 2017-2018 BMI vs Body Fat Analysis

[![CI](https://github.com/altalanta/nhanes-bmi-bodyfat/workflows/CI/badge.svg)](https://github.com/altalanta/nhanes-bmi-bodyfat/actions)
[![Last Build](https://img.shields.io/github/last-commit/altalanta/nhanes-bmi-bodyfat)](https://github.com/altalanta/nhanes-bmi-bodyfat/commits/main)

Reproducible analysis of the association between Body Mass Index (BMI) and whole-body percent body fat using NHANES 2017-2018 data for U.S. civilian non-institutionalized adults aged 20-59 years.

## Overview

This analysis uses design-based (survey-weighted) statistical methods with full reproducibility through automated data fetching, testing, and reporting:

- **Survey-weighted Pearson correlations** between BMI and DXA-measured % body fat
- **Mean % body fat by BMI class and sex** with 95% confidence intervals
- **Distribution analysis** (5th, 50th, 95th percentiles) by BMI class and sex
- **Linearity assessment** and sensitivity analyses
- **Machine learning modeling** for advanced prediction and feature importance analysis
- **Automated testing** for survey correctness and data quality

All estimates account for NHANES complex sampling design using MEC examination weights (WTMEC2YR), stratification (SDMVSTRA), and primary sampling units (SDMVPSU) with Taylor linearization for variance estimation.

## Key Results

- **Overall BMI-body fat correlation**: 0.914 (95% CI: 0.885-0.943)
- **Male correlation**: 0.917 (95% CI: 0.885-0.949)  
- **Female correlation**: 0.954 (95% CI: 0.941-0.967)
- **Sample size**: ~2,240 adults with complete data
- **Significant non-linearity detected** (BMI² term p < 0.01)
- **Machine learning predictions** with XGBoost achieving best performance (R² > 0.95)

## Quickstart

### Option 1: Local R Environment

```bash
# Clone repository
git clone https://github.com/altalanta/nhanes-bmi-bodyfat.git
cd nhanes-bmi-bodyfat

# Restore R environment
R -e "renv::restore()"

# Run complete pipeline (data fetch → analysis → visualization → report)
make all

# View results
open outputs/report/report.html  # macOS
xdg-open outputs/report/report.html  # Linux
```

### Option 2: Docker (Recommended for reproducibility)

```bash
# Clone repository
git clone https://github.com/altalanta/nhanes-bmi-bodyfat.git
cd nhanes-bmi-bodyfat

# Build and run with Docker
make docker-build
make docker-run

# Or use docker-compose for development
docker-compose up --build

# View results (outputs are mounted as volumes)
open outputs/report/report.html  # macOS
xdg-open outputs/report/report.html  # Linux

# Access API (when running in Docker)
curl http://localhost:8000/api/correlations
```

## Reproducible Pipeline

This project uses a fully automated pipeline:

1. **Data Fetching** (`make fetch`): Downloads NHANES files with SHA256 verification
2. **Data Processing** (`make cleandata`): Applies exclusions and creates analytic dataset  
3. **Analysis** (`make analysis`): Computes survey-weighted estimates
4. **Visualization** (`make viz`): Creates publication-ready figures
5. **Reporting** (`make report`): Generates comprehensive HTML report
6. **Testing** (`make test`): Validates survey methodology and data quality

### Environment Management

### Local R Environment with renv

This project uses [renv](https://rstudio.github.io/renv/) for reproducible R environments:

```bash
# Install renv (if needed)
R -e "install.packages('renv')"

# Restore project library
R -e "renv::restore()"

# Add new packages (developers)
R -e "renv::install('package_name'); renv::snapshot()"
```

### Docker Environment (Recommended)

For complete cross-platform reproducibility, use Docker:

```bash
# Build the Docker image
make docker-build

# Run analysis in container
make docker-run

# Interactive development
make docker-shell

# Using docker-compose
docker-compose up --build

# Clean up Docker resources
make docker-clean
```

**Docker Benefits:**
- **Zero setup** - No R installation or dependency management needed
- **Identical results** across Windows, Mac, and Linux
- **Isolated environment** - No conflicts with system R packages
- **Easy sharing** - Anyone can run `docker run altalanta/nhanes-bmi-bodyfat`
- **CI/CD ready** - Perfect for automated testing and deployment

## REST API

The project includes a comprehensive REST API for programmatic access to analysis results:

### API Endpoints

- **`/api/correlations`** - BMI-body fat correlation data by group (Overall, Male, Female)
- **`/api/bodyfat/bmi/{category}`** - Body fat statistics by BMI category
- **`/api/bodyfat/sex/{sex}`** - Body fat statistics by sex
- **`/api/population`** - Population counts by demographic groups
- **`/api/statistics`** - Summary statistics and metadata
- **`/health`** - API health check

### API Usage Examples

```bash
# Start API server
make api

# Get correlations data
curl http://localhost:8000/api/correlations

# Get body fat data for normal BMI category
curl "http://localhost:8000/api/bodyfat/bmi/Normal"

# Get population data for males
curl "http://localhost:8000/api/population?sex=Male"

# View API documentation
open http://localhost:8000/__docs__/
```

### API Features

- **JSON responses** with proper error handling
- **Filtering and querying** capabilities
- **OpenAPI/Swagger documentation** at `/__docs__/`
- **Cross-origin support** for web applications
- **Health monitoring** endpoints

## Data Sources

**NHANES 2017-2018** data files are automatically downloaded from CDC:

- **Demographics**: [DEMO_J.XPT](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT)
- **Body Measures**: [BMX_J.XPT](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT)
- **DXA Whole Body**: [DXX_J.XPT](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXX_J.XPT)
- **DXA Android/Gynoid**: [DXXAG_J.XPT](https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXXAG_J.XPT)

**Official NHANES Information:**
- [NHANES Program](https://www.cdc.gov/nchs/nhanes/index.htm)
- [Data Portal](https://wwwn.cdc.gov/nchs/nhanes/Default.aspx)

**Disclaimer:** This analysis is for research purposes. NHANES data are collected by CDC/NCHS. Users should cite original NHANES sources appropriately.

## Repository Structure

```
nhanes-bmi-bodyfat/
├── scripts/                    # Analysis scripts
│   ├── fetch_nhanes.R         # Data download with checksums
│   ├── derive_dataset.R       # Data cleaning and exclusions
│   ├── nhanes_bmi_bodyfat_analysis.R  # Main analysis
│   ├── make_visualization.R   # Publication figures
│   └── sensitivity_analysis.R # Sensitivity analyses
├── tests/                     # Automated testing
│   ├── test_survey_checks.R   # Survey methodology validation
│   └── test_exclusions.R      # Data quality checks
├── deployment/                # Deployment scripts
│   ├── deploy-docker.R        # Docker deployment automation
│   ├── deploy-shinyapps.R     # Shiny deployment
│   └── prepare-cran.R         # CRAN preparation
├── scripts/
│   ├── api_server.R           # API server launcher
│   └── advanced_ml_analysis.R # Machine learning analysis
├── R/
│   ├── api.R                  # REST API definition
│   └── advanced_analytics.R   # Machine learning module
├── data/
│   ├── raw/                   # Raw NHANES files (auto-downloaded)
│   └── derived/               # Processed datasets
├── outputs/
│   ├── tables/                # CSV results
│   ├── figures/               # PNG/PDF visualizations
│   ├── logs/                  # Analysis logs
│   └── report/                # HTML report
├── .github/workflows/ci.yml   # GitHub Actions CI
├── Dockerfile                 # Docker container definition
├── docker-compose.yml         # Multi-container setup
├── .dockerignore             # Docker build exclusions
├── report.qmd                 # Quarto report source
├── Makefile                   # Build automation (with Docker targets)
├── renv.lock                  # R environment specification
└── README.md                  # This file
```

## Continuous Integration

The project includes comprehensive CI/CD:

- **Automated testing** on R 4.3.2 and 4.4.0
- **Data fetching and analysis pipeline** validation
- **Survey methodology testing** to ensure correct weights usage
- **Artifact uploads** for tables and figures
- **GitHub Pages deployment** for reports

## Advanced Usage

### Custom Analyses

```bash
# Run specific components
make fetch                    # Download data only
make cleandata               # Process data only
make analysis               # Core analysis only
make viz                    # Visualization only
make test                   # Run test suite
make report                 # Generate report only
make advanced               # Run machine learning analysis

# Clean outputs
make clean                  # Remove derived files
make cleanall              # Remove everything including raw data
```

### Docker Usage

```bash
# Build Docker image
make docker-build

# Run analysis in container
make docker-run

# Interactive shell for development
make docker-shell

# Test Docker setup
make docker-test

# Development with docker-compose
make docker-up

# Clean up Docker resources
make docker-clean

# Deploy with automated script
Rscript deployment/deploy-docker.R

# Push to Docker Hub (after login)
Rscript deployment/deploy-docker.R push
```

### Docker for CI/CD

The Docker setup enables automated testing across platforms:

```bash
# In CI pipeline
- make docker-build
- make docker-test
- make docker-clean  # Clean up after tests
```

### API Usage

```bash
# Start API server
make api

# Start API server for local development (localhost only)
make api-dev

# Test API endpoints
curl http://localhost:8000/health
curl http://localhost:8000/api/correlations
curl "http://localhost:8000/api/bodyfat/bmi/Normal"

# In Docker
docker run -p 8000:8000 nhanes-bmi-bodyfat api
```

### Development

```bash
# Check code syntax
Rscript -e "lintr::lint_dir('scripts')"

# Run specific tests
Rscript -e "testthat::test_file('tests/test_survey_checks.R')"

# Update dependencies
R -e "renv::snapshot()"
```

## Methods Summary

**Study Population:** U.S. civilian non-institutionalized adults (20-59 years)

**Exclusions:**
- Missing BMI or DXA body fat measurements
- Invalid DXA scan status
- Missing survey design variables
- Pregnant women (if pregnancy data available)

**Survey Design:**
- Weights: WTMEC2YR (2-year MEC examination weights)
- Strata: SDMVSTRA  
- PSUs: SDMVPSU
- Variance: Taylor linearization

**Analysis:** Design-based estimation using R survey package

See `outputs/report/report.html` for complete methodology and results.

## Machine Learning Analysis

The project includes advanced machine learning approaches for BMI-body fat prediction:

### ML Models Compared

- **Linear Regression** - Baseline interpretable model
- **Random Forest** - Tree-based ensemble method
- **XGBoost** - Gradient boosting for optimal performance
- **Bayesian Regression** - Uncertainty quantification with MCMC

### ML Features

- **Multi-variate prediction** using age, sex, race/ethnicity, and BMI interactions
- **Feature importance analysis** identifying key predictors
- **Model comparison** with cross-validation and performance metrics
- **Prediction intervals** and uncertainty quantification

### Running ML Analysis

```bash
# Run machine learning analysis
make advanced

# View ML results
open outputs/figures/ml_model_comparison.png
open outputs/figures/ml_predictions.png
open outputs/tables/ml_model_comparison.csv
```

### ML Results Summary

- **Best performing model**: XGBoost (R² ≈ 0.95 on test set)
- **Key predictors**: BMI, sex, age, and BMI-sex interactions
- **Prediction accuracy**: RMSE < 3% body fat on held-out test data

## Citation

If you use this analysis, please cite:

```
NHANES 2017-2018 BMI vs Body Fat Analysis [Computer software]. 
https://github.com/altalanta/nhanes-bmi-bodyfat
```

Also cite the original NHANES data source:

```
National Center for Health Statistics. National Health and Nutrition 
Examination Survey, 2017-2018. Hyattsville, MD: U.S. Department of 
Health and Human Services, Centers for Disease Control and Prevention.
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make changes and add tests
4. Ensure CI passes (`make test`)
5. Submit a pull request

For major changes, please open an issue first to discuss the proposed changes.