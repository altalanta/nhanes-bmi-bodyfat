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
- **Automated testing** for survey correctness and data quality

All estimates account for NHANES complex sampling design using MEC examination weights (WTMEC2YR), stratification (SDMVSTRA), and primary sampling units (SDMVPSU) with Taylor linearization for variance estimation.

## Key Results

- **Overall BMI-body fat correlation**: 0.914 (95% CI: 0.885-0.943)
- **Male correlation**: 0.917 (95% CI: 0.885-0.949)  
- **Female correlation**: 0.954 (95% CI: 0.941-0.967)
- **Sample size**: ~2,240 adults with complete data
- **Significant non-linearity detected** (BMI² term p < 0.01)

## Quickstart

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

## Reproducible Pipeline

This project uses a fully automated pipeline:

1. **Data Fetching** (`make fetch`): Downloads NHANES files with SHA256 verification
2. **Data Processing** (`make cleandata`): Applies exclusions and creates analytic dataset  
3. **Analysis** (`make analysis`): Computes survey-weighted estimates
4. **Visualization** (`make viz`): Creates publication-ready figures
5. **Reporting** (`make report`): Generates comprehensive HTML report
6. **Testing** (`make test`): Validates survey methodology and data quality

### Environment Management

This project uses [renv](https://rstudio.github.io/renv/) for reproducible R environments:

```bash
# Install renv (if needed)
R -e "install.packages('renv')"

# Restore project library
R -e "renv::restore()"

# Add new packages (developers)
R -e "renv::install('package_name'); renv::snapshot()"
```

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
├── data/
│   ├── raw/                   # Raw NHANES files (auto-downloaded)
│   └── derived/               # Processed datasets
├── outputs/
│   ├── tables/                # CSV results
│   ├── figures/               # PNG/PDF visualizations
│   ├── logs/                  # Analysis logs
│   └── report/                # HTML report
├── .github/workflows/ci.yml   # GitHub Actions CI
├── report.qmd                 # Quarto report source
├── Makefile                   # Build automation
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

# Clean outputs
make clean                  # Remove derived files
make cleanall              # Remove everything including raw data
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