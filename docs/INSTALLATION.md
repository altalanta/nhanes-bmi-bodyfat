# Installation and Setup Guide

This comprehensive guide will help you get the NHANES BMI Body Fat Analysis platform up and running, whether you're a beginner with no R experience or an advanced researcher.

## 📋 Quick Start Checklist

- [ ] **System Requirements**: R 4.0+, sufficient disk space (~100MB for data)
- [ ] **Git Repository**: Clone or download the source code
- [ ] **R Environment**: Install required packages via renv
- [ ] **Configuration**: Set up analysis parameters
- [ ] **Data Download**: Fetch NHANES datasets
- [ ] **Verification**: Run health checks and validation

---

## 🚀 For Beginners (No R Experience Required)

### Step 1: Install R and RStudio

**Windows:**
1. Download R from: https://cran.r-project.org/bin/windows/base/
2. Download RStudio from: https://posit.co/download/rstudio-desktop/
3. Install R first, then RStudio

**macOS:**
1. Install R from: https://cran.r-project.org/bin/macosx/
2. Install RStudio from: https://posit.co/download/rstudio-desktop/
3. Or use Homebrew: `brew install r rstudio`

**Linux (Ubuntu/Debian):**
```bash
sudo apt update
sudo apt install r-base r-base-dev
# Install RStudio from: https://posit.co/download/rstudio-desktop/
```

### Step 2: Get the Source Code

**Option A: Git Clone (Recommended)**
```bash
git clone https://github.com/altalanta/nhanes-bmi-bodyfat.git
cd nhanes-bmi-bodyfat
```

**Option B: Download ZIP**
1. Go to: https://github.com/altalanta/nhanes-bmi-bodyfat
2. Click "Code" → "Download ZIP"
3. Extract to your desired location

### Step 3: Launch Interactive Tutorial

```bash
# Navigate to the project directory
cd nhanes-bmi-bodyfat

# Launch the interactive tutorial (works without R setup)
make tutorial
```

The tutorial will guide you through:
- ✅ Environment verification
- ✅ Package installation
- ✅ Configuration setup
- ✅ First analysis run

**That's it!** The interactive tutorial handles everything else automatically.

---

## 💻 For Researchers (R Experience)

### Step 1: Clone Repository and Setup R Environment

```bash
# Clone repository
git clone https://github.com/altalanta/nhanes-bmi-bodyfat.git
cd nhanes-bmi-bodyfat

# Initialize R environment with all dependencies
R -e "renv::restore()"

# Verify installation
R -e "library(nhanesbmi); cat('Package loaded successfully!\n')"
```

### Step 2: Configure Analysis Parameters

**Option A: Use Configuration Wizard (Recommended)**
```bash
make config-wizard
# Web interface for parameter customization
```

**Option B: Edit Configuration File**
```bash
# Edit config/config.yml with your preferred settings
nano config/config.yml
# or
vim config/config.yml
```

### Step 3: Run Analysis

```bash
# High-performance parallel pipeline (recommended)
make parallel-pipeline

# Traditional sequential pipeline
make all

# Check results
ls outputs/
```

---

## 🛠️ For Developers (Advanced Setup)

### Step 1: Complete Development Environment

```bash
# Clone repository
git clone https://github.com/altalanta/nhanes-bmi-bodyfat.git
cd nhanes-bmi-bodyfat

# Setup R environment with development dependencies
R -e "renv::restore()"

# Install additional development packages
R -e "renv::install(c('devtools', 'roxygen2', 'testthat', 'pkgdown'))"
```

### Step 2: Verify System Health

```bash
# Run comprehensive health check
make health-check

# Check data integrity (after downloading data)
make data-health

# Run test suite
make test
```

### Step 3: Development Workflow

```bash
# Make code changes
# Edit files in R/, scripts/, tutorials/

# Run specific tests
Rscript -e "testthat::test_file('tests/test_survey_checks.R')"

# Check code style
Rscript -e "lintr::lint_dir('scripts')"

# Update documentation
R -e "devtools::document()"

# Run analysis with your changes
make parallel-pipeline
```

---

## 📦 Package Dependencies

### Core Dependencies (Automatically Installed)
```r
# Essential packages for analysis
dplyr, ggplot2, survey, foreign, readr, yaml, lintr, styler, testthat

# Performance and caching
future, furrr, digest

# Interactive features
shiny, learnr

# Data management
jsonlite, httr
```

### Optional Dependencies (For Advanced Features)
```r
# API and web services
plumber, jsonld

# Advanced statistics
rstanarm, bayesplot, MatchIt, WeightIt

# Machine learning
randomForest, xgboost, caret

# Specialized tools
nhanesA, quarto, bookdown
```

### System Dependencies

**Required for some packages:**
- **gfortran**: For compiled packages (Linux/macOS)
- **libxml2-dev**: For XML processing (Linux)
- **pandoc**: For document conversion

**Installation (Ubuntu/Debian):**
```bash
sudo apt install gfortran libxml2-dev pandoc
```

**Installation (macOS with Homebrew):**
```bash
brew install gfortran libxml2 pandoc
```

---

## 🔧 Configuration Setup

### Directory Structure Requirements

The system expects the following directory structure:

```
nhanes-bmi-bodyfat/
├── config/              # Configuration files
├── data/
│   ├── raw/            # NHANES source files (auto-created)
│   ├── derived/        # Processed datasets (auto-created)
│   └── registry/       # Data version registry (auto-created)
├── outputs/            # Analysis results (auto-created)
│   ├── tables/         # CSV results
│   ├── figures/        # PNG/PDF plots
│   ├── logs/           # Analysis logs
│   └── report/         # HTML documentation
├── tutorials/          # Interactive guides
├── cache/              # Performance caching (auto-created)
└── R/                  # R package source
```

### Automatic Directory Creation

The system automatically creates required directories:

```bash
# This happens automatically during first run
# But you can create them manually if needed:
mkdir -p data/raw data/derived outputs/{tables,figures,logs,report} config cache tutorials
```

### Configuration File Structure

**Default Configuration** (`config/config.yml`):

```yaml
# Data directories
data:
  raw_dir: "data/raw"
  derived_dir: "data/derived"

# Output directories
outputs:
  tables_dir: "outputs/tables"
  figures_dir: "outputs/figures"
  logs_dir: "outputs/logs"
  report_dir: "outputs/report"

# NHANES data files (2017-2018)
nhanes:
  demo_file: "DEMO_J.XPT"
  bmx_file: "BMX_J.XPT"
  dxx_file: "DXX_J.XPT"
  dxxag_file: "DXXAG_J.XPT"

# Analysis parameters
analysis:
  age_range: [20, 59]
  survey_weights_col: "WTMEC2YR"
  strata_col: "SDMVSTRA"
  psu_col: "SDMVPSU"

# Logging configuration
logging:
  level: "INFO"
  file: "analysis_log.txt"
```

---

## 🔍 Verification and Health Checks

### System Health Check

```bash
# Run comprehensive system verification
make health-check
```

**What it checks:**
- ✅ Required directories exist
- ✅ Configuration file is valid
- ✅ Required R packages are installed
- ✅ System dependencies are available
- ✅ Data files are accessible (if downloaded)

### Data Integrity Validation

```bash
# Validate data file integrity
make data-integrity

# Check for available updates
make data-updates

# Generate quality report
make data-health
```

### Expected Output

**Healthy System:**
```
✅ PIPELINE HEALTH CHECK: All systems operational!
🚀 Ready to run analysis with: make parallel-pipeline
📚 Get started with: make tutorial
⚙️ Configure settings with: make config-wizard
```

**System with Issues:**
```
⚠️ PIPELINE HEALTH ISSUES DETECTED
❌ Missing packages: survey, ggplot2
❌ Missing NHANES data files: data/raw/DEMO_J.XPT

💡 Quick Fixes:
  1. Install packages: install.packages(c('survey', 'ggplot2'))
  2. Download data: make fetch
  3. Configure settings: make config-wizard
```

---

## 🚨 Troubleshooting Common Issues

### Issue: "Package not found" errors

**Solution:**
```bash
# Install missing packages
R -e "install.packages(c('dplyr', 'ggplot2', 'survey', 'foreign'))"

# Or use the configuration wizard
make config-wizard
```

### Issue: "Directory not found" errors

**Solution:**
```bash
# Create required directories
mkdir -p data/raw data/derived outputs/{tables,figures,logs,report} config

# Or run the health check for automatic detection
make health-check
```

### Issue: "Configuration file not found"

**Solution:**
```bash
# Use the configuration wizard
make config-wizard

# Or create manually
mkdir -p config
cp config/config.yml.example config/config.yml  # if example exists
```

### Issue: "Data download failed"

**Solution:**
```bash
# Check internet connection
ping -c 3 google.com

# Retry download with verbose output
make fetch

# Check available disk space
df -h

# Manual download alternative available in troubleshooting guide
make tutorial-troubleshooting
```

---

## 📚 Additional Resources

### Interactive Help Systems

1. **Getting Started Tutorial**: `make tutorial`
   - Complete step-by-step guidance
   - Interactive exercises and quizzes
   - Visual progress tracking

2. **Troubleshooting Guide**: `make tutorial-troubleshooting`
   - Common issues and solutions
   - Interactive diagnostics
   - Prevention best practices

3. **Configuration Wizard**: `make config-wizard`
   - Web-based parameter setup
   - Real-time preview and validation
   - Guided configuration process

### Documentation Files

- **README.md**: Comprehensive overview and quick start
- **docs/INSTALLATION.md**: This detailed installation guide
- **outputs/report/report.html**: Complete methodology and results
- **vignettes/**: Additional technical documentation

### Community Support

- **GitHub Issues**: https://github.com/altalanta/nhanes-bmi-bodyfat/issues
- **GitHub Discussions**: https://github.com/altalanta/nhanes-bmi-bodyfat/discussions
- **Email Support**: analysis@nhanes-bmi.org

---

## ✅ Verification Checklist

After installation, verify everything works:

```bash
# 1. Check system health
make health-check

# 2. Launch interactive tutorial
make tutorial

# 3. Test configuration wizard
make config-wizard

# 4. Run performance demo
make demo

# 5. Execute a quick analysis
make parallel-pipeline

# 6. Verify outputs exist
ls outputs/tables/
ls outputs/figures/
ls outputs/report/
```

**Expected Results:**
- ✅ Health check passes
- ✅ Tutorials launch successfully
- ✅ Configuration wizard opens in browser
- ✅ Demo runs without errors
- ✅ Analysis completes and generates outputs
- ✅ All output directories contain expected files

---

## 🎯 Next Steps

1. **Complete the interactive tutorial** (`make tutorial`) for comprehensive guidance
2. **Customize your analysis** using the configuration wizard (`make config-wizard`)
3. **Run your first analysis** (`make parallel-pipeline`)
4. **Explore the results** in the `outputs/` directory
5. **Join the community** for support and collaboration

**Welcome to the NHANES BMI Body Fat Analysis platform!** 🚀📊


