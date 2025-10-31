# API Reference Guide

This comprehensive API reference documents all the functions and capabilities added to the NHANES BMI Body Fat Analysis platform, organized by functional area.

## 📊 Core Analysis Functions

### Statistical Analysis (`R/nhanes_analysis.R`)

#### `compute_correlations_parallel()`
Parallel computation of BMI-body fat correlations with survey weighting.

```r
compute_correlations_parallel <- function(design)
```

**Parameters:**
- `design`: Survey design object (from `survey::svydesign()`)

**Returns:** Data frame with correlation results
```r
# Example output:
  group     correlation std_error ci_lower ci_upper
  Overall   0.914       0.012     0.890    0.938
  Male      0.917       0.013     0.891    0.943
  Female    0.954       0.008     0.938    0.970
```

#### `compute_bmi_class_stats_parallel()`
Parallel computation of BMI class statistics by sex.

```r
compute_bmi_class_stats_parallel <- function(design, data)
```

**Parameters:**
- `design`: Survey design object
- `data`: Complete dataset with BMI categories

**Returns:** Data frame with BMI class statistics
```r
# Example output:
  bmi_cat   sex   mean_bodyfat mean_se q05 q50 q95 n_unweighted pop_total
  Normal    Male  15.2         0.8     8.1 15.1 22.3  245         12500000
  Normal    Female 18.7         0.9     11.2 18.5 26.1  312         15800000
```

## ⚡ Parallel Processing Functions

### Pipeline Management (`parallel_pipeline.R`)

#### `run_parallel_pipeline()`
Main entry point for the parallel processing pipeline.

```r
run_parallel_pipeline <- function()
```

**Returns:** List with analysis results and metadata
```r
list(
  correlation_results = data.frame(...),
  bmi_class_results = data.frame(...),
  linearity_results = list(...),
  plot_files = c("path1.png", "path2.png"),
  processing_time = 22.5  # seconds
)
```

#### Cache Management Functions

##### `load_from_cache()`
Load cached computation results.

```r
load_from_cache <- function(name)
```

**Parameters:**
- `name`: Cache key (content-based hash)

**Returns:** Cached data or NULL if not found

##### `save_to_cache()`
Save computation results to cache.

```r
save_to_cache <- function(name, data)
```

**Parameters:**
- `name`: Cache key (content-based hash)
- `data`: Data to cache

**Returns:** The cached data

##### `get_cache_path()`
Get file path for cache entry.

```r
get_cache_path <- function(name)
```

**Returns:** Full path to cache file

## 🔒 Data Version Management (`R/data_versioning.R`)

### Registry Management

#### `initialize_data_registry()`
Create new data registry for version tracking.

```r
initialize_data_registry <- function()
```

**Returns:** Logical indicating success

#### `load_data_registry()`
Load existing data registry.

```r
load_data_registry <- function()
```

**Returns:** Registry data structure

#### `save_data_registry()`
Save registry to disk.

```r
save_data_registry <- function(registry)
```

**Returns:** Logical indicating success

#### `add_to_registry()`
Add or update file in registry.

```r
add_to_registry <- function(file_path, data_type, nhanes_cycle = NULL, version = NULL)
```

**Parameters:**
- `file_path`: Path to data file
- `data_type`: Type of data ("demographics", "body_measures", etc.)
- `nhanes_cycle`: NHANES cycle (e.g., "2017-2018")
- `version`: Version string

**Returns:** Logical indicating success

#### `update_data_registry()`
Update registry with all current NHANES files.

```r
update_data_registry <- function()
```

**Returns:** Number of files processed

### Integrity Verification

#### `compute_file_hash()`
Compute SHA256 hash of file.

```r
compute_file_hash <- function(file_path)
```

**Returns:** SHA256 hash string or NULL if file doesn't exist

#### `validate_data_integrity()`
Validate integrity of all registered files.

```r
validate_data_integrity <- function()
```

**Returns:** List with validation results
```r
list(
  valid = TRUE,  # Overall validity
  issues = list()  # List of specific issues
)
```

#### `check_for_updates()`
Check for available data updates.

```r
check_for_updates <- function()
```

**Returns:** List with update information
```r
list(
  uptodate = TRUE,  # Whether all files are current
  updates = list()  # Details of available updates
)
```

### Quality Reporting

#### `generate_data_manifest()`
Generate reproducibility manifest.

```r
generate_data_manifest <- function(output_path = DATA_MANIFEST_FILE)
```

**Returns:** Manifest data structure

#### `display_quality_report()`
Display comprehensive data quality report.

```r
display_quality_report <- function()
```

**Returns:** None (displays to console)

#### `display_registry_summary()`
Display data registry summary.

```r
display_registry_summary <- function()
```

**Returns:** None (displays to console)

## 🎓 Interactive Documentation Functions

### Tutorial System (`tutorials/`)

#### Tutorial Files
- `tutorials/getting_started.Rmd`: Interactive learning tutorial
- `tutorials/help_troubleshooting.Rmd`: Troubleshooting guide

### Configuration Wizard (`app.R`)

#### Shiny Application Functions

##### `ui` (User Interface)
Defines the web-based configuration interface.

##### `server()` (Server Logic)
Reactive server functions for configuration management.

**Key Reactive Functions:**
- `config()`: Builds configuration object from inputs
- `observeEvent()`: Handles user interactions
- `renderText()`: Generates preview outputs

## 🛠️ Error Handling (`R/error_handling.R`)

### Custom Error Classes

#### `NhanesError()`
Create NHANES-specific error with suggestions.

```r
NhanesError <- function(message, code = NULL, details = NULL, suggestions = NULL)
```

**Returns:** Custom error object

#### `DataValidationError()`
Create data validation error.

```r
DataValidationError <- function(message, field = NULL, expected = NULL, actual = NULL)
```

#### `FileNotFoundError()`
Create file not found error.

```r
FileNotFoundError <- function(message, file_path = NULL)
```

### Error Handling Functions

#### `safe_execute()`
Execute code with comprehensive error handling.

```r
safe_execute <- function(expr, operation_name, config = NULL)
```

**Parameters:**
- `expr`: Expression to execute
- `operation_name`: Name for logging
- `config`: Configuration object

#### `safe_log()`
Enhanced logging with error context.

```r
safe_log <- function(message, level = "INFO", config = NULL)
```

**Parameters:**
- `message`: Log message
- `level`: Log level ("DEBUG", "INFO", "WARNING", "ERROR")
- `config`: Configuration object

#### `display_user_friendly_error()`
Display error with actionable suggestions.

```r
display_user_friendly_error <- function(error_obj)
```

#### `check_pipeline_health()`
Check overall system health.

```r
check_pipeline_health <- function()
```

**Returns:** List of health issues

## 📋 Configuration Management

### Configuration Loading (`R/load_config.R`)

#### `load_config()`
Load configuration from YAML file.

```r
load_config <- function(config_file = "config/config.yml")
```

**Returns:** Configuration list

### Configuration Structure

**Complete Configuration Schema:**
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

# NHANES data files
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

# Performance settings (optional)
performance:
  max_workers: "auto"
  memory_limit: "8GB"
  cache_enabled: true
```

## 📈 Performance Monitoring

### Timing and Metrics

#### Pipeline Timing
```r
# Automatic timing in parallel pipeline
start_time <- Sys.time()
# ... pipeline execution ...
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "secs")
```

#### Memory Usage Monitoring
```r
# Monitor memory usage during execution
gc()  # Force garbage collection
memory.size()  # Current memory usage
memory.limit()  # Memory limit
```

### Cache Performance

#### Cache Hit Rate
```r
# Calculate cache effectiveness
cache_hits <- sum(sapply(cache_keys, function(k) !is.null(load_from_cache(k))))
total_operations <- length(cache_keys)
hit_rate <- cache_hits / total_operations
```

#### Cache Size Management
```r
# Monitor cache directory size
cache_size <- sum(file.info(list.files("cache/", full.names = TRUE))$size)
cache_mb <- cache_size / 1024 / 1024
```

## 🔧 Utility Functions

### File Operations

#### `ensure_output_dirs()`
Create required output directories.

```r
ensure_output_dirs <- function(config)
```

#### `safe_read_xpt()`
Read XPT files with error handling.

```r
safe_read_xpt <- function(file_path, dataset_name)
```

#### `validate_nhanes_data()`
Validate NHANES dataset structure.

```r
validate_nhanes_data <- function(data, dataset_name, required_cols)
```

### Survey Design

#### `create_survey_design()`
Create survey design object.

```r
create_survey_design <- function(data, config)
```

#### `validate_survey_design()`
Validate survey design parameters.

```r
validate_survey_design <- function(data, weights_col, strata_col, psu_col)
```

## 📚 Constants and File Paths

### File Path Constants (`R/data_versioning.R`)

```r
# Registry and manifest file paths
DATA_REGISTRY_FILE <- "data/registry/data_registry.json"
DATA_MANIFEST_FILE <- "data/raw/manifest.json"

# Ensure registry directories exist
ensure_registry_dirs <- function()
```

### Cache Directory Management

```r
# Cache directory path
cache_dir <- "cache"

# Create cache directory if needed
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir)
}
```

## 🚨 Error Codes and Messages

### Standard Error Codes

| Code | Description | Typical Cause |
|------|-------------|---------------|
| DL001 | Data loading failed | File not found or corrupted |
| DV001 | Data validation failed | Missing required columns |
| SD001 | Survey design error | Invalid design parameters |
| SA001 | Statistical analysis error | Insufficient sample size |
| FO001 | File operation failed | Permission or disk space issues |
| CF001 | Configuration error | Invalid YAML or missing sections |
| PD001 | Package dependency error | Missing required packages |

### Error Message Structure

```r
# Enhanced error messages include:
# 1. Clear description of the problem
# 2. Specific error code for programmatic handling
# 3. Technical details for debugging
# 4. Actionable suggestions for resolution
# 5. Links to relevant documentation
```

## 📋 Function Categories

### Data Management Functions
- `compute_file_hash()` - SHA256 hashing
- `create_registry_entry()` - Registry entry creation
- `add_to_registry()` - Registry management
- `validate_data_integrity()` - Integrity checking
- `check_for_updates()` - Update detection
- `generate_data_manifest()` - Manifest creation

### Analysis Functions
- `run_parallel_pipeline()` - Main pipeline execution
- `compute_correlations_parallel()` - Parallel correlation analysis
- `compute_bmi_class_stats_parallel()` - Parallel BMI statistics
- `assess_linearity()` - Linearity testing

### Utility Functions
- `safe_execute()` - Error-safe execution
- `safe_log()` - Enhanced logging
- `display_user_friendly_error()` - User-friendly error display
- `check_pipeline_health()` - System health checking

### Interactive Functions
- Tutorial system functions (via learnr package)
- Configuration wizard functions (via Shiny package)

## 🔗 Function Dependencies

### Package Dependencies

**Required Packages:**
- `digest`: SHA256 hashing and caching
- `jsonlite`: JSON serialization for registry
- `yaml`: Configuration file parsing
- `future`: Parallel processing framework
- `furrr`: Functional programming for parallel operations
- `httr`: HTTP requests for update checking
- `survey`: Survey-weighted statistical analysis
- `foreign`: XPT file reading
- `dplyr`: Data manipulation
- `ggplot2`: Data visualization
- `shiny`: Interactive web applications
- `learnr`: Interactive tutorials

**Optional Packages:**
- `testthat`: Unit testing framework
- `lintr`: Code style checking
- `styler`: Code formatting
- `profvis`: Performance profiling
- `bench`: Benchmarking utilities

---

**This API reference provides comprehensive documentation for all functions in the enhanced NHANES BMI Body Fat Analysis platform, enabling both beginner users and advanced developers to leverage the full capabilities of the system.**







