# Data Version Management Guide

This guide explains the robust data version management system that ensures **reproducible research** through cryptographic integrity verification, automated version tracking, and quality monitoring.

## 🔒 Overview

The data version management system provides:

- **SHA256-based file integrity** verification using cryptographic hashing
- **Automated data registry** with comprehensive metadata and version history
- **Update detection** for new NHANES data releases
- **Quality monitoring** with systematic validation and health reports
- **Complete audit trails** for scientific reproducibility

## 📊 Core Features

### File Integrity Verification
```bash
# Every data file is verified using SHA256 hashing
# Prevents data corruption and ensures authenticity

# Example hash verification:
File: DEMO_J.XPT
SHA256: a1b2c3d4e5f6789012345678901234567890123456789012345678901234567890
Status: ✅ VERIFIED
```

### Version History Tracking
```bash
# Complete audit trail of all data changes
Registry Entry:
- File ID: abc123def456
- Original Hash: xyz789...
- Current Hash: abc123...
- Status: ACTIVE
- Last Updated: 2025-10-20 14:30:15
- Version History: Available
```

### Automated Update Detection
```bash
# Checks for new NHANES releases and data updates
🔍 Checking for NHANES data updates...
📋 Found 4 files that may need updates:
  • DEMO_J - Demographics data - file not found locally
  • BMX_J - Body measures data - file not found locally
  • DXX_J - DXA whole body data - file not found locally
  • DXXAG_J - DXA android/gynoid data - file not found locally
```

## 🎯 Usage Guide

### Quick Start

```bash
# Initialize data registry (first time only)
make data-registry-init

# Update registry after downloading data
make data-registry-update

# Check data integrity
make data-integrity

# View registry summary
make data-registry-summary

# Run comprehensive health check
make data-health
```

### Advanced Operations

```bash
# Check for available updates
make data-updates

# Generate reproducibility manifest
make data-manifest

# Manual registry operations
R -e "source('R/data_versioning.R'); update_data_registry()"
R -e "source('R/data_versioning.R'); display_quality_report()"
```

## 🔧 Data Registry System

### Registry Structure

```json
{
  "metadata": {
    "created": "2025-10-20T10:49:40Z",
    "version": "1.0",
    "description": "NHANES Data Registry for BMI Body Fat Analysis",
    "project": "nhanes-bmi-bodyfat"
  },
  "entries": [
    {
      "file_id": "abc123def456",
      "file_path": "data/raw/DEMO_J.XPT",
      "basename": "DEMO_J.XPT",
      "data_type": "demographics",
      "nhanes_cycle": "2017-2018",
      "version": "1.0",
      "hash_sha256": "a1b2c3d4e5f67890...",
      "file_size": 2847293,
      "created_date": "2025-10-20T10:30:15Z",
      "modified_date": "2025-10-20T10:30:15Z",
      "registry_timestamp": "2025-10-20T10:49:40Z",
      "status": "active"
    }
  ]
}
```

### Registry Operations

#### Initialize Registry
```bash
# Create new data registry (first time only)
make data-registry-init

# Or manually:
R -e "source('R/data_versioning.R'); initialize_data_registry()"
```

#### Update Registry
```bash
# Add/update files in registry after data changes
make data-registry-update

# Manual update:
R -e "
source('R/data_versioning.R')
update_data_registry()  # Updates all NHANES files
"
```

#### View Registry Status
```bash
# Display comprehensive registry information
make data-registry-summary

# Expected output:
📊 NHANES Data Registry Summary
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Registry created: 2025-10-20 10:49:40
Total entries: 4

active files: 4

📁 Files by type:
  • demographics: 1
  • body_measures: 1
  • dxa_whole_body: 1
  • dxa_android_gynoid: 1

🕐 Recent activity:
  • DEMO_J.XPT - demographics (2025-10-20 10:49:40)
  • BMX_J.XPT - body_measures (2025-10-20 10:49:40)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## 🔍 Data Quality Assurance

### Integrity Validation

```bash
# Validate all registered files
make data-integrity

# Expected output:
✅ Data integrity validation passed - all files verified!

# Or with issues:
⚠️ Found 2 data integrity issues:
  • DEMO_J.XPT - File integrity compromised - hash mismatch
  • BMX_J.XPT - File size changed (expected: 2847293 actual: 2847294)
```

### Update Detection

```bash
# Check for newer NHANES releases
make data-updates

# Expected output:
🌐 Checking for newer NHANES data updates...
📅 Current analysis uses NHANES cycle: 2017-2018
📋 Available cycles: 2017-2018, 2015-2016, 2013-2014, 2011-2012, 2009-2010
🔄 To check for updates, visit: https://www.cdc.gov/nchs/nhanes/

$current_cycle
[1] "2017-2018"

$available_cycles
[1] "2017-2018" "2015-2016" "2013-2014" "2011-2012" "2009-2010"

$update_available
[1] FALSE
```

### Comprehensive Health Report

```bash
# Generate detailed quality report
make data-health

# Expected output:
📊 Data Quality Report
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Generated: 2025-10-20 14:30:15
NHANES Cycle: 2017-2018

✅ Data Integrity: PASSED

📋 Registry Summary:
  Total files: 4
  Active files: 4
  Total size: 11.2 MB

💡 Recommendations:
  • ✅ All data quality checks passed - analysis ready!
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## 📋 Data Manifest Generation

### Reproducibility Manifests

```bash
# Generate manifest for current analysis
make data-manifest

# Creates: data/raw/manifest.json
```

### Manifest Contents

```json
{
  "metadata": {
    "generated_at": "2025-10-20T14:30:15Z",
    "analysis_version": "1.0.0",
    "nhanes_cycle": "2017-2018",
    "description": "Data manifest for NHANES BMI Body Fat Analysis"
  },
  "data_files": [
    {
      "file_id": "abc123def456",
      "file_path": "data/raw/DEMO_J.XPT",
      "basename": "DEMO_J.XPT",
      "data_type": "demographics",
      "nhanes_cycle": "2017-2018",
      "hash_sha256": "a1b2c3d4e5f67890...",
      "file_size": 2847293,
      "status": "active"
    }
  ],
  "summary": {
    "total_files": 4,
    "total_size": 11284729,
    "file_types": {
      "demographics": 1,
      "body_measures": 1,
      "dxa_whole_body": 1,
      "dxa_android_gynoid": 1
    }
  }
}
```

## 🔧 Technical Implementation

### Hash Computation

```r
# SHA256 hashing for file integrity
compute_file_hash <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)

  tryCatch({
    hash <- digest(file_path, algo = "sha256", file = TRUE)
    return(hash)
  }, error = function(e) {
    warning(paste("Could not compute hash for", file_path, ":", e$message))
    return(NULL)
  })
}

# Usage:
hash <- compute_file_hash("data/raw/DEMO_J.XPT")
# Returns: "a1b2c3d4e5f6789012345678901234567890123456789012345678901234567890"
```

### Registry Entry Creation

```r
# Create registry entry for new file
create_registry_entry <- function(file_path, data_type, nhanes_cycle = NULL, version = NULL) {
  hash <- compute_file_hash(file_path)
  metadata <- get_file_metadata(file_path)

  entry <- list(
    file_id = digest(file_path, algo = "md5"),
    file_path = file_path,
    basename = basename(file_path),
    data_type = data_type,
    nhanes_cycle = nhanes_cycle,
    version = version,
    hash_sha256 = hash,
    file_size = metadata$size,
    created_date = metadata$ctime,
    modified_date = metadata$mtime,
    registry_timestamp = as.character(Sys.time()),
    status = "active"
  )

  return(entry)
}
```

### Update Detection Logic

```r
# Check for file updates
check_for_updates <- function() {
  nhanes_files <- list(
    DEMO_J = list(
      url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT",
      local_path = "data/raw/DEMO_J.XPT",
      description = "Demographics data"
    )
    # ... other files
  )

  for (file_name in names(nhanes_files)) {
    file_info <- nhanes_files[[file_name]]

    # Check local vs remote file properties
    local_hash <- compute_file_hash(file_info$local_path)
    remote_size <- get_remote_file_size(file_info$url)

    # Compare with registry entry
    registry_entry <- find_registry_entry(file_info$local_path)

    if (is.null(registry_entry)) {
      # New file - needs registration
      status = "new"
    } else if (registry_entry$hash_sha256 != local_hash) {
      # File changed
      status = "modified"
    } else if (remote_size != file.info(file_info$local_path)$size) {
      # Remote file different
      status = "outdated"
    }
  }
}
```

## 🛠️ Advanced Usage

### Custom Registry Operations

```r
# Load and modify registry manually
library(jsonlite)

registry <- fromJSON("data/registry/data_registry.json")

# Add custom entry
new_entry <- create_registry_entry(
  "data/custom/my_file.csv",
  "custom_data",
  nhanes_cycle = "2017-2018",
  version = "1.0"
)

registry$entries <- c(registry$entries, list(new_entry))
write_json(registry, "data/registry/data_registry.json", pretty = TRUE)
```

### Bulk Registry Operations

```r
# Update multiple files at once
files_to_register <- c(
  "data/raw/DEMO_J.XPT",
  "data/raw/BMX_J.XPT",
  "data/raw/DXX_J.XPT",
  "data/raw/DXXAG_J.XPT"
)

for (file_path in files_to_register) {
  if (file.exists(file_path)) {
    data_type <- switch(basename(file_path),
      "DEMO_J.XPT" = "demographics",
      "BMX_J.XPT" = "body_measures",
      "DXX_J.XPT" = "dxa_whole_body",
      "DXXAG_J.XPT" = "dxa_android_gynoid"
    )

    add_to_registry(file_path, data_type, "2017-2018")
  }
}
```

### Custom Quality Checks

```r
# Add custom validation rules
custom_quality_check <- function() {
  registry <- load_data_registry()

  # Check for files larger than expected
  large_files <- list()
  for (entry in registry$entries) {
    if (entry$file_size > 10000000) {  # 10MB threshold
      large_files <- c(large_files, entry$basename)
    }
  }

  if (length(large_files) > 0) {
    warning("Unusually large files detected: ", paste(large_files, collapse = ", "))
  }

  return(length(large_files) == 0)
}
```

## 📈 Performance Considerations

### Registry Performance

- **File Operations**: Hash computation is I/O intensive for large files
- **Memory Usage**: Registry loading is memory-efficient (JSON format)
- **Network Operations**: Update checking requires internet connectivity
- **Storage**: Registry file grows with version history

### Optimization Tips

```bash
# For large datasets:
# 1. Use SSD storage for faster hash computation
# 2. Schedule registry updates during low-usage periods
# 3. Archive old versions to reduce registry size
# 4. Use incremental updates for frequent changes
```

### Scalability

- **File Count**: Tested with hundreds of files
- **File Sizes**: Handles multi-gigabyte datasets
- **Network Latency**: Graceful handling of slow connections
- **Concurrent Access**: Thread-safe registry operations

## 🚨 Troubleshooting

### Registry Corruption

```bash
# Backup and recreate registry if corrupted
cp data/registry/data_registry.json data/registry/backup_$(date +%Y%m%d_%H%M%S).json
make data-registry-init  # Creates fresh registry
make data-registry-update  # Repopulates with current files
```

### Hash Mismatches

```bash
# Investigate hash mismatches
R -e "
source('R/data_versioning.R')
integrity <- validate_data_integrity()
print(integrity$issues)
"

# Solutions:
# 1. Re-download files: make fetch
# 2. Check file permissions
# 3. Verify disk integrity
# 4. Check for file modifications during analysis
```

### Update Detection Issues

```bash
# Debug update detection
R -e "
source('R/data_versioning.R')
updates <- check_for_updates()
print(updates$updates)
"

# Solutions:
# 1. Check internet connectivity
# 2. Verify CDC website accessibility
# 3. Check firewall/proxy settings
# 4. Use manual file size comparison
```

## 📚 Integration Examples

### With Version Control

```bash
# Track registry changes in Git
git add data/registry/
git commit -m "Update data registry with new NHANES files"

# View registry changes over time
git log --oneline data/registry/data_registry.json
```

### With CI/CD Pipelines

```yaml
# Example GitHub Actions workflow
name: Data Validation
on: [push, pull_request]

jobs:
  validate-data:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - uses: r-lib/actions/setup-r@v2
    - name: Install dependencies
      run: R -e "renv::restore()"
    - name: Validate data integrity
      run: R -e "source('R/data_versioning.R'); integrity <- validate_data_integrity(); stopifnot(integrity\$valid)"
```

### With R Markdown Reports

```r
# Include registry information in reports
```{r data-integrity}
source("../R/data_versioning.R")
integrity <- validate_data_integrity()
cat("Data Integrity:", ifelse(integrity$valid, "✅ VALID", "❌ ISSUES"))
```
```

## 📞 Support and Maintenance

### Regular Maintenance

```bash
# Monthly maintenance tasks
make data-health                    # Check data quality
make data-registry-summary         # Review registry status
make data-integrity               # Validate file integrity

# Quarterly tasks
# - Archive old registry versions
# - Review and update data retention policies
# - Check for new NHANES releases
```

### Backup Strategy

```bash
# Regular backups of registry and manifests
cp data/registry/data_registry.json backups/
cp data/raw/manifest.json backups/
cp outputs/logs/data_quality_report.json backups/

# Automated backup script
#!/bin/bash
BACKUP_DIR="backups/$(date +%Y%m%d)"
mkdir -p "$BACKUP_DIR"
cp data/registry/data_registry.json "$BACKUP_DIR/"
cp data/raw/manifest.json "$BACKUP_DIR/"
echo "Backup created: $BACKUP_DIR"
```

---

**The data version management system ensures that every analysis is built on verified, traceable data, making the NHANES BMI Body Fat Analysis platform a gold standard for reproducible epidemiological research.**


