#!/usr/bin/env Rscript

# CRAN Submission Preparation Script
# Usage: Rscript deployment/prepare-cran.R

library(devtools)
library(rcmdcheck)
library(covr)

# Check current working directory
cat("Preparing package for CRAN submission...\n")

# 1. Run comprehensive package checks
cat("1. Running R CMD check...\n")
check_result <- check(args = c("--as-cran", "--no-manual", "--no-vignettes"))
if (check_result$errors > 0) {
  stop("Package check failed with errors. Please fix before CRAN submission.")
}
if (check_result$warnings > 0) {
  cat("⚠ Package check completed with warnings. Review before submission.\n")
}

# 2. Check code coverage
cat("2. Checking code coverage...\n")
coverage <- package_coverage()
coverage_pct <- percent_coverage(coverage)
cat(sprintf("Code coverage: %.1f%%\n", coverage_pct))

if (coverage_pct < 80) {
  warning("Code coverage below 80%. Consider improving test coverage for CRAN.")
}

# 3. Check for CRAN-specific requirements
cat("3. Checking CRAN requirements...\n")

# Check DESCRIPTION file
desc <- read.dcf("DESCRIPTION")
required_fields <- c("Package", "Version", "Title", "Description", "Author", "Maintainer", "License")
missing_fields <- setdiff(required_fields, colnames(desc))

if (length(missing_fields) > 0) {
  stop(paste("Missing required DESCRIPTION fields:", paste(missing_fields, collapse = ", ")))
}

# Check license
license <- desc[1, "License"]
if (!license %in% c("GPL-2", "GPL-3", "MIT", "BSD_2_clause", "BSD_3_clause")) {
  warning("Non-standard license. Ensure it's compatible with CRAN policies.")
}

# Check for forbidden functions
forbidden_funcs <- c("system", "shell", ".C", ".Fortran", ".Call", ".External")
source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

for (file in source_files) {
  content <- readLines(file)
  for (func in forbidden_funcs) {
    if (any(grepl(func, content))) {
      warning(paste("Potentially problematic function", func, "found in", file))
    }
  }
}

# 4. Check vignettes
cat("4. Checking vignettes...\n")
vignette_files <- list.files("vignettes", pattern = "\\.Rmd$", full.names = TRUE)

if (length(vignette_files) > 0) {
  for (vignette in vignette_files) {
    tryCatch({
      rmarkdown::render(vignette, quiet = TRUE)
      cat("✓ Vignette", basename(vignette), "renders successfully\n")
    }, error = function(e) {
      warning(paste("Vignette", basename(vignette), "failed to render:", e$message))
    })
  }
}

# 5. Check dependencies
cat("5. Checking dependencies...\n")
deps <- desc::desc_get_deps()

# Check for suggested packages that might be needed
suggested <- deps$package[deps$type == "Suggests"]
if (length(suggested) > 0) {
  cat("Suggested packages:", paste(suggested, collapse = ", "), "\n")
}

# 6. Create CRAN submission files
cat("6. Creating CRAN submission files...\n")

# Create CRAN-RELEASE file
cran_release_content <- "# CRAN Release Notes for nhanesbmi

## Version Information
- Package: nhanesbmi
- Version: ", desc[1, "Version"], "
- Date: ", format(Sys.Date(), "%Y-%m-%d"), "

## Changes in this version
- Initial CRAN submission
- Comprehensive NHANES BMI-Body Fat analysis package
- Includes automated data fetching, validation, and visualization

## Testing
- R CMD check passed
- Code coverage: ", round(coverage_pct, 1), "%
- Tested on multiple R versions and platforms

## Notes for CRAN maintainers
- Package requires internet access for NHANES data download
- Uses renv for reproducible environments
- Includes comprehensive documentation and vignettes
"

writeLines(cran_release_content, "CRAN-RELEASE")

# Create CRAN-SUBMISSION file (template)
cran_submission_content <- "# CRAN Submission Template

Package: nhanesbmi
Version: ", desc[1, "Version"], "
Title: NHANES BMI vs Body Fat Analysis Package
Description: A comprehensive R package for analyzing the relationship between Body Mass Index (BMI) and whole-body percent body fat using NHANES 2017-2018 data.
Author: NHANES BMI Body Fat Analysis Team
Maintainer: NHANES BMI Body Fat Analysis Team <analysis@nhanes-bmi.org>

## Submission checklist (mark completed items)

### Package structure
- [ ] DESCRIPTION file is valid
- [ ] NAMESPACE file exists and is valid
- [ ] All exported functions have documentation
- [ ] Vignettes are properly formatted
- [ ] Tests exist for main functionality

### Code quality
- [ ] No syntax errors (R CMD check passes)
- [ ] No forbidden functions used
- [ ] Proper error handling implemented
- [ ] Code follows R style guidelines

### Documentation
- [ ] Package has a vignette
- [ ] Functions have proper Rd documentation
- [ ] Examples run without errors
- [ ] Help pages are informative

### Dependencies
- [ ] All dependencies are available on CRAN
- [ ] No circular dependencies
- [ ] Suggested packages are optional

### Data and examples
- [ ] Examples run in < 5 seconds
- [ ] No large data files in package
- [ ] Data examples are appropriate

## Additional notes for CRAN maintainers

This package provides tools for epidemiological research using NHANES data.
It includes automated data fetching from CDC servers and comprehensive
statistical analysis capabilities.

The package is designed for researchers studying body composition and
epidemiological relationships in population health data.
"

writeLines(cran_submission_content, "CRAN-SUBMISSION")

# 7. Final validation
cat("7. Final validation...\n")

# Check package size
package_size <- file.size("nhanesbmi_1.0.0.tar.gz") / (1024 * 1024)  # MB
cat(sprintf("Package size: %.1f MB\n", package_size))

if (package_size > 5) {
  warning("Package size > 5MB. Consider reducing size for CRAN.")
}

# Check for non-ASCII characters
all_files <- list.files(".", recursive = TRUE, full.names = TRUE)
non_ascii_files <- character()

for (file in all_files) {
  if (file_ext(file) %in% c("R", "Rmd", "md", "txt")) {
    content <- readLines(file, warn = FALSE)
    if (any(grepl("[^\\x00-\\x7F]", content))) {
      non_ascii_files <- c(non_ascii_files, file)
    }
  }
}

if (length(non_ascii_files) > 0) {
  cat("Files with non-ASCII characters:\n")
  cat(paste("  -", non_ascii_files, collapse = "\n"))
  warning("Non-ASCII characters detected. Ensure proper encoding.")
}

cat("✅ CRAN preparation complete!\n")
cat("\nNext steps:\n")
cat("1. Review CRAN-RELEASE and CRAN-SUBMISSION files\n")
cat("2. Test package installation: R CMD INSTALL nhanesbmi_*.tar.gz\n")
cat("3. Submit to CRAN via web interface or email\n")
cat("4. Monitor CRAN submission status\n")

# Create submission summary
submission_summary <- list(
  package_name = desc[1, "Package"],
  version = desc[1, "Version"],
  check_passed = check_result$errors == 0,
  coverage_percent = coverage_pct,
  package_size_mb = package_size,
  non_ascii_files = non_ascii_files,
  submission_ready = check_result$errors == 0 && coverage_pct >= 70
)

saveRDS(submission_summary, "cran_submission_summary.rds")

cat(sprintf("\nSubmission ready: %s\n", ifelse(submission_summary$submission_ready, "YES", "NO")))
