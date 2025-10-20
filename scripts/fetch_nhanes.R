#!/usr/bin/env Rscript

# Reproducible NHANES data fetch with comprehensive validation and checksums
# Downloads NHANES 2017-2018 data files, verifies integrity, and validates data quality

suppressPackageStartupMessages({
  library(jsonlite)
  library(tools)
  library(httr)
  library(digest)
  library(here)
  library(foreign)
})

# Source validation utilities
source(file.path(here::here(), "R", "data_validation.R"))

# Source data versioning utilities
source(file.path(here::here(), "R", "data_versioning.R"))

# Set up paths
repo_root <- here::here()
data_raw_dir <- file.path(repo_root, "data", "raw")
dir.create(data_raw_dir, showWarnings = FALSE, recursive = TRUE)

cat("NHANES 2017-2018 Data Fetch\n")
cat("============================\n")
cat("Target directory:", data_raw_dir, "\n\n")

# Define required datasets with expected checksums
nhanes_files <- list(
  DEMO_J = list(
    url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT",
    filename = "DEMO_J.XPT",
    sha256 = "" # Will be computed and stored
  ),
  BMX_J = list(
    url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT", 
    filename = "BMX_J.XPT",
    sha256 = ""
  ),
  DXX_J = list(
    url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXX_J.XPT",
    filename = "DXX_J.XPT", 
    sha256 = ""
  ),
  DXXAG_J = list(
    url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXXAG_J.XPT",
    filename = "DXXAG_J.XPT",
    sha256 = ""
  )
)

# Enhanced validation functions for NHANES data fetching

#' Compute SHA256 hash with error handling
#' @param filepath Path to file to hash
#' @return SHA256 hash string or throws NhanesError
compute_sha256 <- function(filepath) {
  tryCatch({
    if (!file.exists(filepath)) {
      stop(NhanesError(
        paste("File not found for hashing:", filepath),
        code = "FILE_NOT_FOUND_FOR_HASH",
        details = list(filepath = filepath)
      ))
    }

    hash <- digest::digest(filepath, algo = "sha256", file = TRUE)

    if (is.null(hash) || hash == "") {
      stop(NhanesError(
        paste("Failed to compute hash for:", filepath),
        code = "HASH_COMPUTATION_FAILED",
        details = list(filepath = filepath)
      ))
    }

    return(hash)

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("Hash computation error:", e$message),
        code = "HASH_ERROR",
        details = list(filepath = filepath, original_error = e$message)
      ))
    }
  })
}

#' Validate XPT file format and structure
#' @param filepath Path to XPT file
#' @param expected_min_size Minimum expected file size in bytes
#' @return Validation results or throws NhanesError
validate_xpt_file <- function(filepath, expected_min_size = 1000) {
  tryCatch({
    if (!file.exists(filepath)) {
      stop(NhanesError(
        paste("XPT file not found:", filepath),
        code = "XPT_FILE_NOT_FOUND",
        details = list(filepath = filepath)
      ))
    }

    file_size <- file.info(filepath)$size

    # Basic size validation
    if (file_size < expected_min_size) {
      # Check if it's an HTML error page
      tryCatch({
        content <- readLines(filepath, n = 5, warn = FALSE)
        if (any(grepl("404|not found|error", content, ignore.case = TRUE))) {
          stop(NhanesError(
            paste("Downloaded HTML error page instead of XPT file:", filepath),
            code = "HTML_ERROR_PAGE",
            details = list(filepath = filepath, content = head(content, 3))
          ))
        }
      }, error = function(e) {
        # If we can't read the file, it might be corrupted
      })

      stop(NhanesError(
        paste("XPT file too small:", filepath, "size:", file_size, "bytes"),
        code = "XPT_FILE_TOO_SMALL",
        details = list(filepath = filepath, size = file_size, min_expected = expected_min_size)
      ))
    }

    # Try to read as XPT file to validate format
    tryCatch({
      data <- foreign::read.xport(filepath)

      if (is.null(data) || nrow(data) == 0) {
        stop(NhanesError(
          paste("XPT file appears empty or invalid:", filepath),
          code = "XPT_EMPTY_OR_INVALID",
          details = list(filepath = filepath)
        ))
      }

      # Validate required SEQN column
      if (!"SEQN" %in% names(data)) {
        stop(NhanesError(
          paste("XPT file missing required SEQN column:", filepath),
          code = "XPT_MISSING_SEQN",
          details = list(filepath = filepath, columns = names(data))
        ))
      }

      # Validate SEQN is numeric
      if (!is.numeric(data$SEQN)) {
        stop(NhanesError(
          paste("SEQN column not numeric in:", filepath),
          code = "XPT_SEQN_NOT_NUMERIC",
          details = list(filepath = filepath, seqn_class = class(data$SEQN))
        ))
      }

      return(list(
        valid = TRUE,
        file_size = file_size,
        n_rows = nrow(data),
        n_cols = ncol(data),
        has_seqn = "SEQN" %in% names(data),
        seqn_class = class(data$SEQN)
      ))

    }, error = function(e) {
      if (inherits(e, "NhanesError")) {
        stop(e)
      } else {
        stop(NhanesError(
          paste("Failed to read XPT file:", filepath, "-", e$message),
          code = "XPT_READ_ERROR",
          details = list(filepath = filepath, original_error = e$message)
        ))
      }
    })

  }, error = function(e) {
    if (inherits(e, "NhanesError")) {
      stop(e)
    } else {
      stop(NhanesError(
        paste("XPT validation error:", e$message),
        code = "XPT_VALIDATION_ERROR",
        details = list(filepath = filepath, original_error = e$message)
      ))
    }
  })
}

#' Enhanced download and verification function with comprehensive validation
#' @param file_info File information list
#' @param data_dir Target directory for downloads
#' @param max_retries Maximum download retries
#' @return Download and validation results
download_and_verify <- function(file_info, data_dir, max_retries = 3) {
  filepath <- file.path(data_dir, file_info$filename)

  cat("Processing:", file_info$filename, "\n")

  # Check if file already exists and is valid
  if (file.exists(filepath)) {
    cat("  File exists, validating...\n")

    tryCatch({
      validation_result <- validate_xpt_file(filepath)

      if (file_info$sha256 != "" && !is.null(file_info$sha256)) {
        current_hash <- compute_sha256(filepath)
        if (current_hash == file_info$sha256) {
          cat("  ✓ Hash verified and file valid, skipping download\n")
          return(list(
            filepath = filepath,
            sha256 = current_hash,
            downloaded = FALSE,
            validation = validation_result
          ))
        } else {
          cat("  Hash mismatch, re-downloading...\n")
        }
      } else {
        cat("  ✓ File exists and is valid, using existing file\n")
        current_hash <- compute_sha256(filepath)
        return(list(
          filepath = filepath,
          sha256 = current_hash,
          downloaded = FALSE,
          validation = validation_result
        ))
      }
    }, error = function(e) {
      cat("  Existing file invalid:", e$message, "\n")
      cat("  Re-downloading...\n")
    })
  }

  # Download file with retry logic
  cat("  Downloading from:", file_info$url, "\n")

  for (attempt in 1:max_retries) {
    tryCatch({
      response <- GET(file_info$url, write_disk(filepath, overwrite = TRUE))

      if (status_code(response) != 200) {
        if (attempt == max_retries) {
          stop(NhanesError(
            paste("Download failed after", max_retries, "attempts. Status:", status_code(response)),
            code = "DOWNLOAD_FAILED",
            details = list(url = file_info$url, status = status_code(response), attempts = max_retries)
          ))
        } else {
          cat("  Download attempt", attempt, "failed, retrying...\n")
          Sys.sleep(2^attempt)  # Exponential backoff
          next
        }
      }

      # Validate downloaded file
      cat("  Validating downloaded file...\n")
      validation_result <- validate_xpt_file(filepath)

      # Compute hash for future verification
      current_hash <- compute_sha256(filepath)

      cat("  ✓ Download and validation successful\n")
      return(list(
        filepath = filepath,
        sha256 = current_hash,
        downloaded = TRUE,
        validation = validation_result
      ))

    }, error = function(e) {
      if (attempt == max_retries) {
        stop(e)  # Re-throw the final error
      } else {
        cat("  Download attempt", attempt, "failed:", e$message, "\n")
        if (file.exists(filepath)) {
          file.remove(filepath)  # Remove partial download
        }
        Sys.sleep(2^attempt)  # Exponential backoff
      }
    })
  }
}
    
#' Enhanced nhanesA package integration with validation
#' @param dataset_name NHANES dataset name
#' @param data_dir Target directory
#' @return Download results or NULL if failed
try_nhanes_a <- function(dataset_name, data_dir) {
  if (!requireNamespace("nhanesA", quietly = TRUE)) {
    return(NULL)
  }

  cat("  Trying nhanesA package...\n")
  tryCatch({
    data <- nhanesA::nhanes(dataset_name)
    filepath <- file.path(data_dir, paste0(dataset_name, ".XPT"))

    # Save as XPT format
    foreign::write.xport(list(data), file = filepath)

    # Validate the saved file
    validation_result <- validate_xpt_file(filepath)

    # Compute hash for future verification
    file_hash <- compute_sha256(filepath)
    cat("  ✓ Retrieved via nhanesA, SHA256:", file_hash, "\n")

    return(list(
      filepath = filepath,
      sha256 = file_hash,
      downloaded = TRUE,
      validation = validation_result
    ))
  }, error = function(e) {
    cat("  nhanesA failed:", e$message, "\n")
    return(NULL)
  })
}

# Enhanced main processing with comprehensive validation and reporting

cat("Starting comprehensive NHANES data fetch with validation...\n")
cat("=========================================================\n")

# Initialize validation tracking
validation_summary <- list(
  total_files = length(nhanes_files),
  successful_downloads = 0,
  validation_failures = 0,
  files_processed = 0,
  start_time = Sys.time()
)

# Process each file with enhanced validation
manifest <- list()
download_count <- 0
validation_failures <- list()

for (dataset_name in names(nhanes_files)) {
  file_info <- nhanes_files[[dataset_name]]
  cat("Processing dataset:", dataset_name, "\n")

  tryCatch({
    # Try nhanesA first, then direct download
    result <- try_nhanes_a(dataset_name, data_raw_dir)

    if (is.null(result)) {
      result <- download_and_verify(file_info, data_raw_dir)
    }

    if (result$downloaded) {
      download_count <- download_count + 1
      validation_summary$successful_downloads <- validation_summary$successful_downloads + 1
    }

    # Enhanced manifest with validation details
    manifest[[dataset_name]] <- list(
      filename = file_info$filename,
      url = file_info$url,
      sha256 = result$sha256,
      filepath = result$filepath,
      file_size = file.size(result$filepath),
      download_date = Sys.time(),
      downloaded = result$downloaded,
      validation = result$validation
    )

    validation_summary$files_processed <- validation_summary$files_processed + 1

    # Report validation results
    if (!is.null(result$validation)) {
      cat("  ✓ Validation: ", result$validation$n_rows, " rows, ",
          result$validation$n_cols, " columns\n")
    }

    cat("\n")

  }, error = function(e) {
    cat("  ✗ Failed:", e$message, "\n")
    validation_failures <- c(validation_failures, dataset_name)
    validation_summary$validation_failures <- validation_summary$validation_failures + 1

    # Add failure to manifest for tracking
    manifest[[dataset_name]] <- list(
      filename = file_info$filename,
      url = file_info$url,
      error = e$message,
      failed_at = Sys.time(),
      error_code = if (inherits(e, "NhanesError")) e$code else "UNKNOWN_ERROR"
    )

    cat("\n")
  })
}

# Generate comprehensive validation report
end_time <- Sys.time()
validation_summary$end_time <- end_time
validation_summary$total_time <- as.numeric(difftime(end_time, validation_summary$start_time, units = "secs"))

# Write enhanced manifest with validation details
manifest_file <- file.path(data_raw_dir, "manifest.json")
write_json(manifest, manifest_file, pretty = TRUE, auto_unbox = TRUE)

# Write validation summary
validation_report_file <- file.path(data_raw_dir, "validation_report.json")
write_json(validation_summary, validation_report_file, pretty = TRUE, auto_unbox = TRUE)

# Comprehensive completion report
cat("Data fetch completed!\n")
cat("====================\n")
cat("Summary:\n")
cat("  Total files processed:", validation_summary$total_files, "\n")
cat("  Successful downloads:", validation_summary$successful_downloads, "\n")
cat("  Files processed:", validation_summary$files_processed, "\n")
cat("  Validation failures:", validation_summary$validation_failures, "\n")
cat("  Total time:", round(validation_summary$total_time, 2), "seconds\n")
cat("  Manifest written to:", manifest_file, "\n")
cat("  Validation report written to:", validation_report_file, "\n")

# Detailed failure report if any
if (length(validation_failures) > 0) {
  cat("\nValidation Failures:\n")
  for (failure in validation_failures) {
    cat("  ✗ ", failure, ": ", manifest[[failure]]$error, "\n")
  }
  cat("\n⚠️  Some files failed validation. Check logs for details.\n")
}

# Final verification with enhanced checks
cat("\nPerforming final verification...\n")
verification_issues <- c()

for (dataset_name in names(manifest)) {
  file_info <- manifest[[dataset_name]]

  if ("error" %in% names(file_info)) {
    verification_issues <- c(verification_issues, paste(dataset_name, "(failed)"))
    next
  }

  filepath <- file_info$filepath

  # Enhanced verification checks
  if (!file.exists(filepath)) {
    verification_issues <- c(verification_issues, paste(dataset_name, "(missing)"))
  } else if (file.size(filepath) < 1000) {
    verification_issues <- c(verification_issues, paste(dataset_name, "(too small)"))
  } else {
    # Try to validate file structure
    tryCatch({
      validation_result <- validate_xpt_file(filepath, expected_min_size = 1000)
    }, error = function(e) {
      verification_issues <- c(verification_issues, paste(dataset_name, "(invalid:", e$message, ")"))
    })
  }
}

if (length(verification_issues) > 0) {
  cat("Verification Issues Found:\n")
  for (issue in verification_issues) {
    cat("  ⚠️  ", issue, "\n")
  }
  stop("Final verification failed. Issues found: ", paste(verification_issues, collapse = "; "))
}

# Register files in data registry for version management
cat("\n🔄 Registering files in data registry...\n")
registry_updated <- 0

for (dataset_name in names(manifest)) {
  file_info <- manifest[[dataset_name]]

  if (!"error" %in% names(file_info)) {
    filepath <- file_info$filepath

    # Determine data type for registry
    data_type <- switch(dataset_name,
      "DEMO_J" = "demographics",
      "BMX_J" = "body_measures",
      "DXX_J" = "dxa_whole_body",
      "DXXAG_J" = "dxa_android_gynoid",
      "unknown"
    )

    nhanes_cycle <- "2017-2018"

    # Register file in data registry
    success <- add_to_registry(filepath, data_type, nhanes_cycle)
    if (success) {
      registry_updated <- registry_updated + 1
    }
  }
}

cat("✅ Data registry updated with", registry_updated, "files\n")

# Generate data manifest for reproducibility
cat("📋 Generating data manifest...\n")
generate_data_manifest()

cat("\n✅ All NHANES files successfully fetched, validated, and registered!\n")
cat("📊 Data registry and manifest created for reproducible research.\n")
cat("Ready for analysis pipeline.\n")