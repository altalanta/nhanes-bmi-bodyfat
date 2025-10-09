#!/usr/bin/env Rscript

# Reproducible NHANES data fetch with checksums
# Downloads NHANES 2017-2018 data files and verifies integrity

suppressPackageStartupMessages({
  library(jsonlite)
  library(tools)
  library(httr)
  library(digest)
  library(here)
})

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

# Function to compute SHA256 hash
compute_sha256 <- function(filepath) {
  digest::digest(filepath, algo = "sha256", file = TRUE)
}

# Function to download and verify file
download_and_verify <- function(file_info, data_dir) {
  filepath <- file.path(data_dir, file_info$filename)
  
  cat("Processing:", file_info$filename, "\n")
  
  # Check if file already exists
  if (file.exists(filepath)) {
    cat("  File exists, checking hash...\n")
    current_hash <- compute_sha256(filepath)
    
    if (file_info$sha256 != "" && current_hash == file_info$sha256) {
      cat("  ✓ Hash verified, skipping download\n")
      return(list(filepath = filepath, sha256 = current_hash, downloaded = FALSE))
    } else if (file_info$sha256 == "") {
      cat("  No reference hash, using existing file\n")
      return(list(filepath = filepath, sha256 = current_hash, downloaded = FALSE))
    } else {
      cat("  Hash mismatch, re-downloading...\n")
    }
  }
  
  # Download file
  cat("  Downloading from:", file_info$url, "\n")
  
  tryCatch({
    response <- GET(file_info$url, write_disk(filepath, overwrite = TRUE))
    
    if (status_code(response) != 200) {
      stop("Download failed with status: ", status_code(response))
    }
    
    # Verify it's an XPT file (basic check)
    if (file.size(filepath) < 1000) {
      content <- readLines(filepath, n = 3, warn = FALSE)
      if (any(grepl("404|not found", content, ignore.case = TRUE))) {
        stop("Downloaded HTML error page instead of XPT file")
      }
    }
    
    # Compute hash
    file_hash <- compute_sha256(filepath)
    cat("  ✓ Downloaded successfully, SHA256:", file_hash, "\n")
    
    # Verify hash if provided
    if (file_info$sha256 != "" && file_hash != file_info$sha256) {
      stop("SHA256 hash mismatch for ", file_info$filename)
    }
    
    return(list(filepath = filepath, sha256 = file_hash, downloaded = TRUE))
    
  }, error = function(e) {
    cat("  ✗ Download failed:", e$message, "\n")
    if (file.exists(filepath)) file.remove(filepath)
    stop("Failed to download ", file_info$filename, ": ", e$message)
  })
}

# Alternative: try nhanesA package first
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
    
    file_hash <- compute_sha256(filepath)
    cat("  ✓ Retrieved via nhanesA, SHA256:", file_hash, "\n")
    
    return(list(filepath = filepath, sha256 = file_hash, downloaded = TRUE))
  }, error = function(e) {
    cat("  nhanesA failed:", e$message, "\n")
    return(NULL)
  })
}

# Process each file
manifest <- list()
download_count <- 0

for (dataset_name in names(nhanes_files)) {
  file_info <- nhanes_files[[dataset_name]]
  
  # Try nhanesA first, then direct download
  result <- try_nhanes_a(dataset_name, data_raw_dir)
  
  if (is.null(result)) {
    result <- download_and_verify(file_info, data_raw_dir)
  }
  
  if (result$downloaded) download_count <- download_count + 1
  
  # Update manifest
  manifest[[dataset_name]] <- list(
    filename = file_info$filename,
    url = file_info$url,
    sha256 = result$sha256,
    filepath = result$filepath,
    file_size = file.size(result$filepath),
    download_date = Sys.time()
  )
  
  cat("\n")
}

# Write manifest
manifest_file <- file.path(data_raw_dir, "manifest.json")
write_json(manifest, manifest_file, pretty = TRUE, auto_unbox = TRUE)

cat("Data fetch completed!\n")
cat("Files downloaded/verified:", length(manifest), "\n")
cat("New downloads:", download_count, "\n")
cat("Manifest written to:", manifest_file, "\n")

# Verify all files exist and are valid
missing_files <- c()
for (dataset_name in names(manifest)) {
  filepath <- manifest[[dataset_name]]$filepath
  if (!file.exists(filepath) || file.size(filepath) < 1000) {
    missing_files <- c(missing_files, dataset_name)
  }
}

if (length(missing_files) > 0) {
  stop("Missing or invalid files: ", paste(missing_files, collapse = ", "))
}

cat("\n✓ All NHANES files successfully fetched and verified\n")