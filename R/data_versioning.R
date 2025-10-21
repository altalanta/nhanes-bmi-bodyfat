# Data Version Management for NHANES BMI Body Fat Analysis
# Provides robust versioning, hashing, and update detection for reproducible research

library(digest)
library(yaml)
library(jsonlite)
library(httr)
library(xml2)

# Data registry file path
DATA_REGISTRY_FILE <- "data/registry/data_registry.json"
DATA_MANIFEST_FILE <- "data/raw/manifest.json"

# Ensure registry directory exists
ensure_registry_dirs <- function() {
  dirs <- c("data/registry", "data/raw", "data/derived")
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
}

# Compute SHA256 hash of a file
compute_file_hash <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  tryCatch({
    hash <- digest(file_path, algo = "sha256", file = TRUE)
    return(hash)
  }, error = function(e) {
    warning(paste("Could not compute hash for", file_path, ":", e$message))
    return(NULL)
  })
}

# Get file metadata
get_file_metadata <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  info <- file.info(file_path)
  return(list(
    size = info$size,
    mtime = as.character(info$mtime),
    ctime = as.character(info$ctime),
    file_path = file_path,
    basename = basename(file_path)
  ))
}

# Create data registry entry
create_registry_entry <- function(file_path, data_type, nhanes_cycle = NULL, version = NULL) {
  hash <- compute_file_hash(file_path)
  metadata <- get_file_metadata(file_path)

  if (is.null(hash) || is.null(metadata)) {
    return(NULL)
  }

  entry <- list(
    file_id = digest(file_path, algo = "md5"),
    file_path = file_path,
    basename = metadata$basename,
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

# Load existing data registry
load_data_registry <- function() {
  if (!file.exists(DATA_REGISTRY_FILE)) {
    return(list(
      metadata = list(
        created = as.character(Sys.time()),
        version = "1.0",
        description = "NHANES Data Registry for BMI Body Fat Analysis"
      ),
      entries = list()
    ))
  }

  tryCatch({
    registry <- fromJSON(DATA_REGISTRY_FILE)
    return(registry)
  }, error = function(e) {
    warning(paste("Could not load data registry:", e$message))
    return(list(
      metadata = list(created = as.character(Sys.time())),
      entries = list()
    ))
  })
}

# Save data registry
save_data_registry <- function(registry) {
  ensure_registry_dirs()

  tryCatch({
    write_json(registry, DATA_REGISTRY_FILE, pretty = TRUE, auto_unbox = TRUE)
    return(TRUE)
  }, error = function(e) {
    warning(paste("Could not save data registry:", e$message))
    return(FALSE)
  })
}

# Add entry to data registry
add_to_registry <- function(file_path, data_type, nhanes_cycle = NULL, version = NULL) {
  registry <- load_data_registry()

  # Check if entry already exists
  entry_id <- digest(file_path, algo = "md5")
  existing_entry <- NULL

  for (i in seq_along(registry$entries)) {
    if (registry$entries[[i]]$file_id == entry_id) {
      existing_entry <- registry$entries[[i]]
      break
    }
  }

  # Create new entry or update existing
  new_entry <- create_registry_entry(file_path, data_type, nhanes_cycle, version)

  if (is.null(new_entry)) {
    return(FALSE)
  }

  if (is.null(existing_entry)) {
    # Add new entry
    registry$entries <- c(registry$entries, list(new_entry))
    message(paste("✅ Added new registry entry for:", basename(file_path)))
  } else {
    # Update existing entry
    if (existing_entry$hash_sha256 != new_entry$hash_sha256) {
      # File has changed - mark old as inactive and add new
      existing_entry$status <- "superseded"
      existing_entry$superseded_date <- as.character(Sys.time())

      new_entry$previous_version_id <- existing_entry$file_id
      registry$entries <- c(registry$entries, list(new_entry))

      message(paste("📝 File updated - added new version for:", basename(file_path)))
    } else {
      # File unchanged - just update timestamp
      existing_entry$registry_timestamp <- as.character(Sys.time())
      message(paste("✅ Registry updated for unchanged file:", basename(file_path)))
    }
  }

  return(save_data_registry(registry))
}

# Check if file needs update based on remote manifest
check_for_updates <- function() {
  message("🔍 Checking for NHANES data updates...")

  # Define NHANES data URLs and expected files
  nhanes_files <- list(
    DEMO_J = list(
      url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT",
      local_path = "data/raw/DEMO_J.XPT",
      description = "Demographics data"
    ),
    BMX_J = list(
      url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT",
      local_path = "data/raw/BMX_J.XPT",
      description = "Body measures data"
    ),
    DXX_J = list(
      url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXX_J.XPT",
      local_path = "data/raw/DXX_J.XPT",
      description = "DXA whole body data"
    ),
    DXXAG_J = list(
      url = "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXXAG_J.XPT",
      local_path = "data/raw/DXXAG_J.XPT",
      description = "DXA android/gynoid data"
    )
  )

  updates_available <- list()
  registry <- load_data_registry()

  for (file_name in names(nhanes_files)) {
    file_info <- nhanes_files[[file_name]]

    # Check if local file exists
    if (!file.exists(file_info$local_path)) {
      updates_available[[file_name]] <- list(
        status = "missing",
        action = "download",
        description = paste(file_info$description, "- file not found locally")
      )
      next
    }

    # Compute local hash
    local_hash <- compute_file_hash(file_info$local_path)

    # Try to get remote file info (this is a simplified check)
    # In a real implementation, you might want to check CDC's file modification dates
    remote_size <- tryCatch({
      response <- HEAD(file_info$url)
      as.numeric(headers(response)$`content-length`)
    }, error = function(e) {
      NA
    })

    local_size <- file.info(file_info$local_path)$size

    # Check if file exists in registry and compare hashes
    entry_id <- digest(file_info$local_path, algo = "md5")
    registry_entry <- NULL

    for (entry in registry$entries) {
      if (entry$file_id == entry_id) {
        registry_entry <- entry
        break
      }
    }

    if (is.null(registry_entry)) {
      # New file - needs registration
      updates_available[[file_name]] <- list(
        status = "new",
        action = "register",
        description = paste(file_info$description, "- not in registry")
      )
    } else if (registry_entry$hash_sha256 != local_hash) {
      # File changed
      updates_available[[file_name]] <- list(
        status = "modified",
        action = "update",
        description = paste(file_info$description, "- hash mismatch")
      )
    } else if (!is.na(remote_size) && remote_size != local_size) {
      # Remote file different size
      updates_available[[file_name]] <- list(
        status = "outdated",
        action = "download",
        description = paste(file_info$description, "- size mismatch (remote:", remote_size, "vs local:", local_size, ")")
      )
    }
  }

  if (length(updates_available) == 0) {
    message("✅ All NHANES data files are up to date!")
    return(list(uptodate = TRUE, updates = list()))
  } else {
    message(paste("📋 Found", length(updates_available), "files that may need updates:"))
    for (file_name in names(updates_available)) {
      update <- updates_available[[file_name]]
      message(paste("  •", file_name, "-", update$description))
    }

    return(list(uptodate = FALSE, updates = updates_available))
  }
}

# Generate data manifest for current analysis
generate_data_manifest <- function(output_path = DATA_MANIFEST_FILE) {
  registry <- load_data_registry()

  # Filter active entries
  active_entries <- list()
  for (entry in registry$entries) {
    if (entry$status == "active") {
      active_entries <- c(active_entries, list(entry))
    }
  }

  manifest <- list(
    metadata = list(
      generated_at = as.character(Sys.time()),
      analysis_version = "1.0.0",
      nhanes_cycle = "2017-2018",
      description = "Data manifest for NHANES BMI Body Fat Analysis"
    ),
    data_files = active_entries,
    summary = list(
      total_files = length(active_entries),
      total_size = sum(sapply(active_entries, function(x) x$file_size)),
      file_types = list()
    )
  )

  # Count by data type
  data_types <- table(sapply(active_entries, function(x) x$data_type))
  manifest$summary$file_types <- as.list(data_types)

  # Save manifest
  ensure_registry_dirs()
  write_json(manifest, output_path, pretty = TRUE, auto_unbox = TRUE)

  message(paste("✅ Data manifest generated:", output_path))
  return(manifest)
}

# Validate data integrity using registry
validate_data_integrity <- function() {
  registry <- load_data_registry()
  issues <- list()

  for (entry in registry$entries) {
    if (entry$status != "active") next

    file_path <- entry$file_path

    # Check if file exists
    if (!file.exists(file_path)) {
      issues <- c(issues, list(list(
        type = "missing_file",
        file = entry$basename,
        message = paste("File not found:", file_path)
      )))
      next
    }

    # Check file hash
    current_hash <- compute_file_hash(file_path)
    if (current_hash != entry$hash_sha256) {
      issues <- c(issues, list(list(
        type = "hash_mismatch",
        file = entry$basename,
        message = paste("File integrity compromised - hash mismatch")
      )))
    }

    # Check file size
    current_size <- file.info(file_path)$size
    if (current_size != entry$file_size) {
      issues <- c(issues, list(list(
        type = "size_mismatch",
        file = entry$basename,
        message = paste("File size changed (expected:", entry$file_size, "actual:", current_size, ")")
      )))
    }
  }

  if (length(issues) == 0) {
    message("✅ Data integrity validation passed - all files verified!")
    return(list(valid = TRUE, issues = list()))
  } else {
    message(paste("⚠️ Found", length(issues), "data integrity issues:"))
    for (issue in issues) {
      message(paste("  •", issue$file, "-", issue$message))
    }

    return(list(valid = FALSE, issues = issues))
  }
}

# Display data registry summary
display_registry_summary <- function() {
  registry <- load_data_registry()

  cat("📊 NHANES Data Registry Summary\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Registry created:", registry$metadata$created, "\n")
  cat("Total entries:", length(registry$entries), "\n\n")

  # Handle empty registry
  if (length(registry$entries) == 0) {
    cat("No files registered yet.\n")
    cat("💡 Run 'make data-registry-update' after downloading data files.\n")
  } else {
    # Count by status
    status_counts <- table(sapply(registry$entries, function(x) x$status))
    for (status in names(status_counts)) {
      cat(status, "files:", status_counts[status], "\n")
    }

    cat("\n📁 Files by type:\n")
    type_counts <- table(sapply(registry$entries, function(x) x$data_type))
    for (type in names(type_counts)) {
      cat("  •", type, ":", type_counts[type], "\n")
    }

    # Show recent entries
    recent_entries <- registry$entries[order(sapply(registry$entries, function(x) x$registry_timestamp), decreasing = TRUE)][1:5]

    if (length(recent_entries) > 0) {
      cat("\n🕐 Recent activity:\n")
      for (entry in recent_entries) {
        cat("  •", entry$basename, "-", entry$data_type, "(", entry$registry_timestamp, ")\n")
      }
    }
  }

  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
}

# Initialize data registry for new project
initialize_data_registry <- function() {
  message("🚀 Initializing NHANES data registry...")

  # Check if registry already exists
  if (file.exists(DATA_REGISTRY_FILE)) {
    message("ℹ️ Data registry already exists. Use update_registry() to refresh.")
    return(FALSE)
  }

  # Create empty registry
  registry <- list(
    metadata = list(
      created = as.character(Sys.time()),
      version = "1.0",
      description = "NHANES Data Registry for BMI Body Fat Analysis",
      project = "nhanes-bmi-bodyfat"
    ),
    entries = list()
  )

  success <- save_data_registry(registry)

  if (success) {
    message("✅ Data registry initialized successfully!")
    message("📍 Registry file:", DATA_REGISTRY_FILE)
  } else {
    message("❌ Failed to initialize data registry")
  }

  return(success)
}

# Update registry with current data files
update_data_registry <- function() {
  message("🔄 Updating data registry...")

  nhanes_files <- c(
    "data/raw/DEMO_J.XPT",
    "data/raw/BMX_J.XPT",
    "data/raw/DXX_J.XPT",
    "data/raw/DXXAG_J.XPT"
  )

  updated_count <- 0

  for (file_path in nhanes_files) {
    if (file.exists(file_path)) {
      # Determine data type and cycle
      data_type <- switch(basename(file_path),
        "DEMO_J.XPT" = "demographics",
        "BMX_J.XPT" = "body_measures",
        "DXX_J.XPT" = "dxa_whole_body",
        "DXXAG_J.XPT" = "dxa_android_gynoid",
        "unknown"
      )

      nhanes_cycle <- "2017-2018"

      success <- add_to_registry(file_path, data_type, nhanes_cycle)

      if (success) {
        updated_count <- updated_count + 1
      }
    }
  }

  message(paste("✅ Registry update complete -", updated_count, "files processed"))

  # Generate manifest
  generate_data_manifest()

  return(updated_count > 0)
}

# Check for newer NHANES data releases
check_nhanes_updates <- function() {
  message("🌐 Checking for newer NHANES data releases...")

  # This is a simplified check - in practice, you might want to:
  # 1. Check CDC's NHANES website for new releases
  # 2. Compare with current data versions
  # 3. Alert users to potential updates

  current_cycles <- c("2017-2018", "2015-2016", "2013-2014", "2011-2012", "2009-2010")
  latest_cycle <- current_cycles[1]

  cat("📅 Current analysis uses NHANES cycle:", latest_cycle, "\n")
  cat("📋 Available cycles:", paste(current_cycles, collapse = ", "), "\n")
  cat("🔄 To check for updates, visit: https://www.cdc.gov/nchs/nhanes/\n")

  # In a more advanced implementation, you could:
  # - Parse CDC's website for release dates
  # - Compare file modification times
  # - Alert users when new data becomes available

  return(list(
    current_cycle = latest_cycle,
    available_cycles = current_cycles,
    update_available = FALSE # Would be determined by actual web scraping
  ))
}

# Generate data quality report
generate_quality_report <- function() {
  registry <- load_data_registry()
  integrity <- validate_data_integrity()
  updates <- check_for_updates()

  report <- list(
    metadata = list(
      generated_at = as.character(Sys.time()),
      analysis_cycle = "2017-2018"
    ),
    integrity_check = integrity,
    update_check = updates,
    registry_summary = list(
      total_files = length(registry$entries),
      active_files = sum(unlist(sapply(registry$entries, function(x) x$status == "active"))),
      total_size = sum(unlist(sapply(registry$entries, function(x) x$file_size)))
    ),
    recommendations = list()
  )

  # Generate recommendations
  if (!integrity$valid) {
    report$recommendations <- c(report$recommendations,
      "🔧 Data integrity issues detected - consider re-downloading affected files")
  }

  if (!updates$uptodate) {
    report$recommendations <- c(report$recommendations,
      "📥 Data updates available - consider updating to latest NHANES release")
  }

  if (report$registry_summary$total_files == 0) {
    report$recommendations <- c(report$recommendations,
      "📋 Data registry is empty - run update_data_registry() to populate")
  }

  if (length(report$recommendations) == 0) {
    report$recommendations <- c("✅ All data quality checks passed - analysis ready!")
  }

  # Save report
  report_file <- "outputs/logs/data_quality_report.json"
  dir.create(dirname(report_file), showWarnings = FALSE, recursive = TRUE)
  write_json(report, report_file, pretty = TRUE, auto_unbox = TRUE)

  message("✅ Data quality report generated:", report_file)

  return(report)
}

# Display data quality report
display_quality_report <- function() {
  report <- generate_quality_report()

  cat("📊 Data Quality Report\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("Generated:", report$metadata$generated_at, "\n")
  cat("NHANES Cycle:", report$metadata$analysis_cycle, "\n\n")

  # Integrity status
  if (report$integrity_check$valid) {
    cat("✅ Data Integrity: PASSED\n")
  } else {
    cat("❌ Data Integrity: FAILED\n")
    cat("Issues found:", length(report$integrity_check$issues), "\n")
  }

  cat("\n📋 Registry Summary:\n")
  cat("  Total files:", report$registry_summary$total_files, "\n")
  cat("  Active files:", report$registry_summary$active_files, "\n")
  cat("  Total size:", round(report$registry_summary$total_size / 1024 / 1024, 2), "MB\n")

  cat("\n💡 Recommendations:\n")
  for (rec in report$recommendations) {
    cat("  •", rec, "\n")
  }

  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
}
