#!/usr/bin/env Rscript
# Automated Backup and Recovery System for NHANES BMI Body Fat Analysis Platform

library(jsonlite)
library(yaml)
library(digest)

# Backup configuration
BACKUP_CONFIG <- list(
  backup_dir = "backups",
  max_backups = 30,  # Keep last 30 backups
  backup_schedule = "daily",  # daily, weekly, monthly
  compression = TRUE,
  encryption = FALSE,  # Future enhancement
  retention_policy = list(
    daily = 7,      # Keep 7 daily backups
    weekly = 4,     # Keep 4 weekly backups
    monthly = 12    # Keep 12 monthly backups
  )
)

# Create backup
create_backup <- function(backup_name = NULL, description = NULL) {
  cat("📦 Creating system backup...\n")

  # Generate backup name if not provided
  if (is.null(backup_name)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_name <- paste0("backup_", timestamp)
  }

  # Create backup directory
  backup_path <- file.path(BACKUP_CONFIG$backup_dir, backup_name)
  dir.create(backup_path, recursive = TRUE, showWarnings = FALSE)

  # Files and directories to backup
  backup_items <- list(
    "config/config.yml",
    "data/registry/",
    "outputs/",
    "cache/",
    "R/",
    "scripts/",
    "tests/",
    "Makefile",
    "DESCRIPTION",
    "renv.lock"
  )

  # Copy files
  success_count <- 0
  for (item in backup_items) {
    source_path <- item
    target_path <- file.path(backup_path, item)

    tryCatch({
      if (dir.exists(source_path)) {
        # Directory copy
        dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
        file.copy(source_path, dirname(target_path), recursive = TRUE)
      } else if (file.exists(source_path)) {
        # File copy
        dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
        file.copy(source_path, target_path)
      }
      success_count <- success_count + 1
    }, error = function(e) {
      warning(paste("Failed to backup", item, ":", e$message))
    })
  }

  # Create backup metadata
  backup_metadata <- list(
    name = backup_name,
    created_at = as.character(Sys.time()),
    description = description %||% "Automated backup",
    items_backed_up = success_count,
    total_items = length(backup_items),
    system_info = list(
      r_version = R.version.string,
      platform = Sys.info()["sysname"],
      user = Sys.info()["user"]
    ),
    checksum = compute_backup_checksum(backup_path)
  )

  # Save metadata
  write_json(backup_metadata, file.path(backup_path, "backup_metadata.json"), pretty = TRUE)

  # Create README for backup
  readme_content <- paste0(
    "# Backup: ", backup_name, "\n",
    "Created: ", backup_metadata$created_at, "\n",
    "Description: ", backup_metadata$description, "\n",
    "Items backed up: ", backup_metadata$items_backed_up, "/", backup_metadata$total_items, "\n",
    "Checksum: ", backup_metadata$checksum, "\n\n",
    "To restore this backup:\n",
    "Rscript deployment/backup_recovery.R restore ", backup_name, "\n"
  )

  writeLines(readme_content, file.path(backup_path, "README.md"))

  cat("✅ Backup created:", backup_name, "\n")
  cat("📍 Location:", backup_path, "\n")
  cat("📦 Items backed up:", success_count, "/", length(backup_items), "\n")

  return(backup_name)
}

# List available backups
list_backups <- function() {
  if (!dir.exists(BACKUP_CONFIG$backup_dir)) {
    cat("No backup directory found\n")
    return(list())
  }

  backup_dirs <- list.dirs(BACKUP_CONFIG$backup_dir, full.names = FALSE, recursive = FALSE)
  backup_dirs <- backup_dirs[backup_dirs != ""]

  if (length(backup_dirs) == 0) {
    cat("No backups found\n")
    return(list())
  }

  backups <- list()
  for (backup_dir in backup_dirs) {
    backup_path <- file.path(BACKUP_CONFIG$backup_dir, backup_dir)
    metadata_file <- file.path(backup_path, "backup_metadata.json")

    if (file.exists(metadata_file)) {
      metadata <- fromJSON(metadata_file)
      backups[[backup_dir]] <- metadata
    } else {
      # Create minimal metadata for old backups
      backups[[backup_dir]] <- list(
        name = backup_dir,
        created_at = as.character(file.info(backup_path)$ctime),
        description = "Legacy backup",
        items_backed_up = "unknown",
        checksum = "unknown"
      )
    }
  }

  return(backups)
}

# Restore from backup
restore_backup <- function(backup_name, target_items = NULL, force = FALSE) {
  cat("🔄 Restoring from backup:", backup_name, "\n")

  backup_path <- file.path(BACKUP_CONFIG$backup_dir, backup_name)

  if (!dir.exists(backup_path)) {
    stop("Backup not found:", backup_path)
  }

  # Load backup metadata
  metadata_file <- file.path(backup_path, "backup_metadata.json")
  if (file.exists(metadata_file)) {
    metadata <- fromJSON(metadata_file)
    cat("📅 Backup created:", metadata$created_at, "\n")
    cat("📋 Description:", metadata$description, "\n")
  }

  # Verify checksum if available
  if (!is.null(metadata$checksum) && metadata$checksum != "unknown") {
    current_checksum <- compute_backup_checksum(backup_path)
    if (current_checksum != metadata$checksum) {
      if (!force) {
        stop("Backup checksum mismatch. Use force=TRUE to restore anyway.")
      } else {
        warning("Backup checksum mismatch - proceeding with restore")
      }
    }
  }

  # Items to restore (all by default)
  if (is.null(target_items)) {
    target_items <- c(
      "config/",
      "data/registry/",
      "outputs/",
      "cache/",
      "R/",
      "scripts/",
      "tests/"
    )
  }

  # Restore items
  restored_count <- 0
  for (item in target_items) {
    source_path <- file.path(backup_path, item)
    target_path <- item

    tryCatch({
      if (dir.exists(source_path)) {
        # Directory restore
        if (dir.exists(target_path)) {
          unlink(target_path, recursive = TRUE)
        }
        dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
        file.copy(source_path, dirname(target_path), recursive = TRUE)
        cat("✅ Restored directory:", item, "\n")
      } else if (file.exists(source_path)) {
        # File restore
        dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
        file.copy(source_path, target_path, overwrite = TRUE)
        cat("✅ Restored file:", item, "\n")
      }
      restored_count <- restored_count + 1
    }, error = function(e) {
      warning(paste("Failed to restore", item, ":", e$message))
    })
  }

  cat("✅ Restore completed:", restored_count, "items restored\n")
  cat("🔄 System may need to be restarted for changes to take effect\n")

  return(restored_count)
}

# Compute backup checksum
compute_backup_checksum <- function(backup_path) {
  tryCatch({
    # Get all files in backup
    all_files <- list.files(backup_path, recursive = TRUE, full.names = TRUE)
    all_files <- all_files[file.exists(all_files)]  # Only existing files

    if (length(all_files) == 0) {
      return("empty_backup")
    }

    # Compute combined hash of all files
    file_hashes <- sapply(all_files, function(file) {
      digest(file, algo = "sha256", file = TRUE)
    })

    # Combine hashes and compute final hash
    combined_hash <- paste(file_hashes, collapse = "")
    final_hash <- digest(combined_hash, algo = "sha256")

    return(final_hash)
  }, error = function(e) {
    warning("Could not compute backup checksum:", e$message)
    return("checksum_error")
  })
}

# Cleanup old backups
cleanup_old_backups <- function() {
  cat("🧹 Cleaning up old backups...\n")

  backups <- list_backups()

  if (length(backups) == 0) {
    cat("No backups to clean up\n")
    return(0)
  }

  # Sort backups by creation date
  backup_dates <- sapply(backups, function(b) as.POSIXct(b$created_at))
  sorted_backups <- names(backups)[order(backup_dates)]

  # Determine which backups to keep
  current_date <- Sys.Date()
  keep_backups <- c()

  # Daily backups (last 7 days)
  daily_cutoff <- current_date - BACKUP_CONFIG$retention_policy$daily
  daily_backups <- sorted_backups[as.Date(sapply(backups[sorted_backups], function(b) b$created_at)) >= daily_cutoff]
  keep_backups <- c(keep_backups, daily_backups)

  # Weekly backups (last 4 weeks)
  weekly_cutoff <- current_date - (BACKUP_CONFIG$retention_policy$weekly * 7)
  weekly_backups <- sorted_backups[as.Date(sapply(backups[sorted_backups], function(b) b$created_at)) >= weekly_cutoff]
  # Keep one backup per week (the most recent from each week)
  weekly_keep <- c()
  for (backup in weekly_backups) {
    backup_date <- as.Date(backups[[backup]]$created_at)
    week_start <- floor_date(backup_date, "week")
    if (!any(sapply(weekly_keep, function(b) {
      as.Date(backups[[b]]$created_at) >= week_start &&
      as.Date(backups[[b]]$created_at) < week_start + 7
    }))) {
      weekly_keep <- c(weekly_keep, backup)
    }
  }
  keep_backups <- c(keep_backups, weekly_keep)

  # Monthly backups (last 12 months)
  monthly_cutoff <- current_date - (BACKUP_CONFIG$retention_policy$monthly * 30)
  monthly_backups <- sorted_backups[as.Date(sapply(backups[sorted_backups], function(b) b$created_at)) >= monthly_cutoff]
  # Keep one backup per month (the most recent from each month)
  monthly_keep <- c()
  for (backup in monthly_backups) {
    backup_date <- as.Date(backups[[backup]]$created_at)
    month_start <- floor_date(backup_date, "month")
    if (!any(sapply(monthly_keep, function(b) {
      as.Date(backups[[b]]$created_at) >= month_start &&
      as.Date(backups[[b]]$created_at) < month_start + months(1)
    }))) {
      monthly_keep <- c(monthly_keep, backup)
    }
  }
  keep_backups <- c(keep_backups, monthly_keep)

  # Remove duplicates and sort
  keep_backups <- unique(keep_backups)
  keep_backups <- keep_backups[order(sapply(keep_backups, function(b) as.POSIXct(backups[[b]]$created_at)))]

  # Remove old backups
  removed_count <- 0
  for (backup in sorted_backups) {
    if (!(backup %in% keep_backups)) {
      backup_path <- file.path(BACKUP_CONFIG$backup_dir, backup)
      tryCatch({
        unlink(backup_path, recursive = TRUE)
        cat("🗑️ Removed old backup:", backup, "\n")
        removed_count <- removed_count + 1
      }, error = function(e) {
        warning(paste("Failed to remove backup", backup, ":", e$message))
      })
    }
  }

  cat("✅ Cleanup completed:", removed_count, "backups removed\n")
  cat("📦 Kept backups:", length(keep_backups), "\n")

  return(removed_count)
}

# Display backup status
display_backup_status <- function() {
  cat("📦 Backup System Status\n")
  cat("=======================\n")

  # Check backup directory
  if (!dir.exists(BACKUP_CONFIG$backup_dir)) {
    cat("❌ Backup directory not found\n")
    cat("💡 Create it: mkdir -p", BACKUP_CONFIG$backup_dir, "\n")
    return()
  }

  backups <- list_backups()

  if (length(backups) == 0) {
    cat("📭 No backups found\n")
    cat("💡 Create first backup: Rscript deployment/backup_recovery.R backup\n")
    return()
  }

  cat("📊 Backup Summary:\n")
  cat("  Total backups:", length(backups), "\n")
  cat("  Backup directory:", BACKUP_CONFIG$backup_dir, "\n")
  cat("  Retention policy:\n")
  cat("    Daily:", BACKUP_CONFIG$retention_policy$daily, "backups\n")
  cat("    Weekly:", BACKUP_CONFIG$retention_policy$weekly, "backups\n")
  cat("    Monthly:", BACKUP_CONFIG$retention_policy$monthly, "backups\n")

  # Show recent backups
  cat("\n🕐 Recent Backups:\n")
  recent_backups <- head(names(backups)[order(sapply(backups, function(b) as.POSIXct(b$created_at)), decreasing = TRUE)], 5)

  for (backup_name in recent_backups) {
    backup_info <- backups[[backup_name]]
    created_date <- format(as.POSIXct(backup_info$created_at), "%Y-%m-%d %H:%M")
    cat("  •", backup_name, "(", created_date, ")\n")
  }

  # Show oldest backup
  if (length(backups) > 0) {
    oldest_backup <- names(backups)[order(sapply(backups, function(b) as.POSIXct(b$created_at)))][1]
    oldest_info <- backups[[oldest_backup]]
    oldest_date <- format(as.POSIXct(oldest_info$created_at), "%Y-%m-%d %H:%M")
    cat("\n📅 Oldest backup:", oldest_backup, "(", oldest_date, ")\n")
  }

  cat("\n💡 Available commands:\n")
  cat("  Rscript deployment/backup_recovery.R backup [name] [description]\n")
  cat("  Rscript deployment/backup_recovery.R restore [backup_name]\n")
  cat("  Rscript deployment/backup_recovery.R list\n")
  cat("  Rscript deployment/backup_recovery.R cleanup\n")
  cat("  Rscript deployment/backup_recovery.R status\n")
}

# Validate backup integrity
validate_backup <- function(backup_name) {
  cat("🔍 Validating backup:", backup_name, "\n")

  backup_path <- file.path(BACKUP_CONFIG$backup_dir, backup_name)

  if (!dir.exists(backup_path)) {
    cat("❌ Backup not found\n")
    return(FALSE)
  }

  # Load metadata
  metadata_file <- file.path(backup_path, "backup_metadata.json")
  if (!file.exists(metadata_file)) {
    cat("❌ Backup metadata missing\n")
    return(FALSE)
  }

  metadata <- fromJSON(metadata_file)

  # Verify checksum
  current_checksum <- compute_backup_checksum(backup_path)
  stored_checksum <- metadata$checksum

  if (current_checksum != stored_checksum) {
    cat("❌ Backup corrupted (checksum mismatch)\n")
    cat("  Stored:", stored_checksum, "\n")
    cat("  Current:", current_checksum, "\n")
    return(FALSE)
  }

  # Check key files exist
  key_files <- c(
    file.path(backup_path, "config/config.yml"),
    file.path(backup_path, "data/registry/data_registry.json")
  )

  missing_files <- 0
  for (key_file in key_files) {
    if (!file.exists(key_file)) {
      cat("⚠️ Missing key file:", basename(key_file), "\n")
      missing_files <- missing_files + 1
    }
  }

  if (missing_files == 0) {
    cat("✅ Backup validation passed\n")
    return(TRUE)
  } else {
    cat("⚠️ Backup validation passed with warnings (", missing_files, "missing files)\n")
    return(TRUE)
  }
}

# Main backup/restore function
main_backup_restore <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    display_backup_status()
    return()
  }

  action <- args[1]

  switch(action,
    "backup" = {
      backup_name <- if (length(args) > 1) args[2] else NULL
      description <- if (length(args) > 2) args[3] else NULL
      create_backup(backup_name, description)
    },
    "restore" = {
      if (length(args) < 2) {
        stop("Backup name required for restore")
      }
      backup_name <- args[2]
      target_items <- if (length(args) > 2) args[3:length(args)] else NULL
      force <- "--force" %in% args

      restore_backup(backup_name, target_items, force)
    },
    "list" = {
      backups <- list_backups()
      if (length(backups) > 0) {
        cat("Available backups:\n")
        for (name in names(backups)) {
          info <- backups[[name]]
          cat("  •", name, "(", format(as.POSIXct(info$created_at), "%Y-%m-%d %H:%M"), ")\n")
        }
      }
    },
    "cleanup" = {
      cleanup_old_backups()
    },
    "validate" = {
      if (length(args) < 2) {
        stop("Backup name required for validation")
      }
      backup_name <- args[2]
      validate_backup(backup_name)
    },
    "status" = {
      display_backup_status()
    },
    {
      cat("❌ Unknown action:", action, "\n")
      cat("Available actions: backup, restore, list, cleanup, validate, status\n")
      quit(status = 1)
    }
  )
}

# Run main function
main_backup_restore()




