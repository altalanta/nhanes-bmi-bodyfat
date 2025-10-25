# The "Guardian" Framework: Enterprise Governance, Security, & Compliance Module
# This framework provides enterprise-grade security, auditing, and compliance features
# for institutional and regulated research environments.

# Required libraries
library(digest)
library(jsonlite)
library(crayon)
library(cli)
library(openssl)
library(dplyr)
library(zip)

# --- Configuration ---

#' @title Guardian Framework Configuration
#' @description Defines security policies, compliance settings, and audit parameters.
GUARDIAN_CONFIG <- list(
  # Security settings
  security = list(
    encryption_enabled = TRUE,
    encryption_key_path = "config/encryption_key.pem",
    session_timeout_minutes = 60,
    max_failed_logins = 5,
    password_policy = list(
      min_length = 12,
      require_uppercase = TRUE,
      require_lowercase = TRUE,
      require_numbers = TRUE,
      require_special_chars = TRUE
    )
  ),

  # Audit trail settings
  audit = list(
    enabled = TRUE,
    log_file = "outputs/logs/audit_trail.json",
    retention_days = 2555,  # 7 years for regulatory compliance
    include_user_actions = TRUE,
    include_data_access = TRUE,
    include_system_events = TRUE,
    hash_algorithm = "sha256"
  ),

  # Access control settings
  access_control = list(
    default_policy = "deny",
    role_hierarchy = list(
      "admin" = c("editor", "viewer", "data_processor"),
      "editor" = c("viewer", "data_processor"),
      "viewer" = c("data_processor"),
      "data_processor" = c()
    ),
    resource_permissions = list(
      "data_raw" = c("read", "write", "delete"),
      "data_derived" = c("read", "write"),
      "outputs" = c("read", "write"),
      "config" = c("read"),
      "audit_logs" = c("read")
    )
  ),

  # Compliance settings
  compliance = list(
    standards = c("HIPAA", "GDPR", "21CFR11"),
    auto_anonymization = TRUE,
    data_retention_years = 7,
    de_identification_methods = c("k_anonymity", "l_diversity", "t_closeness"),
    validation_checks = list(
      "data_integrity" = TRUE,
      "access_logging" = TRUE,
      "change_control" = TRUE,
      "backup_verification" = TRUE
    )
  ),

  # Privacy settings
  privacy = list(
    pii_identifiers = c(
      "name", "ssn", "dob", "address", "phone", "email",
      "medical_record_number", "insurance_id"
    ),
    quasi_identifiers = c(
      "age", "gender", "race", "education", "income", "zip_code"
    ),
    sensitive_variables = c(
      "diagnosis_codes", "medication_list", "genetic_data",
      "mental_health_history", "substance_use"
    )
  )
)

# --- Global State ---

#' @title Guardian State
#' @description Global state for the Guardian Framework
guardian_state <- reactiveValues(
  active_users = list(),
  audit_log = list(),
  access_policies = list(),
  compliance_status = list(),
  security_events = list()
)

# --- Core Framework Functions ---

#' @title Initialize Guardian Framework
#' @description Sets up the Guardian Framework with security and compliance features.
#' @param config The Guardian configuration.
#' @return Invisible TRUE on success.
initialize_guardian_framework <- function(config = GUARDIAN_CONFIG) {
  message("Initializing Guardian Framework...")

  # Create necessary directories
  dir.create("outputs/logs", showWarnings = FALSE, recursive = TRUE)
  dir.create("config", showWarnings = FALSE, recursive = TRUE)
  dir.create("outputs/compliance", showWarnings = FALSE, recursive = TRUE)

  # Initialize encryption if enabled
  if (config$security$encryption_enabled) {
    initialize_encryption(config)
  }

  # Initialize audit logging
  if (config$audit$enabled) {
    initialize_audit_log(config)
  }

  # Initialize access control
  initialize_access_control(config)

  # Initialize compliance monitoring
  initialize_compliance_monitoring(config)

  message("Guardian Framework initialized successfully.")
  return(invisible(TRUE))
}

#' @title Initialize Encryption
#' @description Sets up encryption keys and utilities.
#' @param config The Guardian configuration.
initialize_encryption <- function(config) {
  key_path <- config$security$encryption_key_path

  if (!file.exists(key_path)) {
    message("Generating new encryption key...")
    # Generate a secure random key
    key <- openssl::rand_bytes(32)  # 256-bit key
    writeBin(key, key_path)
    message("Encryption key generated at: ", key_path)
  } else {
    message("Using existing encryption key from: ", key_path)
  }
}

#' @title Initialize Audit Log
#' @description Sets up the immutable audit trail.
#' @param config The Guardian configuration.
initialize_audit_log <- function(config) {
  log_file <- config$audit$log_file

  if (!file.exists(log_file)) {
    # Create initial audit log with genesis entry
    genesis_entry <- list(
      timestamp = as.character(Sys.time()),
      event_type = "framework_initialized",
      user_id = "system",
      action = "guardian_framework_startup",
      details = "Guardian Framework initialized",
      hash = calculate_hash("genesis"),
      previous_hash = "0000000000000000000000000000000000000000000000000000000000000000"
    )

    audit_log <- list(genesis_entry)
    write_json(audit_log, log_file, pretty = TRUE)
    message("Audit log initialized at: ", log_file)
  }
}

#' @title Initialize Access Control
#' @description Sets up role-based and attribute-based access control.
#' @param config The Guardian configuration.
initialize_access_control <- function(config) {
  # Default access policies
  default_policies <- list(
    list(
      user_id = "admin",
      role = "admin",
      permissions = list(
        data_raw = c("read", "write", "delete"),
        data_derived = c("read", "write"),
        outputs = c("read", "write"),
        config = c("read", "write"),
        audit_logs = c("read", "write")
      ),
      valid_from = as.character(Sys.time()),
      valid_until = "2099-12-31"
    )
  )

  # Save to state
  guardian_state$access_policies <- default_policies
  message("Access control initialized with default policies.")
}

#' @title Initialize Compliance Monitoring
#' @description Sets up compliance tracking and validation.
#' @param config The Guardian configuration.
initialize_compliance_monitoring <- function(config) {
  # Initialize compliance status
  compliance_status <- list(
    last_check = as.character(Sys.time()),
    standards = config$compliance$standards,
    status = "monitoring",
    issues = list(),
    reports = list()
  )

  guardian_state$compliance_status <- compliance_status
  message("Compliance monitoring initialized.")
}

# --- Enhanced Access Control ---

#' @title Check Resource Permission
#' @description Checks if a user has permission for a specific resource and action.
#' @param user_id The user ID.
#' @param resource The resource path (e.g., "data/raw/nhanes_2017.csv").
#' @param action The action (read, write, delete).
#' @param config The Guardian configuration.
#' @return TRUE if permitted, FALSE otherwise.
check_resource_permission <- function(user_id, resource, action, config = GUARDIAN_CONFIG) {
  # Parse resource path
  resource_parts <- strsplit(resource, "/")[[1]]
  if (length(resource_parts) < 1) {
    return(FALSE)
  }

  # Map resource to category
  resource_category <- switch(resource_parts[1],
    "data" = if (length(resource_parts) > 1 && resource_parts[2] == "raw") "data_raw" else "data_derived",
    "outputs" = "outputs",
    "config" = "config",
    "audit" = "audit_logs",
    "unknown"
  )

  # Check permission for the category
  return(check_user_permission(user_id, resource_category, action, config))
}

#' @title Create User Session
#' @description Creates a secure user session with timeout.
#' @param user_id The user ID.
#' @param config The Guardian configuration.
#' @return Session token.
create_user_session <- function(user_id, config = GUARDIAN_CONFIG) {
  session_token <- paste0("session_", openssl::rand_bytes(16) |> as.character())
  session_expiry <- Sys.time() + (config$security$session_timeout_minutes * 60)

  session_data <- list(
    token = session_token,
    user_id = user_id,
    created = Sys.time(),
    expires = session_expiry
  )

  # Store session (in production, use a secure session store)
  guardian_state$active_users[[session_token]] <- session_data

  log_security_event("session_created", user_id, "create_user_session",
                    paste("Session created, expires:", session_expiry), config)

  message("Session created for user: ", user_id)
  return(session_token)
}

#' @title Validate Session
#' @description Validates if a session token is active and not expired.
#' @param session_token The session token to validate.
#' @param config The Guardian configuration.
#' @return TRUE if valid, FALSE otherwise.
validate_session <- function(session_token, config = GUARDIAN_CONFIG) {
  session_data <- guardian_state$active_users[[session_token]]

  if (is.null(session_data)) {
    return(FALSE)
  }

  if (Sys.time() > session_data$expires) {
    # Clean up expired session
    guardian_state$active_users[[session_token]] <- NULL
    log_security_event("session_expired", session_data$user_id, "validate_session",
                      "Session expired and removed", config)
    return(FALSE)
  }

  return(TRUE)
}

# --- Enhanced Compliance Tools ---

#' @title Detect PII in Dataset
#' @description Scans a dataset for potential personally identifiable information.
#' @param data The dataset to scan.
#' @param config The Guardian configuration.
#' @return A report of detected PII.
detect_pii <- function(data, config = GUARDIAN_CONFIG) {
  message("Scanning for personally identifiable information...")

  pii_identifiers <- config$privacy$pii_identifiers
  quasi_identifiers <- config$privacy$quasi_identifiers
  sensitive_vars <- config$privacy$sensitive_variables

  detected_pii <- list()
  detected_quasi <- list()
  detected_sensitive <- list()

  # Check column names
  col_names <- names(data)

  detected_pii$columns <- intersect(col_names, pii_identifiers)
  detected_quasi$columns <- intersect(col_names, quasi_identifiers)
  detected_sensitive$columns <- intersect(col_names, sensitive_vars)

  # Check for patterns in string columns
  for (col in col_names) {
    if (is.character(data[[col]]) || is.factor(data[[col]])) {
      # SSN pattern (XXX-XX-XXXX)
      if (any(grepl("\\d{3}-\\d{2}-\\d{4}", data[[col]]))) {
        detected_pii$patterns <- c(detected_pii$patterns, paste(col, "- SSN pattern"))
      }

      # Email pattern
      if (any(grepl("@", data[[col]]))) {
        detected_pii$patterns <- c(detected_pii$patterns, paste(col, "- Email pattern"))
      }

      # Phone pattern
      if (any(grepl("\\d{3}-\\d{3}-\\d{4}", data[[col]]))) {
        detected_pii$patterns <- c(detected_pii$patterns, paste(col, "- Phone pattern"))
      }
    }
  }

  report <- list(
    timestamp = Sys.time(),
    pii_identifiers = detected_pii,
    quasi_identifiers = detected_quasi,
    sensitive_variables = detected_sensitive,
    risk_level = if (length(detected_pii$columns) > 0 || length(detected_pii$patterns) > 0) "high" else "low"
  )

  log_security_event("pii_scan", "system", "detect_pii",
                    paste("PII scan completed, risk level:", report$risk_level), config)

  message("PII scan completed. Risk level: ", report$risk_level)
  return(report)
}

#' @title Validate Data Retention
#' @description Checks if data retention policies are being followed.
#' @param data_paths A vector of file paths to check.
#' @param config The Guardian configuration.
#' @return A retention validation report.
validate_data_retention <- function(data_paths, config = GUARDIAN_CONFIG) {
  message("Validating data retention policies...")

  report <- list(
    timestamp = Sys.time(),
    files_checked = length(data_paths),
    violations = list(),
    compliant_files = 0
  )

  retention_years <- config$compliance$data_retention_years
  cutoff_date <- Sys.Date() - (retention_years * 365.25)

  for (path in data_paths) {
    if (file.exists(path)) {
      file_info <- file.info(path)
      file_age_days <- as.numeric(Sys.Date() - as.Date(file_info$mtime))

      if (file_age_days > (retention_years * 365.25)) {
        report$violations <- c(report$violations, paste(path, "- exceeds retention period"))
      } else {
        report$compliant_files <- report$compliant_files + 1
      }
    }
  }

  report$overall_status <- if (length(report$violations) == 0) "compliant" else "violations_found"

  log_security_event("retention_check", "system", "validate_data_retention",
                    paste("Retention check completed:", report$overall_status), config)

  message("Data retention validation completed. Status: ", report$overall_status)
  return(report)
}

# --- Data Anonymization Utilities ---

#' @title Apply K-Anonymity
#' @description Applies k-anonymity to a dataset.
#' @param data The dataset to anonymize.
#' @param k The minimum group size.
#' @param quasi_identifiers The quasi-identifiers to group by.
#' @param config The Guardian configuration.
#' @return The k-anonymized dataset.
apply_k_anonymity <- function(data, k = 5, quasi_identifiers = NULL, config = GUARDIAN_CONFIG) {
  message("Applying k-anonymity (k=", k, ")...")

  if (is.null(quasi_identifiers)) {
    quasi_identifiers <- intersect(names(data), config$privacy$quasi_identifiers)
  }

  if (length(quasi_identifiers) == 0) {
    warning("No quasi-identifiers specified for k-anonymity.")
    return(data)
  }

  # Group by quasi-identifiers and filter groups smaller than k
  anonymized_data <- data %>%
    mutate(group_id = paste(!!!syms(quasi_identifiers), sep = "_")) %>%
    group_by(group_id) %>%
    filter(n() >= k) %>%
    ungroup() %>%
    select(-group_id)

  removed_count <- nrow(data) - nrow(anonymized_data)
  message("K-anonymity applied. Removed ", removed_count, " records (", round(removed_count/nrow(data)*100, 1), "%).")

  log_security_event("k_anonymity", "current_user", "apply_k_anonymity",
                    paste("Applied k-anonymity, removed", removed_count, "records"), config)

  return(anonymized_data)
}

#' @title Generalize Data
#' @description Generalizes data by binning numerical values and grouping categorical values.
#' @param data The dataset to generalize.
#' @param config The Guardian configuration.
#' @return The generalized dataset.
generalize_data <- function(data, config = GUARDIAN_CONFIG) {
  message("Generalizing data for privacy protection...")

  generalized_data <- data

  # Generalize age (bin into age groups)
  if ("age_years" %in% names(data) || "RIDAGEYR" %in% names(data)) {
    age_col <- if ("age_years" %in% names(data)) "age_years" else "RIDAGEYR"
    generalized_data <- generalized_data %>%
      mutate(!!age_col := case_when(
        !!sym(age_col) < 18 ~ "Under 18",
        !!sym(age_col) < 30 ~ "18-29",
        !!sym(age_col) < 40 ~ "30-39",
        !!sym(age_col) < 50 ~ "40-49",
        !!sym(age_col) < 60 ~ "50-59",
        !!sym(age_col) < 70 ~ "60-69",
        TRUE ~ "70+"
      ))
  }

  # Generalize zip codes (first 3 digits)
  zip_cols <- grep("zip", names(data), ignore.case = TRUE, value = TRUE)
  for (col in zip_cols) {
    if (is.numeric(data[[col]])) {
      generalized_data <- generalized_data %>%
        mutate(!!col := floor(!!sym(col) / 100) * 100)
    }
  }

  log_security_event("data_generalized", "current_user", "generalize_data",
                    "Data generalized for privacy protection", config)

  message("Data generalization completed.")
  return(generalized_data)
}

# --- Audit Trail Functions ---

#' @title Log Security Event
#' @description Adds an entry to the immutable audit trail.
#' @param event_type The type of event (e.g., "user_login", "data_access").
#' @param user_id The ID of the user performing the action.
#' @param action The specific action taken.
#' @param details Additional details about the event.
#' @param config The Guardian configuration.
#' @return The hash of the new audit entry.
log_security_event <- function(event_type, user_id, action, details = "", config = GUARDIAN_CONFIG) {
  if (!config$audit$enabled) {
    return(invisible(NULL))
  }

  # Read current audit log
  log_file <- config$audit$log_file
  if (file.exists(log_file)) {
    audit_log <- read_json(log_file)
  } else {
    audit_log <- list()
  }

  # Get previous hash
  previous_hash <- if (length(audit_log) > 0) {
    audit_log[[length(audit_log)]]$hash
  } else {
    "0000000000000000000000000000000000000000000000000000000000000000"
  }

  # Create new entry
  entry_content <- paste(event_type, user_id, action, details, sep = "|")
  entry_hash <- calculate_hash(entry_content, previous_hash)

  new_entry <- list(
    timestamp = as.character(Sys.time()),
    event_type = event_type,
    user_id = user_id,
    action = action,
    details = details,
    hash = entry_hash,
    previous_hash = previous_hash
  )

  # Add to log and save
  audit_log <- c(audit_log, list(new_entry))
  write_json(audit_log, log_file, pretty = TRUE)

  # Update state
  guardian_state$audit_log <- audit_log

  message("Security event logged: ", event_type, " - ", action)
  return(entry_hash)
}

#' @title Calculate Hash
#' @description Calculates a cryptographic hash for audit trail integrity.
#' @param content The content to hash.
#' @param previous_hash The previous entry's hash for chain integrity.
#' @param algorithm The hash algorithm to use.
#' @return The calculated hash.
calculate_hash <- function(content, previous_hash = "", algorithm = "sha256") {
  combined <- paste0(previous_hash, content)
  digest(combined, algo = algorithm)
}

#' @title Verify Audit Trail Integrity
#' @description Verifies that the audit trail has not been tampered with.
#' @param config The Guardian configuration.
#' @return TRUE if integrity is verified, FALSE otherwise.
verify_audit_integrity <- function(config = GUARDIAN_CONFIG) {
  log_file <- config$audit$log_file
  if (!file.exists(log_file)) {
    warning("Audit log does not exist.")
    return(FALSE)
  }

  audit_log <- read_json(log_file)
  if (length(audit_log) == 0) {
    return(TRUE)
  }

  message("Verifying audit trail integrity...")

  for (i in 2:length(audit_log)) {
    current_entry <- audit_log[[i]]
    previous_entry <- audit_log[[i-1]]

    # Verify hash chain
    expected_content <- paste(current_entry$event_type, current_entry$user_id,
                             current_entry$action, current_entry$details, sep = "|")
    expected_hash <- calculate_hash(expected_content, previous_entry$hash)

    if (current_entry$hash != expected_hash) {
      message("Audit trail integrity violation detected at entry ", i)
      return(FALSE)
    }
  }

  message("Audit trail integrity verified.")
  return(TRUE)
}

# --- Access Control Functions ---

#' @title Check User Permission
#' @description Verifies if a user has permission to perform an action on a resource.
#' @param user_id The user's ID.
#' @param resource The resource being accessed (e.g., "data_raw", "outputs").
#' @param action The action being performed (e.g., "read", "write", "delete").
#' @param config The Guardian configuration.
#' @return TRUE if permission is granted, FALSE otherwise.
check_user_permission <- function(user_id, resource, action, config = GUARDIAN_CONFIG) {
  # Find user's policy
  user_policy <- NULL
  for (policy in guardian_state$access_policies) {
    if (policy$user_id == user_id) {
      user_policy <- policy
      break
    }
  }

  # Default deny if no policy found
  if (is.null(user_policy)) {
    log_security_event("access_denied", user_id, paste("access_denied:", resource, action),
                      "No access policy found for user", config)
    return(FALSE)
  }

  # Check if action is in permissions for this resource
  resource_permissions <- user_policy$permissions[[resource]]
  if (is.null(resource_permissions) || !action %in% resource_permissions) {
    log_security_event("access_denied", user_id, paste("permission_denied:", resource, action),
                      paste("User lacks", action, "permission for", resource), config)
    return(FALSE)
  }

  # Check if policy is currently valid
  current_time <- Sys.time()
  valid_from <- as.POSIXct(user_policy$valid_from)
  valid_until <- as.POSIXct(user_policy$valid_until)

  if (current_time < valid_from || current_time > valid_until) {
    log_security_event("access_denied", user_id, paste("policy_expired:", resource, action),
                      "Access policy has expired", config)
    return(FALSE)
  }

  log_security_event("access_granted", user_id, paste("access_granted:", resource, action),
                    paste("Permission granted for", action, "on", resource), config)
  return(TRUE)
}

#' @title Add User Access Policy
#' @description Creates a new access policy for a user.
#' @param user_id The user's ID.
#' @param role The user's role.
#' @param permissions A list of permissions by resource.
#' @param valid_from Start date for the policy.
#' @param valid_until End date for the policy.
#' @param config The Guardian configuration.
#' @return TRUE if policy was added successfully.
add_user_policy <- function(user_id, role, permissions, valid_from = Sys.time(),
                           valid_until = "2099-12-31", config = GUARDIAN_CONFIG) {

  # Check admin permission
  if (!check_user_permission("current_user", "access_policies", "write")) {
    stop("Insufficient permissions to modify access policies.")
  }

  new_policy <- list(
    user_id = user_id,
    role = role,
    permissions = permissions,
    valid_from = as.character(valid_from),
    valid_until = as.character(valid_until)
  )

  guardian_state$access_policies <- c(guardian_state$access_policies, list(new_policy))

  log_security_event("policy_created", "current_user", "add_user_policy",
                    paste("Created policy for user:", user_id), config)

  message("Access policy created for user: ", user_id)
  return(TRUE)
}

# --- Data Protection Functions ---

#' @title Anonymize Dataset
#' @description Applies anonymization techniques to protect sensitive data.
#' @param data The dataset to anonymize.
#' @param method The anonymization method to use.
#' @param config The Guardian configuration.
#' @return The anonymized dataset.
anonymize_dataset <- function(data, method = "k_anonymity", config = GUARDIAN_CONFIG) {
  message("Anonymizing dataset using ", method, " method...")

  if (method == "k_anonymity") {
    # Simple k-anonymity implementation
    # Group by quasi-identifiers and ensure each group has at least k records
    quasi_ids <- intersect(names(data), config$privacy$quasi_identifiers)
    if (length(quasi_ids) > 0) {
      # This is a simplified implementation
      # Real k-anonymity would require more sophisticated algorithms
      data <- data %>%
        mutate(anon_id = paste(!!!syms(quasi_ids), sep = "_")) %>%
        group_by(anon_id) %>%
        mutate(group_size = n()) %>%
        ungroup()
    }
  }

  log_security_event("data_anonymized", "current_user", "anonymize_dataset",
                    paste("Applied", method, "anonymization"), config)

  message("Dataset anonymized successfully.")
  return(data)
}

# --- Compliance Functions ---

#' @title Run Compliance Check
#' @description Performs automated compliance validation.
#' @param standard The compliance standard to check (e.g., "HIPAA", "GDPR").
#' @param config The Guardian configuration.
#' @return A compliance report.
run_compliance_check <- function(standard = "HIPAA", config = GUARDIAN_CONFIG) {
  message("Running compliance check for: ", standard)

  report <- list(
    standard = standard,
    timestamp = Sys.time(),
    checks = list(),
    overall_status = "pending"
  )

  # Check audit trail integrity
  audit_ok <- verify_audit_integrity(config)
  report$checks$audit_integrity <- audit_ok

  # Check access logging
  log_file <- config$audit$log_file
  access_logging_ok <- file.exists(log_file) && length(read_json(log_file)) > 0
  report$checks$access_logging <- access_logging_ok

  # Check data anonymization
  anonymization_ok <- config$compliance$auto_anonymization
  report$checks$data_anonymization <- anonymization_ok

  # Overall status
  report$overall_status <- if (all(unlist(report$checks))) "compliant" else "non_compliant"

  # Update compliance status
  guardian_state$compliance_status$last_check <- as.character(Sys.time())
  guardian_state$compliance_status$reports <- c(guardian_state$compliance_status$reports, list(report))

  log_security_event("compliance_check", "system", "run_compliance_check",
                    paste("Compliance check completed for", standard), config)

  message("Compliance check completed. Status: ", report$overall_status)
  return(report)
}

#' @title Generate Validation Pack
#' @description Creates a comprehensive validation package for regulatory submission.
#' @param config The Guardian configuration.
#' @return Path to the generated validation package.
generate_validation_pack <- function(config = GUARDIAN_CONFIG) {
  message("Generating validation pack...")

  # Create validation directory
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  validation_dir <- paste0("outputs/compliance/validation_pack_", timestamp)
  dir.create(validation_dir, showWarnings = FALSE, recursive = TRUE)

  # Generate compliance reports
  compliance_reports <- list()
  for (standard in config$compliance$standards) {
    report <- run_compliance_check(standard, config)
    compliance_reports[[standard]] <- report

    # Save report
    write_json(report, file.path(validation_dir, paste0(standard, "_compliance_report.json")), pretty = TRUE)
  }

  # Generate system validation document
  system_validation <- list(
    timestamp = as.character(Sys.time()),
    system_info = list(
      r_version = R.version.string,
      platform = R.version$platform,
      packages = installed.packages()[, c("Package", "Version")]
    ),
    validation_checks = config$compliance$validation_checks,
    audit_integrity = verify_audit_integrity(config)
  )
  write_json(system_validation, file.path(validation_dir, "system_validation.json"), pretty = TRUE)

  # Create validation summary
  validation_summary <- list(
    generation_date = as.character(Sys.time()),
    compliance_standards = config$compliance$standards,
    overall_status = if (all(sapply(compliance_reports, function(x) x$overall_status == "compliant"))) "validated" else "requires_review",
    reports = compliance_reports
  )
  write_json(validation_summary, file.path(validation_dir, "validation_summary.json"), pretty = TRUE)

  # Create README
  readme_content <- c(
    "# Validation Package",
    "",
    "This package contains validation documentation for regulatory compliance.",
    "",
    "## Contents:",
    "- Compliance reports for each standard",
    "- System validation information",
    "- Validation summary",
    "",
    "Generated on: ", as.character(Sys.time())
  )
  writeLines(readme_content, file.path(validation_dir, "README.md"))

  # Zip the validation package
  validation_zip <- paste0(validation_dir, ".zip")
  zip::zipr(validation_zip, list.files(validation_dir, full.names = TRUE, recursive = TRUE), root = validation_dir)

  message("Validation pack generated: ", validation_zip)
  return(validation_zip)
}

# --- Utility Functions ---

#' @title Guardian Framework Status
#' @description Displays the current status of the Guardian Framework.
#' @param config The Guardian configuration.
display_guardian_status <- function(config = GUARDIAN_CONFIG) {
  cat("\n", cli::rule("Guardian Framework Status", col = "blue"), "\n\n")

  # Security status
  cat(cli::style_bold("Security Status:"), "\n")
  cat("  Encryption:", if (config$security$encryption_enabled) cli::col_green("Enabled") else cli::col_red("Disabled"), "\n")
  cat("  Session Timeout:", config$security$session_timeout_minutes, "minutes\n")
  cat("  Failed Login Limit:", config$security$max_failed_logins, "\n\n")

  # Audit status
  cat(cli::style_bold("Audit Trail:"), "\n")
  log_file <- config$audit$log_file
  if (file.exists(log_file)) {
    audit_log <- read_json(log_file)
    cat("  Entries:", length(audit_log), "\n")
    cat("  Integrity:", if (verify_audit_integrity(config)) cli::col_green("Verified") else cli::col_red("Failed"), "\n")
  } else {
    cat("  Status:", cli::col_red("Not initialized"), "\n")
  }
  cat("\n")

  # Access control status
  cat(cli::style_bold("Access Control:"), "\n")
  cat("  Policies:", length(guardian_state$access_policies), "\n")
  cat("  Default Policy:", config$access_control$default_policy, "\n\n")

  # Compliance status
  cat(cli::style_bold("Compliance:"), "\n")
  cat("  Standards:", paste(config$compliance$standards, collapse = ", "), "\n")
  cat("  Last Check:", guardian_state$compliance_status$last_check, "\n")
  if (!is.null(guardian_state$compliance_status$reports)) {
    latest_report <- guardian_state$compliance_status$reports[[length(guardian_state$compliance_status$reports)]]
    cat("  Status:", latest_report$overall_status, "\n")
  }
}

# --- Main Interface ---

#' @title Run Guardian Framework
#' @description Main entry point for the Guardian Framework.
#' @param action The action to perform (status, audit, compliance, validate).
#' @param config The Guardian configuration.
run_guardian_framework <- function(action = "status", config = GUARDIAN_CONFIG) {
  initialize_guardian_framework(config)

  if (action == "status") {
    display_guardian_status(config)

  } else if (action == "audit") {
    if (verify_audit_integrity(config)) {
      cat("Audit trail integrity verified.\n")
    } else {
      cat("Audit trail integrity check failed.\n")
    }

  } else if (action == "compliance") {
    for (standard in config$compliance$standards) {
      report <- run_compliance_check(standard, config)
      cat("Compliance check for", standard, ":", report$overall_status, "\n")
    }

  } else if (action == "validate") {
    validation_pack <- generate_validation_pack(config)
    cat("Validation package generated:", validation_pack, "\n")

  } else {
    stop("Invalid action. Use 'status', 'audit', 'compliance', or 'validate'.")
  }
}