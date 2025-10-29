#!/usr/bin/env Rscript

# Guardian Framework Command-Line Interface
# Description: This script provides a CLI for security, compliance, and privacy features.

# --- Setup ---
# Load project configurations and error handling
source("scripts/error_handling.R")
config <- safe_load_config()
ensure_output_dirs(config)

# Set library paths
.libPaths(c('~/R_libs', .libPaths()))

# Load required packages
required_packages <- c("digest", "jsonlite", "crayon", "cli", "openssl", "dplyr", "zip")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Source the core framework
source("R/guardian_framework.R")

# --- Command-Line Argument Parsing ---
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cat("Guardian Framework - Enterprise Security & Compliance\n\n")
  cat("Usage: Rscript scripts/guardian.R <command> [options...]\n\n")
  cat("Commands:\n")
  cat("  status           Display Guardian Framework status\n")
  cat("  audit            Verify audit trail integrity\n")
  cat("  compliance       Run compliance checks\n")
  cat("  validate         Generate validation package for regulatory submission\n")
  cat("  anonymize        Apply data anonymization techniques\n")
  cat("  pii-scan         Scan dataset for personally identifiable information\n")
  cat("  session          Manage user sessions\n")
  cat("  policy           Manage access control policies\n\n")
  cat("Options:\n")
  cat("  --help           Show this help message\n")
  cat("  --file=FILE      Specify input file for anonymization/PII scan\n")
  cat("  --method=METHOD  Anonymization method (k_anonymity, generalization)\n")
  cat("  --k=NUMBER       K value for k-anonymity (default: 5)\n")
  quit(status = 1)
}

command <- args[1]
options <- args[-1]

# Helper to parse --key=value arguments
parse_option <- function(flag, default = NULL) {
  val <- grep(paste0("^", flag, "="), options, value = TRUE)
  if (length(val) > 0) {
    return(sub(paste0("^", flag, "="), "", val))
  }
  return(default)
}

# Check for help flag
if ("--help" %in% options) {
  cat("Guardian Framework - Enterprise Security & Compliance\n\n")
  cat("Command:", command, "\n\n")
  switch(command,
    status = cat("Display current status of security, audit, and compliance systems.\n"),
    audit = cat("Verify the integrity of the audit trail.\n"),
    compliance = cat("Run automated compliance checks for HIPAA, GDPR, 21CFR11.\n"),
    validate = cat("Generate comprehensive validation package for regulatory submission.\n"),
    anonymize = cat("Apply anonymization techniques to protect sensitive data.\nOptions: --file=FILE --method=METHOD --k=NUMBER\n"),
    "pii-scan" = cat("Scan dataset for personally identifiable information.\nOptions: --file=FILE\n"),
    session = cat("Manage user sessions (create, validate, list).\n"),
    policy = cat("Manage access control policies (list, add, remove).\n"),
    cat("No help available for command:", command, "\n")
  )
  quit(status = 0)
}

# --- Main Execution ---
main <- function() {

  tryCatch({
    initialize_guardian_framework()

    if (command == "status") {
      display_guardian_status()
      log_security_event("status_check", "user", "guardian_status", "Guardian status displayed")

    } else if (command == "audit") {
      if (verify_audit_integrity()) {
        cat(cli::col_green("✓ Audit trail integrity verified\n"))
      } else {
        cat(cli::col_red("✗ Audit trail integrity check failed\n"))
      }

    } else if (command == "compliance") {
      cat("Running compliance checks...\n\n")
      for (standard in GUARDIAN_CONFIG$compliance$standards) {
        report <- run_compliance_check(standard)
        status_icon <- if (report$overall_status == "compliant") cli::col_green("✓") else cli::col_red("✗")
        cat(status_icon, standard, ":", report$overall_status, "\n")
      }

    } else if (command == "validate") {
      cat("Generating validation package...\n")
      validation_pack <- generate_validation_pack()
      cat(cli::col_green("✓ Validation package generated:"), validation_pack, "\n")

    } else if (command == "anonymize") {
      input_file <- parse_option("--file")
      method <- parse_option("--method", "k_anonymity")
      k_value <- as.numeric(parse_option("--k", "5"))

      if (is.null(input_file) || !file.exists(input_file)) {
        cat(cli::col_red("Error: Please specify a valid input file with --file=\n"))
        quit(status = 1)
      }

      cat("Loading data from:", input_file, "\n")
      data <- readRDS(input_file)  # Assume RDS format for now

      if (method == "k_anonymity") {
        cat("Applying k-anonymity (k =", k_value, ")...\n")
        anonymized_data <- apply_k_anonymity(data, k = k_value)
      } else if (method == "generalization") {
        cat("Applying data generalization...\n")
        anonymized_data <- generalize_data(data)
      } else {
        cat(cli::col_red("Error: Unknown anonymization method:"), method, "\n")
        quit(status = 1)
      }

      # Save anonymized data
      output_file <- sub("\\.[^.]*$", "_anonymized.rds", input_file)
      saveRDS(anonymized_data, output_file)
      cat(cli::col_green("✓ Anonymized data saved to:"), output_file, "\n")

    } else if (command == "pii-scan") {
      input_file <- parse_option("--file")

      if (is.null(input_file) || !file.exists(input_file)) {
        cat(cli::col_red("Error: Please specify a valid input file with --file=\n"))
        quit(status = 1)
      }

      cat("Loading data from:", input_file, "\n")
      data <- readRDS(input_file)  # Assume RDS format for now

      cat("Scanning for PII...\n")
      pii_report <- detect_pii(data)

      # Display results
      cat("\nPII Scan Results:\n")
      cat("Risk Level:", pii_report$risk_level, "\n\n")

      if (length(pii_report$pii_identifiers$columns) > 0) {
        cat("PII Columns Detected:\n")
        cat(paste("- ", pii_report$pii_identifiers$columns, collapse = "\n"), "\n\n")
      }

      if (length(pii_report$pii_identifiers$patterns) > 0) {
        cat("PII Patterns Detected:\n")
        cat(paste("- ", pii_report$pii_identifiers$patterns, collapse = "\n"), "\n\n")
      }

      if (length(pii_report$quasi_identifiers$columns) > 0) {
        cat("Quasi-identifiers:\n")
        cat(paste("- ", pii_report$quasi_identifiers$columns, collapse = "\n"), "\n\n")
      }

      if (pii_report$risk_level == "high") {
        cat(cli::col_red("⚠ High risk: PII detected. Consider anonymization before use.\n"))
      } else {
        cat(cli::col_green("✓ Low risk: No PII detected.\n"))
      }

    } else if (command == "session") {
      subcommand <- parse_option("--subcommand")

      if (is.null(subcommand)) {
        cat("Session Management Commands:\n")
        cat("  create --user=USERID    Create new session for user\n")
        cat("  validate --token=TOKEN  Validate session token\n")
        cat("  list                    List active sessions\n")
        quit(status = 0)
      }

      if (subcommand == "create") {
        user_id <- parse_option("--user")
        if (is.null(user_id)) {
          cat(cli::col_red("Error: Please specify user ID with --user=\n"))
          quit(status = 1)
        }
        token <- create_user_session(user_id)
        cat("Session created for", user_id, "\n")
        cat("Token:", token, "\n")

      } else if (subcommand == "validate") {
        token <- parse_option("--token")
        if (is.null(token)) {
          cat(cli::col_red("Error: Please specify token with --token=\n"))
          quit(status = 1)
        }
        if (validate_session(token)) {
          cat(cli::col_green("✓ Session is valid\n"))
        } else {
          cat(cli::col_red("✗ Session is invalid or expired\n"))
        }

      } else if (subcommand == "list") {
        sessions <- guardian_state$active_users
        if (length(sessions) == 0) {
          cat("No active sessions.\n")
        } else {
          cat("Active Sessions:\n")
          for (token in names(sessions)) {
            session <- sessions[[token]]
            cat("- User:", session$user_id, "| Expires:", session$expires, "\n")
          }
        }

      } else {
        cat(cli::col_red("Error: Unknown session subcommand:"), subcommand, "\n")
        quit(status = 1)
      }

    } else if (command == "policy") {
      subcommand <- parse_option("--subcommand")

      if (is.null(subcommand)) {
        cat("Policy Management Commands:\n")
        cat("  list                    List current access policies\n")
        cat("  add --user=USER --role=ROLE --permissions=PERMS  Add new policy\n")
        quit(status = 0)
      }

      if (subcommand == "list") {
        policies <- guardian_state$access_policies
        if (length(policies) == 0) {
          cat("No access policies defined.\n")
        } else {
          cat("Access Policies:\n")
          for (policy in policies) {
            cat("- User:", policy$user_id, "| Role:", policy$role, "\n")
            cat("  Permissions:", paste(names(policy$permissions), collapse = ", "), "\n")
            cat("  Valid:", policy$valid_from, "to", policy$valid_until, "\n\n")
          }
        }

      } else if (subcommand == "add") {
        user_id <- parse_option("--user")
        role <- parse_option("--role")
        permissions_str <- parse_option("--permissions")

        if (is.null(user_id) || is.null(role) || is.null(permissions_str)) {
          cat(cli::col_red("Error: Please specify --user, --role, and --permissions\n"))
          quit(status = 1)
        }

        # Parse permissions (format: "resource1:action1,action2;resource2:action3")
        permissions <- list()
        for (resource_perm in strsplit(permissions_str, ";")[[1]]) {
          parts <- strsplit(resource_perm, ":")[[1]]
          if (length(parts) == 2) {
            resource <- parts[1]
            actions <- strsplit(parts[2], ",")[[1]]
            permissions[[resource]] <- actions
          }
        }

        add_user_policy(user_id, role, permissions)
        cat(cli::col_green("✓ Policy added for user:"), user_id, "\n")

      } else {
        cat(cli::col_red("Error: Unknown policy subcommand:"), subcommand, "\n")
        quit(status = 1)
      }

    } else {
      cat(cli::col_red("Error: Unknown command:"), command, "\n")
      cat("Use 'Rscript scripts/guardian.R --help' for usage information.\n")
      quit(status = 1)
    }

  }, error = function(e) {
    cat(cli::col_red("Error:"), e$message, "\n")
    log_security_event("error", "user", command, paste("Error:", e$message))
    quit(status = 1)
  })
}

# Run the main function
main()


