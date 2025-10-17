# Performance Optimization and Caching for NHANES Analysis

#' Create a cache directory and initialize caching system
#'
#' Sets up a persistent cache for expensive operations like data loading and processing.
#'
#' @param cache_dir Directory path for cache storage
#' @return None
#' @export
initialize_cache <- function(cache_dir = "cache") {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create cache metadata file
  cache_meta_file <- file.path(cache_dir, "cache_metadata.rds")
  if (!file.exists(cache_meta_file)) {
    cache_metadata <- list(
      created = Sys.time(),
      version = "1.0",
      entries = list()
    )
    saveRDS(cache_metadata, cache_meta_file)
  }
}

#' Generate cache key for given parameters
#'
#' Creates a unique cache key based on function parameters and data characteristics.
#'
#' @param func_name Name of the function being cached
#' @param params List of parameters that affect the result
#' @return Cache key string
#' @export
generate_cache_key <- function(func_name, params) {
  # Create a deterministic hash of the parameters
  param_str <- paste(names(params), unlist(params), sep = "=", collapse = "|")
  key_data <- paste(func_name, param_str, sep = "|")
  digest::digest(key_data, algo = "md5")
}

#' Check if cached result exists and is valid
#'
#' Verifies if a cached result exists and hasn't expired.
#'
#' @param cache_key Cache key to check
#' @param cache_dir Cache directory
#' @param max_age_hours Maximum age of cached result in hours (default: 24)
#' @return List with exists and data if found
#' @export
check_cache <- function(cache_key, cache_dir = "cache", max_age_hours = 24) {
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

  if (!file.exists(cache_file)) {
    return(list(exists = FALSE, data = NULL))
  }

  # Check if cache has expired
  file_info <- file.info(cache_file)
  age_hours <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))

  if (age_hours > max_age_hours) {
    # Remove expired cache file
    file.remove(cache_file)
    return(list(exists = FALSE, data = NULL))
  }

  # Load cached data
  tryCatch({
    cached_data <- readRDS(cache_file)
    list(exists = TRUE, data = cached_data$result)
  }, error = function(e) {
    # Remove corrupted cache file
    file.remove(cache_file)
    list(exists = FALSE, data = NULL)
  })
}

#' Save result to cache
#'
#' Stores a result in the cache with metadata.
#'
#' @param cache_key Cache key
#' @param result Result to cache
#' @param cache_dir Cache directory
#' @return None
#' @export
save_to_cache <- function(cache_key, result, cache_dir = "cache") {
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

  # Prepare cache data with metadata
  cache_data <- list(
    result = result,
    timestamp = Sys.time(),
    cache_key = cache_key
  )

  # Save to file
  saveRDS(cache_data, cache_file)

  # Update cache metadata
  cache_meta_file <- file.path(cache_dir, "cache_metadata.rds")
  if (file.exists(cache_meta_file)) {
    cache_metadata <- readRDS(cache_meta_file)
    cache_metadata$entries[[cache_key]] <- list(
      created = Sys.time(),
      size = file.size(cache_file)
    )
    saveRDS(cache_metadata, cache_meta_file)
  }
}

#' Cached version of data loading function
#'
#' Loads NHANES datasets with intelligent caching based on file modification times.
#'
#' @param config Configuration list
#' @param cache_dir Cache directory (default: "cache")
#' @param force_refresh Force refresh of cache (default: FALSE)
#' @return List of loaded datasets
#' @export
cached_load_datasets <- function(config, cache_dir = "cache", force_refresh = FALSE) {
  initialize_cache(cache_dir)

  # Check if any source files have been modified
  source_files <- c(
    config$nhanes_demo_path,
    config$nhanes_bmx_path,
    config$nhanes_dxx_path,
    config$nhanes_dxxag_path
  )

  # Generate cache key based on file modification times and config
  file_mtimes <- file.info(source_files)$mtime
  cache_params <- list(
    files_modified = as.numeric(file_mtimes),
    config_version = "1.0"  # Increment when config structure changes
  )

  cache_key <- generate_cache_key("load_datasets", cache_params)

  # Check cache unless force refresh
  if (!force_refresh) {
    cache_result <- check_cache(cache_key, cache_dir)
    if (cache_result$exists) {
      safe_log("Using cached datasets", "INFO")
      return(cache_result$data)
    }
  }

  safe_log("Loading datasets from source (cache miss or refresh)", "INFO")

  # Load fresh data
  datasets <- load_nhanes_datasets(config)

  # Cache the result
  save_to_cache(cache_key, datasets, cache_dir)

  safe_log(paste("Cached datasets for future use"), "INFO")
  return(datasets)
}

#' Cached version of correlation computation
#'
#' Computes survey-weighted correlations with caching.
#'
#' @param svy_design Survey design object
#' @param cache_dir Cache directory (default: "cache")
#' @return List of correlation results
#' @export
cached_compute_correlations <- function(svy_design, cache_dir = "cache") {
  initialize_cache(cache_dir)

  # Generate cache key based on survey design characteristics
  design_info <- list(
    n_obs = nrow(svy_design$variables),
    strata_count = length(unique(svy_design$variables$strata)),
    psu_count = length(unique(svy_design$variables$psu)),
    variables = names(svy_design$variables)
  )

  cache_key <- generate_cache_key("compute_correlations", design_info)

  # Check cache
  cache_result <- check_cache(cache_key, cache_dir)
  if (cache_result$exists) {
    safe_log("Using cached correlation results", "INFO")
    return(cache_result$data)
  }

  safe_log("Computing correlations (cache miss)", "INFO")

  # Compute fresh results
  correlations <- compute_correlations(svy_design)

  # Cache the result
  save_to_cache(cache_key, correlations, cache_dir)

  safe_log("Cached correlation results for future use", "INFO")
  return(correlations)
}

#' Parallel processing wrapper for data validation
#'
#' Runs data quality checks in parallel where appropriate.
#'
#' @param datasets List of datasets to validate
#' @param config Configuration list
#' @param n_cores Number of cores to use (default: 2)
#' @return Validation results
#' @export
parallel_validate_datasets <- function(datasets, config, n_cores = 2) {
  safe_log(paste("Running parallel data validation on", n_cores, "cores"), "INFO")

  # Check if parallel package is available
  if (!requireNamespace("parallel", quietly = TRUE)) {
    safe_log("Parallel package not available, falling back to sequential processing", "WARNING")
    return(run_data_validation(datasets, config))
  }

  # Run validation in parallel for each dataset
  dataset_names <- names(datasets)

  # Create cluster for parallel processing
  cl <- parallel::makeCluster(min(n_cores, length(datasets)))

  # Export required functions and variables to workers
  parallel::clusterExport(cl, c("assess_data_quality", "safe_log", "config"),
                         envir = environment())

  # Run validation in parallel
  validation_results <- parallel::parLapply(cl, dataset_names, function(dataset_name) {
    assess_data_quality(datasets[[dataset_name]], dataset_name, config)
  })

  # Stop cluster
  parallel::stopCluster(cl)

  # Name the results
  names(validation_results) <- dataset_names

  # Run consistency validation sequentially
  consistency_report <- validate_data_consistency(datasets)

  safe_log("Parallel data validation completed", "INFO")

  return(list(
    quality_reports = validation_results,
    consistency_report = consistency_report,
    overall_status = "COMPLETED",
    timestamp = Sys.time(),
    parallel_cores = n_cores
  ))
}

#' Performance monitoring decorator
#'
#' Wraps functions with performance monitoring and logging.
#'
#' @param func Function to monitor
#' @param func_name Name of the function for logging
#' @return Wrapped function with performance monitoring
#' @export
with_performance_monitor <- function(func, func_name) {
  function(...) {
    start_time <- Sys.time()
    safe_log(paste("Starting", func_name), "INFO")

    tryCatch({
      result <- func(...)

      end_time <- Sys.time()
      duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

      safe_log(paste(func_name, "completed in", round(duration, 2), "seconds"), "INFO")

      return(result)
    }, error = function(e) {
      end_time <- Sys.time()
      duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

      safe_log(paste(func_name, "failed after", round(duration, 2), "seconds:", e$message), "ERROR")
      stop(e)
    })
  }
}

#' Clean cache directory
#'
#' Removes old cache files to free up disk space.
#'
#' @param cache_dir Cache directory
#' @param max_age_hours Maximum age of files to keep (default: 168 = 1 week)
#' @return Number of files removed
#' @export
clean_cache <- function(cache_dir = "cache", max_age_hours = 168) {
  initialize_cache(cache_dir)

  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  removed_count <- 0

  for (cache_file in cache_files) {
    file_info <- file.info(cache_file)
    age_hours <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))

    if (age_hours > max_age_hours) {
      file.remove(cache_file)
      removed_count <- removed_count + 1
    }
  }

  safe_log(paste("Cleaned cache: removed", removed_count, "old files"), "INFO")
  return(removed_count)
}

#' Get cache statistics
#'
#' Returns information about cache usage and performance.
#'
#' @param cache_dir Cache directory
#' @return List with cache statistics
#' @export
get_cache_stats <- function(cache_dir = "cache") {
  if (!dir.exists(cache_dir)) {
    return(list(
      cache_exists = FALSE,
      total_files = 0,
      total_size = 0,
      oldest_file = NULL,
      newest_file = NULL
    ))
  }

  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(cache_files) == 0) {
    return(list(
      cache_exists = TRUE,
      total_files = 0,
      total_size = 0,
      oldest_file = NULL,
      newest_file = NULL
    ))
  }

  file_info <- file.info(cache_files)
  total_size <- sum(file_info$size)

  list(
    cache_exists = TRUE,
    total_files = length(cache_files),
    total_size = total_size,
    total_size_mb = total_size / (1024 * 1024),
    oldest_file = min(file_info$mtime),
    newest_file = max(file_info$mtime),
    average_file_size = mean(file_info$size)
  )
}

#' Performance benchmarking class
#'
#' @param name Benchmark name
#' @param start_time Start time
#' @param end_time End time
#' @param memory_used Memory usage in MB
#' @param cpu_time CPU time in seconds
#' @param metadata Additional metadata
#' @export
PerformanceBenchmark <- function(name, start_time, end_time, memory_used, cpu_time, metadata = NULL) {
  benchmark <- list(
    name = name,
    start_time = start_time,
    end_time = end_time,
    duration = as.numeric(difftime(end_time, start_time, units = "secs")),
    memory_used = memory_used,
    cpu_time = cpu_time,
    metadata = metadata,
    timestamp = Sys.time()
  )
  class(benchmark) <- "PerformanceBenchmark"
  return(benchmark)
}

#' Global performance tracking environment
performance_tracker <- new.env()

#' Initialize performance tracking
#'
#' @param session_name Name of the analysis session
#' @param output_dir Directory for performance logs
#' @export
initialize_performance_tracking <- function(session_name = "nhanes_analysis",
                                          output_dir = "outputs/logs") {

  performance_tracker$session_name <- session_name
  performance_tracker$benchmarks <- list()
  performance_tracker$output_dir <- output_dir
  performance_tracker$start_time <- Sys.time()

  # Create performance log directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Initialize performance log file
  performance_tracker$log_file <- file.path(output_dir, "performance_log.txt")

  # Log session start
  log_performance("=== Performance tracking session started ===")
  log_performance(paste("Session:", session_name))
  log_performance(paste("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

  # Record initial memory state
  initial_memory <- get_memory_usage()
  log_performance(paste("Initial memory usage:", round(initial_memory, 2), "MB"))

  return(invisible(TRUE))
}

#' Get current memory usage
#'
#' @return Memory usage in MB
#' @export
get_memory_usage <- function() {
  tryCatch({
    # Try different methods to get memory usage
    if (.Platform$OS.type == "windows") {
      # Windows method
      mem_info <- system("wmic OS get FreePhysicalMemory /Value", intern = TRUE)
      total_mem_mb <- as.numeric(gsub("FreePhysicalMemory=", "", mem_info[grepl("FreePhysicalMemory", mem_info)])) / 1024
      return(total_mem_mb)
    } else {
      # Unix-like systems (Linux, macOS)
      mem_info <- system("free -m", intern = TRUE)
      if (length(mem_info) >= 2) {
        mem_values <- as.numeric(strsplit(gsub("\\s+", " ", mem_info[2]), " ")[[1]])
        total_mem_mb <- mem_values[2]
        used_mem_mb <- total_mem_mb - mem_values[4]
        return(used_mem_mb)
      }
    }

    # Fallback: R memory usage
    gc()
    mem_usage <- sum(gc()[, 2])
    return(mem_usage / 1024 / 1024)  # Convert to MB

  }, error = function(e) {
    # Fallback if system commands fail
    gc()
    mem_usage <- sum(gc()[, 2])
    return(mem_usage / 1024 / 1024)
  })
}

#' Get CPU time
#'
#' @return CPU time in seconds
#' @export
get_cpu_time <- function() {
  tryCatch({
    proc_time <- proc.time()
    return(proc_time[1] + proc_time[2])  # user + system time
  }, error = function(e) {
    return(0)
  })
}

#' Performance benchmarking wrapper
#'
#' @param expr Expression to benchmark
#' @param benchmark_name Name of the benchmark
#' @param metadata Additional metadata to record
#' @return Result of the expression
#' @export
benchmark_operation <- function(expr, benchmark_name, metadata = NULL) {
  start_time <- Sys.time()
  start_cpu <- get_cpu_time()
  start_memory <- get_memory_usage()

  tryCatch({
    result <- eval(expr)

    end_time <- Sys.time()
    end_cpu <- get_cpu_time()
    end_memory <- get_memory_usage()

    # Calculate metrics
    duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    cpu_time <- end_cpu - start_cpu
    memory_used <- end_memory - start_memory

    # Create benchmark object
    benchmark <- PerformanceBenchmark(
      name = benchmark_name,
      start_time = start_time,
      end_time = end_time,
      memory_used = memory_used,
      cpu_time = cpu_time,
      metadata = metadata
    )

    # Store benchmark
    performance_tracker$benchmarks <- c(
      performance_tracker$benchmarks,
      list(benchmark)
    )

    # Log benchmark
    log_performance(sprintf(
      "[BENCHMARK] %s: %.2fs, %.2fMB, %.2fs CPU",
      benchmark_name, duration, memory_used, cpu_time
    ))

    return(result)

  }, error = function(e) {
    end_time <- Sys.time()
    end_cpu <- get_cpu_time()
    end_memory <- get_memory_usage()

    duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    cpu_time <- end_cpu - start_cpu
    memory_used <- end_memory - start_memory

    # Log failed benchmark
    log_performance(sprintf(
      "[BENCHMARK_FAILED] %s: %.2fs, %.2fMB, %.2fs CPU - ERROR: %s",
      benchmark_name, duration, memory_used, cpu_time, e$message
    ))

    stop(e)
  })
}

#' Log performance message
#'
#' @param message Message to log
#' @param level Log level (INFO, WARNING, ERROR)
#' @export
log_performance <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (exists("log_file", envir = performance_tracker)) {
    cat(sprintf("[%s] %s: %s\n", level, timestamp, message),
        file = performance_tracker$log_file, append = TRUE)
  }

  # Also print to console if not in quiet mode
  if (getOption("verbose", TRUE)) {
    cat(sprintf("[%s] %s\n", level, message))
  }
}

#' Generate performance report
#'
#' @param output_file Output file for the report
#' @return Performance summary statistics
#' @export
generate_performance_report <- function(output_file = NULL) {
  if (is.null(output_file)) {
    output_file <- file.path(performance_tracker$output_dir, "performance_report.html")
  }

  benchmarks <- performance_tracker$benchmarks
  n_benchmarks <- length(benchmarks)

  if (n_benchmarks == 0) {
    log_performance("No benchmarks recorded")
    return(NULL)
  }

  # Calculate summary statistics
  durations <- sapply(benchmarks, function(b) b$duration)
  memory_usage <- sapply(benchmarks, function(b) b$memory_used)
  cpu_times <- sapply(benchmarks, function(b) b$cpu_time)

  summary_stats <- list(
    total_benchmarks = n_benchmarks,
    total_duration = sum(durations),
    mean_duration = mean(durations),
    median_duration = median(durations),
    min_duration = min(durations),
    max_duration = max(durations),
    total_memory = sum(memory_usage),
    mean_memory = mean(memory_usage),
    max_memory = max(memory_usage),
    total_cpu = sum(cpu_times),
    mean_cpu = mean(cpu_times)
  )

  # Create HTML report
  report_content <- paste0("
<!DOCTYPE html>
<html>
<head>
    <title>NHANES Analysis Performance Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .section { margin-bottom: 30px; }
        .metric { background-color: #f5f5f5; padding: 10px; margin: 5px 0; }
        .benchmark { background-color: #e8f4fd; padding: 10px; margin: 5px 0; border-left: 4px solid #2196F3; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        .slow { background-color: #ffebee; }
        .memory-intensive { background-color: #fff3e0; }
    </style>
</head>
<body>
    <h1>NHANES Analysis Performance Report</h1>
    <p><strong>Session:</strong> ", performance_tracker$session_name, "</p>
    <p><strong>Generated:</strong> ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>

    <div class='section'>
        <h2>Summary Statistics</h2>
        <div class='metric'>
            <p><strong>Total Operations:</strong> ", summary_stats$total_benchmarks, "</p>
            <p><strong>Total Duration:</strong> ", round(summary_stats$total_duration, 2), " seconds</p>
            <p><strong>Mean Duration:</strong> ", round(summary_stats$mean_duration, 2), " seconds</p>
            <p><strong>Median Duration:</strong> ", round(summary_stats$median_duration, 2), " seconds</p>
            <p><strong>Total Memory Used:</strong> ", round(summary_stats$total_memory, 2), " MB</p>
            <p><strong>Peak Memory Usage:</strong> ", round(summary_stats$max_memory, 2), " MB</p>
        </div>
    </div>

    <div class='section'>
        <h2>Benchmark Details</h2>
")

  # Add benchmark details
  for (i in 1:n_benchmarks) {
    benchmark <- benchmarks[[i]]

    # Determine performance classes
    duration_class <- if (benchmark$duration > 60) "slow" else ""
    memory_class <- if (benchmark$memory_used > 100) "memory-intensive" else ""

    report_content <- paste0(report_content, "
        <div class='benchmark ", duration_class, " ", memory_class, "'>
            <h4>", benchmark$name, "</h4>
            <p><strong>Duration:</strong> ", round(benchmark$duration, 2), " seconds</p>
            <p><strong>Memory Used:</strong> ", round(benchmark$memory_used, 2), " MB</p>
            <p><strong>CPU Time:</strong> ", round(benchmark$cpu_time, 2), " seconds</p>
            <p><strong>Start Time:</strong> ", format(benchmark$start_time, "%H:%M:%S"), "</p>
")

    if (!is.null(benchmark$metadata)) {
      report_content <- paste0(report_content, "
            <p><strong>Metadata:</strong> ", paste(benchmark$metadata, collapse = ", "), "</p>
")
    }

    report_content <- paste0(report_content, "
        </div>
")
  }

  # Add performance recommendations
  report_content <- paste0(report_content, "
    <div class='section'>
        <h2>Performance Recommendations</h2>
")

  if (summary_stats$mean_duration > 30) {
    report_content <- paste0(report_content, "
        <div class='metric' style='background-color: #fff3cd; border-left: 4px solid #ffc107;'>
            <p><strong>⚠️ High Average Duration:</strong> Consider optimizing slow operations or parallelizing computations.</p>
        </div>
")
  }

  if (summary_stats$max_memory > 500) {
    report_content <- paste0(report_content, "
        <div class='metric' style='background-color: #f8d7da; border-left: 4px solid #dc3545;'>
            <p><strong>⚠️ High Memory Usage:</strong> Monitor memory-intensive operations and consider data chunking for large datasets.</p>
        </div>
")
  }

  if (summary_stats$total_benchmarks > 50) {
    report_content <- paste0(report_content, "
        <div class='metric' style='background-color: #d4edda; border-left: 4px solid #28a745;'>
            <p><strong>✅ Good Modularization:</strong> Analysis is well-broken into manageable operations.</p>
        </div>
")
  }

  report_content <- paste0(report_content, "
    </div>
")

  # Close HTML
  report_content <- paste0(report_content, "
</body>
</html>
")

  # Write report
  writeLines(report_content, output_file)

  log_performance(paste("Performance report generated:", output_file))
  log_performance(sprintf("Total benchmarks: %d, Total time: %.2fs", n_benchmarks, summary_stats$total_duration))

  return(summary_stats)
}

#' Performance-optimized version of complete analysis
#'
#' Runs the complete analysis pipeline with caching and performance optimizations.
#'
#' @param config_file Configuration file path
#' @param use_cache Use caching for expensive operations (default: TRUE)
#' @param force_refresh Force refresh of cached results (default: FALSE)
#' @param n_cores Number of cores for parallel processing (default: 2)
#' @param track_performance Enable detailed performance tracking (default: TRUE)
#' @return Complete analysis results
#' @export
run_optimized_analysis <- function(config_file = "config/config.yml",
                                 use_cache = TRUE,
                                 force_refresh = FALSE,
                                 n_cores = 2,
                                 track_performance = TRUE) {

  # Initialize performance tracking if requested
  if (track_performance) {
    initialize_performance_tracking()
  }

  safe_log("Starting optimized NHANES analysis", "INFO")

  # Load configuration
  config <- safe_load_config(config_file)
  ensure_output_dirs(config)

  # Use caching for data loading if enabled
  if (use_cache) {
    datasets <- benchmark_operation(
      cached_load_datasets(config, force_refresh = force_refresh),
      "data_loading",
      metadata = list(cached = TRUE, force_refresh = force_refresh)
    )
  } else {
    datasets <- benchmark_operation(
      load_nhanes_datasets(config),
      "data_loading",
      metadata = list(cached = FALSE)
    )
  }

  # Create analytic dataset
  analytic_data <- benchmark_operation(
    create_analytic_dataset(datasets, config),
    "analytic_dataset_creation",
    metadata = list(n_datasets = length(datasets), sample_size = nrow(datasets$demo))
  )

  # Create survey design
  svy_design <- benchmark_operation(
    create_survey_design(analytic_data, config),
    "survey_design_creation",
    metadata = list(n_observations = nrow(analytic_data))
  )

  # Use caching for correlation computation if enabled
  if (use_cache) {
    correlations <- benchmark_operation(
      cached_compute_correlations(svy_design),
      "correlation_computation",
      metadata = list(cached = TRUE)
    )
  } else {
    correlations <- benchmark_operation(
      compute_correlations(svy_design),
      "correlation_computation",
      metadata = list(cached = FALSE)
    )
  }

  # Run data validation with parallel processing
  validation_results <- benchmark_operation(
    parallel_validate_datasets(datasets, config, n_cores),
    "data_validation",
    metadata = list(parallel_cores = n_cores, n_datasets = length(datasets))
  )

  # Save results with performance monitoring
  results_file <- file.path(config$outputs$tables_dir, "nhanes_analysis_results.rds")
  analysis_results <- list(
    correlations = correlations,
    validation_results = validation_results,
    config = config,
    timestamp = Sys.time(),
    sample_size = nrow(analytic_data),
    performance_info = list(
      used_cache = use_cache,
      parallel_cores = n_cores,
      cache_stats = get_cache_stats(),
      performance_tracking = track_performance
    )
  )

  safe_save_data(analysis_results, results_file)

  # Generate performance report if tracking was enabled
  if (track_performance) {
    perf_report <- generate_performance_report()
    analysis_results$performance_report <- perf_report
  }

  safe_log("Optimized analysis completed successfully", "INFO")
  return(analysis_results)
}
