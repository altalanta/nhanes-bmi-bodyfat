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

#' Performance-optimized version of complete analysis
#'
#' Runs the complete analysis pipeline with caching and performance optimizations.
#'
#' @param config_file Configuration file path
#' @param use_cache Use caching for expensive operations (default: TRUE)
#' @param force_refresh Force refresh of cached results (default: FALSE)
#' @param n_cores Number of cores for parallel processing (default: 2)
#' @return Complete analysis results
#' @export
run_optimized_analysis <- function(config_file = "config/config.yml",
                                 use_cache = TRUE,
                                 force_refresh = FALSE,
                                 n_cores = 2) {

  safe_log("Starting optimized NHANES analysis", "INFO")

  # Load configuration
  config <- safe_load_config(config_file)
  ensure_output_dirs(config)

  # Use caching for data loading if enabled
  if (use_cache) {
    datasets <- cached_load_datasets(config, force_refresh = force_refresh)
  } else {
    datasets <- load_nhanes_datasets(config)
  }

  # Create analytic dataset
  analytic_data <- with_performance_monitor(
    create_analytic_dataset,
    "analytic dataset creation"
  )(datasets, config)

  # Create survey design
  svy_design <- with_performance_monitor(
    create_survey_design,
    "survey design creation"
  )(analytic_data, config)

  # Use caching for correlation computation if enabled
  if (use_cache) {
    correlations <- cached_compute_correlations(svy_design)
  } else {
    correlations <- with_performance_monitor(
      compute_correlations,
      "correlation computation"
    )(svy_design)
  }

  # Run data validation with parallel processing
  validation_results <- parallel_validate_datasets(datasets, config, n_cores)

  # Save results with performance monitoring
  results_file <- file.path(config$outputs_tables_path, "nhanes_analysis_results.rds")
  analysis_results <- list(
    correlations = correlations,
    validation_results = validation_results,
    config = config,
    timestamp = Sys.time(),
    sample_size = correlations$sample_size,
    performance_info = list(
      used_cache = use_cache,
      parallel_cores = n_cores,
      cache_stats = get_cache_stats()
    )
  )

  safe_save_data(analysis_results, results_file)

  safe_log("Optimized analysis completed successfully", "INFO")
  return(analysis_results)
}
