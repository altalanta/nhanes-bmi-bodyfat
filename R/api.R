# NHANES BMI Body Fat Analysis API
# REST API for programmatic access to analysis results

#' @title NHANES BMI Body Fat API
#' @description Provides REST API endpoints for accessing NHANES BMI vs Body Fat analysis results
#' @import plumber
#' @import jsonlite
#' @import readr
#' @import dplyr
#' @export

# Global variables for data paths
API_DATA_PATH <- "outputs/tables"
API_VERSION <- "1.0.0"

#* @apiTitle NHANES BMI Body Fat Analysis API
#* @apiDescription REST API for accessing NHANES BMI vs Body Fat analysis results
#* @apiVersion 1.0.0
#* @apiTag correlations BMI BodyFat Statistics Population

# Load data function
load_api_data <- function() {
  tryCatch({
    # Load correlation data
    corr_file <- file.path(API_DATA_PATH, "corr_bmi_bodyfat_overall_and_by_sex.csv")
    correlations <- read_csv(corr_file, show_col_types = FALSE) %>%
      mutate(across(where(is.numeric), ~round(., 4)))

    # Load body fat by BMI class data
    bodyfat_file <- file.path(API_DATA_PATH, "bodyfat_by_bmi_class_by_sex.csv")
    bodyfat_data <- read_csv(bodyfat_file, show_col_types = FALSE) %>%
      mutate(across(where(is.numeric), ~round(., 2)))

    # Load population counts
    pop_file <- file.path(API_DATA_PATH, "population_counts_by_group.csv")
    population <- read_csv(pop_file, show_col_types = FALSE)

    list(
      correlations = correlations,
      bodyfat = bodyfat_data,
      population = population,
      last_updated = Sys.time()
    )
  }, error = function(e) {
    stop(paste("Error loading API data:", e$message))
  })
}

# Global data cache
api_data <- NULL

#* Health check endpoint
#* @get /health
#* @tag System
#* @response 200 OK
#* @serializer json
function() {
  list(
    status = "healthy",
    timestamp = Sys.time(),
    version = API_VERSION,
    data_available = !is.null(api_data)
  )
}

#* API info endpoint
#* @get /api/info
#* @tag System
#* @response 200 OK
#* @serializer json
function() {
  list(
    name = "NHANES BMI Body Fat Analysis API",
    version = API_VERSION,
    description = "REST API for accessing NHANES BMI vs Body Fat analysis results",
    endpoints = list(
      correlations = "/api/correlations",
      bodyfat_by_bmi = "/api/bodyfat/bmi/{bmi_cat}",
      bodyfat_by_sex = "/api/bodyfat/sex/{sex}",
      population = "/api/population",
      statistics = "/api/statistics"
    ),
    last_updated = if (!is.null(api_data)) api_data$last_updated else NULL
  )
}

#* Get BMI-Body Fat correlations
#* @get /api/correlations
#* @tag correlations
#* @param group Filter by group (Overall, Male, Female). If not provided, returns all groups.
#* @response 200 OK
#* @serializer json
function(group = NULL) {
  if (is.null(api_data)) {
    api_data <<- load_api_data()
  }

  data <- api_data$correlations

  if (!is.null(group)) {
    data <- data %>% filter(group == !!group)
  }

  if (nrow(data) == 0) {
    return(list(error = "No data found for specified parameters"))
  }

  list(
    data = data,
    count = nrow(data),
    query = list(group = group)
  )
}

#* Get body fat statistics by BMI category
#* @get /api/bodyfat/bmi/<bmi_cat>
#* @tag bodyfat
#* @param bmi_cat BMI category (Underweight, Normal, Overweight, Obesity I, Obesity II, Obesity III)
#* @param sex Filter by sex (Male, Female). If not provided, returns both sexes.
#* @response 200 OK
#* @serializer json
function(bmi_cat, sex = NULL) {
  if (is.null(api_data)) {
    api_data <<- load_api_data()
  }

  data <- api_data$bodyfat %>% filter(bmi_cat == bmi_cat)

  if (!is.null(sex)) {
    data <- data %>% filter(sex == sex)
  }

  if (nrow(data) == 0) {
    return(list(error = "No data found for specified BMI category"))
  }

  list(
    bmi_category = bmi_cat,
    data = data,
    count = nrow(data),
    query = list(sex = sex)
  )
}

#* Get body fat statistics by sex
#* @get /api/bodyfat/sex/<sex>
#* @tag bodyfat
#* @param sex Sex (Male, Female)
#* @param bmi_cat Filter by BMI category. If not provided, returns all categories.
#* @response 200 OK
#* @serializer json
function(sex, bmi_cat = NULL) {
  if (is.null(api_data)) {
    api_data <<- load_api_data()
  }

  data <- api_data$bodyfat %>% filter(sex == sex)

  if (!is.null(bmi_cat)) {
    data <- data %>% filter(bmi_cat == bmi_cat)
  }

  if (nrow(data) == 0) {
    return(list(error = "No data found for specified sex"))
  }

  list(
    sex = sex,
    data = data,
    count = nrow(data),
    query = list(bmi_category = bmi_cat)
  )
}

#* Get population counts by group
#* @get /api/population
#* @tag population
#* @param bmi_cat Filter by BMI category. If not provided, returns all categories.
#* @param sex Filter by sex (Male, Female). If not provided, returns both sexes.
#* @response 200 OK
#* @serializer json
function(bmi_cat = NULL, sex = NULL) {
  if (is.null(api_data)) {
    api_data <<- load_api_data()
  }

  data <- api_data$population

  if (!is.null(bmi_cat)) {
    data <- data %>% filter(bmi_cat == bmi_cat)
  }

  if (!is.null(sex)) {
    data <- data %>% filter(sex == sex)
  }

  if (nrow(data) == 0) {
    return(list(error = "No population data found for specified parameters"))
  }

  list(
    data = data,
    count = nrow(data),
    query = list(bmi_category = bmi_cat, sex = sex)
  )
}

#* Get summary statistics
#* @get /api/statistics
#* @tag statistics
#* @response 200 OK
#* @serializer json
function() {
  if (is.null(api_data)) {
    api_data <<- load_api_data()
  }

  # Calculate summary statistics
  corr_data <- api_data$correlations
  bodyfat_data <- api_data$bodyfat
  pop_data <- api_data$population

  list(
    correlations = list(
      overall = corr_data %>% filter(group == "Overall") %>% select(correlation, ci_lower, ci_upper),
      by_sex = corr_data %>% filter(group != "Overall") %>% select(group, correlation, ci_lower, ci_upper)
    ),
    population = list(
      total_sample = sum(pop_data$pop_total, na.rm = TRUE),
      by_bmi_category = pop_data %>% group_by(bmi_cat) %>% summarise(total = sum(pop_total, na.rm = TRUE)) %>% ungroup(),
      by_sex = pop_data %>% group_by(sex) %>% summarise(total = sum(pop_total, na.rm = TRUE)) %>% ungroup()
    ),
    data_summary = list(
      correlation_records = nrow(corr_data),
      bodyfat_records = nrow(bodyfat_data),
      population_records = nrow(pop_data),
      last_updated = api_data$last_updated
    )
  )
}

#* @plumber
function(pr) {
  pr %>%
    pr_set_api_spec(function(spec) {
      spec$info$title <- "NHANES BMI Body Fat Analysis API"
      spec$info$description <- "REST API for accessing NHANES BMI vs Body Fat analysis results"
      spec$info$version <- API_VERSION
      spec$info$license <- list(name = "MIT")
      spec$info$contact <- list(name = "NHANES BMI Body Fat Analysis Team")
      spec
    })
}

