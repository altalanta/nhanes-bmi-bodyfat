# Helper functions for the pipeline
fetch_nhanes_data <- function(config) {
  source("scripts/fetch_nhanes.R")
  return(file.path(config$data$raw_dir, c(
    config$nhanes$demo_file,
    config$nhanes$bmx_file,
    config$nhanes$dxx_file,
    config$nhanes$dxxag_file
  )))
}

validate_nhanes_datasets <- function(datasets) {
  results <- furrr::future_map(datasets, function(dataset) {
    # Validate each dataset
    validate_nhanes_data(dataset, "dataset", required_cols = c("SEQN"))
  })
  return(results)
}

identify_bodyfat_variable <- function(dxx_data) {
  # Logic to identify body fat variable (same as current script)
  dxx_labels <- attr(dxx_data, "var.labels")
  fat_patterns <- c("percent fat", "%fat", "total % fat", "% fat")
  bodyfat_vars <- c()

  for (pattern in fat_patterns) {
    matches <- grep(pattern, dxx_labels, ignore.case = TRUE)
    if (length(matches) > 0) {
      bodyfat_vars <- c(bodyfat_vars, names(dxx_data)[matches])
    }
  }

  common_names <- c("DXDTOFAT", "DXDTOPF", "DXDPFAT")
  for (name in common_names) {
    if (name %in% names(dxx_data)) {
      bodyfat_vars <- c(bodyfat_vars, name)
    }
  }

  return(bodyfat_vars[1]) # Return first match
}

identify_android_fat <- function(dxxag_data) {
  if (nrow(dxxag_data) == 0) return(NULL)

  dxxag_labels <- attr(dxxag_data, "var.labels")
  android_matches <- grep("android.*fat", dxxag_labels, ignore.case = TRUE)
  if (length(android_matches) > 0) {
    return(names(dxxag_data)[android_matches[1]])
  }
  return(NULL)
}

identify_gynoid_fat <- function(dxxag_data) {
  if (nrow(dxxag_data) == 0) return(NULL)

  dxxag_labels <- attr(dxxag_data, "var.labels")
  gynoid_matches <- grep("gynoid.*fat", dxxag_labels, ignore.case = TRUE)
  if (length(gynoid_matches) > 0) {
    return(names(dxxag_data)[gynoid_matches[1]])
  }
  return(NULL)
}

merge_nhanes_datasets <- function(demo, bmx, dxx, dxxag, bodyfat_var, android_var, gynoid_var) {
  # Merge datasets (same logic as current script)
  nhanes <- demo %>%
    select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, WTMEC2YR, SDMVSTRA, SDMVPSU) %>%
    left_join(bmx %>% select(SEQN, BMXBMI), by = "SEQN") %>%
    left_join(dxx %>% select_at(c("SEQN", bodyfat_var)), by = "SEQN")

  if (!is.null(android_var) && !is.null(gynoid_var)) {
    nhanes <- nhanes %>%
      left_join(dxxag %>% select_at(c("SEQN", android_var, gynoid_var)), by = "SEQN")
  }

  return(nhanes)
}

clean_analytic_dataset <- function(data, config) {
  # Apply inclusion criteria (same as current script)
  nhanes_adults <- data %>%
    filter(RIDAGEYR >= config$analysis$age_range[1] &
           RIDAGEYR <= config$analysis$age_range[2])

  nhanes_complete <- nhanes_adults %>%
    filter(!is.na(BMXBMI) & !is.na(bodyfat_pct) &
           !is.na(WTMEC2YR) & !is.na(SDMVSTRA) & !is.na(SDMVPSU))

  # Create BMI categories
  nhanes_complete <- nhanes_complete %>%
    mutate(
      bmi_cat = case_when(
        BMXBMI < 18.5 ~ "Underweight",
        BMXBMI >= 18.5 & BMXBMI < 25 ~ "Normal",
        BMXBMI >= 25 & BMXBMI < 30 ~ "Overweight",
        BMXBMI >= 30 & BMXBMI < 35 ~ "Obesity I",
        BMXBMI >= 35 & BMXBMI < 40 ~ "Obesity II",
        BMXBMI >= 40 ~ "Obesity III"
      ),
      bmi_cat = factor(bmi_cat, levels = c("Underweight", "Normal", "Overweight",
                                           "Obesity I", "Obesity II", "Obesity III")),
      sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female"))
    )

  # Rename body fat variable
  names(nhanes_complete)[names(nhanes_complete) == bodyfat_var] <- "bodyfat_pct"

  return(nhanes_complete)
}

create_survey_design <- function(data, config) {
  validate_survey_design(
    data,
    config$analysis$survey_weights_col,
    config$analysis$strata_col,
    config$analysis$psu_col
  )

  svy_design <- svydesign(
    ids = as.formula(paste("~", config$analysis$psu_col)),
    strata = as.formula(paste("~", config$analysis$strata_col)),
    weights = as.formula(paste("~", config$analysis$survey_weights_col)),
    nest = TRUE,
    data = data
  )

  return(svy_design)
}

compute_correlations_parallel <- function(design) {
  # Compute correlations in parallel by sex
  future_map(c("Overall", "Male", "Female"), function(group) {
    if (group == "Overall") {
      design_subset <- design
    } else if (group == "Male") {
      design_subset <- subset(design, sex == "Male")
    } else {
      design_subset <- subset(design, sex == "Female")
    }

    corr_data <- svyvar(~BMXBMI + bodyfat_pct, design_subset)
    correlation <- corr_data[1,2] / sqrt(corr_data[1,1] * corr_data[2,2])

    # Standard error using delta method
    corr_se <- sqrt((1 - correlation^2)^2 / (4 * correlation^2) *
                    (corr_data[1,1]/corr_data[1,2]^2 + corr_data[2,2]/corr_data[1,2]^2 -
                     2/(corr_data[1,1] * corr_data[2,2])))

    data.frame(
      group = group,
      correlation = correlation,
      std_error = corr_se,
      ci_lower = correlation - 1.96 * corr_se,
      ci_upper = correlation + 1.96 * corr_se
    )
  }) %>% bind_rows()
}

compute_bmi_class_stats_parallel <- function(design, data) {
  # Compute stats by BMI class and sex in parallel
  bmi_sex_grid <- expand.grid(
    bmi_cat = levels(data$bmi_cat),
    sex = levels(data$sex),
    stringsAsFactors = FALSE
  )

  future_pmap(bmi_sex_grid, function(bmi_cat, sex) {
    subset_design <- subset(design, bmi_cat == bmi_cat & sex == sex)

    if (nrow(subset_design$variables) > 0) {
      n_unweighted <- nrow(subset_design$variables)
      pop_total <- sum(weights(subset_design))

      mean_bf <- svymean(~bodyfat_pct, subset_design)
      mean_est <- as.numeric(mean_bf)
      mean_se <- as.numeric(SE(mean_bf))
      mean_ci <- confint(mean_bf)

      q05 <- as.numeric(svyquantile(~bodyfat_pct, subset_design, quantiles = 0.05)[[1]])
      q50 <- as.numeric(svyquantile(~bodyfat_pct, subset_design, quantiles = 0.50)[[1]])
      q95 <- as.numeric(svyquantile(~bodyfat_pct, subset_design, quantiles = 0.95)[[1]])

      data.frame(
        bmi_cat = bmi_cat,
        sex = sex,
        n_unweighted = n_unweighted,
        pop_total = pop_total,
        mean_bodyfat = mean_est,
        mean_se = mean_se,
        mean_ci_lower = as.numeric(mean_ci[1]),
        mean_ci_upper = as.numeric(mean_ci[2]),
        q05 = q05,
        q50 = q50,
        q95 = q95
      )
    } else {
      data.frame(
        bmi_cat = bmi_cat,
        sex = sex,
        n_unweighted = 0,
        pop_total = 0,
        mean_bodyfat = NA,
        mean_se = NA,
        mean_ci_lower = NA,
        mean_ci_upper = NA,
        q05 = NA,
        q50 = NA,
        q95 = NA
      )
    }
  }) %>% bind_rows()
}

assess_linearity <- function(design) {
  linear_model <- svyglm(bodyfat_pct ~ BMXBMI, design = design)
  quad_model <- svyglm(bodyfat_pct ~ BMXBMI + I(BMXBMI^2), design = design)

  quad_p <- summary(quad_model)$coefficients["I(BMXBMI^2)", "Pr(>|t|)"]

  list(
    linear_aic = AIC(linear_model),
    quad_aic = AIC(quad_model),
    quad_p_value = quad_p,
    significant_nonlinearity = quad_p < 0.05
  )
}

create_bmi_bodyfat_plot <- function(data, output_file) {
  plot_data <- data %>%
    mutate(plot_weight = WTMEC2YR / sum(WTMEC2YR) * nrow(data))

  p <- ggplot(plot_data, aes(x = BMXBMI, y = bodyfat_pct)) +
    geom_point(aes(size = plot_weight), alpha = 0.3) +
    geom_smooth(aes(weight = plot_weight), method = "loess", se = TRUE) +
    facet_wrap(~sex) +
    labs(
      title = "BMI vs Whole-Body % Body Fat by Sex",
      subtitle = "U.S. civilian non-institutionalized adults (20-59), NHANES 2017-2018",
      x = "Body Mass Index (kg/m²)",
      y = "Whole-Body % Body Fat (DXA)",
      size = "Survey Weight"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave(output_file, p, width = 10, height = 6, dpi = 300)
  return(output_file)
}

create_bmi_class_plot <- function(bmi_results, output_file) {
  p <- ggplot(bmi_results, aes(x = bmi_cat, y = mean_bodyfat, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean_ci_lower, ymax = mean_ci_upper),
                  position = position_dodge(width = 0.9), width = 0.25) +
    labs(
      title = "Mean Body Fat by BMI Class and Sex",
      x = "BMI Category",
      y = "Mean % Body Fat",
      fill = "Sex"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(output_file, p, width = 10, height = 6, dpi = 300)
  return(output_file)
}

run_sensitivity_analyses <- function(design, data) {
  # Placeholder for sensitivity analyses
  list(sensitivity_complete = TRUE)
}

run_advanced_statistics <- function(design, data) {
  # Placeholder for advanced statistics
  list(advanced_complete = TRUE)
}

export_all_results <- function(corr, bmi, linear, sens, adv, corr_file, bmi_file, methods_file) {
  # Export results to CSV files
  write.csv(corr, corr_file, row.names = FALSE)
  write.csv(bmi, bmi_file, row.names = FALSE)

  # Create methods documentation
  methods_text <- paste0(
    "NHANES 2017-2018 BMI vs % Body Fat Analysis Methods (Pipeline Version)\n",
    "========================================================================\n\n",
    "Pipeline: targets-based with parallel processing\n",
    "Workers: ", availableCores() - 1, "\n",
    "Analysis date: ", Sys.Date(), "\n"
  )

  writeLines(methods_text, methods_file)

  return(methods_file)
}

render_quarto_report <- function(results_file, output_file) {
  # Render Quarto report
  quarto::quarto_render("report.qmd", output_file = output_file)
  return(output_file)
}
