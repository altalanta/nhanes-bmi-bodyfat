# Set user library path
.libPaths(c('~/R_libs', .libPaths()))

# Load performance utilities first
source("R/performance.R")

# Initialize performance tracking for visualization creation
initialize_performance_tracking("nhanes_visualization")

# Install missing packages if needed with performance tracking
required_packages <- c("readr", "dplyr", "stringr", "ggplot2", "tidyr", "gridExtra", "forcats")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

package_installation <- benchmark_operation({
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse=", "), "\n")
    install.packages(missing_packages, repos='https://cran.rstudio.com/', lib='~/R_libs')
  }
  return(missing_packages)
}, "package_installation", metadata = list(missing_packages = missing_packages, n_missing = length(missing_packages)))

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
  library(ggplot2); library(tidyr); library(gridExtra)
  library(forcats)
})

repo <- Sys.getenv("REPO_DIR", unset = normalizePath("~/Documents/Coding_Projects/nhanes-bmi-bodyfat"))
tabdir <- file.path(repo, "outputs", "tables")
figdir <- file.path(repo, "outputs", "figures")
logs_dir <- file.path(repo, "outputs", "logs")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)
dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)

# Enhanced logging function for visualization
viz_log <- function(msg) {
  log_performance(msg, "INFO")
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", msg, "\n")
}

viz_log("Starting NHANES visualization creation with performance benchmarking...")

# Read data files with performance tracking
data_loading <- benchmark_operation({
  cat("Loading data files for visualization...\n")

  # Validate data files exist
  required_files <- c(
    "corr_bmi_bodyfat_overall_and_by_sex.csv",
    "bodyfat_by_bmi_class_by_sex.csv",
    "population_counts_by_group.csv"
  )

  missing_files <- c()
  for (file in required_files) {
    filepath <- file.path(tabdir, file)
    if (!file.exists(filepath)) {
      missing_files <- c(missing_files, file)
    }
  }

  if (length(missing_files) > 0) {
    stop(NhanesError(
      paste("Missing required data files:", paste(missing_files, collapse = ", ")),
      code = "MISSING_VIZ_DATA_FILES",
      details = list(missing_files = missing_files, required_files = required_files)
    ))
  }

  # Load data files
  corr <- readr::read_csv(file.path(tabdir,"corr_bmi_bodyfat_overall_and_by_sex.csv"), show_col_types = FALSE)
  bf   <- readr::read_csv(file.path(tabdir,"bodyfat_by_bmi_class_by_sex.csv"), show_col_types = FALSE)
  pop  <- readr::read_csv(file.path(tabdir,"population_counts_by_group.csv"), show_col_types = FALSE)

  return(list(corr = corr, bf = bf, pop = pop))
}, "data_loading", metadata = list(required_files = required_files))

corr <- data_loading$corr
bf <- data_loading$bf
pop <- data_loading$pop

# Check required columns and adapt to actual column names
viz_log(paste("Correlation data columns:", paste(names(corr), collapse=", ")))
viz_log(paste("Body fat data columns:", paste(names(bf), collapse=", ")))

# Data processing with performance tracking
data_processing <- benchmark_operation({
  viz_log("Processing data for visualization...")

  # Rename columns to match expected names
  corr <- corr %>%
    rename(r = correlation, lcl = ci_lower, ucl = ci_upper)

  bf <- bf %>%
    rename(BMIclass = bmi_cat) %>%
    mutate(
      BMIclass = case_when(
        BMIclass == "Underweight" ~ "Under",
        BMIclass == "Normal" ~ "Normal",
        BMIclass == "Overweight" ~ "Overweight",
        BMIclass == "Obesity I" ~ "Obesity I",
        BMIclass == "Obesity II" ~ "Obesity II",
        BMIclass == "Obesity III" ~ "Obesity III",
        TRUE ~ BMIclass
      ),
      BMIclass = factor(BMIclass, levels=c("Under","Normal","Overweight","Obesity I","Obesity II","Obesity III"))
    )

  # Validate required columns
  need_corr <- c("group","r","lcl","ucl")
  if (!all(need_corr %in% names(corr))) {
    stop(NhanesError(
      paste("Missing required correlation columns:", paste(setdiff(need_corr, names(corr)), collapse=", ")),
      code = "MISSING_CORRELATION_COLUMNS",
      details = list(available = names(corr), required = need_corr)
    ))
  }

  need_bf <- c("BMIclass","sex","mean_bodyfat","mean_ci_lower","mean_ci_upper","q05","q50","q95")
  if (!all(need_bf %in% names(bf))) {
    stop(NhanesError(
      paste("Missing required body fat columns:", paste(setdiff(need_bf, names(bf)), collapse=", ")),
      code = "MISSING_BODYFAT_COLUMNS",
      details = list(available = names(bf), required = need_bf)
    ))
  }

  return(list(corr = corr, bf = bf))
}, "data_processing", metadata = list(n_corr_rows = nrow(corr), n_bf_rows = nrow(bf)))

corr <- data_processing$corr
bf <- data_processing$bf

# Plot creation with performance tracking
plot_creation <- benchmark_operation({
  viz_log("Creating visualization plots...")

  # Panel A: correlations
  pA <- ggplot(corr, aes(x=r, y=fct_rev(factor(group, levels=c("Overall","Male","Female"))))) +
    geom_pointrange(aes(xmin=lcl, xmax=ucl), size=0.8) +
    geom_text(aes(label=sprintf("%.3f", r)), hjust=-0.2, size=3.5) +
    labs(x="Correlation (r) with 95% CI", y=NULL, title="BMI vs DXA % Body Fat") +
    xlim(0.85, 1.0) +
    theme_minimal(base_size=12) +
    theme(panel.grid.minor = element_blank())

  # Panel B: mean %BF + CI by class × sex
  pB <- ggplot(bf, aes(x=BMIclass, y=mean_bodyfat, color=sex)) +
    geom_pointrange(aes(ymin=mean_ci_lower, ymax=mean_ci_upper),
                    position=position_dodge(width=0.5), size=0.6) +
    geom_text(aes(label=sprintf("%.1f", mean_bodyfat)),
              position=position_dodge(width=0.5), vjust=-0.8, size=3) +
    labs(x=NULL, y="% Body Fat (mean ± 95% CI)", title="Mean %BF by BMI Class × Sex") +
    scale_color_manual(values=c("Male"="#2166ac", "Female"="#d73027"), name="Sex") +
    theme_minimal(base_size=12) +
    theme(axis.text.x=element_text(angle=20, hjust=1),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")

  # Panel C: distribution (5th–95th) + median
  pC <- ggplot(bf, aes(x=BMIclass, color=sex)) +
    geom_linerange(aes(ymin=q05, ymax=q95), position=position_dodge(width=0.5), size=1.2) +
    geom_point(aes(y=q50), position=position_dodge(width=0.5), size=2) +
    labs(x="BMI Class", y="% Body Fat (5th–95th; dot=median)", title="Distribution of %BF by BMI Class × Sex") +
    scale_color_manual(values=c("Male"="#2166ac", "Female"="#d73027"), name="Sex") +
    theme_minimal(base_size=12) +
    theme(axis.text.x=element_text(angle=20, hjust=1),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")

  # Create a simpler combined plot using gridExtra
  caption <- "NHANES 2017–2018; adults 20–59; design-based estimates (MEC weights, strata, PSU)."

  # Add caption to bottom plot
  pC_with_caption <- pC +
    labs(caption = caption) +
    theme(plot.caption = element_text(size=9, hjust=0))

  return(list(pA = pA, pB = pB, pC_with_caption = pC_with_caption))
}, "plot_creation", metadata = list(n_corr_points = nrow(corr), n_bf_points = nrow(bf)))

pA <- plot_creation$pA
pB <- plot_creation$pB
pC_with_caption <- plot_creation$pC_with_caption

# Save outputs with performance tracking
output_saving <- benchmark_operation({
  viz_log("Saving visualization outputs...")

  png_out <- file.path(figdir, "nhanes_bmi_bodyfat_visualization.png")
  pdf_out <- file.path(figdir, "nhanes_bmi_bodyfat_visualization.pdf")

  # Save PNG
  png(png_out, width=12, height=10, units="in", res=300)
  grid.arrange(
    pA,
    arrangeGrob(pB, pC_with_caption, ncol=1),
    ncol=2,
    widths=c(1, 1)
  )
  dev.off()

  # Save PDF
  pdf(pdf_out, width=12, height=10)
  grid.arrange(
    pA,
    arrangeGrob(pB, pC_with_caption, ncol=1),
    ncol=2,
    widths=c(1, 1)
  )
  dev.off()

  return(list(png_out = png_out, pdf_out = pdf_out))
}, "output_saving", metadata = list(output_formats = c("PNG", "PDF"), dimensions = "12x10"))

viz_log("Visualization creation completed successfully")
viz_log(paste("PNG saved to:", output_saving$png_out))
viz_log(paste("PDF saved to:", output_saving$pdf_out))

# Generate visualization performance summary and cleanup
viz_log("Generating visualization performance summary...")

viz_performance_summary <- benchmark_operation({
  # Calculate visualization metrics
  viz_summary <- list(
    timestamp = Sys.time(),
    plots_created = 3,  # pA, pB, pC
    output_formats = 2,  # PNG, PDF
    data_points_processed = nrow(corr) + nrow(bf),
    total_benchmarks = length(performance_tracker$benchmarks),
    peak_memory_usage = max(sapply(performance_tracker$benchmarks, function(b) b$memory_used)),
    plots = list(
      correlation_plot = list(
        type = "point range with confidence intervals",
        data_points = nrow(corr),
        variables = c("correlation", "confidence_intervals")
      ),
      mean_bodyfat_plot = list(
        type = "point range with error bars",
        data_points = nrow(bf),
        variables = c("mean_bodyfat", "confidence_intervals", "sex")
      ),
      distribution_plot = list(
        type = "line range with median points",
        data_points = nrow(bf),
        variables = c("quantiles", "median", "sex")
      )
    )
  )

  # Save visualization performance summary
  viz_perf_file <- file.path(logs_dir, "visualization_performance_summary.json")
  jsonlite::write_json(viz_summary, viz_perf_file, pretty = TRUE, auto_unbox = TRUE)

  return(viz_perf_file)
}, "visualization_performance_summary", metadata = list(n_benchmarks = length(performance_tracker$benchmarks)))

viz_log("Visualization performance summary generated")

# Display the PNG inline using magick if available
if (requireNamespace("magick", quietly = TRUE)) {
  img <- magick::image_read(output_saving$png_out)
  plot(img)
} else {
  cat("Install 'magick' package to display image inline\n")
  cat("Image saved to:", output_saving$png_out, "\n")
}

# Cleanup performance tracking
cleanup_performance_tracking()

viz_log("Visualization creation completed successfully with comprehensive performance tracking!")
viz_log(paste("Total benchmarks:", length(performance_tracker$benchmarks)))
viz_log(paste("Performance report saved to:", file.path(logs_dir, "performance_report.html")))
viz_log(paste("Visualization summary saved to:", viz_performance_summary))