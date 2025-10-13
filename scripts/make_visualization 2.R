# Set user library path
.libPaths(c('~/R_libs', .libPaths()))

# Install missing packages if needed
required_packages <- c("readr", "dplyr", "stringr", "ggplot2", "tidyr", "gridExtra", "forcats")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse=", "), "\n")
  install.packages(missing_packages, repos='https://cran.rstudio.com/', lib='~/R_libs')
}

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
  library(ggplot2); library(tidyr); library(gridExtra)
  library(forcats)
})

repo <- Sys.getenv("REPO_DIR", unset = normalizePath("~/Documents/Coding_Projects/nhanes-bmi-bodyfat"))
tabdir <- file.path(repo, "outputs", "tables")
figdir <- file.path(repo, "outputs", "figures")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)

# Read data files
corr <- readr::read_csv(file.path(tabdir,"corr_bmi_bodyfat_overall_and_by_sex.csv"), show_col_types = FALSE)
bf   <- readr::read_csv(file.path(tabdir,"bodyfat_by_bmi_class_by_sex.csv"), show_col_types = FALSE)
pop  <- readr::read_csv(file.path(tabdir,"population_counts_by_group.csv"), show_col_types = FALSE)

# Check required columns and adapt to actual column names
cat("Correlation data columns:", paste(names(corr), collapse=", "), "\n")
cat("Body fat data columns:", paste(names(bf), collapse=", "), "\n")

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

# Checks
need_corr <- c("group","r","lcl","ucl")
if (!all(need_corr %in% names(corr))) {
  stop("Missing required correlation columns: ", paste(setdiff(need_corr, names(corr)), collapse=", "))
}

need_bf <- c("BMIclass","sex","mean_bodyfat","mean_ci_lower","mean_ci_upper","q05","q50","q95")
if (!all(need_bf %in% names(bf))) {
  stop("Missing required body fat columns: ", paste(setdiff(need_bf, names(bf)), collapse=", "))
}

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

# Save outputs
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

cat("Wrote:\n", png_out, "\n", pdf_out, "\n")

# Display the PNG inline using magick if available
if (requireNamespace("magick", quietly = TRUE)) {
  img <- magick::image_read(png_out)
  plot(img)
} else {
  cat("Install 'magick' package to display image inline\n")
  cat("Image saved to:", png_out, "\n")
}