R=Rscript

.PHONY: all fetch cleandata analysis viz report test clean help

# Main pipeline
all: fetch cleandata analysis viz report

# Data pipeline
fetch:
	$(R) scripts/fetch_nhanes.R

cleandata: fetch
	$(R) scripts/derive_dataset.R

# Analysis pipeline  
analysis: cleandata
	$(R) scripts/nhanes_bmi_bodyfat_analysis.R

sensitivity: analysis
	$(R) scripts/sensitivity_analysis.R

viz: sensitivity
	$(R) scripts/make_visualization.R

report: viz
	quarto render report.qmd --output-dir outputs/report

# Testing
test:
	$(R) -e "testthat::test_dir('tests')"

# Code quality
lint:
	$(R) -e "lintr::lint_dir('scripts')"

format:
	$(R) -e "styler::style_dir('scripts')"

format-check:
	$(R) -e "styler::style_dir('scripts', dry = 'on')"

quality: lint format-check

# Utilities
clean:
	rm -f data/derived/*
	rm -f outputs/tables/*.csv
	rm -f outputs/figures/*.png outputs/figures/*.pdf
	rm -f outputs/logs/*.txt
	rm -f outputs/report/*

cleanall: clean
	rm -f data/raw/*.XPT data/raw/manifest.json

help:
	@echo "Available targets:"
	@echo "  all        - Run complete analysis pipeline (fetch -> analysis -> viz -> report)"
	@echo "  fetch      - Download NHANES data files with checksum verification"
	@echo "  cleandata  - Convert XPT to derived formats with exclusions"
	@echo "  analysis   - Run main NHANES BMI vs body fat analysis"
	@echo "  sensitivity - Run sensitivity analyses and model comparisons"
	@echo "  viz        - Generate publication-ready visualization"
	@echo "  report     - Render Quarto report to HTML"
	@echo "  test       - Run test suite"
	@echo "  lint       - Check code style and quality"
	@echo "  format     - Auto-format R code to project style"
	@echo "  format-check - Check if code needs formatting"
	@echo "  quality    - Run linting and format checks"
	@echo "  clean      - Remove generated output files"
	@echo "  cleanall   - Remove all generated files including raw data"
	@echo "  help       - Show this help message"