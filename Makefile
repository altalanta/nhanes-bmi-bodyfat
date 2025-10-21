R=Rscript

.PHONY: all fetch cleandata analysis viz report test clean help

# Main pipeline using custom parallel processing and caching
all: parallel-pipeline

# Pipeline targets
parallel-pipeline:
	$(R) parallel_pipeline.R

parallel-pipeline-dry:
	$(R) -e "source('parallel_pipeline.R'); cat('Pipeline structure loaded successfully\n')"

# Interactive documentation and tools
tutorial:
	$(R) -e "learnr::run_tutorial('tutorials/getting_started.Rmd', package = 'learnr')"

tutorial-troubleshooting:
	$(R) -e "learnr::run_tutorial('tutorials/help_troubleshooting.Rmd', package = 'learnr')"

config-wizard:
	$(R) -e "shiny::runApp('app.R')"

health-check:
	$(R) -e "source('R/error_handling.R'); display_pipeline_health()"

demo:
	$(R) performance_demo.R

# Data version management
data-registry-init:
	$(R) -e "source('R/data_versioning.R'); initialize_data_registry()"

data-registry-update:
	$(R) -e "source('R/data_versioning.R'); update_data_registry()"

data-registry-summary:
	$(R) -e "source('R/data_versioning.R'); display_registry_summary()"

data-health:
	$(R) -e "source('R/data_versioning.R'); display_quality_report()"

data-integrity:
	$(R) -e "source('R/data_versioning.R'); integrity <- validate_data_integrity(); cat('Data integrity:', ifelse(integrity\$valid, 'VALID', 'ISSUES DETECTED'), '\n')"

data-updates:
	$(R) -e "source('R/data_versioning.R'); updates <- check_for_updates(); cat('Updates needed:', ifelse(updates\$uptodate, 'NO', 'YES'), '\n')"

data-manifest:
	$(R) -e "source('R/data_versioning.R'); generate_data_manifest()"

# Legacy targets (for backward compatibility)
fetch:
	$(R) -e "targets::tar_make(names = 'nhanes_files')"

validate: fetch
	$(R) -e "targets::tar_make(names = c('nhanes_files', 'validation_results'))"

cleandata: validate
	$(R) -e "targets::tar_make(names = c('nhanes_files', 'demo_data', 'bmx_data', 'dxx_data', 'dxxag_data', 'validation_results', 'merged_data', 'cleaned_data'))"

analysis: cleandata
	$(R) -e "targets::tar_make(names = c('survey_design', 'correlation_results', 'bmi_class_results', 'linearity_results'))"

sensitivity: analysis
	$(R) -e "targets::tar_make(names = 'sensitivity_results')"

viz: sensitivity
	$(R) -e "targets::tar_make(names = c('main_plot', 'bmi_class_plot'))"

report: viz
	$(R) -e "targets::tar_make(names = c('results_export', 'report_html'))"

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

# Deployment
deploy-shiny:
	Rscript deployment/deploy-shinyapps.R

deploy-docker:
	Rscript deployment/deploy-docker.R

prepare-cran:
	Rscript deployment/prepare-cran.R

deploy: deploy-shiny deploy-docker

# Advanced analysis
advanced:
	Rscript -e "library(nhanesbmi); results <- run_optimized_analysis(); advanced <- run_complete_advanced_analysis(results\$analytic_data, results\$config); saveRDS(advanced, 'outputs/tables/advanced_analysis_results.rds')"

advanced-stats:
	Rscript scripts/advanced_statistical_analysis.R

# API and integration
api:
	Rscript -e "library(nhanesbmi); results <- readRDS('outputs/tables/nhanes_analysis_results.rds'); create_results_api(results)"

api-launch:
	Rscript -e "pr <- plumb('api/api.R'); pr\$run(port=8000, host='0.0.0.0')"

api-test:
	curl -s http://localhost:8000/health | jq .

api-docs:
	open http://localhost:8000/__docs__/

export-all:
	Rscript -e "library(nhanesbmi); results <- readRDS('outputs/tables/nhanes_analysis_results.rds'); export_comprehensive(results, 'exports')"

# Deployment automation
deploy-docker:
	Rscript deployment/deploy-docker.R

deploy-shiny:
	Rscript deployment/deploy-shinyapps.R

deploy-api:
	Rscript -e "pr <- plumb('api/api.R'); pr\$run(port=8000, host='0.0.0.0')" &

deploy-production:
	@echo "Deploying to production environment..."
	@echo "1. Building Docker container..."
	$(MAKE) deploy-docker
	@echo "2. Starting API server..."
	$(MAKE) deploy-api
	@echo "3. Running health checks..."
	@sleep 5
	@curl -s http://localhost:8000/health | jq -r '.status' | grep -q "healthy" && echo "✅ API deployment successful" || echo "❌ API deployment failed"

prepare-cran:
	Rscript deployment/prepare-cran.R

deploy: deploy-shiny deploy-docker

# Monitoring and maintenance
monitor:
	@echo "Starting monitoring dashboard..."
	R -e "shiny::runApp('deployment/monitoring_dashboard.R')"

backup:
	@echo "Creating system backup..."
	@BACKUP_DIR="backups/$$(date +%Y%m%d_%H%M%S)"; \
	mkdir -p "$$BACKUP_DIR"; \
	cp -r data/registry/ "$$BACKUP_DIR/"; \
	cp -r outputs/ "$$BACKUP_DIR/"; \
	cp config/config.yml "$$BACKUP_DIR/"; \
	echo "✅ Backup created: $$BACKUP_DIR"

health-monitor:
	@echo "Starting continuous health monitoring..."
	@while true; do \
		curl -s http://localhost:8000/health | jq -r '.status' | grep -q "healthy" && echo "✅ System healthy" || echo "❌ System issues detected"; \
		sleep 60; \
	done

# Performance optimization tools
performance-tools:
	Rscript performance_tools.R

performance-benchmark:
	Rscript performance_tools.R benchmark

performance-optimize:
	Rscript performance_tools.R optimize

performance-dashboard:
	Rscript performance_tools.R dashboard

performance-report:
	Rscript performance_tools.R report

performance-compare:
	Rscript performance_tools.R compare

performance-suggest:
	Rscript performance_tools.R suggest

# Release management
release-patch:
	Rscript -e "usethis::use_version('patch')"

release-minor:
	Rscript -e "usethis::use_version('minor')"

release-major:
	Rscript -e "usethis::use_version('major')"

# Utilities
clean:
	rm -f data/derived/*
	rm -f outputs/tables/*.csv
	rm -f outputs/figures/*.png outputs/figures/*.pdf
	rm -f outputs/logs/*.txt
	rm -f outputs/report/*

cleanall: clean
	rm -f data/raw/*.XPT data/raw/manifest.json

clean-cache:
	rm -rf cache/*

help:
	@echo "Available targets:"
	@echo "  all/parallel-pipeline - Run complete parallel processing pipeline with caching"
	@echo "  tutorial - Launch interactive getting started tutorial"
	@echo "  tutorial-troubleshooting - Launch interactive troubleshooting guide"
	@echo "  config-wizard - Launch configuration wizard (Shiny app)"
	@echo "  health-check - Run pipeline health check"
	@echo "  demo - Run performance demonstration"
	@echo "  data-registry-init - Initialize data registry for version tracking"
	@echo "  data-registry-update - Update data registry with current files"
	@echo "  data-registry-summary - Display data registry summary"
	@echo "  data-health - Run comprehensive data quality report"
	@echo "  data-integrity - Validate data file integrity"
	@echo "  data-updates - Check for available data updates"
	@echo "  data-manifest - Generate data manifest for reproducibility"
	@echo "  fetch      - Download NHANES data files with checksum verification"
	@echo "  validate   - Run comprehensive data validation pipeline"
	@echo "  cleandata  - Convert XPT to derived formats with exclusions and validation"
	@echo "  analysis   - Run main NHANES BMI vs body fat analysis with performance monitoring"
	@echo "  sensitivity - Run sensitivity analyses and model comparisons"
	@echo "  viz        - Generate publication-ready visualization"
	@echo "  report     - Render Quarto report to HTML"
	@echo "  test       - Run test suite"
	@echo "  lint       - Check code style and quality"
	@echo "  format     - Auto-format R code to project style"
	@echo "  format-check - Check if code needs formatting"
	@echo "  quality    - Run linting and format checks"
	@echo "  deploy-shiny - Deploy Shiny app to shinyapps.io"
	@echo "  deploy-docker - Build and test Docker container"
	@echo "  prepare-cran - Prepare package for CRAN submission"
	@echo "  deploy     - Deploy to both Shiny and Docker"
	@echo "  advanced   - Run complete advanced statistical analysis"
	@echo "  advanced-stats - Run advanced statistical analysis framework (Bayesian, causal inference, effect sizes, cross-validation)"
	@echo "  api        - Start REST API server for results access"
	@echo "  api-launch - Launch API server from results file"
	@echo "  api-test   - Test API endpoints"
	@echo "  api-docs   - Open API documentation"
	@echo "  export-all - Export results in all supported formats"
	@echo "  deploy-docker - Build and test Docker container"
	@echo "  deploy-shiny - Deploy Shiny app to shinyapps.io"
	@echo "  deploy-api - Launch API server in background"
	@echo "  deploy-production - Complete production deployment"
	@echo "  prepare-cran - Prepare package for CRAN submission"
	@echo "  deploy     - Deploy to both Shiny and Docker"
	@echo "  monitor    - Start monitoring dashboard"
	@echo "  backup     - Create system backup"
	@echo "  health-monitor - Start continuous health monitoring"
	@echo "  performance-tools - Launch comprehensive performance analysis"
	@echo "  performance-benchmark - Run performance benchmarks"
	@echo "  performance-optimize - Auto-tune performance settings"
	@echo "  performance-dashboard - Generate performance dashboard data"
	@echo "  performance-report - Generate comprehensive performance report"
	@echo "  performance-compare - Compare configurations"
	@echo "  performance-suggest - Get performance optimization suggestions"
	@echo "  release-patch - Create patch version bump"
	@echo "  release-minor - Create minor version bump"
	@echo "  release-major - Create major version bump"
	@echo "  clean      - Remove generated output files"
	@echo "  cleanall   - Remove all generated files including raw data"
	@echo "  clean-cache - Remove cached intermediate results"
	@echo "  help       - Show this help message"