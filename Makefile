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

# Deployment
deploy-shiny:
	Rscript deployment/deploy-shinyapps.R

deploy-docker:
	Rscript deployment/deploy-docker.R

prepare-cran:
	Rscript deployment/prepare-cran.R

deploy: deploy-shiny deploy-docker

# Docker targets
docker-build:
	docker build -t nhanes-bmi-bodyfat:latest .

docker-test:
	docker run --rm nhanes-bmi-bodyfat:latest analyze

docker-run:
	docker run -it --rm -v $(PWD)/outputs:/home/nhanes/app/outputs nhanes-bmi-bodyfat:latest analyze

docker-shell:
	docker run -it --rm -v $(PWD):/home/nhanes/app nhanes-bmi-bodyfat:latest shell

docker-clean:
	docker system prune -f
	docker image rm nhanes-bmi-bodyfat:latest 2>/dev/null || true

# Docker Compose targets
docker-up:
	docker-compose up --build

docker-down:
	docker-compose down

docker-dev:
	docker-compose up nhanes-dev

# Advanced analysis
advanced: cleandata
	Rscript scripts/advanced_ml_analysis.R

# API and integration
api:
	Rscript scripts/api_server.R --port 8000

api-dev:
	Rscript scripts/api_server.R --port 8000 --host 127.0.0.1

api-launch:
	Rscript scripts/api_server.R

export-all:
	Rscript -e "library(nhanesbmi); results <- readRDS('outputs/tables/nhanes_analysis_results.rds'); export_comprehensive(results, 'exports')"

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
	@echo "  deploy-shiny - Deploy Shiny app to shinyapps.io"
	@echo "  deploy-docker - Build and test Docker container"
	@echo "  prepare-cran - Prepare package for CRAN submission"
	@echo "  deploy     - Deploy to both Shiny and Docker"
	@echo "  advanced   - Run advanced machine learning analysis"
	@echo "  api        - Start REST API server for results access (0.0.0.0:8000)"
	@echo "  api-dev    - Start API server for local development (localhost only)"
	@echo "  api-launch - Launch API server from results file"
	@echo "  export-all - Export results in all supported formats"
	@echo "  release-patch - Create patch version bump"
	@echo "  release-minor - Create minor version bump"
	@echo "  release-major - Create major version bump"
	@echo "  clean      - Remove generated output files"
	@echo "  cleanall   - Remove all generated files including raw data"
	@echo "  help       - Show this help message"
	@echo ""
	@echo "Docker targets:"
	@echo "  docker-build  - Build Docker image"
	@echo "  docker-test   - Test Docker container"
	@echo "  docker-run    - Run container with volume mounts"
	@echo "  docker-shell  - Start interactive shell in container"
	@echo "  docker-clean  - Clean up Docker images and cache"
	@echo "  docker-up     - Start with docker-compose"
	@echo "  docker-down   - Stop docker-compose services"
	@echo "  docker-dev    - Start development environment"