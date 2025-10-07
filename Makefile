PYTHON ?= python3
VENV ?= .venv
PIP ?= $(VENV)/bin/pip
PY ?= $(VENV)/bin/python

.DEFAULT_GOAL := help

.PHONY: help
help: ## Show this help message
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'

.PHONY: setup
setup: ## Create conda env and install hooks
	@echo "Setting up development environment..."
	@if command -v conda >/dev/null 2>&1; then \
		conda env create -f env/environment.yml --force || conda env update -f env/environment.yml; \
		conda run -n cytoflow-qc pip install -e .; \
		conda run -n cytoflow-qc pip install -e .[dev]; \
		conda run -n cytoflow-qc pre-commit install; \
	else \
		echo "Conda not found. Please install conda or use the setup script."; \
		exit 1; \
	fi

.PHONY: setup-pip
setup-pip: $(VENV)/bin/activate ## Alternative setup with pip/venv
$(VENV)/bin/activate: env/requirements.txt
	$(PYTHON) -m venv $(VENV)
	$(PIP) install --upgrade pip
	$(PIP) install -r env/requirements.txt
	$(PIP) install -e .[dev]
	$(VENV)/bin/pre-commit install

.PHONY: lint
lint: ## Run ruff linter
	ruff check src tests
	black --check src tests

.PHONY: format
format: ## Format code with black and ruff
	ruff check --fix src tests
	black src tests

.PHONY: type-check
type-check: ## Run mypy type checking
	mypy src

.PHONY: test
test: ## Run pytest
	@if command -v conda >/dev/null 2>&1; then \
		conda run -n cytoflow-qc python -m pytest -v; \
	else \
		python -m pytest -v; \
	fi

.PHONY: test-cov
test-cov: ## Run pytest with coverage
	pytest --cov=cytoflow_qc --cov-report=html --cov-report=term

.PHONY: build
build: ## Build package (sdist/wheel)
	$(PY) -m build

.PHONY: clean
clean: ## Clean build artifacts
	rm -rf build/ dist/ *.egg-info/
	rm -rf .pytest_cache/ .coverage htmlcov/
	find . -type d -name __pycache__ -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete

.PHONY: smoke
smoke: ## Run minimal smoke test
	cytoflow-qc run --samplesheet samplesheets/example_samplesheet.csv --config configs/example_config.yaml --out results/smoke

.PHONY: report
report: ## Build HTML report from results
	cytoflow-qc report --in results --template configs/report_template.html.j2 --out results/report.html

.PHONY: docker-build
docker-build: ## Build Docker image
	docker build -t cytoflow-qc .

.PHONY: docker-run
docker-run: ## Run Docker container
	docker-compose up