# Multi-stage Docker image for NHANES BMI Body Fat Analysis
FROM rocker/r-ver:4.4.0 AS base

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R package dependencies
RUN R -e "install.packages(c('remotes', 'renv', 'devtools'))"

# Set up working directory
WORKDIR /app

# Copy package files
COPY DESCRIPTION NAMESPACE ./
COPY R/ ./R/
COPY scripts/ ./scripts/
COPY config/ ./config/
COPY tests/ ./tests/
COPY vignettes/ ./vignettes/
COPY renv.lock ./

# Install the package
RUN R -e "renv::restore()"

# Multi-stage build for runtime
FROM rocker/r-ver:4.4.0 AS runtime

# Install runtime system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy installed package from base stage
COPY --from=base /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=base /usr/local/lib/R/library /usr/local/lib/R/library

# Set up working directory
WORKDIR /app

# Copy package files for runtime
COPY DESCRIPTION NAMESPACE ./
COPY R/ ./R/
COPY scripts/ ./scripts/
COPY config/ ./config/

# Create output directories
RUN mkdir -p data/raw data/derived outputs/tables outputs/figures outputs/logs outputs/report

# Set environment variables
ENV R_LIBS_USER=/usr/local/lib/R/site-library
ENV NHANES_CACHE_DIR=/app/cache

# Create entrypoint script
RUN echo '#!/bin/bash\n\
if [ "$1" = "analyze" ]; then\n\
  Rscript -e "library(nhanesbmi); results <- run_optimized_analysis(); saveRDS(results, \"outputs/tables/nhanes_analysis_results.rds\")"\n\
elif [ "$1" = "dashboard" ]; then\n\
  Rscript -e "library(nhanesbmi); results <- readRDS(\"outputs/tables/nhanes_analysis_results.rds\"); create_dashboard(results, port=3838)"\n\
elif [ "$1" = "report" ]; then\n\
  Rscript -e "library(nhanesbmi); results <- readRDS(\"outputs/tables/nhanes_analysis_results.rds\"); generate_comprehensive_report(results, safe_load_config())"\n\
else\n\
  echo "Usage: docker run <image> {analyze|dashboard|report}"\n\
  echo "  analyze   - Run complete NHANES analysis"\n\
  echo "  dashboard - Launch interactive dashboard"\n\
  echo "  report    - Generate comprehensive report"\n\
  exit 1\n\
fi' > /usr/local/bin/entrypoint.sh && chmod +x /usr/local/bin/entrypoint.sh

# Set default command
CMD ["/usr/local/bin/entrypoint.sh", "analyze"]

# Expose port for Shiny dashboard
EXPOSE 3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
  CMD Rscript -e "library(nhanesbmi); print('Package loaded successfully')" || exit 1

# Labels
LABEL maintainer="NHANES BMI Body Fat Analysis Team"
LABEL version="1.0.0"
LABEL description="Docker image for NHANES BMI vs Body Fat Analysis"
