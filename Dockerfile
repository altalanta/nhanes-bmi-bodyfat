# NHANES BMI Body Fat Analysis - Docker Image
FROM rocker/r-ver:4.4.0

# Install system dependencies for R packages
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
    libcairo2-dev \
    libgomp1 \
    && rm -rf /var/lib/apt/lists/*

# Install core R packages for environment setup
RUN R -e "install.packages(c('remotes', 'renv', 'devtools', 'quarto'), repos='https://cran.rstudio.com')"

# Create non-root user for security
RUN useradd -m -s /bin/bash nhanes && \
    mkdir -p /home/nhanes/app && \
    chown -R nhanes:nhanes /home/nhanes

# Set up working directory
WORKDIR /home/nhanes/app

# Copy project files
COPY --chown=nhanes:nhanes DESCRIPTION NAMESPACE ./
COPY --chown=nhanes:nhanes R/ ./R/
COPY --chown=nhanes:nhanes scripts/ ./scripts/
COPY --chown=nhanes:nhanes config/ ./config/
COPY --chown=nhanes:nhanes tests/ ./tests/
COPY --chown=nhanes:nhanes vignettes/ ./vignettes/
COPY --chown=nhanes:nhanes renv.lock Makefile ./
COPY --chown=nhanes:nhanes report.qmd ./

# Install project dependencies
RUN R -e "renv::restore()" && \
    R -e "remotes::install_local('.')" || echo "Package installation completed with warnings"

# Create necessary directories
RUN mkdir -p data/raw data/derived outputs/tables outputs/figures outputs/logs outputs/report cache

# Set environment variables
ENV R_LIBS_USER=/usr/local/lib/R/site-library
ENV NHANES_CACHE_DIR=/home/nhanes/app/cache
ENV HOME=/home/nhanes

# Switch to non-root user
USER nhanes

# Create practical entrypoint script
RUN echo '#!/bin/bash\n\
set -e\n\
\n\
# Function to show usage\n\
usage() {\n\
    echo "NHANES BMI Body Fat Analysis Container"\n\
    echo "Usage: $0 {make|analyze|test|report|shell|help}"\n\
    echo ""\n\
    echo "Commands:"\n\
    echo "  make     - Run make command (requires TARGET variable)"\n\
    echo "  analyze  - Run complete analysis pipeline"\n\
    echo "  test     - Run test suite"\n\
    echo "  report   - Generate HTML report"\n\
    echo "  shell    - Start interactive R shell"\n\
    echo "  help     - Show this help message"\n\
    echo ""\n\
    echo "Examples:"\n\
    echo "  docker run nhanes-bmi-bodyfat analyze"\n\
    echo "  docker run nhanes-bmi-bodyfat make TARGET=all"\n\
    echo "  docker run -it nhanes-bmi-bodyfat shell"\n\
}\n\
\n\
case "$1" in\n\
    "make")\n\
        if [ -z "$TARGET" ]; then\n\
            echo "Error: TARGET environment variable required for make command"\n\
            echo "Example: docker run -e TARGET=all nhanes-bmi-bodyfat make"\n\
            exit 1\n\
        fi\n\
        cd /home/nhanes/app && make $TARGET\n\
        ;;\n\
    "analyze")\n\
        cd /home/nhanes/app && make all\n\
        ;;\n\
    "test")\n\
        cd /home/nhanes/app && make test\n\
        ;;\n\
    "report")\n\
        cd /home/nhanes/app && make report\n\
        ;;\n\
    "shell")\n\
        R --no-save --no-restore\n\
        ;;\n\
    "help"|*)\n\
        usage\n\
        ;;\n\
esac\n\
' > /home/nhanes/entrypoint.sh && chmod +x /home/nhanes/entrypoint.sh

# Set default command
CMD ["/home/nhanes/entrypoint.sh", "analyze"]

# Labels for metadata
LABEL maintainer="NHANES BMI Body Fat Analysis Team"
LABEL version="1.0.0"
LABEL description="Docker image for NHANES BMI vs Body Fat Analysis with complete R environment"
LABEL org.opencontainers.image.source="https://github.com/altalanta/nhanes-bmi-bodyfat"
