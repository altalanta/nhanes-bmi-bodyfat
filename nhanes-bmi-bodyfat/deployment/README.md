# Deployment Guide

This guide explains how to deploy the NHANES BMI-Body Fat Analysis package to various platforms for sharing and collaboration.

## Quick Deployment Options

### 1. GitHub Pages (Automatic)

The package documentation is automatically deployed to GitHub Pages via CI/CD.

- **URL**: `https://altalanta.github.io/nhanes-bmi-bodyfat/`
- **Trigger**: Pushes to main branch
- **Content**: Package documentation and vignettes

### 2. Shiny Apps (Interactive Dashboard)

#### Option A: Automated Deployment (GitHub Actions)
- **Trigger**: GitHub releases
- **URL**: `https://altalanta.shinyapps.io/nhanes-bmi-bodyfat/`
- **Requires**: `SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET` secrets in GitHub repository

#### Option B: Manual Deployment
```r
# Set up rsconnect account (one-time setup)
library(rsconnect)
setAccountInfo(name = "your-username",
              token = "your-token",
              secret = "your-secret")

# Deploy using script
Rscript deployment/deploy-shinyapps.R
```

### 3. Docker Container

#### Option A: Automated Build (GitHub Actions)
- **Trigger**: Pushes to main branch
- **Registry**: Docker Hub (`altalanta/nhanes-bmi-bodyfat`)
- **Platforms**: Linux AMD64/ARM64

#### Option B: Local Deployment
```bash
# Build and test locally
Rscript deployment/deploy-docker.R

# Or manually:
docker build -t nhanes-bmi-bodyfat .
docker run -p 3838:3838 nhanes-bmi-bodyfat dashboard
```

## Detailed Deployment Instructions

### Shiny Apps Deployment

#### Prerequisites
1. **RStudio Account**: Create account at [shinyapps.io](https://www.shinyapps.io)
2. **rsconnect Package**: Install in R: `install.packages("rsconnect")`

#### Step-by-Step Setup
```r
# 1. Install rsconnect
install.packages("rsconnect")

# 2. Set up account (run once)
library(rsconnect)
setAccountInfo(name = "your-username",
              token = "your-token",
              secret = "your-secret")

# 3. Deploy
Rscript deployment/deploy-shinyapps.R
```

#### Troubleshooting
- **Authentication Errors**: Verify token and secret in rsconnect account
- **Build Errors**: Check that all dependencies are installed
- **Port Conflicts**: Change port in deployment script if needed

### Docker Deployment

#### Prerequisites
1. **Docker Desktop**: Install from [docker.com](https://www.docker.com)
2. **Sufficient Resources**: At least 4GB RAM recommended

#### Usage Examples
```bash
# Build and run analysis
docker run altalanta/nhanes-bmi-bodyfat analyze

# Launch dashboard
docker run -p 3838:3838 altalanta/nhanes-bmi-bodyfat dashboard

# Generate report
docker run -v $(pwd)/outputs:/app/outputs altalanta/nhanes-bmi-bodyfat report
```

#### Custom Configuration
```bash
# Build with custom tag
docker build -t my-nhanes-app .

# Run with volume mounting for persistent data
docker run -v $(pwd)/data:/app/data \
           -v $(pwd)/outputs:/app/outputs \
           -p 3838:3838 my-nhanes-app dashboard
```

### Package Distribution

#### CRAN/Bioconductor Submission (Future)

For official package distribution:

1. **CRAN Preparation**:
   - Ensure all CRAN policies are followed
   - Run `R CMD check --as-cran` to validate
   - Create submission tarball

2. **Bioconductor Preparation**:
   - Follow Bioconductor submission guidelines
   - Ensure compatibility with Bioconductor requirements
   - Submit through Bioconductor review process

#### GitHub Releases (Current)

Automated releases are created when version tags are pushed:

```bash
# Create version tag
git tag v1.0.0
git push origin v1.0.0

# This triggers:
# - GitHub release creation
# - Package building and artifact upload
# - Shiny app deployment (if secrets configured)
# - Docker image building (if Docker Hub secrets configured)
```

## Configuration for Deployment

### Environment Variables

Create a `.Renviron` file or set these variables:

```bash
# For rsconnect deployment
SHINYAPPS_TOKEN=your_token_here
SHINYAPPS_SECRET=your_secret_here

# For Docker Hub (optional)
DOCKER_USERNAME=your_username
DOCKER_PASSWORD=your_password
```

### Custom Deployment Configuration

Modify `deployment/deploy-shinyapps.R` for custom settings:

```r
# Customize app configuration
deployApp(
  appDir = "deployment_app/app",
  appName = "my-nhanes-app",  # Custom app name
  appTitle = "My Custom NHANES Analysis",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
```

## Monitoring and Maintenance

### Health Checks

#### For Shiny Apps
```r
# Check app status
library(rsconnect)
applications(account = "your-account")

# View logs
showLogs(account = "your-account", appName = "nhanes-bmi-bodyfat")
```

#### For Docker Containers
```bash
# Check container status
docker ps -a | grep nhanes

# View container logs
docker logs <container_id>

# Monitor resource usage
docker stats <container_id>
```

### Updates and Maintenance

#### Updating Deployed Applications

1. **Shiny Apps**:
   ```r
   # Redeploy with updated code
   Rscript deployment/deploy-shinyapps.R
   ```

2. **Docker Images**:
   ```bash
   # Rebuild and push updated image
   docker build -t nhanes-bmi-bodyfat:latest .
   docker push altalanta/nhanes-bmi-bodyfat:latest
   ```

#### Rollback Procedures

1. **Shiny Apps**:
   ```r
   # View deployment history
   library(rsconnect)
   deployments(account = "your-account", appName = "nhanes-bmi-bodyfat")

   # Rollback to previous version if needed
   ```

2. **Docker**:
   ```bash
   # Use previous image tag
   docker run altalanta/nhanes-bmi-bodyfat:v1.0.0 dashboard
   ```

## Troubleshooting Common Issues

### Deployment Failures

#### Network Issues
- **Problem**: "Connection refused" or timeout errors
- **Solution**: Check internet connection and firewall settings

#### Authentication Errors
- **Problem**: "Invalid credentials" for shinyapps.io
- **Solution**: Verify token/secret in rsconnect account settings

#### Build Errors
- **Problem**: Package installation fails during deployment
- **Solution**: Ensure all dependencies are properly specified in DESCRIPTION

#### Resource Limits
- **Problem**: Application runs out of memory or times out
- **Solution**: Adjust memory limits in deployment configuration

### Performance Issues

#### Slow Loading
- **Problem**: Dashboard takes too long to load
- **Solution**: Enable caching and optimize data processing

#### High Memory Usage
- **Problem**: Container crashes due to memory limits
- **Solution**: Increase memory allocation or use sampling for large datasets

## Best Practices

### Security
- Store credentials securely (use GitHub secrets, not plain text)
- Regularly rotate API tokens and passwords
- Use private repositories for sensitive configurations

### Reliability
- Implement health checks and monitoring
- Use version pinning for reproducible deployments
- Test deployments in staging environment first

### Performance
- Enable caching for repeated analyses
- Use appropriate resource allocation
- Monitor and optimize for production workloads

### Maintenance
- Keep dependencies updated but tested
- Monitor application logs regularly
- Plan for regular updates and maintenance windows

## Support and Resources

- **GitHub Issues**: [Report deployment issues](https://github.com/altalanta/nhanes-bmi-bodyfat/issues)
- **Shiny Apps Documentation**: [Official deployment guide](https://docs.rstudio.com/shinyapps.io/)
- **Docker Documentation**: [Container best practices](https://docs.docker.com/develop/dev-best-practices/)
- **R Package Deployment**: [R Packages book](https://r-pkgs.org/)

## Integration with CI/CD

The deployment workflows integrate with:

- **GitHub Actions**: Automated testing and deployment
- **Docker Hub**: Container registry and automated builds
- **shinyapps.io**: Cloud hosting for Shiny applications
- **GitHub Releases**: Version management and distribution

## Future Deployment Options

Planned enhancements:

- **RStudio Connect** deployment for enterprise environments
- **AWS/Azure/GCP** cloud deployment options
- **Kubernetes** orchestration for scalable deployments
- **Conda-forge** packaging for Python ecosystem integration

---

*This deployment guide covers all current deployment options for the NHANES BMI-Body Fat Analysis package.*
