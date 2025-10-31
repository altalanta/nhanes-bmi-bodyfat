# Troubleshooting and Maintenance Guide

This comprehensive guide provides solutions for common issues, maintenance procedures, and best practices for keeping the NHANES BMI Body Fat Analysis platform running smoothly.

## 🚨 Quick Troubleshooting

### Health Check First
Always start with a system health check:
```bash
make health-check
```

**Expected Output:**
```
✅ PIPELINE HEALTH CHECK: All systems operational!
🚀 Ready to run analysis with: make parallel-pipeline
```

**If Issues Found:**
```
⚠️ PIPELINE HEALTH ISSUES DETECTED
❌ Missing packages: survey, ggplot2
❌ Missing NHANES data files: data/raw/DEMO_J.XPT

💡 Quick Fixes:
  1. Install packages: install.packages(c('survey', 'ggplot2'))
  2. Download data: make fetch
  3. Configure settings: make config-wizard
```

## 🔧 Common Issues & Solutions

### Installation Problems

#### ❌ "Package 'X' not found" errors

**Symptoms:**
- Error messages about missing packages during installation or runtime
- `library(package_name)` fails

**Quick Fix:**
```bash
# Install missing packages
R -e "install.packages(c('dplyr', 'ggplot2', 'survey', 'foreign', 'yaml', 'future', 'furrr'))"

# Or use the configuration wizard
make config-wizard
```

**Advanced Troubleshooting:**
```bash
# Check R version compatibility
R -e "sessionInfo()"

# Verify package installation directory
R -e ".libPaths()"

# Check for conflicting packages
R -e "installed.packages()[installed.packages()[,'Package'] == 'conflicted',]"
```

#### ❌ "Directory not found" errors

**Symptoms:**
- Missing directories in error messages
- Cannot write to output directories
- File creation fails

**Quick Fix:**
```bash
# Create all required directories
mkdir -p data/raw data/derived outputs/{tables,figures,logs,report} config cache tutorials

# Or run health check for automatic detection
make health-check
```

**Permission Issues:**
```bash
# Check current directory permissions
ls -la

# Fix permissions if needed
chmod 755 data outputs config

# Alternative: Run in user directory
mkdir -p ~/nhanes-analysis
cd ~/nhanes-analysis
git clone <repository-url> .
```

### Data Download Issues

#### ❌ "File not found" or "Data download failed" errors

**Symptoms:**
- `make fetch` fails
- Missing files in `data/raw/` directory
- Network timeout errors

**Quick Fix:**
```bash
# Check internet connection
ping -c 3 google.com

# Retry download with verbose output
make fetch

# Check available disk space
df -h
```

**Network Issues:**
```bash
# Check firewall/proxy settings
curl -I https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT

# Alternative: Manual download
wget https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT -P data/raw/
wget https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT -P data/raw/
wget https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXX_J.XPT -P data/raw/
wget https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DXXAG_J.XPT -P data/raw/
```

**Alternative Data Sources:**
```bash
# Try nhanesA package (if available)
R -e "
if (requireNamespace('nhanesA', quietly = TRUE)) {
  nhanesA::nhanes('DEMO_J') %>% write.csv('data/raw/DEMO_J.csv')
  # Convert to XPT format as needed
}
"
```

### Configuration Problems

#### ❌ "Configuration file not found" or "Invalid YAML" errors

**Symptoms:**
- Missing `config/config.yml` file
- YAML parsing errors
- Missing required configuration sections

**Quick Fix:**
```bash
# Use the configuration wizard
make config-wizard

# Or create manually
mkdir -p config
# Edit config/config.yml with your preferred settings
```

**YAML Syntax Issues:**
```yaml
# ✅ Correct YAML syntax
data:
  raw_dir: "data/raw"
  derived_dir: "data/derived"

# ❌ Common YAML mistakes to avoid:
# data:                          # Wrong indentation
#   raw_dir: "data/raw"         # Inconsistent spacing
#   derived_dir:"data/derived"  # Missing space after colon
```

**Validation:**
```bash
# Test configuration validity
R -e "
tryCatch({
  config <- yaml::read_yaml('config/config.yml')
  cat('✅ Configuration is valid!\n')
}, error = function(e) {
  cat('❌ Configuration error:', e$message, '\n')
})
"
```

### Performance Issues

#### ❌ "Out of memory" or "Process killed" errors

**Symptoms:**
- R crashes during analysis
- High memory usage
- Slow performance

**Quick Fix:**
```bash
# Use parallel processing (more memory efficient)
make parallel-pipeline

# Monitor system resources
top -o mem    # Linux/macOS
Task Manager  # Windows
```

**Memory Management:**
```bash
# Increase R memory limit
R -e "memory.limit(size = 8192)"  # 8GB limit

# Force garbage collection
R -e "gc()"

# Monitor memory usage during execution
R -e "
while (TRUE) {
  cat('Memory usage:', memory.size(), 'MB\n')
  Sys.sleep(5)
}
"
```

**Alternative Solutions:**
```bash
# Use sequential processing for memory-constrained systems
make all  # Instead of make parallel-pipeline

# Reduce worker count in configuration
# Edit config/config.yml:
# performance:
#   max_workers: 2
```

#### ❌ "Slow performance" issues

**Symptoms:**
- Analysis takes longer than expected
- High CPU usage but slow progress
- Frequent disk I/O operations

**Quick Fix:**
```bash
# Check available CPU cores
R -e "parallel::detectCores()"

# Use SSD storage for better I/O performance
# Move project to SSD if using HDD

# Check for background processes consuming resources
ps aux | grep R  # Linux/macOS
```

**Optimization:**
```bash
# Enable caching for repeated analyses
make parallel-pipeline  # Automatically uses caching

# Clean cache if corrupted
make clean-cache
make parallel-pipeline
```

### Data Quality Issues

#### ❌ "Data integrity compromised" errors

**Symptoms:**
- Hash mismatches in data files
- File size discrepancies
- Corrupted or incomplete downloads

**Quick Fix:**
```bash
# Re-download and validate files
make fetch

# Check data integrity
make data-integrity

# View detailed quality report
make data-health
```

**Manual Validation:**
```bash
# Check file sizes match expectations
ls -lh data/raw/

# Verify file contents (basic check)
R -e "
library(foreign)
demo <- read.xport('data/raw/DEMO_J.XPT')
cat('DEMO_J records:', nrow(demo), '\n')
cat('Columns:', ncol(demo), '\n')
"
```

#### ❌ "Update detection failed" errors

**Symptoms:**
- Cannot check for new NHANES releases
- Network connectivity issues
- CDC website accessibility problems

**Quick Fix:**
```bash
# Check internet connectivity
ping -c 3 www.cdc.gov

# Manual update check
R -e "
# Check file modification times manually
file.info('data/raw/DEMO_J.XPT')$mtime
# Compare with CDC website if needed
"
```

## 🔍 Diagnostic Tools

### System Health Monitoring

#### Comprehensive Health Check
```bash
# Run complete system verification
make health-check

# Individual component checks
make data-integrity      # Data file validation
make data-updates       # Update availability
R -e "parallel::detectCores()"  # CPU capability
```

#### Log Analysis
```bash
# View analysis logs
cat outputs/logs/analysis_log.txt

# Check for error patterns
grep -i "error" outputs/logs/analysis_log.txt

# Monitor real-time logging
tail -f outputs/logs/analysis_log.txt
```

#### Performance Profiling
```bash
# Profile pipeline execution
R -e "
library(profvis)
profvis({
  source('parallel_pipeline.R')
  run_parallel_pipeline()
})
"

# Memory profiling
R -e "
library(profmem)
profmem({
  source('parallel_pipeline.R')
  run_parallel_pipeline()
})
"
```

### Interactive Diagnostics

#### Launch Troubleshooting Tutorial
```bash
# Interactive problem diagnosis
make tutorial-troubleshooting

# Features:
# - Automated issue detection
# - Step-by-step solutions
# - Prevention strategies
# - Community resources
```

#### Configuration Wizard Diagnostics
```bash
# Launch diagnostic interface
make config-wizard

# Check configuration validity
R -e "
tryCatch({
  config <- yaml::read_yaml('config/config.yml')
  cat('✅ Configuration valid\n')
}, error = function(e) {
  cat('❌ Configuration issue:', e$message, '\n')
})
"
```

## 🛠️ Maintenance Procedures

### Regular Maintenance Tasks

#### Daily/Weekly Checks
```bash
# Quick health verification
make health-check

# Update data if needed
make data-updates

# Verify data integrity
make data-integrity
```

#### Monthly Maintenance
```bash
# 1. Clean and rebuild cache
make clean-cache
make parallel-pipeline  # Rebuilds cache

# 2. Update data registry
make data-registry-update

# 3. Generate quality report
make data-health

# 4. Archive old results
cp -r outputs/ archives/monthly_$(date +%Y%m%d)/
```

#### Quarterly Maintenance
```bash
# 1. Review and update dependencies
R -e "renv::update()"

# 2. Test all functionality
make test
make quality

# 3. Update documentation if needed
# Check for new NHANES releases

# 4. Performance benchmarking
make demo  # Compare with previous benchmarks
```

### Backup and Recovery

#### Automated Backup Strategy
```bash
#!/bin/bash
# backup_script.sh

BACKUP_DIR="backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

# Backup critical files
cp -r data/registry/ "$BACKUP_DIR/"
cp -r outputs/ "$BACKUP_DIR/"
cp config/config.yml "$BACKUP_DIR/"
cp data/raw/manifest.json "$BACKUP_DIR/"

# Create backup manifest
echo "Backup created: $(date)" > "$BACKUP_DIR/README.txt"
echo "Files backed up:" >> "$BACKUP_DIR/README.txt"
ls -la "$BACKUP_DIR/" >> "$BACKUP_DIR/README.txt"

echo "✅ Backup completed: $BACKUP_DIR"
```

#### Recovery Procedures
```bash
# Restore from backup
BACKUP_DIR="backups/20251020_143000"
cp -r "$BACKUP_DIR/registry/" data/
cp -r "$BACKUP_DIR/outputs/" .
cp "$BACKUP_DIR/config.yml" config/
```

#### Registry Recovery
```bash
# If registry is corrupted
cp data/registry/data_registry.json data/registry/backup_$(date +%Y%m%d_%H%M%S).json

# Recreate registry
make data-registry-init
make data-registry-update

# Verify recovery
make data-health
```

### Performance Optimization

#### Cache Management
```bash
# Monitor cache size and effectiveness
R -e "
cache_files <- list.files('cache/', full.names = TRUE)
cache_size <- sum(file.info(cache_files)$size)
cat('Cache size:', round(cache_size / 1024 / 1024, 2), 'MB\n')
cat('Cache files:', length(cache_files), '\n')
"

# Clean cache if too large
make clean-cache

# Optimize cache for specific workflows
# Edit config/config.yml:
# performance:
#   cache_strategy: "content"  # content, time, or hybrid
```

#### Memory Optimization
```bash
# Monitor and optimize memory usage
R -e "
# Check memory usage
print(memory.size())

# Force garbage collection
gc()

# Set memory limits
memory.limit(size = 8192)  # 8GB
"
```

## 📊 Monitoring and Alerting

### Automated Monitoring Setup

#### Log Monitoring Script
```bash
#!/bin/bash
# monitor_logs.sh

LOG_FILE="outputs/logs/analysis_log.txt"
ERROR_COUNT=$(grep -c "ERROR" "$LOG_FILE" 2>/dev/null || echo 0)

if [ "$ERROR_COUNT" -gt 5 ]; then
  echo "⚠️ High error count detected in analysis logs"
  echo "Recent errors:"
  tail -10 "$LOG_FILE"
  # Send alert if needed
fi
```

#### Performance Monitoring
```r
# Create performance monitoring function
monitor_performance <- function() {
  start_time <- Sys.time()

  # Run analysis
  results <- run_parallel_pipeline()

  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "secs")

  # Log performance metrics
  performance_data <- data.frame(
    timestamp = Sys.time(),
    execution_time = execution_time,
    memory_peak = max(sapply(gc(), function(x) x[2])),
    cache_hits = calculate_cache_effectiveness(),
    cpu_cores = availableCores()
  )

  write.csv(performance_data, "outputs/logs/performance_metrics.csv", append = TRUE)

  return(results)
}
```

### Alert Configuration

#### Email Notifications
```bash
# Setup email alerts for critical issues
# 1. Install mail utility
# 2. Configure SMTP settings
# 3. Add to cron job or CI/CD pipeline

# Example cron job for daily health check
# 0 9 * * * cd /path/to/project && make health-check | mail -s "Daily Health Check" admin@example.com
```

#### Dashboard Integration
```r
# Create monitoring dashboard
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("NHANES Analysis Monitoring"),

  sidebarLayout(
    sidebarPanel(
      actionButton("run_health_check", "Run Health Check"),
      actionButton("clear_cache", "Clear Cache")
    ),

    mainPanel(
      plotOutput("performance_plot"),
      verbatimTextOutput("health_status")
    )
  )
)

server <- function(input, output) {
  output$health_status <- renderText({
    input$run_health_check
    system("make health-check", intern = TRUE)
  })

  output$performance_plot <- renderPlot({
    # Load performance metrics and visualize
    perf_data <- read.csv("outputs/logs/performance_metrics.csv")
    ggplot(perf_data, aes(x = timestamp, y = execution_time)) +
      geom_line() +
      labs(title = "Analysis Performance Over Time")
  })
}
```

## 🔒 Security and Compliance

### Data Security

#### File Permissions
```bash
# Set appropriate permissions
chmod 755 data/ outputs/ config/
chmod 644 data/raw/*.XPT
chmod 644 config/config.yml

# Verify permissions
find . -name "*.XPT" -exec ls -la {} \;
```

#### Sensitive Data Handling
```r
# If working with sensitive data, implement additional security
# 1. Encrypt sensitive files
# 2. Use secure file transfer protocols
# 3. Implement access controls
# 4. Regular security audits
```

### Compliance Considerations

#### HIPAA Compliance (if applicable)
```bash
# For healthcare data handling:
# 1. Ensure data de-identification
# 2. Implement access logging
# 3. Regular compliance audits
# 4. Secure data storage and transmission
```

#### Data Retention Policies
```yaml
# In config/config.yml
retention:
  data_retention_days: 2555  # 7 years for research data
  log_retention_days: 365    # 1 year for logs
  backup_retention_days: 2555 # Match data retention

# Automated cleanup script
cleanup_old_files <- function(retention_days) {
  cutoff_date <- Sys.Date() - retention_days

  # Clean old backups
  old_backups <- list.files("backups/", pattern = "[0-9]{8}", full.names = TRUE)
  for (backup in old_backups) {
    backup_date <- as.Date(substr(basename(backup), 1, 8), "%Y%m%d")
    if (backup_date < cutoff_date) {
      unlink(backup, recursive = TRUE)
    }
  }
}
```

## 🌐 Network and Connectivity Issues

### Internet Connectivity Problems

#### Diagnostic Commands
```bash
# Test basic connectivity
ping -c 3 8.8.8.8

# Test CDC website access
curl -I https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT

# Check DNS resolution
nslookup wwwn.cdc.gov
```

#### Proxy Configuration
```bash
# If behind corporate proxy
export http_proxy="http://proxy.company.com:8080"
export https_proxy="http://proxy.company.com:8080"

# Test with proxy
curl -I --proxy http://proxy.company.com:8080 https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT
```

### Firewall Issues

#### Common Firewall Rules
```bash
# Allow R and curl traffic
# Ubuntu/Debian:
sudo ufw allow 80
sudo ufw allow 443

# CentOS/RHEL:
sudo firewall-cmd --permanent --add-port=80/tcp
sudo firewall-cmd --permanent --add-port=443/tcp
sudo firewall-cmd --reload
```

## 📞 Getting Additional Help

### Self-Service Resources

1. **Interactive Tutorial**: `make tutorial`
   - Complete learning experience with built-in help

2. **Troubleshooting Guide**: `make tutorial-troubleshooting`
   - Interactive problem diagnosis and solutions

3. **Configuration Wizard**: `make config-wizard`
   - Guided setup with real-time validation

4. **Health Check**: `make health-check`
   - Automated system diagnostics

### Community Support

- **GitHub Issues**: https://github.com/altalanta/nhanes-bmi-bodyfat/issues
- **GitHub Discussions**: https://github.com/altalanta/nhanes-bmi-bodyfat/discussions
- **Email**: analysis@nhanes-bmi.org (for research collaborations)

### Professional Support

For organizations requiring dedicated support:

- **Training Workshops**: Hands-on instruction for research teams
- **Custom Development**: Specialized analysis implementations
- **Code Review**: Professional assessment of research pipelines
- **Consulting**: Methodological guidance for complex studies
- **24/7 Support**: Priority assistance for critical research projects

### Emergency Procedures

#### Critical System Failure
```bash
# 1. Run health check to identify issues
make health-check

# 2. Check logs for detailed error information
cat outputs/logs/analysis_log.txt | tail -50

# 3. Try recovery procedures
make clean-cache
make data-registry-init

# 4. Contact support if issues persist
# Email: analysis@nhanes-bmi.org
# Include: Error logs, system information, reproduction steps
```

#### Data Loss Recovery
```bash
# 1. Check for recent backups
ls -la backups/

# 2. Restore from most recent backup
BACKUP_DIR=$(ls -td backups/*/ | head -1)
cp -r "$BACKUP_DIR"/* .

# 3. Verify restoration
make data-integrity

# 4. Re-run analysis if needed
make parallel-pipeline
```

## ✅ Best Practices

### Prevention Strategies

1. **Regular Health Checks**: Run `make health-check` before major analyses
2. **Automated Backups**: Implement regular backup procedures
3. **Version Control**: Track configuration and code changes
4. **Documentation**: Maintain updated documentation for team members
5. **Testing**: Run test suite before deploying changes

### Performance Optimization

1. **Use SSD Storage**: Faster I/O for data loading and caching
2. **Monitor Resource Usage**: Track memory and CPU utilization
3. **Clean Cache Regularly**: Prevent cache bloat and corruption
4. **Update Dependencies**: Keep packages current for security and performance
5. **Network Optimization**: Use local mirrors when possible

### Security Best Practices

1. **File Permissions**: Set appropriate access controls
2. **Data Encryption**: Encrypt sensitive research data
3. **Access Logging**: Track who accesses analysis results
4. **Regular Audits**: Review system security periodically
5. **Update Management**: Keep software dependencies current

---

**This comprehensive troubleshooting and maintenance guide ensures the NHANES BMI Body Fat Analysis platform remains reliable, secure, and performant for all research applications.**







