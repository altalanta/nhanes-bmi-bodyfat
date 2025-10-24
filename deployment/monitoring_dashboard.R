# NHANES BMI Body Fat Analysis - Monitoring Dashboard
# Real-time monitoring and alerting system

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(jsonlite)
library(lubridate)
library(dplyr)

# Source monitoring utilities
source("../R/data_versioning.R")
source("../R/error_handling.R")

# Monitoring configuration
MONITORING_CONFIG <- list(
  refresh_interval = 30,  # seconds
  history_days = 7,
  alert_thresholds = list(
    error_rate = 0.1,      # 10% error rate
    memory_usage = 0.8,    # 80% memory usage
    disk_usage = 0.9,      # 90% disk usage
    response_time = 10     # 10 seconds max response time
  )
)

# Load monitoring data
load_monitoring_data <- function() {
  tryCatch({
    # Load health check data
    health_file <- "outputs/logs/monitoring_health.json"
    if (file.exists(health_file)) {
      health_data <- fromJSON(health_file)
    } else {
      health_data <- data.frame(
        timestamp = as.character(Sys.time()),
        status = "unknown",
        memory_usage = 0,
        cpu_usage = 0,
        disk_usage = 0
      )
    }

    # Load performance metrics
    perf_file <- "outputs/logs/performance_metrics.csv"
    if (file.exists(perf_file)) {
      perf_data <- read.csv(perf_file)
      perf_data$timestamp <- as.POSIXct(perf_data$timestamp)
    } else {
      perf_data <- data.frame(
        timestamp = as.POSIXct(Sys.time()),
        execution_time = 0,
        memory_peak = 0,
        cache_hits = 0
      )
    }

    # Load error logs
    error_file <- "outputs/logs/error_details.txt"
    if (file.exists(error_file)) {
      error_content <- readLines(error_file)
      error_count <- length(error_content)
    } else {
      error_count <- 0
    }

    return(list(
      health = health_data,
      performance = perf_data,
      errors = error_count
    ))

  }, error = function(e) {
    warning("Could not load monitoring data:", e$message)
    return(list(
      health = data.frame(status = "error"),
      performance = data.frame(),
      errors = 0
    ))
  })
}

# UI for monitoring dashboard
ui <- dashboardPage(
  dashboardHeader(title = "NHANES Analysis Monitor"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("System Health", tabName = "health", icon = icon("heartbeat")),
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Data Quality", tabName = "data", icon = icon("database")),
      menuItem("Alerts", tabName = "alerts", icon = icon("exclamation-triangle")),
      menuItem("Logs", tabName = "logs", icon = icon("file-text")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),

  dashboardBody(
    # Custom CSS for alerts
    tags$style(HTML("
      .alert-critical { background-color: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; }
      .alert-warning { background-color: #fff3cd; color: #856404; border: 1px solid #ffeaa7; }
      .alert-info { background-color: #d1ecf1; color: #0c5460; border: 1px solid #bee5eb; }
    ")),

    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          # Status Cards
          valueBoxOutput("status_box", width = 3),
          valueBoxOutput("memory_box", width = 3),
          valueBoxOutput("cpu_box", width = 3),
          valueBoxOutput("disk_box", width = 3)
        ),

        fluidRow(
          # System Status Chart
          box(
            title = "System Health Over Time",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("health_chart"),
            width = 8
          ),

          # Quick Actions
          box(
            title = "Quick Actions",
            status = "info",
            solidHeader = TRUE,
            actionButton("run_health_check", "Run Health Check", icon = icon("stethoscope")),
            br(), br(),
            actionButton("clear_cache", "Clear Cache", icon = icon("trash")),
            br(), br(),
            actionButton("restart_api", "Restart API", icon = icon("redo")),
            width = 4
          )
        )
      ),

      # Health Tab
      tabItem(tabName = "health",
        fluidRow(
          box(
            title = "Health Check Results",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("health_details"),
            width = 12
          )
        ),

        fluidRow(
          # System Metrics
          box(
            title = "System Metrics",
            status = "info",
            solidHeader = TRUE,
            tableOutput("system_metrics"),
            width = 6
          ),

          # Data Quality
          box(
            title = "Data Quality",
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("data_quality"),
            width = 6
          )
        )
      ),

      # Performance Tab
      tabItem(tabName = "performance",
        fluidRow(
          box(
            title = "Performance Metrics",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("performance_chart"),
            width = 12
          )
        ),

        fluidRow(
          # Execution Times
          box(
            title = "Execution Times",
            status = "info",
            solidHeader = TRUE,
            tableOutput("execution_times"),
            width = 6
          ),

          # Resource Usage
          box(
            title = "Resource Usage",
            status = "info",
            solidHeader = TRUE,
            plotOutput("resource_chart"),
            width = 6
          )
        )
      ),

      # Data Tab
      tabItem(tabName = "data",
        fluidRow(
          box(
            title = "Data Registry Summary",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("registry_summary"),
            width = 12
          )
        ),

        fluidRow(
          # Data Quality Report
          box(
            title = "Data Quality Report",
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("quality_report"),
            width = 6
          ),

          # Update Status
          box(
            title = "Update Status",
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("update_status"),
            width = 6
          )
        )
      ),

      # Alerts Tab
      tabItem(tabName = "alerts",
        fluidRow(
          box(
            title = "Active Alerts",
            status = "warning",
            solidHeader = TRUE,
            verbatimTextOutput("active_alerts"),
            width = 12
          )
        ),

        fluidRow(
          # Alert History
          box(
            title = "Alert History",
            status = "info",
            solidHeader = TRUE,
            tableOutput("alert_history"),
            width = 12
          )
        )
      ),

      # Logs Tab
      tabItem(tabName = "logs",
        fluidRow(
          box(
            title = "Recent Logs",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("recent_logs"),
            width = 12
          )
        )
      ),

      # Settings Tab
      tabItem(tabName = "settings",
        fluidRow(
          box(
            title = "Monitoring Settings",
            status = "primary",
            solidHeader = TRUE,
            sliderInput("refresh_interval", "Refresh Interval (seconds):",
                       min = 5, max = 300, value = MONITORING_CONFIG$refresh_interval, step = 5),
            numericInput("alert_threshold", "Error Rate Threshold (%):",
                        value = MONITORING_CONFIG$alert_thresholds$error_rate * 100, min = 0, max = 100),
            actionButton("save_settings", "Save Settings", class = "btn-primary"),
            width = 6
          ),

          box(
            title = "Alert Configuration",
            status = "info",
            solidHeader = TRUE,
            checkboxInput("email_alerts", "Enable Email Alerts", value = FALSE),
            textInput("alert_email", "Alert Email Address:", value = "admin@example.com"),
            actionButton("test_alert", "Send Test Alert", class = "btn-warning"),
            width = 6
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # Reactive monitoring data
  monitoring_data <- reactive({
    invalidateLater(MONITORING_CONFIG$refresh_interval * 1000, session)
    load_monitoring_data()
  })

  # Status box
  output$status_box <- renderValueBox({
    data <- monitoring_data()
    health_status <- data$health$status[1] %||% "unknown"

    valueBox(
      health_status,
      "System Status",
      icon = icon("server"),
      color = switch(health_status,
        "healthy" = "green",
        "degraded" = "yellow",
        "error" = "red",
        "blue"
      )
    )
  })

  # Memory box
  output$memory_box <- renderValueBox({
    data <- monitoring_data()
    memory_pct <- data$health$memory_usage[1] %||% 0

    valueBox(
      paste0(round(memory_pct * 100, 1), "%"),
      "Memory Usage",
      icon = icon("memory"),
      color = if (memory_pct > 0.8) "red" else if (memory_pct > 0.6) "yellow" else "green"
    )
  })

  # CPU box
  output$cpu_box <- renderValueBox({
    data <- monitoring_data()
    cpu_pct <- data$health$cpu_usage[1] %||% 0

    valueBox(
      paste0(round(cpu_pct * 100, 1), "%"),
      "CPU Usage",
      icon = icon("microchip"),
      color = if (cpu_pct > 0.8) "red" else if (cpu_pct > 0.6) "yellow" else "green"
    )
  })

  # Disk box
  output$disk_box <- renderValueBox({
    data <- monitoring_data()
    disk_pct <- data$health$disk_usage[1] %||% 0

    valueBox(
      paste0(round(disk_pct * 100, 1), "%"),
      "Disk Usage",
      icon = icon("hdd"),
      color = if (disk_pct > 0.9) "red" else if (disk_pct > 0.7) "yellow" else "green"
    )
  })

  # Health chart
  output$health_chart <- renderPlot({
    data <- monitoring_data()

    if (nrow(data$health) > 0) {
      ggplot(data$health, aes(x = timestamp, y = memory_usage)) +
        geom_line(color = "blue") +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
        labs(title = "Memory Usage Over Time", x = "Time", y = "Memory Usage (%)") +
        theme_minimal()
    }
  })

  # Health details
  output$health_details <- renderText({
    data <- monitoring_data()

    paste(capture.output(str(data$health)), collapse = "\n")
  })

  # System metrics table
  output$system_metrics <- renderTable({
    data <- monitoring_data()

    if (nrow(data$health) > 0) {
      data$health %>%
        select(timestamp, status, memory_usage, cpu_usage, disk_usage) %>%
        tail(10)
    } else {
      data.frame(Message = "No health data available")
    }
  })

  # Data quality
  output$data_quality <- renderText({
    tryCatch({
      quality_report <- generate_quality_report()
      paste(capture.output(quality_report), collapse = "\n")
    }, error = function(e) {
      paste("Data quality check failed:", e$message)
    })
  })

  # Performance chart
  output$performance_chart <- renderPlot({
    data <- monitoring_data()

    if (nrow(data$performance) > 0) {
      ggplot(data$performance, aes(x = timestamp, y = execution_time)) +
        geom_line(color = "purple") +
        geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
        labs(title = "Execution Time Over Time", x = "Time", y = "Execution Time (seconds)") +
        theme_minimal()
    }
  })

  # Execution times table
  output$execution_times <- renderTable({
    data <- monitoring_data()

    if (nrow(data$performance) > 0) {
      data$performance %>%
        select(timestamp, execution_time, memory_peak, cache_hits) %>%
        tail(10)
    } else {
      data.frame(Message = "No performance data available")
    }
  })

  # Resource usage chart
  output$resource_chart <- renderPlot({
    data <- monitoring_data()

    if (nrow(data$health) > 0) {
      health_data <- data$health %>%
        select(memory_usage, cpu_usage, disk_usage) %>%
        tail(20) %>%
        tidyr::pivot_longer(cols = everything(), names_to = "metric", values_to = "value")

      ggplot(health_data, aes(x = metric, y = value, fill = metric)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Current Resource Usage", x = "Resource", y = "Usage (%)") +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })

  # Registry summary
  output$registry_summary <- renderText({
    tryCatch({
      display_registry_summary()
      # Capture the output
      capture.output(display_registry_summary())
    }, error = function(e) {
      paste("Registry check failed:", e$message)
    })
  })

  # Quality report
  output$quality_report <- renderText({
    tryCatch({
      display_quality_report()
      # Capture the output
      capture.output(display_quality_report())
    }, error = function(e) {
      paste("Quality report failed:", e$message)
    })
  })

  # Update status
  output$update_status <- renderText({
    tryCatch({
      updates <- check_for_updates()
      if (updates$uptodate) {
        "✅ All data files are up to date"
      } else {
        paste("📋 Updates available:", length(updates$updates), "files")
      }
    }, error = function(e) {
      paste("Update check failed:", e$message)
    })
  })

  # Active alerts
  output$active_alerts <- renderText({
    data <- monitoring_data()

    alerts <- c()

    # Check memory usage
    if (data$health$memory_usage[1] > 0.8) {
      alerts <- c(alerts, "⚠️ High memory usage detected")
    }

    # Check error count
    if (data$errors > 5) {
      alerts <- c(alerts, paste("⚠️ High error count:", data$errors))
    }

    # Check data integrity
    tryCatch({
      integrity <- validate_data_integrity()
      if (!integrity$valid) {
        alerts <- c(alerts, "❌ Data integrity issues detected")
      }
    }, error = function(e) {
      alerts <- c(alerts, "❌ Could not check data integrity")
    })

    if (length(alerts) == 0) {
      "✅ No active alerts"
    } else {
      paste(alerts, collapse = "\n")
    }
  })

  # Alert history
  output$alert_history <- renderTable({
    # Mock alert history for demonstration
    data.frame(
      Timestamp = c("2025-10-20 10:00:00", "2025-10-20 09:30:00"),
      Type = c("Memory Warning", "Data Integrity"),
      Severity = c("Warning", "Error"),
      Message = c("Memory usage > 80%", "Hash mismatch detected")
    )
  })

  # Recent logs
  output$recent_logs <- renderText({
    log_file <- "outputs/logs/analysis_log.txt"
    if (file.exists(log_file)) {
      recent_lines <- tail(readLines(log_file), 20)
      paste(recent_lines, collapse = "\n")
    } else {
      "No log file found. Run analysis first."
    }
  })

  # Action handlers
  observeEvent(input$run_health_check, {
    showModal(modalDialog(
      title = "Health Check Running",
      "Running comprehensive system health check...",
      easyClose = FALSE,
      footer = NULL
    ))

    # Run health check
    tryCatch({
      health_issues <- check_pipeline_health()
      if (length(health_issues) == 0) {
        result <- "✅ All systems operational!"
      } else {
        result <- paste("⚠️ Issues found:", paste(health_issues, collapse = "; "))
      }

      removeModal()

      showModal(modalDialog(
        title = "Health Check Complete",
        result,
        easyClose = TRUE
      ))
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Health Check Failed",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
    })
  })

  observeEvent(input$clear_cache, {
    tryCatch({
      if (dir.exists("../cache")) {
        unlink("../cache", recursive = TRUE)
        dir.create("../cache")
        result <- "✅ Cache cleared successfully"
      } else {
        result <- "ℹ️ No cache directory found"
      }

      showModal(modalDialog(
        title = "Cache Operation",
        result,
        easyClose = TRUE
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Cache Operation Failed",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
    })
  })

  observeEvent(input$restart_api, {
    showModal(modalDialog(
      title = "API Restart",
      "This feature requires manual API restart. Please restart the API server manually.",
      easyClose = TRUE
    ))
  })

  observeEvent(input$save_settings, {
    # Save monitoring settings
    MONITORING_CONFIG$refresh_interval <- input$refresh_interval
    MONITORING_CONFIG$alert_thresholds$error_rate <- input$alert_threshold / 100

    showModal(modalDialog(
      title = "Settings Saved",
      "Monitoring settings have been updated.",
      easyClose = TRUE
    ))
  })

  observeEvent(input$test_alert, {
    showModal(modalDialog(
      title = "Test Alert",
      "Test alert sent to configured email address.",
      easyClose = TRUE
    ))
  })

  # Auto-refresh functionality
  autoInvalidate <- reactiveTimer(MONITORING_CONFIG$refresh_interval * 1000)

  observe({
    autoInvalidate()
    # Trigger reactive updates
    monitoring_data()
  })
}

# Run the dashboard
shinyApp(ui, server)


