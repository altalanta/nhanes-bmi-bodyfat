# Interactive Web Dashboard for NHANES BMI-Body Fat Analysis

#' Create interactive dashboard for NHANES results
#'
#' Launches a Shiny web application for interactive exploration of analysis results.
#'
#' @param results Analysis results from run_nhanes_analysis() or run_optimized_analysis()
#' @param port Port number for the Shiny app (default: 3838)
#' @param launch_browser Whether to launch browser automatically (default: TRUE)
#' @return None (launches Shiny app)
#' @export
create_dashboard <- function(results = NULL, port = 3838, launch_browser = TRUE) {

  # Check if results are provided
  if (is.null(results)) {
    # Try to load results from default location
    results_file <- "outputs/tables/nhanes_analysis_results.rds"
    if (file.exists(results_file)) {
      results <- readRDS(results_file)
    } else {
      stop("No results provided and no results file found. Please run analysis first.")
    }
  }

  # Check if shiny is available
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny package is required for the dashboard. Please install it.")
  }

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Plotly package is required for interactive plots. Please install it.")
  }

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("DT package is required for interactive tables. Please install it.")
  }

  # Extract data from results
  correlations <- results$correlations
  validation_results <- results$validation_results
  config <- results$config

  # Create Shiny app
  shiny_app <- shiny::shinyApp(
    ui = dashboard_ui(correlations, validation_results, config),
    server = dashboard_server(correlations, validation_results, config, results)
  )

  # Run the app
  shiny::runApp(shiny_app, port = port, launch.browser = launch_browser)
}

#' Create dashboard user interface
#'
#' @param correlations Correlation results
#' @param validation_results Validation results
#' @param config Configuration
#' @return Shiny UI
dashboard_ui <- function(correlations, validation_results, config) {
  shiny::fluidPage(
    shiny::titlePanel("NHANES BMI vs Body Fat Analysis Dashboard"),

    shiny::navbarPage("NHANES Analysis",
      shiny::tabPanel("Overview",
        shiny::fluidRow(
          shiny::column(4,
            shiny::wellPanel(
              shiny::h4("Analysis Summary"),
              shiny::p(paste("Sample Size:", correlations$sample_size)),
              shiny::p(paste("Analysis Date:", format(results$timestamp, "%Y-%m-%d %H:%M"))),
              shiny::p(paste("Validation Status:",
                validation_results$overall_status)),
              if (!is.null(results$performance_info)) {
                shiny::p(paste("Performance: Cache",
                  if (results$performance_info$used_cache) "enabled" else "disabled"))
              }
            )
          ),
          shiny::column(8,
            plotly::plotlyOutput("correlation_plot", height = "400px")
          )
        )
      ),

      shiny::tabPanel("Correlations",
        shiny::fluidRow(
          shiny::column(6,
            shiny::h4("Overall Correlation"),
            plotly::plotlyOutput("overall_corr_plot"),
            shiny::verbatimTextOutput("overall_stats")
          ),
          shiny::column(6,
            shiny::h4("By Sex Comparison"),
            plotly::plotlyOutput("sex_corr_plot"),
            shiny::verbatimTextOutput("sex_stats")
          )
        )
      ),

      shiny::tabPanel("Data Quality",
        shiny::fluidRow(
          shiny::column(12,
            DT::dataTableOutput("quality_summary")
          )
        ),
        shiny::fluidRow(
          shiny::column(6,
            plotly::plotlyOutput("missing_data_plot")
          ),
          shiny::column(6,
            plotly::plotlyOutput("outlier_summary_plot")
          )
        )
      ),

      shiny::tabPanel("Data Explorer",
        shiny::fluidRow(
          shiny::column(3,
            shiny::selectInput("dataset_select", "Select Dataset:",
                             choices = c("DEMO", "BMX", "DXX", "Complete"))
          ),
          shiny::column(3,
            shiny::selectInput("variable_select", "Select Variable:",
                             choices = NULL)  # Will be populated dynamically
          ),
          shiny::column(3,
            shiny::selectInput("plot_type", "Plot Type:",
                             choices = c("Histogram", "Box Plot", "Scatter Plot"))
          ),
          shiny::column(3,
            shiny::actionButton("generate_plot", "Generate Plot")
          )
        ),
        shiny::fluidRow(
          shiny::column(12,
            plotly::plotlyOutput("data_plot", height = "500px")
          )
        )
      ),

      shiny::tabPanel("Export",
        shiny::fluidRow(
          shiny::column(12,
            shiny::h4("Export Options"),
            shiny::p("Download analysis results and visualizations."),
            shiny::downloadButton("download_results", "Download Results (RDS)"),
            shiny::downloadButton("download_summary", "Download Summary Report (HTML)"),
            shiny::downloadButton("download_plots", "Download All Plots (ZIP)")
          )
        )
      )
    )
  )
}

#' Create dashboard server logic
#'
#' @param correlations Correlation results
#' @param validation_results Validation results
#' @param config Configuration
#' @param results Full results object
#' @return Shiny server function
dashboard_server <- function(correlations, validation_results, config, results) {
  function(input, output, session) {

    # Reactive data for different datasets
    dataset_data <- shiny::reactive({
      switch(input$dataset_select,
        "DEMO" = results$analytic_data %>% select(SEQN, RIDAGEYR, RIAGENDR),
        "BMX" = results$analytic_data %>% select(SEQN, BMXBMI),
        "DXX" = results$analytic_data %>% select(SEQN, bodyfat_pct),
        "Complete" = results$analytic_data
      )
    })

    # Update variable selection based on dataset
    shiny::observe({
      data <- dataset_data()
      if (!is.null(data)) {
        choices <- names(data)
        shiny::updateSelectInput(session, "variable_select", choices = choices)
      }
    })

    # Correlation overview plot
    output$correlation_plot <- plotly::renderPlotly({
      # Create scatter plot of BMI vs Body Fat
      plot_data <- results$analytic_data %>%
        mutate(sex_label = ifelse(RIAGENDR == 1, "Male", "Female"))

      plotly::plot_ly(data = plot_data, x = ~BMXBMI, y = ~bodyfat_pct,
                     color = ~sex_label, type = "scatter",
                     mode = "markers", alpha = 0.6,
                     text = ~paste("Age:", RIDAGEYR, "<br>BMI:", round(BMXBMI, 1)),
                     hovertemplate = paste(
                       "<b>%{text}</b><br>",
                       "Body Fat: %{y:.1f}%<br>",
                       "<extra></extra>"
                     )) %>%
        plotly::layout(
          title = "BMI vs Body Fat Percentage by Sex",
          xaxis = list(title = "BMI (kg/m²)"),
          yaxis = list(title = "Body Fat (%)"),
          legend = list(title = list(text = "Sex"))
        )
    })

    # Overall correlation statistics
    output$overall_stats <- shiny::renderPrint({
      cat("Overall Correlation Results:\n")
      cat(sprintf("Correlation: %.3f\n", correlations$overall$correlation))
      cat(sprintf("95%% CI: [%.3f, %.3f]\n",
                  correlations$overall$ci_lower,
                  correlations$overall$ci_upper))
      cat(sprintf("Standard Error: %.4f\n", correlations$overall$se))
    })

    # Sex-specific correlation statistics
    output$sex_stats <- shiny::renderPrint({
      cat("Sex-Specific Correlation Results:\n\n")
      cat("Males:\n")
      cat(sprintf("  Correlation: %.3f\n", correlations$male$correlation))
      cat(sprintf("  Sample Size: %d\n", correlations$male$n_obs))
      cat("\nFemales:\n")
      cat(sprintf("  Correlation: %.3f\n", correlations$female$correlation))
      cat(sprintf("  Sample Size: %d\n", correlations$female$n_obs))
    })

    # Overall correlation trend plot
    output$overall_corr_plot <- plotly::renderPlotly({
      # Simple correlation visualization
      data.frame(
        x = c("BMI", "Body Fat"),
        y = c(correlations$overall$correlation, 1)
      ) %>%
        plotly::plot_ly(x = ~x, y = ~y, type = "bar",
                       marker = list(color = c("#1f77b4", "#ff7f0e"))) %>%
        plotly::layout(
          title = "Overall Correlation Strength",
          yaxis = list(title = "Correlation Coefficient", range = c(0, 1))
        )
    })

    # Sex comparison plot
    output$sex_corr_plot <- plotly::renderPlotly({
      data.frame(
        Sex = c("Male", "Female"),
        Correlation = c(correlations$male$correlation, correlations$female$correlation)
      ) %>%
        plotly::plot_ly(x = ~Sex, y = ~Correlation, type = "bar",
                       marker = list(color = c("#2ca02c", "#d62728"))) %>%
        plotly::layout(
          title = "BMI-Body Fat Correlation by Sex",
          yaxis = list(title = "Correlation Coefficient")
        )
    })

    # Data quality summary table
    output$quality_summary <- DT::renderDataTable({
      quality_data <- data.frame(
        Dataset = names(validation_results$quality_reports),
        Status = sapply(validation_results$quality_reports,
                       function(x) x$validation_status),
        Observations = sapply(validation_results$quality_reports,
                             function(x) x$basic_metrics$n_rows),
        Variables = sapply(validation_results$quality_reports,
                          function(x) x$basic_metrics$n_cols),
        Missing_Rate = paste0(
          round(sapply(validation_results$quality_reports,
                      function(x) x$basic_metrics$missing_rate), 2), "%")
      )

      DT::datatable(quality_data,
                   options = list(pageLength = 10, searchHighlight = TRUE),
                   rownames = FALSE) %>%
        DT::formatStyle("Status",
                       backgroundColor = DT::styleEqual(
                         c("PASSED", "WARNING", "FAILED"),
                         c("#d4edda", "#fff3cd", "#f8d7da")
                       ))
    })

    # Missing data visualization
    output$missing_data_plot <- plotly::renderPlotly({
      missing_summary <- data.frame(
        Dataset = names(validation_results$quality_reports),
        Missing_Rate = round(sapply(validation_results$quality_reports,
                                   function(x) x$basic_metrics$missing_rate), 2)
      )

      plotly::plot_ly(missing_summary, x = ~Dataset, y = ~Missing_Rate,
                     type = "bar", marker = list(color = "#17becf")) %>%
        plotly::layout(
          title = "Missing Data Rate by Dataset",
          yaxis = list(title = "Missing Rate (%)")
        )
    })

    # Outlier summary visualization
    output$outlier_summary_plot <- plotly::renderPlotly({
      outlier_data <- data.frame()

      for (report in validation_results$quality_reports) {
        for (var_name in names(report$outlier_summary)) {
          outlier_info <- report$outlier_summary[[var_name]]
          outlier_data <- rbind(outlier_data, data.frame(
            Dataset = report$dataset_name,
            Variable = var_name,
            Outlier_Percentage = outlier_info$outlier_pct
          ))
        }
      }

      plotly::plot_ly(outlier_data, x = ~Variable, y = ~Outlier_Percentage,
                     color = ~Dataset, type = "bar") %>%
        plotly::layout(
          title = "Outlier Percentage by Variable and Dataset",
          yaxis = list(title = "Outlier Percentage (%)"),
          xaxis = list(tickangle = -45)
        )
    })

    # Dynamic data exploration plot
    output$data_plot <- plotly::renderPlotly({
      input$generate_plot

      isolate({
        data <- dataset_data()
        variable <- input$variable_select
        plot_type <- input$plot_type

        if (is.null(data) || is.null(variable) || nrow(data) == 0) {
          return(plotly::plotly_empty())
        }

        if (plot_type == "Histogram") {
          plotly::plot_ly(data = data, x = as.formula(paste0("~", variable)),
                         type = "histogram", nbinsx = 30) %>%
            plotly::layout(
              title = paste("Distribution of", variable, "in", input$dataset_select),
              xaxis = list(title = variable),
              yaxis = list(title = "Frequency")
            )
        } else if (plot_type == "Box Plot") {
          plotly::plot_ly(data = data, y = as.formula(paste0("~", variable)),
                         type = "box") %>%
            plotly::layout(
              title = paste("Box Plot of", variable, "in", input$dataset_select),
              yaxis = list(title = variable)
            )
        } else {
          # Scatter plot - need two variables
          other_vars <- setdiff(names(data), c(variable, "SEQN"))
          if (length(other_vars) > 0) {
            other_var <- other_vars[1]  # Use first available variable
            plotly::plot_ly(data = data,
                           x = as.formula(paste0("~", variable)),
                           y = as.formula(paste0("~", other_var)),
                           type = "scatter", mode = "markers") %>%
              plotly::layout(
                title = paste(variable, "vs", other_var, "in", input$dataset_select),
                xaxis = list(title = variable),
                yaxis = list(title = other_var)
              )
          } else {
            plotly::plot_ly(data = data, x = as.formula(paste0("~", variable)),
                           type = "histogram") %>%
              plotly::layout(title = "No other variables available for scatter plot")
          }
        }
      })
    })

    # Export handlers
    output$download_results <- shiny::downloadHandler(
      filename = function() {
        paste("nhanes_results_", format(Sys.Date(), "%Y%m%d"), ".rds", sep = "")
      },
      content = function(file) {
        saveRDS(results, file)
      }
    )

    output$download_summary <- shiny::downloadHandler(
      filename = function() {
        paste("nhanes_summary_", format(Sys.Date(), "%Y%m%d"), ".html", sep = "")
      },
      content = function(file) {
        # Create summary report
        summary_html <- paste0("
        <!DOCTYPE html>
        <html>
        <head><title>NHANES Analysis Summary</title></head>
        <body>
          <h1>NHANES BMI-Body Fat Analysis Summary</h1>
          <h2>Correlation Results</h2>
          <p><strong>Overall Correlation:</strong> ", round(correlations$overall$correlation, 3), "</p>
          <p><strong>95% CI:</strong> [", round(correlations$overall$ci_lower, 3), ", ", round(correlations$overall$ci_upper, 3), "]</p>
          <h2>Sample Information</h2>
          <p><strong>Total Sample Size:</strong> ", correlations$sample_size, "</p>
          <p><strong>Male Sample Size:</strong> ", correlations$male$n_obs, "</p>
          <p><strong>Female Sample Size:</strong> ", correlations$female$n_obs, "</p>
          <h2>Data Quality</h2>
          <p><strong>Overall Status:</strong> ", validation_results$overall_status, "</p>
        </body>
        </html>
        ")
        writeLines(summary_html, file)
      }
    )

    output$download_plots <- shiny::downloadHandler(
      filename = function() {
        paste("nhanes_plots_", format(Sys.Date(), "%Y%m%d"), ".zip", sep = "")
      },
      content = function(file) {
        # Create temporary directory for plots
        temp_dir <- tempdir()
        plot_files <- c()

        # Save some key plots
        # This is a simplified version - in practice you'd save multiple plots
        htmlwidgets::saveWidget(
          plotly::ggplotly(ggplot2::ggplot(results$analytic_data,
                                          ggplot2::aes(x = BMXBMI, y = bodyfat_pct)) +
                          ggplot2::geom_point(alpha = 0.6) +
                          ggplot2::facet_wrap(~RIAGENDR) +
                          ggplot2::labs(title = "BMI vs Body Fat by Sex")),
          file.path(temp_dir, "correlation_plot.html")
        )

        # Create zip file
        zip::zip(file, files = list.files(temp_dir, full.names = TRUE), mode = "cherry-pick")
      },
      contentType = "application/zip"
    )
  }
}

#' Launch dashboard from results file
#'
#' Convenience function to launch dashboard from a saved results file.
#'
#' @param results_file Path to results RDS file
#' @param port Port number for Shiny app
#' @param launch_browser Whether to launch browser automatically
#' @return None
#' @export
launch_dashboard <- function(results_file = "outputs/tables/nhanes_analysis_results.rds",
                           port = 3838, launch_browser = TRUE) {
  if (!file.exists(results_file)) {
    stop(paste("Results file not found:", results_file))
  }

  results <- readRDS(results_file)
  create_dashboard(results, port, launch_browser)
}

#' Create standalone dashboard app
#'
#' Creates a self-contained Shiny app that can be deployed.
#'
#' @param results_file Path to results RDS file
#' @param output_dir Directory to save the app
#' @return Path to the created app directory
#' @export
create_standalone_app <- function(results_file = "outputs/tables/nhanes_analysis_results.rds",
                                output_dir = "nhanes_dashboard") {

  if (!file.exists(results_file)) {
    stop(paste("Results file not found:", results_file))
  }

  # Create app directory
  app_dir <- file.path(output_dir, "app")
  dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy results file
  file.copy(results_file, file.path(app_dir, "results.rds"))

  # Create app.R
  app_content <- paste0('
library(shiny)
library(plotly)
library(DT)
library(ggplot2)

# Load results
results <- readRDS("results.rds")

# Extract data from results
correlations <- results$correlations
validation_results <- results$validation_results
config <- results$config

# Dashboard UI (same as above)
ui <- fluidPage(
  titlePanel("NHANES BMI vs Body Fat Analysis Dashboard"),
  navbarPage("NHANES Analysis",
    tabPanel("Overview",
      fluidRow(
        column(4,
          wellPanel(
            h4("Analysis Summary"),
            p(paste("Sample Size:", correlations$sample_size)),
            p(paste("Analysis Date:", format(results$timestamp, "%Y-%m-%d %H:%M"))),
            p(paste("Validation Status:", validation_results$overall_status))
          )
        ),
        column(8,
          plotlyOutput("correlation_plot", height = "400px")
        )
      )
    ),
    tabPanel("Export",
      fluidRow(
        column(12,
          h4("Export Options"),
          p("Download analysis results and visualizations."),
          downloadButton("download_results", "Download Results (RDS)")
        )
      )
    )
  )
)

# Dashboard server (simplified)
server <- function(input, output) {
  output$correlation_plot <- renderPlotly({
    plot_data <- results$analytic_data %>%
      mutate(sex_label = ifelse(RIAGENDR == 1, "Male", "Female"))

    plot_ly(data = plot_data, x = ~BMXBMI, y = ~bodyfat_pct,
            color = ~sex_label, type = "scatter", mode = "markers", alpha = 0.6) %>%
      layout(
        title = "BMI vs Body Fat Percentage by Sex",
        xaxis = list(title = "BMI (kg/m²)"),
        yaxis = list(title = "Body Fat (%)")
      )
  })

  output$download_results <- downloadHandler(
    filename = function() {
      paste("nhanes_results_", format(Sys.Date(), "%Y%m%d"), ".rds", sep = "")
    },
    content = function(file) {
      file.copy("results.rds", file)
    }
  )
}

shinyApp(ui = ui, server = server)
')

  writeLines(app_content, file.path(app_dir, "app.R"))

  # Create README
  readme_content <- paste0('
# NHANES BMI-Body Fat Analysis Dashboard

This is a standalone Shiny application for exploring NHANES BMI-body fat analysis results.

## Running the App

From R:
```r
shiny::runApp("app")
```

Or deploy to shinyapps.io, RStudio Connect, or any Shiny server.

## Features

- Interactive correlation plots
- Data quality summaries
- Export capabilities
- Responsive design

Generated on: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '
')

  writeLines(readme_content, file.path(output_dir, "README.md"))

  safe_log(paste("Standalone dashboard created in:", output_dir), "INFO")
  return(output_dir)
}
