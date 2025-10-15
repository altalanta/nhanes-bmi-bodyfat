# NHANES BMI Body Fat Analysis - Shiny Dashboard
# Interactive dashboard for exploring BMI-body fat relationships

#' @title NHANES BMI Body Fat Shiny Dashboard
#' @description Interactive Shiny dashboard for NHANES BMI-body fat analysis
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import dplyr
#' @import DT
#' @import plotly
#' @import readr
#' @import tidyr
#' @export

# Load configuration and utilities
source("scripts/load_config.R")
source("scripts/error_handling.R")

# Global data cache
dashboard_data <- NULL

# Load dashboard data
load_dashboard_data <- function() {
  tryCatch({
    # Load main analysis results
    corr_file <- file.path("outputs/tables", "corr_bmi_bodyfat_overall_and_by_sex.csv")
    bodyfat_file <- file.path("outputs/tables", "bodyfat_by_bmi_class_by_sex.csv")
    pop_file <- file.path("outputs/tables", "population_counts_by_group.csv")

    if (file.exists(corr_file) && file.exists(bodyfat_file) && file.exists(pop_file)) {
      correlations <- read_csv(corr_file, show_col_types = FALSE)
      bodyfat_data <- read_csv(bodyfat_file, show_col_types = FALSE)
      population <- read_csv(pop_file, show_col_types = FALSE)

      # Load ML results if available
      ml_file <- file.path("outputs/tables", "ml_analysis_results.rds")
      ml_results <- NULL
      if (file.exists(ml_file)) {
        ml_results <- readRDS(ml_file)
      }

      return(list(
        correlations = correlations,
        bodyfat = bodyfat_data,
        population = population,
        ml_results = ml_results,
        last_updated = Sys.time()
      ))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    warning("Error loading dashboard data:", e$message)
    return(NULL)
  })
}

# UI function
ui <- dashboardPage(
  dashboardHeader(title = "NHANES BMI-Body Fat Analysis"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Correlations", tabName = "correlations", icon = icon("chart-line")),
      menuItem("Body Fat by BMI", tabName = "bodyfat_bmi", icon = icon("chart-bar")),
      menuItem("Population Analysis", tabName = "population", icon = icon("users")),
      menuItem("ML Predictions", tabName = "ml_predictions", icon = icon("robot")),
      menuItem("Data Tables", tabName = "data_tables", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),

    # Filters sidebar
    conditionalPanel(
      condition = "input.tab != 'about'",
      hr(),
      h4("Filters"),
      selectInput("sex_filter", "Sex:",
                  choices = c("All", "Male", "Female"),
                  selected = "All"),
      selectInput("bmi_cat_filter", "BMI Category:",
                  choices = c("All", "Underweight", "Normal", "Overweight",
                            "Obesity I", "Obesity II", "Obesity III"),
                  selected = "All"),
      sliderInput("age_range", "Age Range:",
                  min = 20, max = 59, value = c(20, 59))
    )
  ),

  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_sample_box"),
          valueBoxOutput("overall_corr_box"),
          valueBoxOutput("ml_performance_box")
        ),

        fluidRow(
          box(
            title = "BMI vs Body Fat Relationship",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("main_plot", height = 400),
            width = 8
          ),

          box(
            title = "Key Findings",
            status = "info",
            solidHeader = TRUE,
            htmlOutput("key_findings"),
            width = 4
          )
        ),

        fluidRow(
          box(
            title = "Correlation by Sex",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("corr_by_sex_plot", height = 300),
            width = 6
          ),

          box(
            title = "Sample Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("sample_dist_plot", height = 300),
            width = 6
          )
        )
      ),

      # Correlations tab
      tabItem(tabName = "correlations",
        fluidRow(
          box(
            title = "Correlation Analysis",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("correlations_table"),
            width = 12
          )
        ),

        fluidRow(
          box(
            title = "Correlation Heatmap",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("corr_heatmap", height = 400),
            width = 12
          )
        )
      ),

      # Body Fat by BMI tab
      tabItem(tabName = "bodyfat_bmi",
        fluidRow(
          box(
            title = "Body Fat by BMI Category",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("bodyfat_bmi_plot", height = 400),
            width = 8
          ),

          box(
            title = "Statistics Summary",
            status = "info",
            solidHeader = TRUE,
            DT::dataTableOutput("bodyfat_stats"),
            width = 4
          )
        ),

        fluidRow(
          box(
            title = "Distribution by Sex",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("bodyfat_sex_plot", height = 400),
            width = 12
          )
        )
      ),

      # Population tab
      tabItem(tabName = "population",
        fluidRow(
          box(
            title = "Population Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("pop_dist_plot", height = 400),
            width = 8
          ),

          box(
            title = "Population Statistics",
            status = "info",
            solidHeader = TRUE,
            DT::dataTableOutput("pop_stats"),
            width = 4
          )
        ),

        fluidRow(
          box(
            title = "Demographic Breakdown",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("demo_breakdown", height = 400),
            width = 12
          )
        )
      ),

      # ML Predictions tab
      tabItem(tabName = "ml_predictions",
        fluidRow(
          conditionalPanel(
            condition = "output.ml_available",
            box(
              title = "Model Performance",
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("ml_performance_plot", height = 300),
              width = 6
            ),

            box(
              title = "Feature Importance",
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("feature_importance_plot", height = 300),
              width = 6
            )
          ),

          conditionalPanel(
            condition = "!output.ml_available",
            box(
              title = "Machine Learning Analysis",
              status = "warning",
              solidHeader = TRUE,
              p("Machine learning analysis is not yet available. Run 'make advanced' to generate ML results."),
              actionButton("run_ml", "Run ML Analysis", class = "btn-primary"),
              width = 12
            )
          )
        ),

        conditionalPanel(
          condition = "output.ml_available",
          fluidRow(
            box(
              title = "Prediction Interface",
              status = "primary",
              solidHeader = TRUE,
              fluidRow(
                column(4,
                  numericInput("pred_bmi", "BMI:", value = 25, min = 15, max = 50),
                  selectInput("pred_sex", "Sex:", choices = c("Male", "Female"), selected = "Male"),
                  numericInput("pred_age", "Age:", value = 40, min = 20, max = 59)
                ),
                column(8,
                  verbatimTextOutput("prediction_result"),
                  plotlyOutput("prediction_plot", height = 200)
                )
              ),
              width = 12
            )
          )
        )
      ),

      # Data Tables tab
      tabItem(tabName = "data_tables",
        fluidRow(
          tabBox(
            title = "Raw Data Tables",
            width = 12,
            tabPanel("Correlations", DT::dataTableOutput("raw_corr_table")),
            tabPanel("Body Fat Data", DT::dataTableOutput("raw_bodyfat_table")),
            tabPanel("Population", DT::dataTableOutput("raw_pop_table"))
          )
        )
      ),

      # About tab
      tabItem(tabName = "about",
        fluidRow(
          box(
            title = "About This Dashboard",
            status = "primary",
            solidHeader = TRUE,
            p("This interactive dashboard provides comprehensive exploration of the relationship between Body Mass Index (BMI) and whole-body percent body fat using NHANES 2017-2018 data."),
            p("The analysis includes:"),
            tags$ul(
              tags$li("Survey-weighted statistical analysis"),
              tags$li("Machine learning predictions"),
              tags$li("Interactive visualizations"),
              tags$li("Population demographics"),
              tags$li("REST API integration")
            ),
            width = 8
          ),

          box(
            title = "Data Sources",
            status = "info",
            solidHeader = TRUE,
            p("NHANES 2017-2018 datasets:"),
            tags$ul(
              tags$li("DEMO_J: Demographics"),
              tags$li("BMX_J: Body Measures"),
              tags$li("DXX_J: DXA Whole Body"),
              tags$li("DXXAG_J: DXA Android/Gynoid")
            ),
            p("Target population: U.S. civilian non-institutionalized adults aged 20-59 years."),
            width = 4
          )
        ),

        fluidRow(
          box(
            title = "Technical Details",
            status = "primary",
            solidHeader = TRUE,
            p("Built with:"),
            tags$ul(
              tags$li("R programming language"),
              tags$li("Shiny framework"),
              tags$li("ggplot2 & plotly for visualizations"),
              tags$li("Survey package for weighted analysis"),
              tags$li("Machine learning with caret, randomForest, XGBoost")
            ),
            width = 12
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {

  # Load data reactively
  data <- reactive({
    if (is.null(dashboard_data)) {
      dashboard_data <<- load_dashboard_data()
    }
    dashboard_data
  })

  # Value boxes
  output$total_sample_box <- renderValueBox({
    if (is.null(data())) return(valueBox("N/A", "Sample Size", color = "blue"))

    total_n <- sum(data()$population$pop_total, na.rm = TRUE)
    valueBox(
      format(total_n, big.mark = ","),
      "Total Sample Size",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$overall_corr_box <- renderValueBox({
    if (is.null(data())) return(valueBox("N/A", "Overall Correlation", color = "green"))

    overall_corr <- data()$correlations %>%
      filter(group == "Overall") %>%
      pull(correlation) %>%
      round(3)

    valueBox(
      overall_corr,
      "Overall BMI-Body Fat Correlation",
      icon = icon("chart-line"),
      color = "green"
    )
  })

  output$ml_performance_box <- renderValueBox({
    if (is.null(data()) || is.null(data()$ml_results)) {
      return(valueBox("Run ML", "Best ML Model", color = "orange"))
    }

    best_model <- data()$ml_results$best_model
    best_r2 <- data()$ml_results$comparison %>%
      filter(Method == best_model) %>%
      pull(Test_R2) %>%
      round(3)

    valueBox(
      paste(best_model, "(R² =", best_r2, ")"),
      "Best ML Model Performance",
      icon = icon("robot"),
      color = "purple"
    )
  })

  # Main plot
  output$main_plot <- renderPlotly({
    if (is.null(data())) return(plotly_empty())

    # Filter data based on inputs
    filtered_data <- data()$bodyfat %>%
      filter(
        if (input$sex_filter != "All") sex == input$sex_filter else TRUE,
        if (input$bmi_cat_filter != "All") bmi_cat == input$bmi_cat_filter else TRUE
      )

    # Create scatter plot
    p <- ggplot(filtered_data, aes(x = bmi, y = mean_bodyfat, color = sex)) +
      geom_point(alpha = 0.7, size = 3) +
      geom_errorbar(aes(ymin = mean_ci_lower, ymax = mean_ci_upper), width = 0.5) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
      facet_wrap(~bmi_cat, scales = "free_x") +
      labs(
        title = "BMI vs Body Fat Percentage by BMI Category",
        x = "Body Mass Index (BMI)",
        y = "Body Fat Percentage (%)",
        color = "Sex"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    ggplotly(p) %>% layout(height = 400)
  })

  # Correlation by sex plot
  output$corr_by_sex_plot <- renderPlotly({
    if (is.null(data())) return(plotly_empty())

    corr_data <- data()$correlations %>% filter(group != "Overall")

    p <- ggplot(corr_data, aes(x = group, y = correlation, fill = group)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
      labs(
        title = "BMI-Body Fat Correlation by Sex",
        x = "Sex",
        y = "Correlation Coefficient"
      ) +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p) %>% layout(height = 300)
  })

  # Sample distribution plot
  output$sample_dist_plot <- renderPlotly({
    if (is.null(data())) return(plotly_empty())

    pop_data <- data()$population %>%
      group_by(bmi_cat, sex) %>%
      summarise(total = sum(pop_total, na.rm = TRUE)) %>%
      ungroup()

    p <- ggplot(pop_data, aes(x = bmi_cat, y = total, fill = sex)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      labs(
        title = "Population Distribution by BMI Category and Sex",
        x = "BMI Category",
        y = "Population Count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p) %>% layout(height = 300)
  })

  # Key findings HTML
  output$key_findings <- renderUI({
    if (is.null(data())) return(HTML("<p>Data not available</p>"))

    findings <- c(
      paste("Overall correlation: ",
            round(filter(data()$correlations, group == "Overall")$correlation, 3)),
      paste("Male correlation: ",
            round(filter(data()$correlations, group == "Male")$correlation, 3)),
      paste("Female correlation: ",
            round(filter(data()$correlations, group == "Female")$correlation, 3)),
      paste("Sample size: ", format(sum(data()$population$pop_total, na.rm = TRUE), big.mark = ",")),
      "Significant non-linearity detected in BMI-body fat relationship"
    )

    HTML(paste("<ul><li>", paste(findings, collapse = "</li><li>"), "</li></ul>", sep = ""))
  })

  # Correlations table
  output$correlations_table <- DT::renderDataTable({
    if (is.null(data())) return(data.frame())

    data()$correlations %>%
      mutate(
        correlation = round(correlation, 3),
        ci_lower = round(ci_lower, 3),
        ci_upper = round(ci_upper, 3)
      )
  })

  # Body fat BMI plot
  output$bodyfat_bmi_plot <- renderPlotly({
    if (is.null(data())) return(plotly_empty())

    filtered_data <- data()$bodyfat %>%
      filter(
        if (input$sex_filter != "All") sex == input$sex_filter else TRUE
      )

    p <- ggplot(filtered_data, aes(x = bmi_cat, y = mean_bodyfat, fill = sex)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_ci_lower, ymax = mean_ci_upper),
                   position = position_dodge(width = 0.9), width = 0.25) +
      labs(
        title = "Mean Body Fat Percentage by BMI Category",
        x = "BMI Category",
        y = "Body Fat Percentage (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p) %>% layout(height = 400)
  })

  # Body fat statistics
  output$bodyfat_stats <- DT::renderDataTable({
    if (is.null(data())) return(data.frame())

    data()$bodyfat %>%
      filter(
        if (input$sex_filter != "All") sex == input$sex_filter else TRUE,
        if (input$bmi_cat_filter != "All") bmi_cat == input$bmi_cat_filter else TRUE
      ) %>%
      group_by(bmi_cat, sex) %>%
      summarise(
        n = mean(n_unweighted),
        mean_bodyfat = mean(mean_bodyfat),
        ci_lower = mean(mean_ci_lower),
        ci_upper = mean(mean_ci_upper)
      ) %>%
      mutate(
        mean_bodyfat = round(mean_bodyfat, 2),
        ci_lower = round(ci_lower, 2),
        ci_upper = round(ci_upper, 2)
      )
  })

  # Population plots
  output$pop_dist_plot <- renderPlotly({
    if (is.null(data())) return(plotly_empty())

    pop_data <- data()$population %>%
      group_by(bmi_cat) %>%
      summarise(total = sum(pop_total, na.rm = TRUE))

    p <- ggplot(pop_data, aes(x = bmi_cat, y = total)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      labs(
        title = "Population Distribution by BMI Category",
        x = "BMI Category",
        y = "Population Count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p) %>% layout(height = 400)
  })

  # Population statistics
  output$pop_stats <- DT::renderDataTable({
    if (is.null(data())) return(data.frame())

    data()$population %>%
      group_by(bmi_cat, sex) %>%
      summarise(
        unweighted_n = mean(n_unweighted),
        weighted_total = mean(pop_total)
      ) %>%
      mutate(
        weighted_total = round(weighted_total, 0)
      )
  })

  # ML performance plot
  output$ml_performance_plot <- renderPlotly({
    if (is.null(data()) || is.null(data()$ml_results)) return(plotly_empty())

    perf_data <- data()$ml_results$comparison %>%
      select(Method, Test_RMSE, Test_R2) %>%
      gather(metric, value, -Method)

    p <- ggplot(perf_data, aes(x = Method, y = value, fill = metric)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      facet_wrap(~metric, scales = "free_y") +
      labs(
        title = "Machine Learning Model Performance",
        x = "Model",
        y = "Performance Metric"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p) %>% layout(height = 300)
  })

  # Feature importance plot
  output$feature_importance_plot <- renderPlotly({
    if (is.null(data()) || is.null(data()$ml_results)) return(plotly_empty())

    best_model <- data()$ml_results$best_model

    if (best_model == "Random Forest") {
      importance_data <- data.frame(
        Feature = rownames(data()$ml_results$models$random_forest$importance),
        Importance = data()$ml_results$models$random_forest$importance[, 1]
      )
    } else if (best_model == "XGBoost") {
      importance_data <- xgb.importance(model = data()$ml_results$models$xgboost$model)
    } else {
      return(plotly_empty())
    }

    p <- ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      coord_flip() +
      labs(
        title = paste("Feature Importance -", best_model),
        x = "Feature",
        y = "Importance"
      ) +
      theme_minimal()

    ggplotly(p) %>% layout(height = 300)
  })

  # Prediction interface
  output$prediction_result <- renderText({
    if (is.null(data()) || is.null(data()$ml_results)) {
      return("Machine learning results not available. Run 'make advanced' first.")
    }

    # Get prediction
    new_data <- data.frame(
      bmi = input$pred_bmi,
      age = input$pred_age,
      sex = input$pred_sex,
      race_ethnicity = "Non-Hispanic White"  # Default for prediction
    )

    tryCatch({
      prediction <- predict_bodyfat(data()$ml_results, new_data)
      paste("Predicted Body Fat %:", round(prediction, 2))
    }, error = function(e) {
      paste("Prediction error:", e$message)
    })
  })

  # Prediction plot
  output$prediction_plot <- renderPlotly({
    if (is.null(data()) || is.null(data()$ml_results)) return(plotly_empty())

    # Create prediction data
    bmi_range <- seq(15, 50, by = 1)
    pred_data <- data.frame(
      bmi = bmi_range,
      age = input$pred_age,
      sex = input$pred_sex,
      race_ethnicity = "Non-Hispanic White"
    )

    predictions <- predict_bodyfat(data()$ml_results, pred_data)

    pred_df <- data.frame(bmi = bmi_range, predicted = predictions)

    p <- ggplot(pred_df, aes(x = bmi, y = predicted)) +
      geom_line(color = "red", size = 2) +
      geom_point(data = data.frame(bmi = input$pred_bmi, predicted = predict_bodyfat(data()$ml_results,
        data.frame(bmi = input$pred_bmi, age = input$pred_age, sex = input$pred_sex, race_ethnicity = "Non-Hispanic White"))),
        aes(x = bmi, y = predicted), color = "blue", size = 4) +
      labs(
        title = paste("Body Fat Prediction for", input$pred_sex, "Age", input$pred_age),
        x = "BMI",
        y = "Predicted Body Fat %"
      ) +
      theme_minimal()

    ggplotly(p) %>% layout(height = 200)
  })

  # Raw data tables
  output$raw_corr_table <- DT::renderDataTable({
    if (is.null(data())) return(data.frame())
    data()$correlations
  })

  output$raw_bodyfat_table <- DT::renderDataTable({
    if (is.null(data())) return(data.frame())
    data()$bodyfat
  })

  output$raw_pop_table <- DT::renderDataTable({
    if (is.null(data())) return(data.frame())
    data()$population
  })

  # ML availability check
  output$ml_available <- reactive({
    !is.null(data()) && !is.null(data()$ml_results)
  })

  outputOptions(output, "ml_available", suspendWhenHidden = FALSE)

  # Run ML analysis button
  observeEvent(input$run_ml, {
    showModal(modalDialog(
      title = "Running ML Analysis",
      "This may take several minutes...",
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch({
      # Run ML analysis
      source("scripts/advanced_ml_analysis.R")

      # Reload data
      dashboard_data <<- load_dashboard_data()

      removeModal()
      showNotification("ML analysis completed!", type = "message")
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

# Run the application
#' @export
run_shiny_dashboard <- function(port = 3838, host = "0.0.0.0", ...) {
  app <- shinyApp(ui = ui, server = server)
  runApp(app, port = port, host = host, ...)
}

# Export functions for use in other scripts
#' @export
if (getRversion() >= "2.15.1") {
  globalVariables(c("bmi", "mean_bodyfat", "sex", "bmi_cat", "group", "correlation",
                    "ci_lower", "ci_upper", "total", "n_unweighted", "pop_total",
                    "mean_ci_lower", "mean_ci_upper"))
}

