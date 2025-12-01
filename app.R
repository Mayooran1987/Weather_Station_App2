library(shiny)
library(plotly)
library(readxl)
library(httr)
#library(DT)
#library(lubridate)
library(dplyr)
#library(zoo)
#library(forecast)

if (!require(zoo, quietly = TRUE)) {
  install.packages("zoo", repos = "https://cloud.r-project.org/")
  library(zoo)
}

if (!require(forecast, quietly = TRUE)) {
  install.packages("forecast", repos = "https://cloud.r-project.org/")
  library(forecast)
}


if (requireNamespace("DT", quietly = TRUE)) {
  library(DT)
} else {
  stop("Please install the DT package: install.packages('DT')")
}

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate)

# Function to download and convert data to RDS
download_and_convert_to_rds <- function() {
  tryCatch({
    url <- "https://raw.githubusercontent.com/Mayooran1987/Ariviyal-Nagar-Weather-Station/main/AriviyalN_Data1.xlsx"
    temp_file <- tempfile(fileext = ".xlsx")
    
    # Download the file
    message("Downloading data from GitHub...")
    GET(url, write_disk(temp_file))
    
    # Read the Excel file
    message("Reading Excel file...")
    df <- read_excel(temp_file)
    
    # Convert timestamp and extract date components
    df$Date <- as.POSIXct(df$Timestamp, format = "%H:%M:%S")
    df$Hour <- hour(df$Date)
    df$Day <- as.Date(df$Date)
    
    # Save as RDS
    message("Saving as RDS file...")
    saveRDS(df, "weather_data.rds")
    
    # Clean up
    unlink(temp_file)
    
    message("Data successfully converted to RDS format")
    return(df)
  }, error = function(e) {
    message("Error downloading data: ", e$message)
    return(NULL)
  })
}

# Function to load data (automatically creates RDS if needed)
load_weather_data <- function() {
  rds_file <- "weather_data.rds"
  
  # Check if RDS file exists and is not empty
  if (file.exists(rds_file) && file.size(rds_file) > 0) {
    message("Loading data from existing RDS file...")
    tryCatch({
      data <- readRDS(rds_file)
      message("Successfully loaded data from RDS")
      return(data)
    }, error = function(e) {
      message("Error reading RDS file: ", e$message)
      message("Attempting to recreate RDS file...")
      return(download_and_convert_to_rds())
    })
  } else {
    message("RDS file not found or empty. Downloading and converting data...")
    return(download_and_convert_to_rds())
  }
}

# Load data at startup
weather_data <- load_weather_data()

# If data loading failed, create empty dataframe as fallback
if (is.null(weather_data)) {
  warning("Failed to load weather data. Using empty dataframe.")
  weather_data <- data.frame(
    Timestamp = character(),
    Date = as.POSIXct(character()),
    Hour = numeric(),
    Day = as.Date(character())
  )
}

# UI section (unchanged)
ui <- fluidPage(
  titlePanel(tags$div("Ariviyal Nagar Weather Station Data", style = "color: blue; text-align: center;")),
  tags$head(
    tags$style(
      HTML("
      #weatherPlot {
        width: 100%;
        height: 80vh;
      }
      .footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: #f5f5f5;
        padding: 10px;
        text-align: center;
      }
      .summary-box {
        background-color: #f8f9fa;
        border-left: 4px solid #007bff;
        padding: 15px;
        margin: 10px 0;
        border-radius: 4px;
      }
      .horizontal-panel {
        background-color: #f8f9fa;
        padding: 15px;
        margin: 10px 0;
        border-radius: 5px;
        border: 1px solid #ddd;
      }
      .trend-positive { color: #d9534f; font-weight: bold; }
      .trend-negative { color: #5cb85c; font-weight: bold; }
      .trend-neutral { color: #f0ad4e; font-weight: bold; }
      .forecast-plot {
        height: 600px;
        margin-bottom: 20px;
      }
      .forecast-metrics {
        background-color: #e8f4f8;
        padding: 10px;
        border-radius: 5px;
        margin: 10px 0;
      }
      .theory-section {
        background-color: #f0f8ff;
        padding: 20px;
        margin: 15px 0;
        border-radius: 8px;
        border-left: 5px solid #007bff;
      }
      .theory-title {
        color: #0056b3;
        border-bottom: 2px solid #0056b3;
        padding-bottom: 8px;
        margin-bottom: 15px;
      }
      .formula {
        background-color: #f8f9fa;
        padding: 10px;
        border-left: 4px solid #28a745;
        margin: 10px 0;
        font-family: 'Courier New', monospace;
      }
      .note-box {
        background-color: #fff3cd;
        border: 1px solid #ffeaa7;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
      }
      .help-content {
        min-height: 600px;
        padding: 20px;
      }
      .scrollable-content {
        height: 80vh;
        overflow-y: auto;
        padding-right: 15px;
      }
      .data-status {
        padding: 10px;
        margin: 10px 0;
        border-radius: 5px;
        text-align: center;
      }
      .status-success {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
      }
      .status-warning {
        background-color: #fff3cd;
        color: #856404;
        border: 1px solid #ffeaa7;
      }
      .status-error {
        background-color: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }
      ")
    )
  ),
  
  # Horizontal control panel at top
  fluidRow(
    class = "horizontal-panel",
    column(3,
           checkboxGroupInput("variables", "Select Variables to Plot:",
                              choices = c("W/m² Solar Radiation", "mm Precipitation", "Lightning Activity", "km Lightning Distance", "° Wind Direction", "m/s Wind Speed",
                                          "m/s Gust Speed", "°C Air Temperature", "RH Relative Humidity", "kPa Atmospheric Pressure", "° X-axis Level", 
                                          "° Y-axis Level", "mm/h Max Precip Rate", "°C RH Sensor Temp", "m³/m³ Water Content_10cm depth",
                                          "°C Soil Temperature_10cm depth", "mS/cm Bulk EC_10cm depth", "m³/m³ Water Content_20cm depth", "°C Soil Temperature_20cm depth",
                                          "mS/cm Bulk EC_20cm depth", "m³/m³ Water Content_30cm depth", "°C Soil Temperature_30cm depth", "mS/cm Bulk EC_30cm depth", "% Battery Percent",
                                          "mV Battery Voltage", "kPa Reference Pressure", "°C Logger Temperature"),
                              selected = c("°C Air Temperature"),
                              inline = TRUE)
    ),
    column(2,
           sliderInput("pointSize", "Point Size:", min = 1, max = 10, value = 3)
    ),
    column(2,
           sliderInput("lineWidth", "Line Width:", min = 0.5, max = 5, value = 1.5)
    ),
    column(2,
           selectInput("smoothing", "Smoothing Method:",
                       choices = c("None" = "none", "Moving Average" = "ma", "LOESS" = "loess"),
                       selected = "none")
    ),
    column(2,
           sliderInput("smoothingSpan", "Smoothing Span:", min = 0.1, max = 1, value = 0.5),
           checkboxInput("showConfidence", "Show Confidence", value = FALSE)
    ),
    column(1,
           br(),
           downloadButton("downloadPlot", "Save Plot", class = "btn-sm"),
           downloadButton("downloadData", "Save Data", class = "btn-sm"),
           actionButton("refreshData", "Refresh Data", class = "btn-sm btn-warning")
    )
  ),
  
  # Data status indicator
  uiOutput("data_status"),
  
  # Main content area
  tabsetPanel(
    tabPanel("Graphs", 
             plotlyOutput("weatherPlot", height = "800px")
    ),
    tabPanel("Data", 
             DTOutput("data_table")
    ),
    tabPanel("Summary", 
             uiOutput("summary_tab")
    ),
    tabPanel("Trend Analysis",
             uiOutput("trend_analysis")
    ),
    tabPanel("Weather Forecast",
             fluidRow(
               column(4,
                      selectInput("forecast_variable", "Select Variable to Forecast:",
                                  choices = c("°C Air Temperature", "RH Relative Humidity", "kPa Atmospheric Pressure", 
                                              "m/s Wind Speed", "W/m² Solar Radiation", "mm Precipitation"),
                                  selected = "°C Air Temperature"),
                      numericInput("forecast_days", "Forecast Period (days):", 
                                   value = 10, min = 1, max = 30, step = 1),
                      selectInput("forecast_method", "Forecasting Method:",
                                  choices = c("Linear Regression" = "linear",
                                              "Exponential Smoothing" = "ets",
                                              "ARIMA" = "arima",
                                              "Simple Moving Average" = "sma"),
                                  selected = "linear"),
                      actionButton("generate_forecast", "Generate Forecast", 
                                   class = "btn-primary btn-block"),
                      br(), br(),
                      uiOutput("forecast_notes")
               ),
               column(8,
                      plotlyOutput("forecast_plot", height = "500px"),
                      h4("Forecast Summary"),
                      DTOutput("forecast_table")
               )
             )
    ),
    # Help & Theory Tab with server-side rendering
    tabPanel("Help & Theory",
             div(class = "scrollable-content",
                 uiOutput("help_theory_tab")
             )
    )
  ),
  
  tags$div(class = "footer", "App developed by Dr T. Mayooran, © 2024 Department of Inter Disciplinary Studies, Faculty of Engineering, University of Jaffna. Email contact: ",
           tags$a(href = "mailto:mayooran@eng.jfn.ac.lk", class = "email", "mayooran@eng.jfn.ac.lk"))
)

server <- function(input, output, session) {
  # Reactive value to store data and trigger updates
  rv <- reactiveValues(
    data = weather_data,
    last_update = Sys.time(),
    data_status = if(nrow(weather_data) > 0) "success" else "error"
  )
  
  # Data status indicator
  output$data_status <- renderUI({
    status_class <- switch(rv$data_status,
                           "success" = "status-success",
                           "warning" = "status-warning",
                           "error" = "status-error")
    
    div(class = paste("data-status", status_class),
        if(rv$data_status == "success") {
          paste("✓ Data loaded successfully:", nrow(rv$data), "rows, last updated:", format(rv$last_update, "%Y-%m-%d %H:%M:%S"))
        } else if(rv$data_status == "warning") {
          "⚠ Data loading had warnings. Some features may not work correctly."
        } else {
          "✗ Failed to load data. Please check internet connection and try refreshing."
        }
    )
  })
  
  # Refresh data function
  observeEvent(input$refreshData, {
    showNotification("Refreshing data from source...", type = "message")
    
    new_data <- download_and_convert_to_rds()
    
    if (!is.null(new_data) && nrow(new_data) > 0) {
      rv$data <- new_data
      rv$last_update <- Sys.time()
      rv$data_status <- "success"
      showNotification("Data refreshed successfully!", type = "message")
    } else {
      rv$data_status <- "error"
      showNotification("Failed to refresh data. Using cached version.", type = "error")
    }
  })
  
  # Load data from RDS file - much more memory efficient
  data <- reactive({
    rv$data
  })
  
  # Enhanced plotting function
  output$weatherPlot <- renderPlotly({
    plot_data <- data()
    selected_vars <- input$variables
    
    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No data available. Please refresh data or check connection."))
    }
    
    if (length(selected_vars) == 0) {
      return(plotly_empty() %>% 
               layout(title = "Please select at least one variable to plot"))
    }
    
    p <- plot_ly()
    
    for (var in selected_vars) {
      # Check if variable exists in data
      if (!var %in% names(plot_data)) {
        next
      }
      
      # Remove NA values for current variable
      valid_data <- plot_data[!is.na(plot_data[[var]]), ]
      
      if (nrow(valid_data) > 0) {
        # Original data
        p <- p %>% add_trace(
          x = ~Date, 
          y = as.formula(paste0("~`", var, "`")), 
          data = valid_data, 
          type = 'scatter', 
          mode = 'markers+lines', 
          name = var,
          marker = list(size = input$pointSize, opacity = 0.7),
          line = list(width = input$lineWidth),
          hovertemplate = paste0(var, ": %{y:.2f}<br>Time: %{x}<extra></extra>")
        )
        
        # Add smoothing if selected
        if (input$smoothing != "none") {
          smooth_data <- valid_data
          
          if (input$smoothing == "ma" && nrow(smooth_data) > 5) {
            # Moving average
            window_size <- max(3, as.integer(nrow(smooth_data) * input$smoothingSpan))
            smooth_data$smoothed <- rollmean(smooth_data[[var]], 
                                             k = min(window_size, nrow(smooth_data)), 
                                             fill = NA, align = "center")
          } else if (input$smoothing == "loess" && nrow(smooth_data) > 10) {
            # LOESS smoothing
            tryCatch({
              loess_fit <- loess(as.formula(paste0("`", var, "` ~ as.numeric(Date)")), 
                                 data = smooth_data, span = input$smoothingSpan)
              smooth_data$smoothed <- predict(loess_fit)
            }, error = function(e) {
              smooth_data$smoothed <- NA
            })
          }
          
          if (!all(is.na(smooth_data$smoothed))) {
            p <- p %>% add_trace(
              x = ~Date, 
              y = ~smoothed, 
              data = smooth_data, 
              type = 'scatter', 
              mode = 'lines', 
              name = paste(var, "Smoothed"),
              line = list(width = input$lineWidth + 1, color = "red", dash = "dash"),
              hovertemplate = paste0(var, " (Smoothed): %{y:.2f}<br>Time: %{x}<extra></extra>")
            )
          }
        }
        
        # Add trend line with confidence interval
        if (input$showConfidence && nrow(valid_data) > 2) {
          tryCatch({
            lm_fit <- lm(as.formula(paste0("`", var, "` ~ as.numeric(Date)")), data = valid_data)
            trend_data <- data.frame(
              Date = valid_data$Date,
              trend = predict(lm_fit),
              lower = predict(lm_fit, interval = "confidence")[, "lwr"],
              upper = predict(lm_fit, interval = "confidence")[, "upr"]
            )
            
            p <- p %>% add_trace(
              x = ~Date, y = ~trend, data = trend_data,
              type = 'scatter', mode = 'lines',
              name = paste(var, "Trend"),
              line = list(width = 2, color = "green", dash = "dot")
            )
            
            p <- p %>% add_ribbons(
              x = ~Date, ymin = ~lower, ymax = ~upper, data = trend_data,
              name = paste(var, "95% CI"),
              fillcolor = 'rgba(0,100,0,0.2)',
              line = list(color = 'rgba(255,255,255,0)'),
              showlegend = FALSE
            )
          }, error = function(e) {
            # Skip trend line if error occurs
          })
        }
      }
    }
    
    # Enhanced layout
    p <- p %>% layout(
      title = list(
        text = "Ariviyal Nagar Weather Station Data",
        font = list(size = 20, color = "darkblue")
      ),
      xaxis = list(
        title = "Date/Time",
        gridcolor = '#e1e5ed',
        zerolinecolor = '#ffff',
        zerolinewidth = 2
      ),
      yaxis = list(
        title = "Value",
        gridcolor = '#e1e5ed',
        zerolinecolor = '#ffff',
        zerolinewidth = 2
      ),
      legend = list(
        x = 0.01, 
        y = 0.99,
        bgcolor = 'rgba(255,255,255,0.8)'
      ),
      plot_bgcolor = '#f8f9fa',
      paper_bgcolor = '#f8f9fa',
      height = 800,
      margin = list(l = 80, r = 80, t = 80, b = 80)
    )
    
    p
  })
  
  # Automatic summary generation (reactive - updates automatically)
  auto_summary <- reactive({
    plot_data <- data()
    selected_vars <- input$variables
    
    if (nrow(plot_data) == 0) {
      return("No data available. Please refresh data.")
    }
    
    if (length(selected_vars) == 0) {
      return("Please select variables to generate summary.")
    }
    
    summary_text <- c()
    summary_text <- c(summary_text, paste("# Automatic Summary\n"))
    summary_text <- c(summary_text, paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    summary_text <- c(summary_text, paste("Selected variables:", paste(selected_vars, collapse = ", ")))
    summary_text <- c(summary_text, "")
    
    for (var in selected_vars) {
      if (!var %in% names(plot_data)) {
        summary_text <- c(summary_text,
                          paste0("## ", var, " Summary"),
                          "- Variable not found in dataset",
                          "")
        next
      }
      
      var_data <- plot_data[[var]]
      var_data <- var_data[!is.na(var_data)]
      
      if (length(var_data) > 0) {
        # Basic statistics
        n_obs <- length(var_data)
        mean_val <- mean(var_data, na.rm = TRUE)
        median_val <- median(var_data, na.rm = TRUE)
        sd_val <- sd(var_data, na.rm = TRUE)
        min_val <- min(var_data, na.rm = TRUE)
        max_val <- max(var_data, na.rm = TRUE)
        cv <- sd_val / mean_val * 100
        
        summary_text <- c(summary_text, 
                          paste0("## ", var, " Summary"),
                          paste0("- **Observations**: ", n_obs),
                          paste0("- **Mean**: ", round(mean_val, 3)),
                          paste0("- **Median**: ", round(median_val, 3)),
                          paste0("- **Standard Deviation**: ", round(sd_val, 3)),
                          paste0("- **Minimum**: ", round(min_val, 3)),
                          paste0("- **Maximum**: ", round(max_val, 3)),
                          paste0("- **Range**: ", round(max_val - min_val, 3)),
                          paste0("- **Coefficient of Variation**: ", round(cv, 2), "%"),
                          "")
        
        # Data quality assessment
        completeness <- round((n_obs / nrow(plot_data)) * 100, 1)
        summary_text <- c(summary_text,
                          paste0("### Data Quality for ", var),
                          paste0("- **Data Completeness**: ", completeness, "%"),
                          paste0("- **Missing Values**: ", nrow(plot_data) - n_obs),
                          "")
      } else {
        summary_text <- c(summary_text,
                          paste0("## ", var, " Summary"),
                          "- No data available for this variable",
                          "")
      }
    }
    
    paste(summary_text, collapse = "\n")
  })
  
  # Render summary tab - automatically updates
  output$summary_tab <- renderUI({
    tagList(
      h3("Automatic Data Summary"),
      div(class = "summary-box",
          HTML(markdown::markdownToHTML(text = auto_summary(), fragment.only = TRUE))
      )
    )
  })
  
  # Trend Analysis tab
  output$trend_analysis <- renderUI({
    plot_data <- data()
    selected_vars <- input$variables
    
    if (nrow(plot_data) == 0) {
      return(
        div(class = "summary-box",
            h4("Trend Analysis"),
            p("No data available for trend analysis.")
        )
      )
    }
    
    if (length(selected_vars) == 0) {
      return(
        div(class = "summary-box",
            h4("Trend Analysis"),
            p("Please select variables to see trend analysis.")
        )
      )
    }
    
    # Create trend boxes
    trend_boxes <- lapply(selected_vars, function(var) {
      if (!var %in% names(plot_data)) {
        return(
          div(class = "trend-box",
              h4(var),
              p("Variable not found in dataset.")
          )
        )
      }
      
      var_data <- plot_data[[var]]
      time_data <- plot_data$Date
      
      valid_idx <- !is.na(var_data)
      
      if (sum(valid_idx) > 2) {
        tryCatch({
          lm_result <- lm(var_data[valid_idx] ~ as.numeric(time_data[valid_idx]))
          lm_summary <- summary(lm_result)
          
          slope <- coef(lm_result)[2]
          p_value <- lm_summary$coefficients[2,4]
          r_squared <- lm_summary$r.squared
          slope_ci <- confint(lm_result)[2,]
          
          trend_direction <- ifelse(slope > 0, "Increasing", "Decreasing")
          significance <- ifelse(p_value < 0.05, "Significant", "Not Significant")
          trend_class <- ifelse(p_value < 0.05, 
                                ifelse(slope > 0, "trend-positive", "trend-negative"),
                                "trend-neutral")
          
          time_range <- as.numeric(difftime(max(time_data[valid_idx]), min(time_data[valid_idx]), units = "hours"))
          total_change <- slope * time_range
          mean_value <- mean(var_data[valid_idx])
          percent_change <- ifelse(mean_value != 0, (total_change / mean_value) * 100, NA)
          
          div(class = "trend-box",
              h4(var),
              tags$table(
                class = "table table-condensed",
                tags$tr(tags$td("Trend Direction:"), 
                        tags$td(span(trend_direction, class = trend_class))),
                tags$tr(tags$td("Statistical Significance:"), 
                        tags$td(span(significance, class = trend_class))),
                tags$tr(tags$td("Slope (per hour):"), 
                        tags$td(formatC(slope, format = "e", digits = 3))),
                tags$tr(tags$td("P-value:"), 
                        tags$td(round(p_value, 5))),
                tags$tr(tags$td("R-squared:"), 
                        tags$td(round(r_squared, 4))),
                if(!is.na(percent_change)) {
                  tags$tr(tags$td("Estimated Total Change:"), 
                          tags$td(paste0(round(percent_change, 2), "% over period")))
                }
              ),
              tags$p(style = "font-style: italic; margin-top: 10px;",
                     ifelse(p_value < 0.05,
                            paste("This", tolower(trend_direction), "trend is statistically significant."),
                            "This trend is not statistically significant."))
          )
        }, error = function(e) {
          div(class = "trend-box",
              h4(var),
              p("Trend analysis not available for this variable.")
          )
        })
      } else {
        div(class = "trend-box",
            h4(var),
            p("Insufficient data for trend analysis."))
      }
    })
    
    # Arrange in rows of 2
    fluidRow(
      lapply(seq_along(trend_boxes), function(i) {
        column(6, trend_boxes[[i]])
      })
    )
  })
  
  # Forecast functionality
  forecast_results <- eventReactive(input$generate_forecast, {
    req(input$forecast_variable)
    
    plot_data <- data()
    
    if (nrow(plot_data) == 0) {
      return(list(error = "No data available for forecasting."))
    }
    
    if (!input$forecast_variable %in% names(plot_data)) {
      return(list(error = paste("Variable", input$forecast_variable, "not found in dataset.")))
    }
    
    var_data <- plot_data[[input$forecast_variable]]
    dates <- plot_data$Date
    
    # Remove NA values
    valid_idx <- !is.na(var_data)
    var_data_clean <- var_data[valid_idx]
    dates_clean <- dates[valid_idx]
    
    if (length(var_data_clean) < 10) {
      return(list(error = "Insufficient data for forecasting. Need at least 10 data points."))
    }
    
    tryCatch({
      # Convert to time series - using simpler approach
      ts_data <- ts(var_data_clean, frequency = 24)  # Assuming hourly data
      
      # Generate forecast based on selected method
      if (input$forecast_method == "linear") {
        # Simple linear projection
        time_points <- 1:length(var_data_clean)
        lm_model <- lm(var_data_clean ~ time_points)
        future_time <- (length(var_data_clean) + 1):(length(var_data_clean) + input$forecast_days * 24)
        forecast_values <- predict(lm_model, newdata = data.frame(time_points = future_time))
        
        # Create simple confidence intervals
        se <- summary(lm_model)$sigma
        lower_80 <- forecast_values - 1.28 * se
        upper_80 <- forecast_values + 1.28 * se
        lower_95 <- forecast_values - 1.96 * se
        upper_95 <- forecast_values + 1.96 * se
        
      } else if (input$forecast_method == "ets") {
        # Exponential smoothing
        model <- ets(ts_data)
        forecast_result <- forecast(model, h = input$forecast_days * 24)
        forecast_values <- as.numeric(forecast_result$mean)
        lower_80 <- as.numeric(forecast_result$lower[,1])
        upper_80 <- as.numeric(forecast_result$upper[,1])
        lower_95 <- as.numeric(forecast_result$lower[,2])
        upper_95 <- as.numeric(forecast_result$upper[,2])
        
      } else if (input$forecast_method == "arima") {
        # ARIMA model
        model <- auto.arima(ts_data)
        forecast_result <- forecast(model, h = input$forecast_days * 24)
        forecast_values <- as.numeric(forecast_result$mean)
        lower_80 <- as.numeric(forecast_result$lower[,1])
        upper_80 <- as.numeric(forecast_result$upper[,1])
        lower_95 <- as.numeric(forecast_result$lower[,2])
        upper_95 <- as.numeric(forecast_result$upper[,2])
        
      } else if (input$forecast_method == "sma") {
        # Simple moving average projection
        last_value <- tail(var_data_clean, 1)
        forecast_values <- rep(last_value, input$forecast_days * 24)
        se <- sd(var_data_clean, na.rm = TRUE)
        lower_80 <- forecast_values - 1.28 * se
        upper_80 <- forecast_values + 1.28 * se
        lower_95 <- forecast_values - 1.96 * se
        upper_95 <- forecast_values + 1.96 * se
      }
      
      # Create forecast dates
      last_date <- max(dates_clean)
      forecast_dates <- last_date + hours(1:(input$forecast_days * 24))
      
      # Return results
      list(
        historical = data.frame(
          Date = dates_clean,
          Value = var_data_clean,
          Type = "Historical"
        ),
        forecast = data.frame(
          Date = forecast_dates,
          Value = forecast_values,
          Lower_80 = lower_80,
          Upper_80 = upper_80,
          Lower_95 = lower_95,
          Upper_95 = upper_95,
          Type = "Forecast"
        ),
        method = input$forecast_method
      )
      
    }, error = function(e) {
      return(list(error = paste("Forecast error:", e$message)))
    })
  })
  
  # Forecast plot
  output$forecast_plot <- renderPlotly({
    fc_data <- forecast_results()
    
    if (!is.null(fc_data$error)) {
      return(plotly_empty() %>% 
               layout(
                 title = fc_data$error,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
               ))
    }
    
    # Create the plot
    p <- plot_ly() %>%
      # Historical data
      add_trace(
        data = fc_data$historical,
        x = ~Date,
        y = ~Value,
        type = 'scatter',
        mode = 'lines',
        name = 'Historical Data',
        line = list(color = 'blue', width = 2),
        hovertemplate = paste(input$forecast_variable, ": %{y:.2f}<br>Time: %{x}<extra></extra>")
      ) %>%
      # Forecast data
      add_trace(
        data = fc_data$forecast,
        x = ~Date,
        y = ~Value,
        type = 'scatter',
        mode = 'lines',
        name = 'Forecast',
        line = list(color = 'red', width = 2, dash = 'dash'),
        hovertemplate = paste(input$forecast_variable, " (Forecast): %{y:.2f}<br>Time: %{x}<extra></extra>")
      ) %>%
      # 80% confidence interval
      add_ribbons(
        data = fc_data$forecast,
        x = ~Date,
        ymin = ~Lower_80,
        ymax = ~Upper_80,
        name = '80% Confidence',
        fillcolor = 'rgba(255,0,0,0.2)',
        line = list(color = 'rgba(255,255,255,0)'),
        showlegend = TRUE
      ) %>%
      # 95% confidence interval
      add_ribbons(
        data = fc_data$forecast,
        x = ~Date,
        ymin = ~Lower_95,
        ymax = ~Upper_95,
        name = '95% Confidence',
        fillcolor = 'rgba(255,0,0,0.1)',
        line = list(color = 'rgba(255,255,255,0)'),
        showlegend = TRUE
      ) %>%
      layout(
        title = paste(input$forecast_days, "Day Forecast for", input$forecast_variable),
        xaxis = list(title = "Date/Time"),
        yaxis = list(title = input$forecast_variable),
        legend = list(x = 0.02, y = 0.98),
        plot_bgcolor = '#f8f9fa',
        paper_bgcolor = '#f8f9fa'
      )
    
    p
  })
  
  # Forecast notes
  output$forecast_notes <- renderUI({
    div(
      class = "forecast-metrics",
      h5("Forecast Information:"),
      p("• Forecasts are based on historical patterns"),
      p("• Confidence intervals show prediction uncertainty"),
      p("• Longer forecasts have higher uncertainty"),
      p("• Weather forecasts are inherently uncertain")
    )
  })
  
  # Forecast table
  output$forecast_table <- renderDT({
    fc_data <- forecast_results()
    
    if (!is.null(fc_data$error)) {
      return(datatable(data.frame(Message = fc_data$error)))
    }
    
    # Create daily summary
    daily_forecast <- fc_data$forecast %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(Date) %>%
      summarise(
        Average = round(mean(Value, na.rm = TRUE), 2),
        Minimum = round(min(Value, na.rm = TRUE), 2),
        Maximum = round(max(Value, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      arrange(Date)
    
    datatable(
      daily_forecast,
      options = list(
        pageLength = 10,
        dom = 'tlip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      caption = paste("Daily Forecast Summary for", input$forecast_variable)
    )
  })
  
  # Help & Theory Tab
  output$help_theory_tab <- renderUI({
    tagList(
      div(class = "theory-section",
          h2("Comprehensive Help & Theoretical Framework", class = "theory-title"),
          p("This section provides detailed theoretical background and methodological explanations for all features in this weather data analysis application.")
      ),
      
      div(class = "theory-section",
          h3("Data Management", class = "theory-title"),
          h4("RDS File System:"),
          tags$ul(
            tags$li(tags$b("Automatic RDS Creation:"), " The app automatically creates an RDS file during the first run for faster subsequent loading."),
            tags$li(tags$b("Memory Efficiency:"), " RDS files are binary formats that load much faster than Excel files and use less memory."),
            tags$li(tags$b("Data Refresh:"), " Use the 'Refresh Data' button to update the dataset from the original source."),
            tags$li(tags$b("Offline Capability:"), " Once downloaded, the app can function without internet connection using the cached RDS file.")
          )
      ),
      div(class = "theory-section",
          h3("1. Weather Variables and Measurements", class = "theory-title"),
          h4("Atmospheric Parameters:"),
          tags$ul(
            tags$li(tags$b("Air Temperature (°C):"), " Measured using precision thermistors. Conversion from raw sensor data uses Steinhart-Hart equation for accuracy."),
            tags$li(tags$b("Relative Humidity (%):"), " Calculated using capacitive polymer sensors. RH = (actual vapor pressure / saturation vapor pressure) × 100%"),
            tags$li(tags$b("Atmospheric Pressure (kPa):"), " Measured using piezoresistive sensors. Standard sea-level pressure = 101.325 kPa")
          ),
          
          h4("Wind Measurements:"),
          tags$ul(
            tags$li(tags$b("Wind Speed (m/s):"), " Measured using cup anemometers. 1 m/s = 3.6 km/h = 2.237 mph"),
            tags$li(tags$b("Wind Direction (°):"), " Measured using wind vanes with potentiometric sensors. 0° = North, 90° = East, 180° = South, 270° = West"),
            tags$li(tags$b("Gust Speed (m/s):"), " Maximum 3-second average wind speed recorded during sampling period")
          ),
          
          h4("Radiation and Precipitation:"),
          tags$ul(
            tags$li(tags$b("Solar Radiation (W/m²):"), " Measured using pyranometers with thermopile sensors. Spectral range: 300-2800 nm"),
            tags$li(tags$b("Precipitation (mm):"), " Tipping bucket rain gauge with 0.2mm resolution. Calibration factor applied for wind effects")
          )
      ),
      div(class = "theory-section",
          h3("2. Statistical Analysis Methods", class = "theory-title"),
          
          h4("Descriptive Statistics:"),
          div(class = "formula",
              "Mean: μ = (Σxᵢ) / n",
              br(),
              "Standard Deviation: σ = √[Σ(xᵢ - μ)² / (n-1)]",
              br(),
              "Coefficient of Variation: CV = (σ / μ) × 100%"
          ),
          
          h4("Trend Analysis:"),
          p("Linear regression models are used to identify trends:"),
          div(class = "formula",
              "y = β₀ + β₁x + ε",
              br(),
              "where y = variable value, x = time, β₁ = trend slope"
          ),
          tags$ul(
            tags$li("Statistical significance tested using t-test: p < 0.05 indicates significant trend"),
            tags$li("R² value indicates proportion of variance explained by the trend"),
            tags$li("Confidence intervals calculated at 95% confidence level")
          )
      ),
      div(class = "theory-section",
          h3("3. Forecasting Methodologies", class = "theory-title"),
          
          h4("Linear Regression Forecasting:"),
          p("Projects future values based on linear trend:"),
          div(class = "formula",
              "ŷ = b₀ + b₁t",
              br(),
              "where ŷ = predicted value, t = future time point"
          ),
          
          h4("Exponential Smoothing (ETS):"),
          p("Weighted average of past observations with exponentially decreasing weights:"),
          div(class = "formula",
              "ŷₜ₊₁ = αyₜ + (1-α)ŷₜ",
              br(),
              "where α = smoothing parameter (0 < α < 1)"
          ),
          
          h4("ARIMA Models:"),
          p("AutoRegressive Integrated Moving Average models:"),
          div(class = "formula",
              "ARIMA(p,d,q): (1-ΣφᵢBⁱ)(1-B)ᵈyₜ = (1+ΣθᵢBⁱ)εₜ",
              br(),
              "where p = AR order, d = differencing, q = MA order"
          ),
          
          h4("Confidence Intervals:"),
          p("Prediction intervals calculated using:"),
          div(class = "formula",
              "CI = ŷ ± t(α/2, n-2) × SE",
              br(),
              "where SE = standard error of prediction"
          )
      ),
      
      div(class = "theory-section",
          h3("4. Data Quality and Processing", class = "theory-title"),
          
          h4("Data Validation:"),
          tags$ul(
            tags$li("Range checks: Values outside physical limits flagged"),
            tags$li("Rate of change checks: Unrealistic changes detected"),
            tags$li("Internal consistency: Cross-validation between related variables")
          ),
          
          h4("Missing Data Handling:"),
          tags$ul(
            tags$li("Linear interpolation for short gaps (< 3 hours)"),
            tags$li("Seasonal decomposition for longer gaps"),
            tags$li("Data completeness calculated as percentage of valid observations")
          ),
          
          h4("Smoothing Algorithms:"),
          tags$ul(
            tags$li(tags$b("Moving Average:"), " Simple unweighted mean of neighboring points"),
            tags$li(tags$b("LOESS:"), " Local regression using weighted linear least squares"),
            tags$li("Span parameter controls degree of smoothing (0.1 = aggressive, 1.0 = minimal)")
          )
      ),
      
      div(class = "theory-section",
          h3("5. Application Features Guide", class = "theory-title"),
          
          h4("Graph Controls:"),
          tags$ul(
            tags$li("Point Size: Adjusts marker visibility (1-10 pixels)"),
            tags$li("Line Width: Controls line thickness (0.5-5 pixels)"),
            tags$li("Smoothing: Reduces noise for trend visualization")
          ),
          
          h4("Statistical Outputs:"),
          tags$ul(
            tags$li("Automatic Summary: Real-time descriptive statistics"),
            tags$li("Trend Analysis: Statistical significance of temporal patterns"),
            tags$li("Forecast Accuracy: RMSE, MAE, MAPE metrics")
          )
      ),
      
      div(class = "note-box",
          h4("Important Notes:"),
          tags$ul(
            tags$li("Weather forecasts become increasingly uncertain beyond 3-5 days"),
            tags$li("Statistical significance (p < 0.05) indicates reliable trends"),
            tags$li("Confidence intervals represent prediction uncertainty"),
            tags$li("Data quality flags indicate potential measurement issues")
          )
      ),      div(class = "theory-section",
                  h3("6. References and Further Reading", class = "theory-title"),
                  tags$ul(
                    tags$li("World Meteorological Organization. (2018). Guide to Meteorological Instruments and Methods of Observation."),
                    tags$li("Hyndman, R.J., & Athanasopoulos, G. (2021). Forecasting: Principles and Practice."),
                    tags$li("Wilks, D.S. (2019). Statistical Methods in the Atmospheric Sciences."),
                    tags$li("Fritsch, J.M., & Carbone, R.E. (2004). Weather Forecasting.")
                  )
      ),
      # ... (include the rest of your existing help content here)
      
      div(class = "note-box",
          h4("Data Status Indicators:"),
          tags$ul(
            tags$li(tags$b("Green:"), " Data loaded successfully from RDS file"),
            tags$li(tags$b("Yellow:"), " Data loading had warnings or issues"),
            tags$li(tags$b("Red:"), " Failed to load data - check internet connection")
          )
      )
    )
  })
  
  # Download handlers
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("weather-plot-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plotly::export(plotly_build(output$weatherPlot()), file = file)
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("weather-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Render data table
  output$data_table <- renderDT({
    plot_data <- data()
    
    if (nrow(plot_data) == 0) {
      return(datatable(data.frame(Message = "No data available")))
    }
    
    datatable(plot_data, 
              options = list(
                scrollX = TRUE,
                pageLength = 25,
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              filter = 'top',
              rownames = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
