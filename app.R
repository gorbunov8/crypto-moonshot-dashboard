library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(rvest)
library(readr)
library(leaflet.extras)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(forecast)

# Function for Scraping Stock Market Index Data
scrape_stock_market_data <- function() {
  url <- "https://www.theglobaleconomy.com/rankings/share_price_index/"
  page <- read_html(url)
  
  # Scrape the Table Containing Stock Market Index Data
  stock_data <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  # Keep only the Necessary Columns and Rename them
  stock_data <- stock_data[, c(1, 2, 4, 5)]
  colnames(stock_data) <- c("country", "Latest Value", "Change 3 Months", "Change 12 Months")
  
  # Convert columns to numeric where appropriate
  stock_data$`Latest Value` <- as.numeric(gsub(",", "", stock_data$`Latest Value`))
  stock_data$`Change 3 Months` <- as.numeric(gsub("%", "", stock_data$`Change 3 Months`))
  stock_data$`Change 12 Months` <- as.numeric(gsub("%", "", stock_data$`Change 12 Months`))
  
  # Remove Turkey from the dataset
  stock_data <- stock_data[stock_data$country != "Turkey", ]
  
  # Fix USA naming issue
  stock_data$country <- gsub("USA.*", "United States", stock_data$country)
  
  # Return Data
  stock_data
}

# Function for Scraping Volatility Data
scrape_volatility_data <- function() {
  url <- "https://www.theglobaleconomy.com/rankings/Stock_price_volatility/"
  page <- read_html(url)
  
  # Scrape the Table Containing Volatility Data
  volatility_data <- page %>%
    html_nodes("table") %>%
    html_table(fill = TRUE) %>%
    .[[1]]
  
  # Keep only the Necessary Columns and Rename them
  volatility_data <- volatility_data[, c(1, 2, 3)]
  colnames(volatility_data) <- c("country", "volatility", "rank")
  
  # Convert columns to numeric where appropriate
  volatility_data$volatility <- as.numeric(volatility_data$volatility)
  volatility_data$rank <- as.numeric(volatility_data$rank)
  
  # Fix USA naming issue
  volatility_data$country <- gsub("US.*", "United States", volatility_data$country)
  
  # Return Data
  volatility_data
}

# Function to Load Geographic Data
getgeodata <- function() {
  geo_data <- read_csv("data/world_country_and_usa_states_latitude_and_longitude_values.csv")
  geo_data <- geo_data[, c("country", "latitude", "longitude")]
  
  # Rename 'US' to 'United States' to match with scraped data
  geo_data$country[geo_data$country == "US"] <- "United States"
  
  return(geo_data)
}

# Function to Load CSV Data from the 'data' Folder and Count Rows and Columns
count_csv_data <- function(folder_path) {
  files <- list.files(folder_path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)
  total_rows <- 0
  total_cols <- 0
  for (file in files) {
    data <- read_csv(file)
    total_rows <- total_rows + nrow(data)
    total_cols <- total_cols + ncol(data)
  }
  return(list(rows = total_rows, cols = total_cols))
}

# Function to Calculate Date Range from 'sp500_index.csv'
calculate_date_range <- function() {
  sp500_data <- read_csv("data/stocks/sp500_index.csv") %>%
    mutate(Date = as.Date(Date))
  date_range <- range(sp500_data$Date, na.rm = TRUE)
  return(date_range)
}

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Global Market Insights Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction"),  # Updated tab name
      menuItem("Market Index", tabName = "market_index"),
      menuItem("Market Changes", tabName = "market_changes"),
      menuItem("Stock Price Volatility", tabName = "volatility"),
      menuItem("S&P 500 Forecast", tabName = "sp500_forecast"),
      menuItem("Data Overview", tabName = "data_overview")
    )
  ),
  dashboardBody(
    tabItems(
      # Introduction Tab
      tabItem(
        tabName = "introduction",
        fluidRow(
          box(
            title = "Project Introduction",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            div(
              HTML("
    <p>Welcome to the <strong>Global Market Insights Dashboard</strong>. This project aims to provide comprehensive insights into global stock markets, including market indices, changes, and volatility. Utilize the various tabs to explore detailed maps, trends, and forecasts.</p>
    <p>You can find this project on <a href='https://github.com/gorbunov8/global-market-insights' target='_blank'>GitHub</a>.</p>
              ")
            )
          )
        ),
        fluidRow(
          box(
            title = "Note",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            div(
              HTML("
              <p>Not a financial advice.</p>
              ")
            )
          )
        ),
        fluidRow(
          box(
            title = "Data Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            valueBoxOutput("total_observations_box"),
            valueBoxOutput("missing_values_box"),
            valueBoxOutput("issues_box"),
            valueBoxOutput("total_variables_box"),
            valueBoxOutput("countries_analyzed_box"),
            valueBoxOutput("date_range_box")
          )
        )
      ),
      
      # Market Index Tab
      tabItem(
        tabName = "market_index",
        fluidRow(
          column(3,
                 box(
                   title = "Country Selection",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   pickerInput(
                     inputId = "selected_countries",
                     label = "Select Countries",
                     choices = NULL,
                     options = list(`actions-box` = TRUE),
                     multiple = TRUE
                   )
                 )
          ),
          column(9,
                 fluidRow(
                   valueBoxOutput("min_index_box", width = 4),
                   valueBoxOutput("max_index_box", width = 4),
                   valueBoxOutput("avg_index_box", width = 4)
                 )
          )
        ),
        fluidRow(
          box(
            title = "Market Index Map",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            leafletOutput("market_map")
          )
        )
      ),
      
      # Market Changes Tab
      tabItem(
        tabName = "market_changes",
        fluidRow(
          column(3,
                 box(
                   title = "Select Period",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   selectInput(
                     inputId = "select_period",
                     label = "Change Period",
                     choices = c("3 Months" = "3m", "12 Months" = "12m")
                   )
                 )
          ),
          column(9,
                 fluidRow(
                   valueBoxOutput("min_change_box", width = 4),
                   valueBoxOutput("max_change_box", width = 4),
                   valueBoxOutput("avg_change_box", width = 4)
                 )
          )
        ),
        fluidRow(
          box(
            title = "Market Changes Map",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "500px",
            leafletOutput("changes_map")
          )
        )
      ),
      
      # Volatility Tab
      tabItem(
        tabName = "volatility",
        fluidRow(
          column(12,
                 box(
                   title = "Volatility Map",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   height = "600px",
                   leafletOutput("volatility_map")
                 )
          )
        ),
        fluidRow(
          column(12,
                 box(
                   title = "Controls",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   sliderInput("top_n", "Top N Countries by Volatility:", min = 1, max = 50, value = 10),
                   sliderInput("volatility_range", "Volatility Range:", min = 0, max = 100, value = c(0, 100)),
                   DTOutput("volatility_table")
                 )
          )
        )
      ),
      
      # S&P 500 Forecast Tab
      tabItem(
        tabName = "sp500_forecast",
        fluidRow(
          box(
            title = "S&P 500 Historical and Forecast Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("sp500_plot", height = "400px"),
            dateRangeInput(
              "forecast_range",
              "Forecast Period:",
              start = Sys.Date(),
              end = Sys.Date() + 365,
              min = Sys.Date(),
              max = Sys.Date() + 3650,
              format = "yyyy-mm",
              startview = "year"
            ),
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              actionButton("calc_btn", "Calculate", class = "btn-primary"),
              actionButton("forecast_btn", "Generate Forecast", class = "btn-primary"),
              actionButton("reset_btn", "Reset", class = "btn-warning")
            )
          )
        ),
        fluidRow(
          box(
            title = "Investment Calculator",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            div(
              style = "display: flex; justify-content: space-between; flex-wrap: wrap;",
              div(
                style = "width: 30%; min-width: 200px;",
                numericInput(
                  inputId = "initial_investment",
                  label = "Initial Investment ($)",
                  value = 10000,
                  min = 0,
                  step = 1000
                )
              ),
              div(
                style = "width: 30%; min-width: 200px;",
                numericInput(
                  inputId = "monthly_investment",
                  label = "Monthly Investment ($)",
                  value = 500,
                  min = 0,
                  step = 50
                )
              ),
              div(
                style = "width: 30%; min-width: 200px;",
                selectInput(
                  inputId = "forecast_type",
                  label = "Forecast Type",
                  choices = c("Optimistic", "Forecast", "Pessimistic"),
                  selected = "Forecast"
                )
              )
            )
          ),
          valueBoxOutput("projected_value_box", width = 4)
        )
      ),
      
      # Data Overview Tab
      tabItem(
        tabName = "data_overview",
        fluidRow(
          box(
            title = "Data Tables",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              id = "data_tables",
              tabPanel("Market Data", 
                       DTOutput("market_data_table")
              ),
              tabPanel("Volatility Data", 
                       DTOutput("volatility_data_table")
              ),
              tabPanel("S&P 500 Data", 
                       DTOutput("sp500_data_table")
              )
            )
          )
        )
      )
    )
  ),
  skin = "blue"
)

# Shiny Server
server <- function(input, output, session) {
  
  # Reactive object for stock data
  stock_data <- reactive({
    scrape_stock_market_data()
  })
  
  # Reactive object for geographic data
  geo_data <- reactive({
    getgeodata()
  })
  
  # Merged data
  merged_data <- reactive({
    req(stock_data(), geo_data())
    merge(stock_data(), geo_data(), by = "country", all.x = TRUE)
  })
  
  # Reactive object for volatility data
  volatility_data <- reactive({
    scrape_volatility_data()
  })
  
  # Merged volatility data with geo data
  merged_volatility_data <- reactive({
    req(volatility_data(), geo_data())
    merge(volatility_data(), geo_data(), by = "country", all.x = TRUE)
  })
  
  # Calculate total rows and columns in CSV files
  csv_data_summary <- reactive({
    data_summary <- count_csv_data("data/") 
    stocks_summary <- count_csv_data("data/stocks/")
    total_rows <- data_summary$rows + stocks_summary$rows
    total_cols <- data_summary$cols + stocks_summary$cols
    return(list(rows = total_rows, cols = total_cols))
  })
  
  # KPIs Calculation
  output$total_observations_box <- renderValueBox({
    req(merged_data(), merged_volatility_data(), csv_data_summary())
    total_observations <- nrow(merged_data()) + nrow(merged_volatility_data()) + csv_data_summary()$rows
    valueBox(
      value = total_observations,
      subtitle = "Total Observations Analyzed",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$missing_values_box <- renderValueBox({
    req(merged_data(), merged_volatility_data())
    total_missing_values <- sum(is.na(merged_data())) + sum(is.na(merged_volatility_data()))
    valueBox(
      value = total_missing_values,
      subtitle = "Total Missing Values",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  output$issues_box <- renderValueBox({
    req(merged_data(), merged_volatility_data())
    data_issues <- 0  # Placeholder for actual data issues
    valueBox(
      value = data_issues,
      subtitle = "Data Issues Identified",
      icon = icon("bug"),
      color = "red"
    )
  })
  
  output$total_variables_box <- renderValueBox({
    req(merged_data(), merged_volatility_data(), csv_data_summary())
    total_variables <- ncol(merged_data()) + ncol(merged_volatility_data()) + csv_data_summary()$cols
    valueBox(
      value = total_variables,
      subtitle = "Total Variables Count",
      icon = icon("th-list"),
      color = "purple"
    )
  })
  
  output$countries_analyzed_box <- renderValueBox({
    req(merged_data(), merged_volatility_data())
    total_countries <- length(unique(c(merged_data()$country, merged_volatility_data()$country)))
    valueBox(
      value = total_countries,
      subtitle = "Total Countries Analyzed",
      icon = icon("globe"),
      color = "green"
    )
  })
  
  output$date_range_box <- renderValueBox({
    date_range <- calculate_date_range()
    valueBox(
      value = paste(format(date_range[1], "%Y"), "to", format(date_range[2], "%Y")),
      subtitle = "Date Range of Data",
      icon = icon("calendar-alt"),
      color = "teal"
    )
  })
  
  # Update country selection input
  observe({
    req(merged_data())
    updatePickerInput(
      session,
      "selected_countries",
      choices = merged_data()$country,
      selected = merged_data()$country  # Select all countries by default
    )
  })
  
  # Market Index Map
  output$market_map <- renderLeaflet({
    req(merged_data(), input$selected_countries)
    
    data <- merged_data() %>% filter(country %in% input$selected_countries)
    
    pal <- colorNumeric(palette = "viridis", domain = data$`Latest Value`)
    
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 8,
        color = ~pal(`Latest Value`),
        fillOpacity = 0.8,
        popup = ~paste(
          "Country: ", country, "<br>",
          "Index: ", `Latest Value`
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~`Latest Value`,
                title = "Market Index",
                opacity = 1
      )
  })
  
  # Market Index KPI Boxes
  output$min_index_box <- renderValueBox({
    req(merged_data(), input$selected_countries)
    data <- merged_data() %>% filter(country %in% input$selected_countries)
    min_index <- min(data$`Latest Value`, na.rm = TRUE)
    min_country <- data$country[which.min(data$`Latest Value`)]
    valueBox(
      value = round(min_index, 2),
      subtitle = paste("Minimum Index (", min_country, ")"),
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  output$max_index_box <- renderValueBox({
    req(merged_data(), input$selected_countries)
    data <- merged_data() %>% filter(country %in% input$selected_countries)
    max_index <- max(data$`Latest Value`, na.rm = TRUE)
    max_country <- data$country[which.max(data$`Latest Value`)]
    valueBox(
      value = round(max_index, 2),
      subtitle = paste("Maximum Index (", max_country, ")"),
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$avg_index_box <- renderValueBox({
    req(merged_data(), input$selected_countries)
    data <- merged_data() %>% filter(country %in% input$selected_countries)
    avg_index <- mean(data$`Latest Value`, na.rm = TRUE)
    valueBox(
      value = round(avg_index, 2),
      subtitle = "Average Index",
      icon = icon("calculator"),
      color = "blue"
    )
  })
  
  # Market Changes Map
  output$changes_map <- renderLeaflet({
    req(merged_data(), input$select_period)
    
    change_column <- if(input$select_period == "3m") "Change 3 Months" else "Change 12 Months"
    
    pal <- colorNumeric(palette = "RdYlGn", domain = merged_data()[[change_column]])
    
    leaflet(merged_data()) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 8,
        color = ~pal(get(change_column)),
        fillOpacity = 0.8,
        popup = ~paste(
          "Country: ", country, "<br>",
          "Change: ", get(change_column), "%"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~get(change_column),
                title = paste("Market Change (", input$select_period, ")"),
                opacity = 1
      )
  })
  
  # Market Changes KPI Boxes
  output$min_change_box <- renderValueBox({
    req(merged_data(), input$select_period)
    change_column <- if(input$select_period == "3m") "Change 3 Months" else "Change 12 Months"
    data <- merged_data()
    min_change <- min(data[[change_column]], na.rm = TRUE)
    min_country <- data$country[which.min(data[[change_column]])]
    valueBox(
      value = paste0(round(min_change, 2), "%"),
      subtitle = paste("Minimum Change (", min_country, ")"),
      icon = icon("arrow-down"),
      color = "red"
    )
  })
  
  output$max_change_box <- renderValueBox({
    req(merged_data(), input$select_period)
    change_column <- if(input$select_period == "3m") "Change 3 Months" else "Change 12 Months"
    data <- merged_data()
    max_change <- max(data[[change_column]], na.rm = TRUE)
    max_country <- data$country[which.max(data[[change_column]])]
    valueBox(
      value = paste0(round(max_change, 2), "%"),
      subtitle = paste("Maximum Change (", max_country, ")"),
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$avg_change_box <- renderValueBox({
    req(merged_data(), input$select_period)
    change_column <- if(input$select_period == "3m") "Change 3 Months" else "Change 12 Months"
    data <- merged_data()
    avg_change <- mean(data[[change_column]], na.rm = TRUE)
    valueBox(
      value = paste0(round(avg_change, 2), "%"),
      subtitle = "Average Change",
      icon = icon("calculator"),
      color = "blue"
    )
  })
  
  # Volatility Map
  output$volatility_map <- renderLeaflet({
    req(merged_volatility_data(), input$top_n, input$volatility_range)
    
    data <- merged_volatility_data() %>%
      filter(volatility >= input$volatility_range[1], volatility <= input$volatility_range[2]) %>%
      top_n(input$top_n, wt = volatility)
    
    pal <- colorNumeric(palette = "YlOrRd", domain = data$volatility)
    
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = 0, lat = 30, zoom = 2) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 8,
        color = ~pal(volatility),
        fillOpacity = 0.8,
        popup = ~paste(
          "Country: ", country, "<br>",
          "Volatility: ", volatility, "<br>",
          "Rank: ", rank
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~volatility,
                title = "Stock Price Volatility",
                opacity = 1
      )
  })
  
  # Volatility Table
  output$volatility_table <- renderDataTable({
    req(volatility_data(), input$top_n, input$volatility_range)
    
    data <- volatility_data() %>%
      filter(volatility >= input$volatility_range[1], volatility <= input$volatility_range[2]) %>%
      top_n(input$top_n, wt = volatility)
    
    datatable(
      data[, c("rank", "country", "volatility")],
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 25, 50),
        order = list(list(0, 'asc'))
      ),
      colnames = c("Global Rank", "Country", "Volatility"),
      rownames = FALSE
    )
  })
  
  # Load S&P 500 data
  sp500_data <- reactive({
    read_csv("data/stocks/sp500_index.csv") %>%
      mutate(Date = as.Date(Date))
  })
  
  # Forecast S&P 500 using ARIMA
  forecast_sp500 <- function(data, forecast_months) {
    model <- auto.arima(data$`S&P500`)
    forecast <- forecast(model, h = forecast_months * 30)  # Assume 30 days per month for simplicity
    return(forecast)
  }
  
  # Reactive value for forecast data
  forecast_data <- reactiveVal(NULL)
  
  # Generate forecast when button is clicked
  observeEvent(input$forecast_btn, {
    req(sp500_data())
    forecast_months <- as.numeric(difftime(input$forecast_range[2], input$forecast_range[1], units = "days")) / 30
    forecast <- forecast_sp500(sp500_data(), forecast_months)
    forecast_data(forecast)
  })
  
  # Reset forecast and plot
  observeEvent(input$reset_btn, {
    forecast_data(NULL)
    updateDateRangeInput(session, "forecast_range", 
                         start = Sys.Date(),
                         end = Sys.Date() + 365)
  })
  
  # Enhanced S&P 500 Plot with forecast
  output$sp500_plot <- renderPlotly({
    req(sp500_data())
    
    data <- sp500_data()
    p <- plot_ly() %>%
      add_trace(data = data, x = ~Date, y = ~`S&P500`, type = 'scatter', mode = 'lines',
                line = list(color = '#1E90FF'), name = 'Historical') %>%
      layout(title = "S&P 500 Index Trend and Forecast",
             xaxis = list(title = "Date"),
             yaxis = list(title = "S&P 500 Index Value"),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1))
    
    if (!is.null(forecast_data())) {
      forecast <- forecast_data()
      forecast_dates <- seq(max(data$Date) + 1, by = "day", length.out = length(forecast$mean))
      p <- p %>%
        add_trace(x = forecast_dates, y = forecast$mean, type = 'scatter', mode = 'lines',
                  line = list(color = '#FF4500', dash = 'dash'), name = 'Forecast') %>%
        add_ribbons(x = forecast_dates, ymin = forecast$lower[, 2], ymax = forecast$upper[, 2],
                    line = list(color = 'rgba(255, 69, 0, 0.3)'), 
                    fillcolor = 'rgba(255, 69, 0, 0.3)', name = '95% Confidence Interval')
    }
    
    return(p)
  })
  
  # Calculate investment returns with improved logic
  calculate_returns <- function(initial_investment, monthly_investment, forecast, forecast_type) {
    total_periods <- length(forecast$mean) / 30  # Convert days to months
    portfolio_value <- initial_investment
    log_data <- data.frame(
      Period = integer(),
      Portfolio_Value_Start = numeric(),
      Monthly_Return = numeric(),
      Investment_Added = numeric(),
      Portfolio_Value_End = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:total_periods) {
      if (i > 1) {
        if (forecast_type == "Optimistic") {
          monthly_return <- (forecast$upper[i*30] - forecast$upper[(i-1)*30]) / forecast$upper[(i-1)*30]
        } else if (forecast_type == "Pessimistic") {
          monthly_return <- (forecast$lower[i*30] - forecast$lower[(i-1)*30]) / forecast$lower[(i-1)*30]
        } else {
          monthly_return <- (forecast$mean[i*30] - forecast$mean[(i-1)*30]) / forecast$mean[(i-1)*30]
        }
      } else {
        monthly_return <- 0  # No return for the first period
      }
      
      portfolio_value_start <- portfolio_value
      
      # Apply return to current portfolio
      portfolio_value <- portfolio_value * (1 + monthly_return)
      
      # Add monthly investment
      portfolio_value <- portfolio_value + monthly_investment
      
      log_data <- rbind(log_data, data.frame(
        Period = i,
        Portfolio_Value_Start = portfolio_value_start,
        Monthly_Return = monthly_return,
        Investment_Added = monthly_investment,
        Portfolio_Value_End = portfolio_value
      ))
    }
    
    # Calculate total invested amount and ROI
    total_invested <- initial_investment + (monthly_investment * total_periods)
    roi <- (portfolio_value - total_invested) / total_invested * 100
    
    # Write log data to a CSV file
    write.csv(log_data, "data/log/investment_calculation_log.csv", row.names = FALSE)
    
    return(list(final_value = portfolio_value, log_data = log_data, total_invested = total_invested, roi = roi))
  }
  
  # Update projected value box with improved calculation
  output$projected_value_box <- renderValueBox({
    req(forecast_data(), input$calc_btn)
    
    forecast <- forecast_data()
    calculation_result <- calculate_returns(input$initial_investment, input$monthly_investment, forecast, input$forecast_type)
    projected_value <- calculation_result$final_value
    log_data <- calculation_result$log_data
    total_invested <- calculation_result$total_invested
    roi <- calculation_result$roi
    
    # Log additional information
    cat("Initial Investment:", input$initial_investment, "\n")
    cat("Monthly Investment:", input$monthly_investment, "\n")
    cat("Forecast Type:", input$forecast_type, "\n")
    cat("Forecast Periods:", length(forecast$mean) / 30, "\n")  # Convert days to months
    cat("Total Invested:", total_invested, "\n")
    cat("Projected Value:", projected_value, "\n")
    cat("ROI:", roi, "%\n")
    
    # Print the first few and last few rows of the log data
    print(head(log_data))
    print(tail(log_data))
    
    valueBox(
      value = paste0("$", format(round(projected_value, 2), big.mark = ",")),
      subtitle = paste0("Projected Value (ROI: ", round(roi, 2), "%)"),
      color = if(roi >= 0) "green" else "red",
      icon = icon("chart-line")
    )
  })
  
  # Data Tables
  output$market_data_table <- renderDataTable({
    req(merged_data())
    datatable(merged_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$volatility_data_table <- renderDataTable({
    req(volatility_data())
    datatable(volatility_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$sp500_data_table <- renderDataTable({
    req(sp500_data())
    datatable(sp500_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Run the application
shinyApp(ui, server)
