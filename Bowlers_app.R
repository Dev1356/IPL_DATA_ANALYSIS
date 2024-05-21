# Load necessary libraries
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(forecast)
library(tseries)
library(prophet)
library(ggplot2)
library(rugarch)

# Load the dataset
bowlers_data <- read_excel('C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\bowlers_data_dev.xlsx')

# Ensure the W column exists and is numeric
if (!"W" %in% names(bowlers_data)) {
  stop("The dataset does not contain a 'W' column.")
}
bowlers_data$W <- as.numeric(bowlers_data$W)

# UI for the Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "Bowler Wickets Forecasting"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "forecast",
              fluidRow(
                box(title = "Select Bowler", status = "primary", solidHeader = TRUE, 
                    selectInput("bowler", "Select Bowler:", choices = unique(bowlers_data$Bowler)),
                    actionButton("forecastButton", "Forecast Wickets")),
                box(title = "Forecasted Wickets for Next 3 Matches", status = "primary", solidHeader = TRUE, 
                    tableOutput("forecastOutput")),
                box(title = "Forecast Plot", status = "primary", solidHeader = TRUE, 
                    plotOutput("forecastPlot")),
                box(title = "Historical Wickets", status = "primary", solidHeader = TRUE, 
                    plotOutput("historyPlot"))
              )
      )
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  observeEvent(input$forecastButton, {
    bowler_data <- bowlers_data %>% filter(Bowler == input$bowler)
    
    # Ensure there is data for the selected bowler
    if (nrow(bowler_data) == 0) {
      output$forecastOutput <- renderPrint({
        "No data available for the selected bowler."
      })
      output$forecastPlot <- renderPlot({ NULL })
      output$historyPlot <- renderPlot({ NULL })
      return()
    }
    
    ts_data <- ts(bowler_data$W, frequency = 1)
    
    # Ensure there are enough observations for time series analysis
    if (length(ts_data) < 2) {
      output$forecastOutput <- renderPrint({
        "Not enough data to perform time series analysis."
      })
      output$forecastPlot <- renderPlot({ NULL })
      output$historyPlot <- renderPlot({ NULL })
      return()
    }
    
    # Fit ARIMA model
    fit_arima <- auto.arima(ts_data)
    forecast_arima <- forecast(fit_arima, h = 3)
    
    # Fit ETS model
    fit_ets <- ets(ts_data)
    forecast_ets <- forecast(fit_ets, h = 3)
    
    # Fit Prophet model
    df <- data.frame(ds = seq.Date(from = Sys.Date() - length(bowler_data$W) + 1, by = "days", length.out = length(bowler_data$W)),
                     y = bowler_data$W)
    fit_prophet <- prophet(df)
    future <- make_future_dataframe(fit_prophet, periods = 3)
    forecast_prophet <- predict(fit_prophet, future)
    
    # Fit GARCH model
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
                       distribution.model = "norm")
    fit_garch <- ugarchfit(spec = spec, data = ts_data)
    forecast_garch <- ugarchforecast(fit_garch, n.ahead = 3)
    forecast_garch_mean <- as.numeric(fitted(forecast_garch))
    
    forecast_df <- data.frame(
      Model = c("ARIMA", "ETS", "Prophet", "GARCH"),
      Match1 = round(c(forecast_arima$mean[1], forecast_ets$mean[1], forecast_prophet$yhat[nrow(forecast_prophet) - 2], forecast_garch_mean[1])),
      Match2 = round(c(forecast_arima$mean[2], forecast_ets$mean[2], forecast_prophet$yhat[nrow(forecast_prophet) - 1], forecast_garch_mean[2])),
      Match3 = round(c(forecast_arima$mean[3], forecast_ets$mean[3], forecast_prophet$yhat[nrow(forecast_prophet)], forecast_garch_mean[3]))
    )
    
    output$forecastOutput <- renderTable({
      forecast_df
    })
    
    output$forecastPlot <- renderPlot({
      # Plot historical data
      plot(ts_data, type = "l", main = paste("Forecast for", input$bowler), xlab = "Match", ylab = "Wickets", xlim = c(1, length(ts_data) + 3), ylim = range(c(ts_data, forecast_arima$mean, forecast_ets$mean, tail(forecast_prophet$yhat, 3), forecast_garch_mean)))
      
      # Add forecasted data
      points(length(ts_data) + 1:3, round(forecast_arima$mean), col = "blue", type = "o", pch = 16)
      points(length(ts_data) + 1:3, round(forecast_ets$mean), col = "red", type = "o", pch = 16)
      points(length(ts_data) + 1:3, round(tail(forecast_prophet$yhat, 3)), col = "green", type = "o", pch = 16)
      points(length(ts_data) + 1:3, round(forecast_garch_mean), col = "purple", type = "o", pch = 16)
      
      # Add legend
      legend("topleft", legend = c("Historical", "ARIMA", "ETS", "Prophet", "GARCH"), col = c("black", "blue", "red", "green", "purple"), lty = 1, pch = 16)
    })
    
    output$historyPlot <- renderPlot({
      ggplot(bowler_data, aes(x = seq_along(W), y = W)) +
        geom_line(color = "blue") +
        geom_point(color = "red") +
        labs(title = paste("Historical Wickets for", input$bowler), x = "Match", y = "Wickets") +
        theme_minimal()
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

# Run the app
shiny::runApp("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\rcodes\\Batsman_app.R")
