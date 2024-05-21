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
data <- read_excel('C:\\Users\\Shiv\\Documents\\Final_Year_Project\\data by praju\\batsman_data_dev.xlsx')

# UI for the Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "Batter Runs Forecasting"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "forecast",
              fluidRow(
                box(title = "Select Batter", status = "primary", solidHeader = TRUE, 
                    selectInput("batter", "Select Batter:", choices = unique(data$Batter)),
                    actionButton("forecastButton", "Forecast Runs")),
                box(title = "Forecasted Runs for Next 3 Matches", status = "primary", solidHeader = TRUE, 
                    tableOutput("forecastOutput")),
                box(title = "Forecast Plot", status = "primary", solidHeader = TRUE, 
                    plotOutput("forecastPlot")),
                box(title = "Historical Runs", status = "primary", solidHeader = TRUE, 
                    plotOutput("historyPlot"))
              )
      )
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  observeEvent(input$forecastButton, {
    batter_data <- data %>% filter(Batter == input$batter)
    ts_data <- ts(batter_data$R, frequency = 1)
    
    # Fit ARIMA model
    fit_arima <- auto.arima(ts_data)
    forecast_arima <- forecast(fit_arima, h = 3)
    
    # Fit ETS model
    fit_ets <- ets(ts_data)
    forecast_ets <- forecast(fit_ets, h = 3)
    
    # Fit Prophet model
    df <- data.frame(ds = seq.Date(from = Sys.Date() - length(batter_data$R) + 1, by = "days", length.out = length(batter_data$R)),
                     y = batter_data$R)
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
      Match1 = c(forecast_arima$mean[1], forecast_ets$mean[1], forecast_prophet$yhat[nrow(forecast_prophet) - 2], forecast_garch_mean[1]),
      Match2 = c(forecast_arima$mean[2], forecast_ets$mean[2], forecast_prophet$yhat[nrow(forecast_prophet) - 1], forecast_garch_mean[2]),
      Match3 = c(forecast_arima$mean[3], forecast_ets$mean[3], forecast_prophet$yhat[nrow(forecast_prophet)], forecast_garch_mean[3])
    )
    
    output$forecastOutput <- renderTable({
      forecast_df
    })
    
    output$forecastPlot <- renderPlot({
      # Plot historical data
      plot(ts_data, type = "l", main = paste("Forecast for", input$batter), xlab = "Match", ylab = "Runs", xlim = c(1, length(ts_data) + 3), ylim = range(c(ts_data, forecast_arima$mean, forecast_ets$mean, tail(forecast_prophet$yhat, 3), forecast_garch_mean)))
      
      # Add forecasted data
      points(length(ts_data) + 1:3, forecast_arima$mean, col = "blue", type = "o", pch = 16)
      points(length(ts_data) + 1:3, forecast_ets$mean, col = "red", type = "o", pch = 16)
      points(length(ts_data) + 1:3, tail(forecast_prophet$yhat, 3), col = "green", type = "o", pch = 16)
      points(length(ts_data) + 1:3, forecast_garch_mean, col = "purple", type = "o", pch = 16)
      
      # Add legend
      legend("topleft", legend = c("Historical", "ARIMA", "ETS", "Prophet", "GARCH"), col = c("black", "blue", "red", "green", "purple"), lty = 1, pch = 16)
    })
    
    output$historyPlot <- renderPlot({
      ggplot(batter_data, aes(x = seq_along(R), y = R)) +
        geom_line(color = "blue") +
        geom_point(color = "red") +
        labs(title = paste("Historical Runs for", input$batter), x = "Match", y = "Runs") +
        theme_minimal()
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

# Run the app
shiny::runApp("C:\\Users\\Shiv\\Documents\\Final_Year_Project\\rcodes\\Batsman_app.R")
