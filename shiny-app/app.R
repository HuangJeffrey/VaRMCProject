library(shiny)
library(tidyquant)
library(depmixS4)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel(
    div(
      h2("VaRMC", style = "margin:0; font-weight:bold;"),
      p("Portfolio Value-at-Risk with Regime-Switching Monte Carlo",
        style = "margin:0; color:grey;")
    )
  ),
  
  hr(),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Portfolio Setup"),
      textInput("tickers", "Tickers (comma-separated)",
                value = "AAPL, NVDA, TSLA, V"),
      dateRangeInput("dates", "Date Range",
                     start = "2015-01-01", end = Sys.Date()),
      textInput("weights", "Weights (comma-separated)",
                value = "0.25, 0.25, 0.25, 0.25"),
      
      hr(),
      h4("VaR Settings"),
      sliderInput("conf", "Confidence Level",
                  min = 0.90, max = 0.99, value = 0.95, step = 0.01),
      numericInput("horizon", "Time Horizon (days)",
                   value = 10, min = 1, max = 200),
      numericInput("num_sims", "Monte Carlo Simulations",
                   value = 10000, min = 1000, max = 50000, step = 1000),
      numericInput("capital", "Capital ($)",
                   value = 1000000, min = 1000),
      
      hr(),
      h4("Bootstrap CI"),
      numericInput("B", "Resamples", value = 500, min = 100, max = 5000),
      sliderInput("ci_level", "CI Level",
                  min = 0.90, max = 0.99, value = 0.95, step = 0.01),
      
      hr(),
      h4("Backtesting"),
      numericInput("bt_window", "Rolling Window Size",
                   value = 500, min = 50, max = 1000),
      
      hr(),
      actionButton("run", "Compute VaR",
                   class = "btn-primary", style = "width:100%; font-size:16px;"),
      br(), br(),
      actionButton("run_bt", "Run Backtest",
                   class = "btn-warning", style = "width:100%; font-size:16px;"),
      br(), br(),
      helpText("VaR runs in ~30s. Backtesting may take several minutes.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Regime Detection",
                 br(),
                 h4("HMM Regime Parameters (Current State)"),
                 tableOutput("regime_table"),
                 hr(),
                 h4("Return Series"),
                 plotOutput("return_plot", height = "400px")
        ),
        
        tabPanel("Correlation",
                 br(),
                 h4("Correlation Matrix"),
                 tableOutput("corr_table"),
                 hr(),
                 h4("Covariance Matrix"),
                 tableOutput("cov_table")
        ),
        
        tabPanel("Simulation",
                 br(),
                 h4("Monte Carlo Fan Chart"),
                 plotOutput("fan_chart", height = "500px"),
                 hr(),
                 h4("Simulated P&L Distribution"),
                 plotOutput("pnl_hist", height = "400px")
        ),
        
        tabPanel("VaR Results",
                 br(),
                 verbatimTextOutput("var_text"),
                 hr(),
                 h4("P&L Distribution with VaR Threshold"),
                 plotOutput("var_plot", height = "400px")
        ),
        
        tabPanel("Backtesting",
                 br(),
                 verbatimTextOutput("bt_text"),
                 helpText("Click 'Run Backtest' in the sidebar. This may take several minutes.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  source(file.path("server-plots.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)
#shiny::runApp("shiny-app")