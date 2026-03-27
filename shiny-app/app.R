library(shiny)
library(tidyquant)
library(depmixS4)
library(dplyr)
library(ggplot2)
library(shinyjs)

# Dark mode & welcome page CSS
dark_mode_css <- "
  * {
    color-scheme: dark;
  }
  
  body {
    background-color: #000000;
    color: #e0e0e0;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
  }
  
  .navbar, .navbar-default {
    background-color: #0f0f0f;
    border-bottom: 1px solid #1a1a1a;
  }
  
  .sidebar-panel {
    background-color: #0f0f0f;
    border-right: 1px solid #1a1a1a;
  }
  
  .main-panel {
    background-color: #000000;
  }
  
  .well {
    background-color: #0f0f0f;
    border: 1px solid #1a1a1a;
    color: #e0e0e0;
  }
  
  .form-control, input[type='text'], input[type='number'], input[type='date'], input[type='range'], select {
    background-color: #1a1a1a;
    color: #e0e0e0;
    border: 1px solid #2a2a2a;
  }
  
  .form-control:focus, input:focus, select:focus {
    background-color: #1a1a1a;
    color: #e0e0e0;
    border-color: #4a9eff;
    box-shadow: 0 0 0 0.2rem rgba(74, 158, 255, 0.25);
  }
  
  .btn-primary {
    background-color: #4a9eff;
    border-color: #4a9eff;
  }
  
  .btn-primary:hover {
    background-color: #2979ca;
    border-color: #2979ca;
  }
  
  .btn-warning {
    background-color: #ff9500;
    border-color: #ff9500;
  }
  
  .btn-warning:hover {
    background-color: #cc7700;
    border-color: #cc7700;
  }
  
  .nav-tabs {
    border-bottom: 1px solid #2a2a2a;
  }
  
  .nav-tabs > li > a {
    color: #808080;
    background-color: #0f0f0f;
    border: 1px solid #2a2a2a;
  }
  
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover,
  .nav-tabs > li.active > a:focus {
    color: #e0e0e0;
    background-color: #000000;
    border: 1px solid #2a2a2a;
    border-bottom-color: #000000;
  }
  
  .nav-tabs > li > a:hover {
    background-color: #1a1a1a;
    border-color: #2a2a2a;
  }
  
  h1, h2, h3, h4, h5, h6 {
    color: #ffffff;
  }
  
  .help-text {
    color: #808080;
  }
  
  table {
    background-color: #0f0f0f;
    color: #e0e0e0;
  }
  
  thead {
    background-color: #1a1a1a;
  }
  
  th, td {
    border-color: #2a2a2a;
    color: #e0e0e0;
  }
  
  .navbar-brand {
    color: #e0e0e0 !important;
  }
  
  .dark-numeric-input input[type='number'] {
    background-color: #1a1a1a !important;
    color: #e0e0e0 !important;
    border: 1px solid #2a2a2a !important;
    padding: 8px !important;
    border-radius: 4px !important;
  }
  
  .dark-numeric-input input[type='number']:focus {
    border-color: #4a9eff !important;
    box-shadow: 0 0 0 0.2rem rgba(74, 158, 255, 0.25) !important;
  }
  
  .verbatim {
    background-color: #1a1a1a;
    color: #00ff00;
    border: 1px solid #2a2a2a;
  }
  
  .stock-btn {
    margin: 3px;
    padding: 6px 14px;
    border: 1px solid #2a2a2a;
    border-radius: 5px;
    cursor: pointer;
    background: #1a1a1a;
    color: #e0e0e0;
    font-weight: bold;
    display: inline-block;
  }
  
  .stock-btn:hover {
    background: #252525;
    border-color: #3a3a3a;
  }
  
  .stock-btn.selected {
    background: #4a9eff;
    color: white;
    border-color: #4a9eff;
  }
  
  .sector-label {
    font-size: 13px;
    color: #666;
    margin-top: 10px;
    margin-bottom: 2px;
  }
  
  /* Welcome page styles */
  .welcome-container {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    min-height: 100vh;
    background: linear-gradient(135deg, #000000 0%, #0f0f0f 100%);
    padding: 40px 20px;
  }
  
  .welcome-box {
    background: #0f0f0f;
    border: 2px solid #4a9eff;
    border-radius: 15px;
    padding: 60px 50px;
    max-width: 700px;
    text-align: center;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5);
  }
  
  .welcome-title {
    color: #4a9eff;
    font-size: 48px;
    font-weight: bold;
    margin-bottom: 20px;
    letter-spacing: 2px;
  }
  
  .welcome-subtitle {
    color: #808080;
    font-size: 18px;
    margin-bottom: 40px;
    line-height: 1.6;
  }
  
  .welcome-btn {
    background: linear-gradient(135deg, #4a9eff 0%, #2979ca 100%);
    border: none;
    color: white;
    padding: 15px 50px;
    font-size: 18px;
    font-weight: bold;
    border-radius: 8px;
    cursor: pointer;
    transition: transform 0.2s, box-shadow 0.2s;
    margin-top: 20px;
  }
  
  .welcome-btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 20px rgba(74, 158, 255, 0.4);
  }
  
  .welcome-description {
    color: #d0d0d0;
    font-size: 15px;
    line-height: 1.8;
    text-align: left;
    background: #1a1a1a;
    padding: 30px;
    border-radius: 10px;
    margin-bottom: 30px;
  }
"

ui <- fluidPage(
  useShinyjs(),
  tags$style(HTML(dark_mode_css)),
  
  # Use shinyjs to toggle between pages
  uiOutput("dynamic_ui")
)

server <- function(input, output, session) {
  
  # Reactive value to track which page to show
  show_main <- reactiveVal(FALSE)
  
  # Welcome page UI
  welcome_ui <- function() {
    div(
      class = "welcome-container",
      div(
        class = "welcome-box",
        div(class = "welcome-title", "VaRMC"),
        div(class = "welcome-subtitle", "Portfolio Value-at-Risk with Regime-Switching Monte Carlo"),
        
        div(
          class = "welcome-description",
          p("Welcome to VaRMC, a sophisticated tool for portfolio risk analysis."),
          p("This application combines Hidden Markov Models with Monte Carlo simulation to estimate Value-at-Risk across multiple market regimes."),
          br(),
          p("Key Features:"),
          tags$ul(
            tags$li("Regime detection using HMM for volatility clustering"),
            tags$li("Correlation-based portfolio simulation with Cholesky decomposition"),
            tags$li("Monte Carlo Value-at-Risk with bootstrap confidence intervals"),
            tags$li("Rolling window backtesting for model validation"),
            tags$li("Real-time analysis with live market data")
          )
        ),
        
        actionButton(
          "start_btn",
          "Start Analysis",
          class = "welcome-btn",
          style = "width: 100%; border: none; padding: 15px; font-size: 16px;"
        )
      )
    )
  }
  
  # Main app UI
  main_ui <- function() {
    fluidPage(
      useShinyjs(),
      tags$style(HTML(dark_mode_css)),
      
      titlePanel(
        div(
          h2("VaRMC", style = "margin:0; font-weight:bold; color:#4a9eff;"),
          p("Portfolio Value-at-Risk with Regime-Switching Monte Carlo",
            style = "margin:0; color:#888;")
        )
      ),
      
      hr(),
      
      sidebarLayout(
        
        sidebarPanel(
          width = 2,
          class = "sidebar-panel",
          
          h4("Portfolio Setup"),
          uiOutput("stock_selector"),
          dateRangeInput("dates", "Date Range",
                         start = "2015-01-01", end = Sys.Date()),
          
          hr(),
          h4("VaR Settings"),
          sliderInput("conf", "Confidence Level",
                      min = 0.95, max = 0.99, value = 0.95, step = 0.01),
          numericInput("horizon", "Time Horizon (days)",
                       value = 10, min = 1, max = 200),
          numericInput("num_sims", "Monte Carlo Simulations",
                       value = 10000, min = 1000, max = 50000, step = 1000),
          
          hidden(
            div(
              numericInput("capital", "Capital ($)",
                           value = 1000000, min = 1000000, max = 1000000, step = 0)
            )
          ),
          
          hr(),
          h4("Bootstrap CI"),
          numericInput("B", "Resamples", value = 500, min = 100, max = 5000),
          sliderInput("ci_level", "CI Level",
                      min = 0.95, max = 0.99, value = 0.95, step = 0.01),
          
          hr(),
          actionButton("run", "Compute VaR",
                       class = "btn-primary", style = "width:100%; font-size:16px;"),
          br(), br(),
          helpText("VaR runs in ~30s.")
        ),
        
        mainPanel(
          width = 9,
          class = "main-panel",
          tabsetPanel(
            id = "tabs",
            
            tabPanel("Introduction",
                     br(),
                     div(style = "background: #0f0f0f; border: 2px solid #4a9eff; border-radius: 12px; padding: 30px;",
                         div(style = "color: #c0c0c0; font-size: 15px; line-height: 1.8;",
                             p(style = "color: #ffffff; font-weight: bold; font-size: 16px;", "Follow these steps to analyze portfolio risk:"),
                             
                             h5(style = "color: #ffffff; margin-top: 20px;", "Step 1: Select Stocks"),
                             p("On the left sidebar, click on stock tickers to select your portfolio. You must select at least 2 stocks. The selected stocks will appear highlighted in blue."),
                             
                             h5(style = "color: #ffffff; margin-top: 20px;", "Step 2: Set Stock Weights"),
                             p("For each selected stock, enter a weight in the Weights section. Your weights", strong("must add up to 1.0"), "(or 100%). For example: if you select 2 stocks, you could use 0.5 and 0.5, or 0.3 and 0.7, etc."),
                             
                             h5(style = "color: #ffffff; margin-top: 20px;", "Step 3: Define the Date Range"),
                             p("Choose the historical date range for analyzing your stocks. The date range should contain sufficient historical data to fit the Hidden Markov Model for regime detection (typically 2+ years of daily returns)."),
                             
                             h5(style = "color: #ffffff; margin-top: 20px;", "Step 4: Set VaR Parameters"),
                             p(strong("Confidence Level: "), "Choose your confidence level (default 95%). This is the threshold for your Value-at-Risk estimate."),
                             p(strong("Time Horizon: "), "Set the number of days you want to project forward (e.g., 10 days). VaR estimates the maximum loss over this period."),
                             p(strong("Monte Carlo Simulations: "), "Choose the number of simulation paths (default 10,000). More simulations provide more accurate estimates but take longer to compute."),
                             
                             h5(style = "color: #ffffff; margin-top: 20px;", "Step 5: Bootstrap Confidence Interval"),
                             p(strong("Resamples: "), "Number of bootstrap resamples to estimate the CI around your VaR (default 500)."),
                             p(strong("CI Level: "), "Confidence level for the interval (default 95%)."),
                             
                             h5(style = "color: #ffffff; margin-top: 20px;", "Step 6: Click 'Compute VaR'"),
                             p("Once all settings are configured, click the", strong("Compute VaR"), "button to run the full pipeline. The process will fetch historical data, fit regime-switching models, run Monte Carlo simulations, and compute VaR estimates (~30 seconds)."),
                             
                             p(style = "margin-top: 30px; color: #808080; font-style: italic;",
                               "After computation, explore the tabs above to view individual stock regimes, correlation matrices, simulation paths, VaR results, and backtesting validation.")
                         )
                     )
            ),
            
            tabPanel("Overview",
                     br(),
                     div(style = "background: #0f0f0f; border: 2px solid #4a9eff; border-radius: 12px; padding: 30px;",
                         div(style = "color: #c0c0c0; font-size: 15px; line-height: 1.8;",
                             h3(style = "color: #ffffff; margin-top: 0;", "What is VaRMC?"),
                             p("VaRMC (Portfolio Value-at-Risk with Regime-Switching Monte Carlo) is a sophisticated quantitative tool for measuring and analyzing portfolio risk. It combines Hidden Markov Models (HMM) for regime detection with Monte Carlo simulations to estimate the Value-at-Risk (VaR) of your investment portfolio under different market regimes."),
                             
                             h4(style = "color: #ffffff; margin-top: 30px;", "Key Concepts"),
                             
                             h5(style = "color: #4a9eff;", "Value-at-Risk (VaR)"),
                             p("VaR is the maximum expected loss over a given time period at a specific confidence level. For example, a 95% VaR of $50,000 means there is a 95% probability that your portfolio will not lose more than $50,000 over the specified time horizon."),
                             
                             h5(style = "color: #4a9eff;", "Regime-Switching Models"),
                             p("Markets don't behave uniformly. Volatility and correlations change over time depending on market regimes (e.g., calm periods vs. crisis periods). VaRMC uses Hidden Markov Models to detect which regime each stock is currently in, allowing for more accurate risk estimates."),
                             
                             h5(style = "color: #4a9eff;", "Monte Carlo Simulation"),
                             p("Rather than assuming returns follow a simple normal distribution, VaRMC runs thousands of simulations of potential future portfolio paths using the estimated volatilities and correlations. This provides a robust, empirical distribution of possible outcomes."),
                             
                             h5(style = "color: #4a9eff;", "Bootstrap Confidence Intervals"),
                             p("To quantify uncertainty in the VaR estimate itself, VaRMC uses bootstrap resampling to compute confidence intervals. This tells you the range within which the true VaR likely falls."),
                             
                             h4(style = "color: #ffffff; margin-top: 30px;", "How the Analysis Works"),
                             
                             p(strong("1. Regime Detection: "), "For each stock, historical returns are analyzed using a Hidden Markov Model to identify periods of high and low volatility. This captures the dynamic nature of market risk."),
                             
                             p(strong("2. Correlation & Covariance: "), "The correlation matrix shows how your stocks move together. The covariance matrix encodes both individual volatilities and correlations—essential inputs for accurate portfolio risk."),
                             
                             p(strong("3. Simulation: "), "Using a Cholesky decomposition of the covariance matrix, VaRMC generates correlated random paths for your portfolio over the specified time horizon. This produces a realistic distribution of potential portfolio returns."),
                             
                             p(strong("4. VaR Estimation: "), "From the simulated distribution, VaR is calculated as the percentile loss at your chosen confidence level. The bootstrap CI gives you statistical confidence in the estimate."),
                             
                             p(strong("5. Backtesting: "), "The Kupiec Proof-of-Concept backtest validates whether your VaR model is accurate by comparing predicted violations to actual historical violations."),
                             
                             h4(style = "color: #ffffff; margin-top: 30px;", "Tab Explanations"),
                             
                             p(strong(style = "color: #4a9eff;", "Introduction: "), "Step-by-step instructions for setting up and running your analysis."),
                             
                             p(strong(style = "color: #4a9eff;", "Regime Detection: "), "View the estimated regime (high/low volatility) for each stock and its price trajectory. This shows how regimes change over time."),
                             
                             p(strong(style = "color: #4a9eff;", "Correlation: "), "Explore the correlation matrix (how stocks move together) and the covariance matrix (volatilities and co-movements). Color intensity indicates correlation strength."),
                             
                             p(strong(style = "color: #4a9eff;", "Simulation: "), "See the Monte Carlo fan chart (100 simulated paths) and the resulting P&L distribution. The fan chart shows the range of possible outcomes."),
                             
                             p(strong(style = "color: #4a9eff;", "VaR Results: "), "The key output: your estimated VaR with 95% bootstrap confidence interval. Also shows portfolio parameters and the P&L distribution with the VaR threshold marked in red."),
                             
                             p(strong(style = "color: #4a9eff;", "Backtesting: "), "Validation results using the Kupiec POF test. Compares expected vs. observed violation rates to assess model accuracy."),
                             
                             p(style = "margin-top: 30px; color: #808080; font-style: italic;",
                               "For questions or feedback on methodology, refer to the academic literature on regime-switching models and Value-at-Risk estimation.")
                         )
                     )
            ),
            
            tabPanel("Regime Detection",
                     br(),
                     div(style = "background: #0f0f0f; border: 2px solid #4a9eff; border-radius: 8px; padding: 20px; margin-bottom: 30px;",
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Individual Stock Charts: "), 
                           "Historical price trajectory for each selected stock. The Hidden Markov Model identifies different volatility regimes (high and low volatility periods) underlying these price movements."),
                         br(),
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Current Regime: "), 
                           "The current volatility state of each stock—either low or high volatility. This regime classification influences your portfolio risk estimates."),
                         br(),
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Volatility: "), 
                           "The annualized standard deviation of returns in the current regime. Higher volatility indicates larger price swings and higher portfolio risk.")
                     ),
                     h4("Individual Stock Charts"),
                     uiOutput("stock_charts")
            ),
            
            tabPanel("Correlation",
                     br(),
                     div(style = "background: #0f0f0f; border: 2px solid #4a9eff; border-radius: 8px; padding: 20px; margin-bottom: 30px;",
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Correlation Matrix: "), 
                           "Measures how stocks move together, with values from -1 (inverse) to +1 (perfectly aligned). Darker blue indicates stronger positive correlation. Helps assess portfolio diversification."),
                         br(),
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Covariance Matrix: "), 
                           "Combines individual volatilities with correlations. These values feed the Monte Carlo simulation to generate realistic correlated price paths.")
                     ),
                     h4("Correlation Matrix", style = "color:#ffffff; margin-bottom:20px;"),
                     uiOutput("corr_table"),
                     h4("Covariance Matrix", style = "color:#ffffff; margin-top:40px; margin-bottom:20px;"),
                     uiOutput("cov_table")
            ),
            
            tabPanel("Simulation",
                     br(),
                     div(style = "background: #0f0f0f; border: 2px solid #4a9eff; border-radius: 8px; padding: 20px; margin-bottom: 30px;",
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Monte Carlo Fan Chart: "), 
                           "Simulated portfolio paths over your time horizon. Each line represents one possible outcome. Tight clustering suggests low risk; wide spread suggests high risk."),
                         br(),
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Simulated P&L Distribution: "), 
                           "Histogram of profits and losses from all simulations. The distribution shape reveals tail risk—the likelihood of large losses.")
                     ),
                     h4("Monte Carlo Fan Chart"),
                     plotOutput("fan_chart", height = "500px"),
                     hr(),
                     h4("Simulated P&L Distribution"),
                     plotOutput("pnl_hist", height = "400px")
            ),
            
            tabPanel("VaR Results",
                     br(),
                     div(style = "background: #0f0f0f; border: 2px solid #4a9eff; border-radius: 8px; padding: 20px; margin-bottom: 30px;",
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("VaR Estimate (Loss Threshold): "), 
                           "The maximum expected loss at your chosen confidence level over the specified time horizon."),
                         br(),
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Bootstrap Confidence Interval: "), 
                           "Range where the true VaR likely falls. Narrows indicate stable estimates; wider intervals suggest higher estimation uncertainty."),
                         br(),
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("P&L Distribution with VaR Threshold: "), 
                           "The red dashed line marks your VaR. Losses beyond this line exceed your risk threshold.")
                     ),
                     uiOutput("var_text"),
                     hr(),
                     h4("P&L Distribution with VaR Threshold", style = "color:#ffffff; margin-top:40px;"),
                     plotOutput("var_plot", height = "400px")
            ),
            
            tabPanel("Backtesting",
                     br(),
                     div(style = "background: #0f0f0f; border: 2px solid #4a9eff; border-radius: 8px; padding: 20px; margin-bottom: 30px;",
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Rolling Window Size: "), 
                           "The historical period used to estimate VaR at each point. Balances stability with responsiveness to recent market changes."),
                         br(),
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Kupiec POF Test: "), 
                           "Validates your VaR model by comparing expected violations (theoretical) with observed violations (actual history). Close alignment confirms model accuracy."),
                         br(),
                         p(style = "color: #c0c0c0; margin: 0;",
                           strong("Expected vs Observed Violations: "), 
                           "At your confidence level, the test compares how many times losses should exceed VaR with how many times they actually did. Significant differences suggest model adjustment may be needed.")
                     ),
                     div(
                       style = "background: #242424; border: 2px solid #4a9eff; border-radius: 12px; padding: 20px; margin-bottom: 30px; width: 100%;",
                       h4("Backtest Settings", style = "color: #ffffff; margin-top: 0;"),
                       div(
                         style = "margin-bottom: 15px;",
                         tags$label("Rolling Window Size", style = "color: #e0e0e0; font-weight: bold; display: block; margin-bottom: 8px;"),
                         div(
                           class = "dark-numeric-input",
                           numericInput("bt_window", NULL,
                                        value = 500, min = 50, max = 1000)
                         )
                       ),
                       actionButton("run_bt", "Run Backtest",
                                    class = "btn-warning", 
                                    style = "width:100%; font-size:14px; padding: 10px;"),
                       br(),
                       helpText("Backtesting may take several minutes.", style = "color: #a0a0a0; margin-top: 10px;")
                     ),
                     hr(),
                     uiOutput("bt_text")
            )
          )
        )
      )
    )
  }
  
  # Dynamic UI output
  output$dynamic_ui <- renderUI({
    if (show_main()) {
      main_ui()
    } else {
      welcome_ui()
    }
  })
  
  # Start button event
  observeEvent(input$start_btn, {
    show_main(TRUE)
  })
  
  # Load server-side logic
  source(file.path("server-plots.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)