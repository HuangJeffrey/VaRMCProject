# Parse user inputs
get_tickers <- reactive({
  trimws(unlist(strsplit(input$tickers, ",")))
})

get_weights <- reactive({
  w <- as.numeric(trimws(unlist(strsplit(input$weights, ","))))
  w / sum(w)
})

# ── Main VaR pipeline ──────────────────────────────────────────────────
results <- eventReactive(input$run, {
  
  tickers <- get_tickers()
  weights <- get_weights()
  
  validate(
    need(length(tickers) >= 2, "Enter at least 2 tickers."),
    need(length(tickers) == length(weights),
         "Number of weights must match number of tickers.")
  )
  
  withProgress(message = "Running VaRMC pipeline...", value = 0, {
    
    # 1. Fetch data & fit HMM
    incProgress(0.15, detail = "Fetching stock data & fitting HMM...")
    portfolio <- lapply(tickers, function(tk) {
      stock <- tq_get(tk, from = input$dates[1], to = input$dates[2])
      stock_detection(stock)
    })
    names(portfolio) <- tickers
    
    # 2. Correlation & covariance
    incProgress(0.15, detail = "Correlation & covariance...")
    corr_mat     <- correlation_matrix_of_stock_portfolio(portfolio)
    regime_vols  <- sapply(portfolio, function(x) x$current_volatility)
    regime_means <- sapply(portfolio, function(x) x$current_mean)
    cov_mat      <- covariance_matrix_of_stock_portfolio(regime_vols, corr_mat)
    
    # 3. Cholesky & shocks
    incProgress(0.15, detail = "Cholesky & shock generation...")
    L      <- cholesky(cov_mat)
    shocks <- shock_gen(L, input$num_sims)
    
    # 4. GBM simulation
    incProgress(0.15, detail = "Simulating GBM...")
    sim_log    <- sim_gbm(regime_means, regime_vols, input$horizon, shocks)
    sim_simple <- log_to_simple_returns(sim_log)
    pnl        <- portfolio_pnl_from_returns(t(sim_simple), weights, input$capital)
    
    # 5. VaR & bootstrap CI
    incProgress(0.2, detail = "Computing VaR & bootstrap CI...")
    var_est <- compute_var(pnl, input$conf)
    ci      <- var_bootstrap_ci(pnl, conf = input$conf,
                                B = input$B, ci = input$ci_level, seed = 123)
    
    incProgress(0.2, detail = "Done!")
    
    list(
      portfolio    = portfolio,
      corr_mat     = corr_mat,
      cov_mat      = cov_mat,
      regime_vols  = regime_vols,
      regime_means = regime_means,
      sim_log      = sim_log,
      pnl          = pnl,
      var_est      = var_est,
      ci           = ci,
      tickers      = tickers,
      weights      = weights
    )
  })
})

# ── Backtesting (separate button) ──────────────────────────────────────
bt_results <- eventReactive(input$run_bt, {
  
  req(results())
  res <- results()
  
  withProgress(message = "Running Kupiec backtest...", value = 0.1, {
    
    total_returns <- res$portfolio[[1]]$returns_data[, c("date", "returns_per_stock")]
    for (i in 2:length(res$portfolio)) {
      temp <- res$portfolio[[i]]$returns_data[, c("date", "returns_per_stock")]
      total_returns <- merge(total_returns, temp, by = "date")
    }
    total_returns$date <- NULL
    returns_matrix <- as.matrix(total_returns)
    
    incProgress(0.3, detail = "Rolling window estimation...")
    
    result <- backtesting(
      returns_matrix = returns_matrix,
      weights        = res$weights,
      conf           = input$conf,
      window         = input$bt_window,
      time_horizon   = input$horizon,
      num_sims       = input$num_sims,
      capital        = input$capital
    )
    
    incProgress(0.6, detail = "Done!")
    result
  })
})

# ════════════════════════════════════════════════════════════════════════
# OUTPUTS
# ════════════════════════════════════════════════════════════════════════

# ── Tab 1: Regime Detection ────────────────────────────────────────────
# Build stock rows with toggles and weight inputs
output$stock_selector <- renderUI({
  
  sectors <- list(
    "Tech"             = c("AAPL", "NVDA", "TSLA"),
    "Financial"        = c("BRK-B", "V", "JPM"),
    "Defense"          = c("LMT", "NOC"),
    "Consumer Staples" = c("WMT", "COST")
  )
  
  tagList(
    tags$style(HTML("
      .stock-btn { margin: 3px; padding: 6px 14px; border: 1px solid #ccc;
                   border-radius: 5px; cursor: pointer; background: #f5f5f5;
                   font-weight: bold; display: inline-block; }
      .stock-btn.selected { background: #337ab7; color: white; border-color: #337ab7; }
      .sector-label { font-size: 13px; color: #888; margin-top: 10px; margin-bottom: 2px; }
    ")),
    
    h5("Select Stocks (click to toggle)"),
    
    lapply(names(sectors), function(sec) {
      tagList(
        p(sec, class = "sector-label"),
        div(
          lapply(sectors[[sec]], function(tk) {
            actionButton(paste0("btn_", gsub("-", "", tk)), label = tk, class = "stock-btn")
          })
        )
      )
    }),
    
    br(),
    h5("Weights"),
    uiOutput("weight_rows")
  )
})

# Track which stocks are selected
selected_stocks <- reactiveVal(character(0))

observe({
  stocks <- c("AAPL", "NVDA", "TSLA", "BRK-B", "V", "JPM", "LMT", "NOC", "WMT", "COST")
  
  lapply(stocks, function(tk) {
    btn_id <- paste0("btn_", gsub("-", "", tk))
    observeEvent(input[[btn_id]], {
      current <- selected_stocks()
      if (tk %in% current) {
        selected_stocks(setdiff(current, tk))
      } else {
        selected_stocks(c(current, tk))
      }
    }, ignoreInit = TRUE)
  })
})

observe({
  stocks <- c("AAPL", "NVDA", "TSLA", "BRK-B", "V", "JPM", "LMT", "NOC", "WMT", "COST")
  current <- selected_stocks()
  
  lapply(stocks, function(tk) {
    btn_id <- paste0("btn_", gsub("-", "", tk))
    if (tk %in% current) {
      shinyjs::addClass(id = btn_id, class = "selected")
    } else {
      shinyjs::removeClass(id = btn_id, class = "selected")
    }
  })
})

# Show weight input only for selected stocks
output$weight_rows <- renderUI({
  sel <- selected_stocks()
  if (length(sel) == 0) return(helpText("No stocks selected."))
  
  default_w <- round(1 / length(sel), 2)
  
  lapply(sel, function(tk) {
    fluidRow(
      column(6, strong(tk, style = "margin-top:8px;")),
      column(6, numericInput(paste0("w_", tk), label = NULL,
                             value = default_w, min = 0, max = 1, step = 0.01))
    )
  })
})

# Getters
get_tickers <- reactive({
  selected_stocks()
})

get_weights <- reactive({
  tickers <- get_tickers()
  req(length(tickers) >= 2)
  w <- sapply(tickers, function(tk) {
    val <- input[[paste0("w_", tk)]]
    if (is.null(val) || val == 0) 0.01 else val
  })
  w / sum(w)
})

# Dynamically create one chart per stock
output$stock_charts <- renderUI({
  res <- results()
  n <- length(res$tickers)
  
  plot_list <- lapply(1:n, function(i) {
    tk <- res$tickers[i]
    plot_id <- paste0("stock_plot_", i)
    info_id <- paste0("stock_info_", i)
    
    tagList(
      h4(tk),
      verbatimTextOutput(info_id),
      plotOutput(plot_id, height = "200px"),
      hr()
    )
  })
  
  do.call(tagList, plot_list)
})

# Render each chart and info box
observe({
  res <- results()
  
  lapply(1:length(res$tickers), function(i) {
    tk <- res$tickers[i]
    plot_id <- paste0("stock_plot_", i)
    info_id <- paste0("stock_info_", i)
    
    output[[plot_id]] <- renderPlot({
      rd <- res$portfolio[[tk]]$returns_data
      rd$price <- cumprod(1 + (exp(rd$returns_per_stock) - 1)) * 100
      
      ggplot(rd, aes(x = date, y = price)) +
        geom_line(colour = "steelblue", linewidth = 0.5) +
        labs(title = paste(tk),
             x = "Date", y = "Price ($)") +
        theme_minimal(base_size = 13)
    })
    
    output[[info_id]] <- renderPrint({
      vol <- res$regime_vols[tk]
      
      # Determine regime label based on volatility ranking
      all_vols <- sort(res$regime_vols)
      regime_rank <- which(names(all_vols) == tk)
      regime_label <- c("Low", "Medium", "High")[min(regime_rank, 3)]
      
      cat(tk, "\n")
      cat("  Current Regime:  ", regime_label, "volatility\n")
      cat("  Volatility:      ", round(vol, 6), "\n")
    })
  })
})

# ── Tab 2: Correlation ─────────────────────────────────────────────────

output$corr_table <- renderTable({
  res <- results()
  cm <- round(res$corr_mat, 4)
  colnames(cm) <- res$tickers
  data.frame(Ticker = res$tickers, cm, check.names = FALSE)
})

output$cov_table <- renderTable({
  res <- results()
  cv <- signif(res$cov_mat, 4)
  colnames(cv) <- res$tickers
  data.frame(Ticker = res$tickers, cv, check.names = FALSE)
}, digits = 8)

# ── Tab 3: Simulation ──────────────────────────────────────────────────

output$fan_chart <- renderPlot({
  res <- results()
  n_paths <- 200
  n_steps <- 5000
  dt <- 1 / 252
  
  port_vol <- sum(res$weights * res$regime_vols)
  
  set.seed(42)
  Z <- matrix(rnorm(n_steps * n_paths), nrow = n_steps, ncol = n_paths)
  paths <- apply(Z, 2, function(z) {
    cumsum(-0.5 * port_vol^2 * dt + port_vol * sqrt(dt) * z)
  })
  
  df <- data.frame(
    step  = rep(1:n_steps, times = n_paths),
    path  = rep(1:n_paths, each = n_steps),
    value = as.vector(paths)
  )
  
  ggplot(df, aes(x = step, y = value, group = path)) +
    geom_line(alpha = 0.25, linewidth = 0.3, colour = "steelblue") +
    labs(title = "Monte Carlo Fan Chart — Cumulative Log Returns",
         x = "Time Step", y = "Cumulative Log Return") +
    theme_minimal(base_size = 14)
})

output$pnl_hist <- renderPlot({
  res <- results()
  df <- data.frame(pnl = res$pnl)
  
  ggplot(df, aes(x = pnl)) +
    geom_histogram(bins = 80, fill = "steelblue", colour = "white", alpha = 0.8) +
    labs(title = "Simulated Portfolio P&L Distribution",
         x = "P&L ($)", y = "Count") +
    theme_minimal(base_size = 14)
})

# ── Tab 4: VaR Results ─────────────────────────────────────────────────

output$var_text <- renderPrint({
  res <- results()
  cat("========================================\n")
  cat("         VaR Results Summary\n")
  cat("========================================\n\n")
  cat("Portfolio:      ", paste(res$tickers, collapse = ", "), "\n")
  cat("Weights:        ", paste(round(res$weights, 2), collapse = ", "), "\n")
  cat("Confidence:     ", input$conf, "\n")
  cat("Time Horizon:   ", input$horizon, "days\n")
  cat("Simulations:    ", input$num_sims, "\n")
  cat("Capital:         $", format(input$capital, big.mark = ","), "\n")
  cat("----------------------------------------\n")
  cat("VaR Estimate:    $", format(round(res$var_est, 2), big.mark = ","), "\n")
  cat("Bootstrap CI:    [$",
      format(round(res$ci["lower"], 2), big.mark = ","), ", $",
      format(round(res$ci["upper"], 2), big.mark = ","), "]\n")
  cat("========================================\n")
})

output$var_plot <- renderPlot({
  res <- results()
  df <- data.frame(pnl = res$pnl)
  
  ggplot(df, aes(x = pnl)) +
    geom_histogram(bins = 80, fill = "steelblue", colour = "white", alpha = 0.7) +
    geom_vline(xintercept = -res$var_est, colour = "red",
               linewidth = 1.2, linetype = "dashed") +
    annotate("text", x = -res$var_est, y = Inf,
             label = paste0("VaR = $", format(round(res$var_est, 0), big.mark = ",")),
             vjust = 2, hjust = -0.1, colour = "red", size = 5, fontface = "bold") +
    labs(title = "P&L Distribution with VaR Threshold",
         x = "P&L ($)", y = "Count") +
    theme_minimal(base_size = 14)
})

# ── Tab 5: Backtesting ─────────────────────────────────────────────────

output$bt_text <- renderPrint({
  bt <- bt_results()
  cat("========================================\n")
  cat("     Kupiec POF Backtest Results\n")
  cat("========================================\n\n")
  cat("Total Observations:    ", bt$total_obs, "\n")
  cat("Expected Violations:   ", round(bt$expected_violations, 2), "\n")
  cat("Observed Violations:   ", bt$violations, "\n")
  cat("Expected Rate:         ", bt$expected_rate, "\n")
  cat("Observed Rate:         ", round(bt$observed_rate, 4), "\n")
  cat("----------------------------------------\n")
  cat("LR Statistic:          ", bt$LR, "\n")
  cat("Critical Value (95%):  ", bt$critical_value, "\n")
  cat("P-Value:               ", bt$p_value, "\n")
  cat("----------------------------------------\n")
  cat("Reject Model?          ", ifelse(bt$reject, "YES", "NO"), "\n")
  cat("========================================\n")
})
