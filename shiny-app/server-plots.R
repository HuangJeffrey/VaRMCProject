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

output$regime_table <- renderTable({
  res <- results()
  data.frame(
    Ticker      = res$tickers,
    Volatility  = round(res$regime_vols, 6),
    Mean_Return = round(res$regime_means, 6)
  )
}, digits = 6)

output$return_plot <- renderPlot({
  res <- results()
  
  all_prices <- do.call(rbind, lapply(res$tickers, function(tk) {
    rd <- res$portfolio[[tk]]$returns_data
    rd$price <- cumprod(1 + (exp(rd$returns_per_stock) - 1)) * 100
    rd$ticker <- tk
    rd
  }))
  
  ggplot(all_prices, aes(x = date, y = price, colour = ticker)) +
    geom_line(linewidth = 0.5) +
    labs(title = "Cumulative Stock Performance (Base = $100)",
         x = "", y = "Price ($)") +
    theme_minimal(base_size = 14)
})
plotOutput("return_plot", height = "700px")

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
