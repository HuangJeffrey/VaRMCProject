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

# Dark mode theme for ggplot2
theme_dark_varmc <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "#000000", color = NA),
      panel.background = element_rect(fill = "#0f0f0f", color = NA),
      panel.grid.major = element_line(color = "#1a1a1a", size = 0.2),
      panel.grid.minor = element_line(color = "#151515", size = 0.1),
      plot.title = element_text(color = "#ffffff", face = "bold", size = 16),
      plot.subtitle = element_text(color = "#808080", size = 13),
      axis.title = element_text(color = "#c0c0c0", size = 12),
      axis.text = element_text(color = "#808080", size = 11),
      legend.background = element_rect(fill = "#0f0f0f", color = NA),
      legend.text = element_text(color = "#c0c0c0"),
      legend.title = element_text(color = "#c0c0c0")
    )
}

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
    h5("Select Stocks"),
    
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

# Track which stock is expanded
expanded_stock <- reactiveVal(NULL)

# Dynamically create stock row buttons
output$stock_charts <- renderUI({
  # Only render when Regime Detection tab is active
  req(input$tabs == "Regime Detection")
  res <- results()
  n <- length(res$tickers)
  
  # Add CSS for accordion styling
  accordion_css <- "
    .stock-row {
      background: #0f0f0f;
      border: 2px solid #2a2a2a;
      border-radius: 8px;
      padding: 16px;
      margin: 10px 0;
      cursor: pointer;
      transition: all 0.3s ease;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }
    
    .stock-row:hover {
      background: #1a1a1a;
      border-color: #4a9eff;
    }
    
    .stock-row.active {
      background: #1a1a1a;
      border-color: #4a9eff;
    }
    
    .stock-row-ticker {
      font-size: 18px;
      font-weight: bold;
      color: #4a9eff;
    }
    
    .stock-row-volatility {
      font-size: 14px;
      color: #808080;
    }
    
    .stock-row-arrow {
      font-size: 20px;
      color: #4a9eff;
      transition: transform 0.3s ease;
    }
    
    .stock-row.active .stock-row-arrow {
      transform: rotate(180deg);
    }
    
    .stock-details {
      max-height: 0;
      overflow: hidden;
      opacity: 0;
      transition: max-height 0.3s ease, opacity 0.3s ease;
      visibility: hidden;
    }
    
    .stock-details.expanded {
      max-height: 600px;
      opacity: 1;
      visibility: visible;
    }
  "
  
  tagList(
    tags$style(HTML(accordion_css)),
    
    # Stock buttons/rows
    lapply(1:n, function(i) {
      tk <- res$tickers[i]
      vol <- res$regime_vols[tk]
      info_id <- paste0("stock_info_", i)
      details_id <- paste0("stock_details_", i)
      plot_id <- paste0("stock_plot_", i)
      
      tagList(
        # Stock row button
        div(
          class = "stock-row",
          id = paste0("stock_row_", i),
          onclick = sprintf("
            var row = document.getElementById('stock_row_%d');
            var details = document.getElementById('stock_details_%d');
            
            // Toggle current stock (without closing others)
            if (row.classList.contains('active')) {
              row.classList.remove('active');
              details.classList.remove('expanded');
            } else {
              row.classList.add('active');
              details.classList.add('expanded');
            }
          ", i, i),
          
          div(
            div(class = "stock-row-ticker", tk),
            div(class = "stock-row-volatility", sprintf("Volatility: %.6f", vol))
          ),
          div(class = "stock-row-arrow", "▼")
        ),
        
        # Expandable details
        div(
          class = "stock-details",
          id = paste0("stock_details_", i),
          style = "padding: 20px; background: #2a2a2a; border-radius: 0 0 8px 8px; border-top: 2px solid #404040; margin-bottom: 10px;",
          
          verbatimTextOutput(info_id),
          plotOutput(plot_id, height = "250px")
        )
      )
    })
  )
})

# Render each chart and info box (lazy loaded - only when tab is active)
observe({
  # Only render when Regime Detection tab is active
  req(input$tabs == "Regime Detection")
  res <- results()
  
  lapply(1:length(res$tickers), function(i) {
    tk <- res$tickers[i]
    plot_id <- paste0("stock_plot_", i)
    info_id <- paste0("stock_info_", i)
    
    output[[plot_id]] <- renderPlot({
      rd <- res$portfolio[[tk]]$returns_data
      rd$price <- cumprod(1 + (exp(rd$returns_per_stock) - 1)) * 100
      
      ggplot(rd, aes(x = date, y = price)) +
        geom_line(colour = "#4a9eff", linewidth = 0.6) +
        labs(title = paste(tk),
             x = "Date", y = "Price ($)") +
        theme_dark_varmc()
    }, bg = "#000000")
    
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

output$corr_table <- renderUI({
  req(input$tabs == "Correlation")
  res <- results()
  cm <- round(res$corr_mat, 4)
  
  # Create styled HTML table
  table_html <- "
    <div style='overflow-x: auto;'>
      <table style='
        width: 100%;
        border-collapse: collapse;
        font-family: monospace;
        font-size: 16px;
        background: #0f0f0f;
      '>
        <thead>
          <tr style='background: #1a1a1a; border-bottom: 3px solid #4a9eff;'>
            <th style='padding: 16px; text-align: left; color: #ffffff; font-weight: bold; border-right: 2px solid #2a2a2a;'>Ticker</th>
  "
  
  # Add ticker headers
  for (i in 1:ncol(cm)) {
    table_html <- paste0(table_html, 
                         "<th style='padding: 16px; text-align: center; color: #ffffff; font-weight: bold; border-right: 2px solid #2a2a2a;'>",
                         res$tickers[i],
                         "</th>"
    )
  }
  
  table_html <- paste0(table_html, "</tr></thead><tbody>")
  
  # Add rows
  for (i in 1:nrow(cm)) {
    table_html <- paste0(table_html, 
                         "<tr style='border-bottom: 1px solid #2a2a2a;'>
        <td style='padding: 16px; color: #4a9eff; font-weight: bold; background: #1a1a1a; border-right: 2px solid #2a2a2a;'>",
                         res$tickers[i],
                         "</td>"
    )
    
    for (j in 1:ncol(cm)) {
      val <- cm[i, j]
      # Color gradient: closer to 1 = more blue, closer to 0 = more gray
      intensity <- val
      bg_color <- sprintf("rgba(74, 158, 255, %.1f)", intensity * 0.3)
      
      table_html <- paste0(table_html,
                           "<td style='padding: 16px; text-align: center; color: #e0e0e0; background: ",
                           bg_color,
                           "; border-right: 1px solid #2a2a2a;'>",
                           val,
                           "</td>"
      )
    }
    
    table_html <- paste0(table_html, "</tr>")
  }
  
  table_html <- paste0(table_html, "</tbody></table></div>")
  
  HTML(table_html)
})

output$cov_table <- renderUI({
  req(input$tabs == "Correlation")
  res <- results()
  cv <- signif(res$cov_mat, 4)
  
  # Create styled HTML table
  table_html <- "
    <div style='overflow-x: auto; margin-top: 40px;'>
      <table style='
        width: 100%;
        border-collapse: collapse;
        font-family: monospace;
        font-size: 16px;
        background: #0f0f0f;
      '>
        <thead>
          <tr style='background: #1a1a1a; border-bottom: 3px solid #4a9eff;'>
            <th style='padding: 16px; text-align: left; color: #ffffff; font-weight: bold; border-right: 2px solid #2a2a2a;'>Ticker</th>
  "
  
  # Add ticker headers
  for (i in 1:ncol(cv)) {
    table_html <- paste0(table_html, 
                         "<th style='padding: 16px; text-align: center; color: #ffffff; font-weight: bold; border-right: 2px solid #2a2a2a;'>",
                         res$tickers[i],
                         "</th>"
    )
  }
  
  table_html <- paste0(table_html, "</tr></thead><tbody>")
  
  # Add rows
  for (i in 1:nrow(cv)) {
    table_html <- paste0(table_html, 
                         "<tr style='border-bottom: 1px solid #2a2a2a;'>
        <td style='padding: 16px; color: #4a9eff; font-weight: bold; background: #1a1a1a; border-right: 2px solid #2a2a2a;'>",
                         res$tickers[i],
                         "</td>"
    )
    
    for (j in 1:ncol(cv)) {
      val <- cv[i, j]
      
      table_html <- paste0(table_html,
                           "<td style='padding: 16px; text-align: center; color: #e0e0e0; background: rgba(74, 158, 255, 0.1); border-right: 1px solid #2a2a2a;'>",
                           format(val, scientific = TRUE, digits = 3),
                           "</td>"
      )
    }
    
    table_html <- paste0(table_html, "</tr>")
  }
  
  table_html <- paste0(table_html, "</tbody></table></div>")
  
  HTML(table_html)
})

# ── Tab 3: Simulation ──────────────────────────────────────────────────

output$fan_chart <- renderPlot({
  req(input$tabs == "Simulation")
  res <- results()
  n_paths <- 100
  n_steps <- min(input$horizon, 20)
  
  port_vol <- sum(res$weights * res$regime_vols)
  
  set.seed(42)
  Z <- matrix(rnorm(n_steps * n_paths), nrow = n_steps, ncol = n_paths)
  paths <- apply(Z, 2, function(z) {
    cumsum(-0.5 * port_vol^2 * (1/252) + port_vol * sqrt(1/252) * z)
  })
  
  df <- data.frame(
    step  = rep(1:n_steps, times = n_paths),
    path  = rep(1:n_paths, each = n_steps),
    value = as.vector(paths)
  )
  
  # Generate random colors for each path
  set.seed(123)
  colors <- rgb(
    runif(n_paths, 0.2, 1),    # R: 0.2 to 1 (avoid too dark)
    runif(n_paths, 0.2, 1),    # G: 0.2 to 1
    runif(n_paths, 0.4, 1),    # B: 0.4 to 1 (bias toward blue tones)
    alpha = 0.6
  )
  
  ggplot(df, aes(x = step, y = value, group = path, color = factor(path))) +
    geom_line(linewidth = 0.5, show.legend = FALSE) +
    scale_color_manual(values = colors) +
    labs(title = "Monte Carlo Fan Chart — Cumulative Log Returns",
         x = "Time Step", y = "Cumulative Log Return") +
    theme_dark_varmc()
}, bg = "#000000")

output$pnl_hist <- renderPlot({
  req(input$tabs == "Simulation")
  res <- results()
  df <- data.frame(pnl = res$pnl)
  
  ggplot(df, aes(x = pnl)) +
    geom_histogram(bins = 80, fill = "#4a9eff", colour = "#2a2a2a", alpha = 0.8) +
    labs(title = "Simulated Portfolio P&L Distribution",
         x = "P&L ($)", y = "Count") +
    theme_dark_varmc()
}, bg = "#000000")

# ── Tab 4: VaR Results ─────────────────────────────────────────────────

output$var_text <- renderUI({
  req(input$tabs == "VaR Results")
  res <- results()
  
  html_content <- paste0("
    <div style='
      background: #0f0f0f;
      border: 2px solid #4a9eff;
      border-radius: 12px;
      padding: 40px;
      font-family: monospace;
      font-size: 15px;
      line-height: 2;
      width: 100%;
      box-sizing: border-box;
    '>
      <div style='
        color: #ffffff;
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 30px;
        text-align: center;
        letter-spacing: 2px;
      '>VaR RESULTS SUMMARY</div>
      
      <div style='border-bottom: 2px solid #4a9eff; padding-bottom: 20px; margin-bottom: 20px;'>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 150px; display: inline-block;'>Portfolio:</span> ", paste(res$tickers, collapse = ", "), "</div>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 150px; display: inline-block;'>Weights:</span> ", paste(round(res$weights, 2), collapse = ", "), "</div>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 150px; display: inline-block;'>Confidence Level:</span> ", input$conf, "</div>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 150px; display: inline-block;'>Time Horizon:</span> ", input$horizon, " days</div>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 150px; display: inline-block;'>Simulations:</span> ", format(input$num_sims, big.mark = ","), "</div>
        <div style='color: #a0a0a0;'><span style='color: #ffffff; font-weight: bold; width: 150px; display: inline-block;'>Capital:</span> $", format(input$capital, big.mark = ","), "</div>
      </div>
      
      <div style='background: rgba(74, 158, 255, 0.1); padding: 20px; border-radius: 8px; border-left: 4px solid #4a9eff;'>
        <div style='color: #ff6b6b; font-size: 18px; font-weight: bold; margin-bottom: 12px;'>VaR Estimate (Loss Threshold)</div>
        <div style='color: #e0e0e0; font-size: 16px; margin-bottom: 16px;'>
          <span style='color: #ff6b6b; font-weight: bold;'>$", format(round(res$var_est, 2), big.mark = ","), "</span>
        </div>
        
        <div style='color: #a0a0a0; font-size: 13px; margin-top: 16px; margin-bottom: 8px;'>95% Bootstrap Confidence Interval:</div>
        <div style='color: #4a9eff; font-weight: bold;'>[  $", format(round(res$ci["lower"], 2), big.mark = ","), "  ,  $", format(round(res$ci["upper"], 2), big.mark = ","), "  ]</div>
      </div>
    </div>
  ")
  
  HTML(html_content)
})

output$var_plot <- renderPlot({
  req(input$tabs == "VaR Results")
  res <- results()
  df <- data.frame(pnl = res$pnl)
  
  ggplot(df, aes(x = pnl)) +
    geom_histogram(bins = 80, fill = "#4a9eff", colour = "#2a2a2a", alpha = 0.7) +
    geom_vline(xintercept = -res$var_est, colour = "#ff6b6b",
               linewidth = 1.2, linetype = "dashed") +
    annotate("text", x = -res$var_est, y = Inf,
             label = paste0("VaR = $", format(round(res$var_est, 0), big.mark = ",")),
             vjust = 2, hjust = -0.1, colour = "#ff6b6b", size = 5, fontface = "bold") +
    labs(title = "P&L Distribution with VaR Threshold",
         x = "P&L ($)", y = "Count") +
    theme_dark_varmc()
}, bg = "#000000")

# ── Tab 5: Backtesting ─────────────────────────────────────────────────

output$bt_text <- renderUI({
  req(input$tabs == "Backtesting")
  bt <- bt_results()
  
  # Determine pass/fail color
  result_color <- ifelse(bt$reject, "#ff6b6b", "#51cf66")
  result_text <- ifelse(bt$reject, "REJECTED", "PASSED")
  
  html_content <- paste0("
    <div style='
      background: #0f0f0f;
      border: 2px solid #4a9eff;
      border-radius: 12px;
      padding: 40px;
      font-family: monospace;
      font-size: 14px;
      line-height: 2;
      width: 100%;
      box-sizing: border-box;
    '>
      <div style='
        color: #ffffff;
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 30px;
        text-align: center;
        letter-spacing: 2px;
      '>KUPIEC POF BACKTEST RESULTS</div>
      
      <div style='border-bottom: 2px solid #4a9eff; padding-bottom: 20px; margin-bottom: 20px;'>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 180px; display: inline-block;'>Total Observations:</span> ", bt$total_obs, "</div>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 180px; display: inline-block;'>Expected Violations:</span> ", round(bt$expected_violations, 2), "</div>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 180px; display: inline-block;'>Observed Violations:</span> ", bt$violations, "</div>
        <div style='color: #a0a0a0; margin-bottom: 8px;'><span style='color: #ffffff; font-weight: bold; width: 180px; display: inline-block;'>Expected Rate:</span> ", round(bt$expected_rate, 4), "</div>
        <div style='color: #a0a0a0;'><span style='color: #ffffff; font-weight: bold; width: 180px; display: inline-block;'>Observed Rate:</span> ", round(bt$observed_rate, 4), "</div>
      </div>
    </div>
  ")
  
  HTML(html_content)
})