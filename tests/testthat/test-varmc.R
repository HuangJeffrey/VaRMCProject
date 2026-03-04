# ============================================================
# Tests for :covariance_matrix_of_stock_portfolio
# ============================================================

test_that("covariance_matrix_of_stock_portfolio returns correct dimensions", {
  vol <- c(0.02, 0.015, 0.03)
  corr <- diag(3)
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  expect_equal(nrow(cov_mat), 3)
  expect_equal(ncol(cov_mat), 3)
})

test_that("covariance_matrix_of_stock_portfolio diagonal equals sigma^2 when correlation is identity", {
  vol <- c(0.02, 0.015, 0.03)
  corr <- diag(3)
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  expect_equal(diag(cov_mat), vol^2, tolerance = 1e-12)
})

test_that("covariance_matrix_of_stock_portfolio off-diagonals equal rho * sigma_i * sigma_j", {
  vol <- c(0.02, 0.015)
  corr <- matrix(c(1, 0.4, 0.4, 1), nrow = 2)
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  expect_equal(cov_mat[1, 2], 0.4 * 0.02 * 0.015, tolerance = 1e-12)
  expect_equal(cov_mat[2, 1], 0.4 * 0.02 * 0.015, tolerance = 1e-12)
})

test_that("covariance_matrix_of_stock_portfolio is symmetric", {
  vol <- c(0.05, 0.03, 0.04)
  corr <- matrix(c(1, 0.3, 0.5,
                   0.3, 1, 0.2,
                   0.5, 0.2, 1), nrow = 3)
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  expect_equal(cov_mat, t(cov_mat), tolerance = 1e-12)
})

test_that("covariance_matrix_of_stock_portfolio with perfect correlation gives outer product", {
  vol <- c(0.02, 0.03)
  corr <- matrix(1, nrow = 2, ncol = 2)
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  expected <- vol %o% vol
  expect_equal(cov_mat, expected, tolerance = 1e-12)
})

test_that("covariance_matrix works for single asset", {
  vol <- 0.05
  corr <- matrix(1, nrow = 1, ncol = 1)
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  expect_equal(cov_mat[1, 1], 0.05^2, tolerance = 1e-12)
})

# ============================================================
# Tests for: cholesky()
# ============================================================

test_that("cholesky returns lower-triangular matrix", {
  S <- matrix(c(4, 2, 2, 3), nrow = 2)
  L <- cholesky(S)
  expect_true(all(L[upper.tri(L)] == 0))
})

test_that("cholesky satisfies L %*% t(L) = original matrix", {
  S <- matrix(c(4, 2, 2, 3), nrow = 2)
  L <- cholesky(S)
  expect_equal(L %*% t(L), S, tolerance = 1e-10)
})

test_that("cholesky matches base R chol() for 3x3 matrix", {
  S <- matrix(c(9, 3, 6,
                3, 5, 4,
                6, 4, 10), nrow = 3)
  L_ours <- cholesky(S)
  L_base <- t(chol(S))
  expect_equal(L_ours, L_base, tolerance = 1e-10)
})

test_that("cholesky works for identity matrix", {
  I <- diag(4)
  L <- cholesky(I)
  expect_equal(L, diag(4), tolerance = 1e-12)
})

test_that("cholesky works for 1x1 matrix", {
  S <- matrix(9, nrow = 1, ncol = 1)
  L <- cholesky(S)
  expect_equal(L[1, 1], 3)
})

test_that("cholesky diagonal elements are positive", {
  S <- matrix(c(4, 1, 1, 2), nrow = 2)
  L <- cholesky(S)
  expect_true(all(diag(L) > 0))
})

test_that("cholesky works end-to-end with covariance_matrix_of_stock_portfolio", {
  vol <- c(0.02, 0.015, 0.03)
  corr <- matrix(c(1, 0.3, 0.5,
                   0.3, 1, 0.2,
                   0.5, 0.2, 1), nrow = 3)
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  L <- cholesky(cov_mat)
  expect_equal(L %*% t(L), cov_mat, tolerance = 1e-10)
})

# ============================================================
# Tests for: shock_gen()
# ============================================================

test_that("shock_gen returns correct dimensions", {
  L <- diag(3)
  shocks <- shock_gen(L, 1000)
  expect_equal(nrow(shocks), 3)
  expect_equal(ncol(shocks), 1000)
})

test_that("shock_gen with identity L produces uncorrelated standard normals", {
  set.seed(42)
  L <- diag(3)
  shocks <- shock_gen(L, 100000)
  for (i in 1:3) {
    expect_equal(mean(shocks[i, ]), 0, tolerance = 0.02)
    expect_equal(sd(shocks[i, ]), 1, tolerance = 0.02)
  }
})

test_that("shock_gen produces correlated shocks matching target correlation", {
  set.seed(42)
  vol <- c(0.02, 0.03)
  corr <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  L <- cholesky(cov_mat)
  shocks <- shock_gen(L, 100000)
  observed_corr <- cor(t(shocks))
  expect_equal(observed_corr[1, 2], 0.8, tolerance = 0.02)
})

test_that("shock_gen with single asset returns 1-row matrix", {
  L <- matrix(0.05, nrow = 1, ncol = 1)
  shocks <- shock_gen(L, 500)
  expect_equal(nrow(shocks), 1)
  expect_equal(ncol(shocks), 500)
})

# ============================================================
# Tests for: sim_gbm()
# ============================================================

test_that("sim_gbm returns correct dimensions", {
  mu <- c(0.01, 0.02)
  vol <- c(0.1, 0.15)
  shocks <- matrix(rnorm(200), nrow = 2, ncol = 100)
  result <- sim_gbm(mu, vol, 10, shocks)
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 100)
})

test_that("sim_gbm with zero volatility returns pure drift", {
  mu <- c(0.05, 0.10)
  vol <- c(0, 0)
  shocks <- matrix(rnorm(200), nrow = 2, ncol = 100)
  result <- sim_gbm(mu, vol, 1, shocks)
  expect_true(all(result[1, ] == 0.05))
  expect_true(all(result[2, ] == 0.10))
})

test_that("sim_gbm with zero shocks returns pure drift", {
  mu <- c(0.05)
  vol <- c(0.2)
  shocks <- matrix(0, nrow = 1, ncol = 50)
  result <- sim_gbm(mu, vol, 1, shocks)
  expected_drift <- (0.05 - 0.5 * 0.2^2) * 1
  expect_true(all(abs(result[1, ] - expected_drift) < 1e-12))
})

test_that("sim_gbm drift formula is correct: (mu - 0.5*sigma^2)*dt", {
  mu <- 0.10
  vol <- 0.20
  dt <- 10
  shocks <- matrix(0, nrow = 1, ncol = 1)
  result <- sim_gbm(mu, vol, dt, shocks)
  expected <- (0.10 - 0.5 * 0.20^2) * 10
  expect_equal(as.numeric(result), expected, tolerance = 1e-12)
})

test_that("sim_gbm diffusion scales with sqrt(dt)", {
  mu <- 0
  vol <- 0.2
  shocks <- matrix(1, nrow = 1, ncol = 1)
  r1 <- sim_gbm(mu, vol, 1, shocks)
  r4 <- sim_gbm(mu, vol, 4, shocks)
  drift1 <- (mu - 0.5 * vol^2) * 1
  drift4 <- (mu - 0.5 * vol^2) * 4
  diff1 <- as.numeric(r1) - drift1
  diff4 <- as.numeric(r4) - drift4
  expect_equal(diff4 / diff1, 2, tolerance = 1e-10)
})

# ============================================================
# Tests for: log_to_simple_returns()
# ============================================================

test_that("log_to_simple_returns computes exp(r) - 1 for vectors", {
  v <- c(0, log(1.1), log(0.9))
  expect_equal(log_to_simple_returns(v), exp(v) - 1, tolerance = 1e-12)
})

test_that("log_to_simple_returns works on matrices", {
  m <- matrix(c(0.01, -0.02, 0.03, -0.01), nrow = 2, ncol = 2)
  expect_equal(log_to_simple_returns(m), exp(m) - 1, tolerance = 1e-12)
})

test_that("log_to_simple_returns of zero is zero", {
  expect_equal(log_to_simple_returns(0), 0)
})

test_that("log_to_simple_returns of log(2) is 1", {
  expect_equal(log_to_simple_returns(log(2)), 1, tolerance = 1e-12)
})

test_that("log_to_simple_returns preserves matrix dimensions", {
  m <- matrix(rnorm(12), nrow = 3, ncol = 4)
  result <- log_to_simple_returns(m)
  expect_equal(dim(result), c(3, 4))
})

# ============================================================
# Tests for: portfolio_pnl_from_returns()
# ============================================================

test_that("portfolio_pnl_from_returns computes capital * (R %*% w)", {
  R <- matrix(c(0.01, -0.02, 0.03,
                0.02,  0.00, -0.01,
                -0.01,  0.01,  0.00),
              nrow = 3, byrow = TRUE)
  w <- c(0.5, 0.3, 0.2)
  cap <- 1e6
  pnl <- portfolio_pnl_from_returns(R, w, capital = cap)
  expected <- cap * as.numeric(R %*% w)
  expect_equal(pnl, expected, tolerance = 1e-12)
})

test_that("portfolio_pnl_from_returns with equal weights averages returns", {
  R <- matrix(c(0.10, 0.10,
                -0.10, -0.10), nrow = 2, byrow = TRUE)
  w <- c(0.5, 0.5)
  pnl <- portfolio_pnl_from_returns(R, w, capital = 100)
  expect_equal(pnl[1], 100 * 0.10)
  expect_equal(pnl[2], 100 * -0.10)
})

test_that("portfolio_pnl_from_returns default capital is 1e6", {
  R <- matrix(0.01, nrow = 1, ncol = 1)
  w <- 1
  pnl <- portfolio_pnl_from_returns(R, w)
  expect_equal(pnl, 1e6 * 0.01)
})

test_that("portfolio_pnl_from_returns returns correct length", {
  R <- matrix(rnorm(20), nrow = 10, ncol = 2)
  w <- c(0.6, 0.4)
  pnl <- portfolio_pnl_from_returns(R, w)
  expect_length(pnl, 10)
})

# ============================================================
# Tests for: compute_var()
# ============================================================

test_that("compute_var returns positive number for centered P&L", {
  set.seed(1)
  pnl <- rnorm(10000, mean = 0, sd = 10000)
  var_95 <- compute_var(pnl, 0.95)
  expect_true(var_95 > 0)
})

test_that("compute_var at higher confidence gives higher VaR", {
  set.seed(1)
  pnl <- rnorm(10000, mean = 0, sd = 10000)
  var_95 <- compute_var(pnl, 0.95)
  var_99 <- compute_var(pnl, 0.99)
  expect_true(var_99 > var_95)
})

test_that("compute_var matches manual quantile calculation", {
  pnl <- c(100, -50, 0, -200, 10)
  conf <- 0.95
  loss <- -pnl
  expected <- as.numeric(stats::quantile(loss, probs = conf, type = 7, na.rm = TRUE))
  expect_equal(compute_var(pnl, conf), expected, tolerance = 1e-12)
})

test_that("compute_var handles NA values", {
  pnl <- c(100, -50, 0, NA, -200, 10)
  var_result <- compute_var(pnl, 0.95)
  expect_true(is.numeric(var_result))
  expect_false(is.na(var_result))
})

test_that("compute_var for all-positive P&L can be negative (no loss)", {
  pnl <- rep(100, 1000)
  var_95 <- compute_var(pnl, 0.95)
  expect_equal(var_95, -100)
})

# ============================================================
# Tests for: var_bootstrap_ci()
# ============================================================

test_that("var_bootstrap_ci returns named lower and upper", {
  set.seed(1)
  pnl <- rnorm(500, 0, 10000)
  ci <- var_bootstrap_ci(pnl, conf = 0.95, B = 200, ci = 0.95, seed = 42)
  expect_named(ci, c("lower", "upper"))
})

test_that("var_bootstrap_ci lower < upper", {
  set.seed(1)
  pnl <- rnorm(1000, 0, 10000)
  ci <- var_bootstrap_ci(pnl, conf = 0.95, B = 500, ci = 0.95, seed = 42)
  expect_true(ci["lower"] < ci["upper"])
})

test_that("var_bootstrap_ci is reproducible with same seed", {
  pnl <- rnorm(500, 0, 10000)
  ci1 <- var_bootstrap_ci(pnl, conf = 0.95, B = 200, ci = 0.95, seed = 123)
  ci2 <- var_bootstrap_ci(pnl, conf = 0.95, B = 200, ci = 0.95, seed = 123)
  expect_equal(ci1, ci2)
})

test_that("var_bootstrap_ci wider CI with higher ci level", {
  set.seed(1)
  pnl <- rnorm(2000, 0, 10000)
  ci_90 <- var_bootstrap_ci(pnl, conf = 0.95, B = 500, ci = 0.90, seed = 42)
  ci_99 <- var_bootstrap_ci(pnl, conf = 0.95, B = 500, ci = 0.99, seed = 42)
  width_90 <- ci_90["upper"] - ci_90["lower"]
  width_99 <- ci_99["upper"] - ci_99["lower"]
  expect_true(width_99 > width_90)
})

test_that("var_bootstrap_ci VaR point estimate falls within CI", {
  set.seed(1)
  pnl <- rnorm(2000, 0, 10000)
  var_est <- compute_var(pnl, 0.95)
  ci <- var_bootstrap_ci(pnl, conf = 0.95, B = 1000, ci = 0.99, seed = 42)
  expect_true(var_est >= ci["lower"] && var_est <= ci["upper"])
})

# ============================================================
# Tests for: correlation_matrix_of_stock_portfolio()
# ============================================================

test_that("correlation_matrix_of_stock_portfolio returns correct dimensions", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  p1 <- list(returns_data = data.frame(date = dates, return = rnorm(100, 0, 0.01)))
  p2 <- list(returns_data = data.frame(date = dates, return = rnorm(100, 0, 0.02)))
  p3 <- list(returns_data = data.frame(date = dates, return = rnorm(100, 0, 0.015)))
  portfolio <- list(p1, p2, p3)
  corr <- correlation_matrix_of_stock_portfolio(portfolio)
  expect_equal(nrow(corr), 3)
  expect_equal(ncol(corr), 3)
})

test_that("correlation_matrix_of_stock_portfolio diagonal is all ones", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 100)
  p1 <- list(returns_data = data.frame(date = dates, return = rnorm(100)))
  p2 <- list(returns_data = data.frame(date = dates, return = rnorm(100)))
  portfolio <- list(p1, p2)
  corr <- correlation_matrix_of_stock_portfolio(portfolio)
  expect_equal(unname(diag(corr)), c(1, 1), tolerance = 1e-12)
})

test_that("correlation_matrix_of_stock_portfolio is symmetric", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 200)
  p1 <- list(returns_data = data.frame(date = dates, return = rnorm(200)))
  p2 <- list(returns_data = data.frame(date = dates, return = rnorm(200)))
  portfolio <- list(p1, p2)
  corr <- correlation_matrix_of_stock_portfolio(portfolio)
  expect_equal(corr, t(corr), tolerance = 1e-12)
})

test_that("correlation_matrix_of_stock_portfolio of identical series returns all ones", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 50)
  r <- rnorm(50)
  p1 <- list(returns_data = data.frame(date = dates, return = r))
  p2 <- list(returns_data = data.frame(date = dates, return = r))
  portfolio <- list(p1, p2)
  corr <- correlation_matrix_of_stock_portfolio(portfolio)
  expect_equal(unname(corr[1, 2]), 1, tolerance = 1e-12)
})

# ============================================================
# Tests for: backtesting()
# ============================================================

test_that("backtesting returns all expected fields", {
  set.seed(42)
  returns_matrix <- matrix(rnorm(120, 0, 0.01), nrow = 60, ncol = 2)
  result <- backtesting(returns_matrix, weights = c(0.5, 0.5),
                        conf = 0.95, window = 40,
                        time_horizon = 1, num_sims = 500)
  expect_true("violations" %in% names(result))
  expect_true("total_obs" %in% names(result))
  expect_true("expected_violations" %in% names(result))
  expect_true("observed_rate" %in% names(result))
  expect_true("expected_rate" %in% names(result))
  expect_true("LR" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("critical_value" %in% names(result))
  expect_true("reject" %in% names(result))
})

test_that("backtesting expected_rate equals 1 - conf", {
  set.seed(42)
  returns_matrix <- matrix(rnorm(120, 0, 0.01), nrow = 60, ncol = 2)
  result <- backtesting(returns_matrix, weights = c(0.5, 0.5),
                        conf = 0.95, window = 40,
                        time_horizon = 1, num_sims = 500)
  expect_equal(result$expected_rate, 0.05)
})

test_that("backtesting total_obs equals T - window - horizon + 1", {
  set.seed(42)
  T_total <- 80
  window <- 50
  horizon <- 5
  returns_matrix <- matrix(rnorm(T_total * 2, 0, 0.01), nrow = T_total, ncol = 2)
  result <- backtesting(returns_matrix, weights = c(0.5, 0.5),
                        conf = 0.95, window = window,
                        time_horizon = horizon, num_sims = 500)
  expect_equal(result$total_obs, T_total - window - horizon + 1)
})

test_that("backtesting violations is non-negative integer", {
  set.seed(42)
  returns_matrix <- matrix(rnorm(120, 0, 0.01), nrow = 60, ncol = 2)
  result <- backtesting(returns_matrix, weights = c(0.5, 0.5),
                        conf = 0.95, window = 40,
                        time_horizon = 1, num_sims = 500)
  expect_true(result$violations >= 0)
  expect_equal(result$violations, as.integer(result$violations))
})

test_that("backtesting critical value is chi-squared 95% quantile", {
  set.seed(42)
  returns_matrix <- matrix(rnorm(120, 0, 0.01), nrow = 60, ncol = 2)
  result <- backtesting(returns_matrix, weights = c(0.5, 0.5),
                        conf = 0.95, window = 40,
                        time_horizon = 1, num_sims = 500)
  expect_equal(result$critical_value, round(qchisq(0.95, df = 1), 4))
})

# ============================================================
# Tests for: stock_detection()
# ============================================================

test_that("stock_detection returns correct structure", {
  skip_on_cran()
  skip_if_not_installed("tidyquant")
  skip_if_not_installed("depmixS4")
  
  stock <- suppressMessages(tidyquant::tq_get("AAPL", from = "2020-01-01", to = "2023-01-01"))
  out <- suppressMessages(suppressWarnings(stock_detection(stock)))
  
  expect_type(out, "list")
  expect_named(out, c("current_volatility", "current_mean", "returns_data"), ignore.order = TRUE)
})

test_that("stock_detection volatility is positive", {
  skip_on_cran()
  skip_if_not_installed("tidyquant")
  skip_if_not_installed("depmixS4")
  
  stock <- suppressMessages(tidyquant::tq_get("AAPL", from = "2020-01-01", to = "2023-01-01"))
  out <- suppressMessages(suppressWarnings(stock_detection(stock)))
  
  expect_true(out$current_volatility > 0)
})

test_that("stock_detection mean is finite", {
  skip_on_cran()
  skip_if_not_installed("tidyquant")
  skip_if_not_installed("depmixS4")
  
  stock <- suppressMessages(tidyquant::tq_get("AAPL", from = "2020-01-01", to = "2023-01-01"))
  out <- suppressMessages(suppressWarnings(stock_detection(stock)))
  
  expect_true(is.finite(out$current_mean))
})

test_that("stock_detection returns_data has date and return columns", {
  skip_on_cran()
  skip_if_not_installed("tidyquant")
  skip_if_not_installed("depmixS4")
  
  stock <- suppressMessages(tidyquant::tq_get("AAPL", from = "2020-01-01", to = "2023-01-01"))
  out <- suppressMessages(suppressWarnings(stock_detection(stock)))
  
  expect_true("date" %in% colnames(out$returns_data))
  expect_true("return" %in% colnames(out$returns_data))
})

test_that("stock_detection returns_data has correct row count", {
  skip_on_cran()
  skip_if_not_installed("tidyquant")
  skip_if_not_installed("depmixS4")
  
  stock <- suppressMessages(tidyquant::tq_get("AAPL", from = "2020-01-01", to = "2023-01-01"))
  out <- suppressMessages(suppressWarnings(stock_detection(stock)))
  
  expect_true(nrow(out$returns_data) > 500)
  expect_true(nrow(out$returns_data) < 900)
})

test_that("stock_detection is reproducible (set.seed inside)", {
  skip_on_cran()
  skip_if_not_installed("tidyquant")
  skip_if_not_installed("depmixS4")
  
  stock <- suppressMessages(tidyquant::tq_get("AAPL", from = "2020-01-01", to = "2023-01-01"))
  out1 <- suppressMessages(suppressWarnings(stock_detection(stock)))
  out2 <- suppressMessages(suppressWarnings(stock_detection(stock)))
  
  expect_equal(out1$current_volatility, out2$current_volatility)
  expect_equal(out1$current_mean, out2$current_mean)
})

test_that("stock_detection works with different tickers", {
  skip_on_cran()
  skip_if_not_installed("tidyquant")
  skip_if_not_installed("depmixS4")
  
  stock <- suppressMessages(tidyquant::tq_get("MSFT", from = "2020-01-01", to = "2023-01-01"))
  out <- suppressMessages(suppressWarnings(stock_detection(stock)))
  
  expect_type(out, "list")
  expect_true(out$current_volatility > 0)
})

# ============================================================
# Tests for: full pipeline integration
# ============================================================

test_that("full pipeline from covariance to VaR runs without error", {
  vol <- c(0.02, 0.015, 0.03)
  mu <- c(0.001, 0.0005, 0.002)
  corr <- matrix(c(1,   0.3, 0.5,
                   0.3, 1,   0.2,
                   0.5, 0.2, 1), nrow = 3)
  
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  L <- cholesky(cov_mat)
  shocks <- shock_gen(L, 10000)
  sim_ret <- sim_gbm(mu, vol, 10, shocks)
  simple_ret <- log_to_simple_returns(sim_ret)
  pnl <- portfolio_pnl_from_returns(t(simple_ret), c(1/3, 1/3, 1/3))
  var_95 <- compute_var(pnl, 0.95)
  
  expect_true(is.numeric(var_95))
  expect_length(var_95, 1)
})

test_that("pipeline preserves dimensions throughout", {
  n_assets <- 4
  n_sims <- 5000
  vol <- rep(0.02, n_assets)
  mu <- rep(0.001, n_assets)
  corr <- diag(n_assets)
  
  cov_mat <- covariance_matrix_of_stock_portfolio(vol, corr)
  expect_equal(dim(cov_mat), c(n_assets, n_assets))
  
  L <- cholesky(cov_mat)
  expect_equal(dim(L), c(n_assets, n_assets))
  
  shocks <- shock_gen(L, n_sims)
  expect_equal(dim(shocks), c(n_assets, n_sims))
  
  sim_ret <- sim_gbm(mu, vol, 10, shocks)
  expect_equal(dim(sim_ret), c(n_assets, n_sims))
  
  simple_ret <- log_to_simple_returns(sim_ret)
  expect_equal(dim(simple_ret), c(n_assets, n_sims))
  
  pnl <- portfolio_pnl_from_returns(t(simple_ret), rep(1/n_assets, n_assets))
  expect_length(pnl, n_sims)
})
