#' Stock detection (HMM) for a single stock
#'
#' @description
#' Fits a 3-state Hidden Markov Model (HMM) to daily log returns of a single stock.
#'
#' @param stock A tibble/data frame of price data (typically from tidyquant) that
#' must include an \code{adjusted} column. If a \code{date} column is present it
#' will be carried through via \code{tq_transmute}.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{current_volatility}: volatility (sd of returns) of the most recent regime
#'   \item \code{current_mean}: mean return of the most recent regime
#'   \item \code{returns_data}: tibble with columns \code{date} and \code{return}
#' }
#'
#' @examples
#' library(tidyquant)
#' AAPL <- tq_get("AAPL", from = "2020-01-01", to = "2024-01-01")
#' out <- stock_detection(AAPL)
#' out$current_volatility
#' out$current_mean
#' head(out$returns_data)
#' @importFrom tidyquant tq_transmute periodReturn
#' @importFrom dplyr mutate group_by summarize arrange row_number
#' @importFrom depmixS4 depmix fit posterior
#' @importFrom stats gaussian sd setNames
#' @importFrom utils tail
#' @export
stock_detection <- function(stock){
  stock_data <- stock %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 type = "log",
                 period = "daily",
                 col_rename = "return"
    )
  
  set.seed(123)
  
  hmm_model <- depmix(return~1,
                      data = stock_data,
                      nstates = 3,
                      family = gaussian()
  )
  
  fitted_hmm <- fit(hmm_model)
  regime_results <- posterior(fitted_hmm)
  raw_states <- regime_results$state
  
  regime_volatilities <- stock_data %>%
    mutate(state = raw_states) %>%
    group_by(state) %>%
    summarize(volatility = sd(return), mean_return = mean(return)) %>%
    arrange(volatility) %>%
    mutate(ordered_regime = row_number())
  
  state_mapping <- setNames(regime_volatilities$ordered_regime,
                            regime_volatilities$state)
  
  current_regime <- state_mapping[as.character(tail(raw_states, 1))]
  
  return(list(
    current_volatility = regime_volatilities$volatility[current_regime],
    current_mean = regime_volatilities$mean_return[current_regime],
    returns_data = stock_data
  ))
}

######################

#' Correlation matrix for a portfolio of regime outputs
#'
#' @description
#' Merges return series by date across a list of regime_detection outputs and
#' computes a correlation matrix of returns.
#'
#' @param portfolio A list where each element is the output of \code{regime_detection()}.
#' @return A correlation matrix of returns (one column per asset).
#' @examples
#' out1 <- regime_detection(tq_get("AAPL"))
#' out2 <- regime_detection(tq_get("MSFT"))
#' corr <- correlation_matrix(list(out1, out2))
#' corr
#' @export
correlation_matrix_of_stock_portfolio <- function(portfolio){
  total_returns <- portfolio[[1]]$returns_data[, c("date", "return")]
  
  for (i in 2:length(portfolio)) {
    temp <- portfolio[[i]]$returns_data[, c("date", "return")]
    total_returns <- merge(total_returns, temp, by = "date")
  }
  
  total_returns$date <- NULL
  corr_matrix <- cor(total_returns)
  return(corr_matrix)
}

######################

#' Build a covariance matrix from volatilities and correlations
#'
#' @description
#' Constructs \eqn{\Sigma} using \eqn{\Sigma_{ij} = \rho_{ij}\sigma_i\sigma_j}.
#'
#' @param regime_volatilities Numeric vector of asset volatilities (sd of returns).
#' @param corr_mat Correlation matrix (same asset order as \code{regime_volatilities}).
#' @return Covariance matrix.
#'
#' @examples
#' vol <- c(0.02, 0.015)
#' corr <- matrix(c(1, 0.4, 0.4, 1), nrow = 2)
#' covariance_matrix_of_stock_portfolio(vol, corr)
#'
#' @export
covariance_matrix_of_stock_portfolio <- function(regime_volatilities, corr_mat) {
  n <- length(regime_volatilities)
  cov_mat <- matrix(0, nrow = n, ncol = n)
  
  for(i in 1:n){
    for(j in 1:n){
      rho_ij <- corr_mat[i, j]
      sigma_i <- regime_volatilities[i]
      sigma_j <- regime_volatilities[j]
      cov_mat[i, j] <- rho_ij * sigma_i * sigma_j
    }
  }
  
  return(cov_mat)
}

#######################

#' Manual Cholesky decomposition (lower-triangular)
#'
#' @description
#' Computes the lower-triangular Cholesky factor \eqn{L} such that
#' \eqn{\Sigma = LL^\top}. Assumes input is symmetric positive definite.
#'
#' @param cov_mat Symmetric positive definite covariance matrix.
#' @return Lower-triangular matrix \code{L}.
#'
#' @examples
#' S <- matrix(c(4, 2, 2, 3), nrow = 2)
#' L <- cholesky(S)
#' all.equal(S, L %*% t(L))
#'
#' @export
cholesky <- function(cov_mat) {
  n <- nrow(cov_mat)
  L <- matrix(0, nrow = n, ncol = n)
  
  for(i in 1:n){
    for(j in 1:i){
      sum <- 0
      
      # diagonal terms
      if(i == j){
        if(j > 1){ # prevents looping error when j = 1
          for(k in 1:(j-1)){
            sum = sum + L[j, k] * L[j, k]
          }
        }
        L[j, j] = sqrt(cov_mat[j, j] - sum)
        
        # off diagonal terms
      } else {
        if(j > 1){
          for(k in 1:(j-1)){
            sum = sum + L[i, k] * L[j, k]
          }
        }
        L[i, j] =  (cov_mat[i, j] - sum) /  L[j, j]
      }
    }
  }
  
  return(L)
}

###################

#' Generate correlated shocks from a Cholesky factor
#'
#' @description
#' Draws i.i.d. standard normals and maps them through \eqn{L} to create correlated shocks.
#'
#' @param L Lower-triangular Cholesky factor (from \code{cholesky()}).
#' @param num_sims Number of simulations (columns) to generate.
#' @return Matrix of shocks with dimension (n assets) x (num_sims).
#'
#' @examples
#' L <- diag(2)
#' shocks <- shock_gen(L, 5)
#' dim(shocks)
#'
#' @importFrom stats rnorm
#' @export
shock_gen <- function(L, num_sims){
  n <- nrow(L)
  Z <- matrix(rnorm(n * num_sims), nrow = n, ncol = num_sims)
  shocks <- L %*% Z
  return(shocks)
}

###################

#' Simulate GBM log-returns using correlated shocks
#'
#' @description
#' Computes GBM log-returns over one step: \eqn{(\mu - 0.5\sigma^2)\Delta t + \sigma\sqrt{\Delta t}Z}.
#'
#' @param expected_returns Numeric vector of expected (mean) returns per asset (same order as vol).
#' @param regime_volatilities Numeric vector of volatilities per asset.
#' @param time_horizon Time step \eqn{\Delta t} (e.g., 1/252 for daily).
#' @param shocks Matrix of correlated shocks (n assets x num_sims).
#' @return Matrix of simulated log-returns (n assets x num_sims).
#'
#' @export
sim_gbm <- function(expected_returns, regime_volatilities, time_horizon, shocks){
  drift <- (expected_returns - (1/2) * regime_volatilities^2) * time_horizon
  sim_log <- drift + regime_volatilities * sqrt(time_horizon) * shocks
  return(sim_log)
}

##########################

#' Convert log-returns to simple returns
#'
#' @param sim_returns Matrix (or vector) of log-returns.
#' @return Matrix (or vector) of simple log \code{exp(r) - 1}.
#'
#' @export
log_to_simple_returns <- function(sim_log) {
  exp(sim_log) - 1
}

###################

#' Compute portfolio P&L from simulated returns
#'
#' @description
#' Takes simulated simple returns and converts them into portfolio P&L given weights and capital.
#'
#' @param sim_simple_returns Matrix of simple returns (num_sims x n assets) OR compatible with \code{%*% weights}.
#' @param weights Numeric vector of portfolio weights (length n assets).
#' @param capital Initial capital (default 1e6).
#' @return Numeric vector of P&L (length = num_sims).
#'
#' @export
portfolio_pnl_from_returns <- function(sim_simple_returns, weights, capital = 1e6) {
  w <- as.numeric(weights)
  port_ret <- as.numeric(sim_simple_returns %*% w)
  capital * port_ret
}

###################

#' Compute Value-at-Risk (VaR) from simulated P&L
#'
#' @description
#' Converts P&L to losses and returns the empirical quantile at confidence level \code{conf}.
#'
#' @param pnl Numeric vector of simulated P&L.
#' @param conf Confidence level (e.g., 0.95, 0.99).
#' @return Numeric VaR (positive number representing a loss threshold).
#'
#' @importFrom stats quantile
#' @export
compute_var <- function(pnl, conf) {
  loss <- -pnl
  VaR <- as.numeric(
    quantile(loss, probs = conf, type = 7, na.rm = TRUE)
  )
  return(VaR)
}

#################

#' Bootstrap confidence interval for VaR
#'
#' @description
#' Uses bootstrap resampling of losses to estimate a confidence interval for VaR.
#'
#' @param pnl Numeric vector of simulated P&L.
#' @param conf VaR confidence level (e.g., 0.99).
#' @param B Number of bootstrap resamples.
#' @param ci Confidence level for the interval (e.g., 0.95).
#' @param seed Optional integer seed for reproducibility.
#' @return Named numeric vector \code{c(lower=..., upper=...)} for the CI of VaR.
#'
#' @examples
#' set.seed(1)
#' pnl <- rnorm(1000, mean = 0, sd = 10000)
#' var_bootstrap_ci(pnl, conf = 0.99, B = 500, ci = 0.95, seed = 123)
#'
#' @importFrom stats quantile
#' @export
var_bootstrap_ci <- function(pnl, conf, B, ci, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  loss <- -pnl
  n <- length(loss)
  
  var_star <- numeric(B)
  for (b in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    var_star[b] <- as.numeric(quantile(loss[idx], probs = conf, type = 7))
  }
  
  alpha <- (1 - ci) / 2
  c(
    lower = as.numeric(quantile(var_star, probs = alpha)),
    upper = as.numeric(quantile(var_star, probs = 1 - alpha))
  )
}

#' Kupiec POF Backtest for VaR
#'
#' @description
#' Performs a rolling-window VaR backtest using the Kupiec Proportion of Failures
#' (POF) likelihood ratio test. Compares observed violation rate against the
#' expected rate implied by the confidence level.
#'
#' @param returns_matrix Matrix of actual log-returns (T x n_assets), full history.
#' @param weights Numeric vector of portfolio weights (length n_assets).
#' @param conf VaR confidence level (e.g., 0.95 or 0.99).
#' @param window Training window size (number of days to estimate parameters).
#' @param time_horizon VaR time horizon in days (e.g., 10).
#' @param num_sims Number of Monte Carlo simulations per day.
#' @param capital Initial portfolio capital (default 1e6).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{violations}: number of observed violations
#'   \item \code{total_obs}: number of test days
#'   \item \code{expected_violations}: T_test * (1 - conf)
#'   \item \code{observed_rate}: violations / total_obs
#'   \item \code{expected_rate}: 1 - conf
#'   \item \code{LR}: Kupiec likelihood ratio statistic
#'   \item \code{p_value}: p-value from chi-squared(1)
#'   \item \code{critical_value}: chi-squared critical value at 95%
#'   \item \code{reject}: logical, TRUE if model is rejected
#' }
#'
#' @importFrom stats rnorm pchisq qchisq
#' @export
backtesting <- function(returns_matrix, weights, conf, window,
                        time_horizon, num_sims, capital = 1e6) {
  
  T_total <- nrow(returns_matrix)
  n_assets <- ncol(returns_matrix)
  T_test <- T_total - window - time_horizon + 1
  p <- 1 - conf
  violations <- 0
  
  for (t in 1:T_test) {

    train <- returns_matrix[t:(t + window - 1), ]

    regime_means <- colMeans(train)
    regime_vols <- apply(train, 2, sd)
    corr_mat <- cor(train)

    cov_mat <- covariance_matrix_of_stock_portfolio(regime_vols, corr_mat)
    L <- cholesky(cov_mat)
    shocks <- shock_gen(L, num_sims)
    sim_return <- sim_gbm(regime_means, regime_vols, time_horizon, shocks)
    simple_ret <- log_to_simple_returns(sim_return)
    pnl <- portfolio_pnl_from_returns(t(simple_ret), weights, capital)
    var_est <- compute_var(pnl, conf)

    actual_window <- returns_matrix[(t + window):(t + window + time_horizon - 1), ]
    if (time_horizon == 1) {
      actual_ret <- sum(weights * actual_window)
    } else {
      actual_ret <- sum(weights * colSums(actual_window))
    }
    actual_pnl <- capital * (exp(actual_ret) - 1)

    if (-actual_pnl > var_est) {
      violations <- violations + 1
    }
  }

  x <- violations
  T_obs <- T_test
  p_hat <- x / T_obs
  
  if (x == 0) {
    LR <- -2 * (T_obs * log(1 - p) - T_obs * log(1 - p_hat))
  } else if (x == T_obs) {
    LR <- -2 * (T_obs * log(p) - T_obs * log(p_hat))
  } else {
    LR <- -2 * log(((1 - p)^(T_obs - x) * p^x) /
                     ((1 - p_hat)^(T_obs - x) * p_hat^x))
  }
  
  p_value <- 1 - pchisq(LR, df = 1)
  critical_value <- qchisq(0.95, df = 1)
  
  return(list(
    violations        = x,
    total_obs         = T_obs,
    expected_violations = T_obs * p,
    observed_rate     = p_hat,
    expected_rate     = p,
    LR                = round(LR, 4),
    p_value           = round(p_value, 4),
    critical_value    = round(critical_value, 4),
    reject            = LR > critical_value
  ))
}