# ==========================================================================================================
# 📊 ADVANCED PERFORMANCE TRACKER V1
# ==========================================================================================================
# Pfad: analytics/performance_tracker.r
# Umfassendes Performance-Tracking und Analytics-System
# Portfolio-Metriken, Risk-Adjusted Returns, Strategy Analytics
# ==========================================================================================================

cat("📊 Loading Advanced Performance Tracker V1...\n")

# ==========================================================================================================
# 🔧 DEPENDENCIES
# ==========================================================================================================

# Load required components
if (!exists("SYSTEM_CONFIG_LOADED")) {
  source("../core/config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
}

if (!exists("API_ENGINE_LOADED")) {
  source("../core/api_engine.r")
  API_ENGINE_LOADED <- TRUE
}

if (!exists("POSITION_MANAGER_LOADED")) {
  source("../trading/position_manager.r")
  POSITION_MANAGER_LOADED <- TRUE
}

# ==========================================================================================================
# 📈 PERFORMANCE CONFIGURATION
# ==========================================================================================================

PERFORMANCE_CONFIG <- list(
  # Tracking intervals
  intervals = list(
    real_time = 60,          # Update every minute
    hourly = 3600,           # Hourly snapshots
    daily = 86400,           # Daily summaries
    weekly = 604800          # Weekly reports
  ),
  
  # Benchmark settings
  benchmarks = list(
    btc = "BTCUSDT",         # BTC as crypto benchmark
    market_return = 0.10,    # 10% annual market return
    risk_free_rate = 0.05    # 5% risk-free rate
  ),
  
  # Risk metrics
  risk_metrics = list(
    var_confidence = 0.95,   # 95% VaR
    cvar_confidence = 0.95,  # 95% CVaR
    max_drawdown_alert = 0.20, # Alert at 20% drawdown
    sharpe_target = 1.5      # Target Sharpe ratio
  ),
  
  # Performance targets
  targets = list(
    daily_return = 0.002,    # 0.2% daily
    weekly_return = 0.015,   # 1.5% weekly
    monthly_return = 0.06,   # 6% monthly
    win_rate = 0.60,         # 60% win rate
    profit_factor = 1.5      # 1.5 profit factor
  ),
  
  # Data retention
  retention = list(
    trades = 1000,           # Keep last 1000 trades
    snapshots = 720,         # Keep 720 hourly snapshots (30 days)
    daily_stats = 365        # Keep 365 days of daily stats
  )
)

# ==========================================================================================================
# 📊 PERFORMANCE DATA STRUCTURES
# ==========================================================================================================

# Initialize performance database
PERFORMANCE_DB <- list(
  # Real-time metrics
  current_metrics = list(),
  
  # Historical data
  trade_history = list(),
  position_snapshots = list(),
  daily_summaries = list(),
  
  # Analytics cache
  calculated_metrics = list(),
  last_update = NULL
)

# ==========================================================================================================
# 🔄 REAL-TIME TRACKING
# ==========================================================================================================

#' Update performance metrics in real-time
update_performance_metrics <- function() {
  
  cat("\n🔄 Updating performance metrics...\n")
  
  # Get current positions
  positions <- get_current_positions()
  
  # Get account balance
  balance <- get_account_balance()
  
  if (is.null(balance)) {
    cat("❌ Cannot fetch balance\n")
    return(FALSE)
  }
  
  # Calculate current metrics
  current_metrics <- calculate_current_metrics(positions, balance)
  
  # Store snapshot
  store_performance_snapshot(current_metrics)
  
  # Update cached calculations
  update_cached_calculations(current_metrics)
  
  # Check performance alerts
  check_performance_alerts(current_metrics)
  
  # Update timestamp
  PERFORMANCE_DB$last_update <<- Sys.time()
  
  return(TRUE)
}

#' Calculate current performance metrics
calculate_current_metrics <- function(positions, balance) {
  
  metrics <- list(
    timestamp = Sys.time(),
    
    # Account metrics
    total_equity = balance$equity,
    available_balance = balance$available,
    used_margin = balance$locked,
    margin_ratio = balance$margin_ratio,
    
    # Position metrics
    open_positions = nrow(positions),
    total_exposure = if (nrow(positions) > 0) 
      sum(positions$size * positions$mark_price) else 0,
    
    # P&L metrics
    unrealized_pnl = if (nrow(positions) > 0) 
      sum(positions$unrealized_pnl) else 0,
    realized_pnl_today = get_realized_pnl_today(),
    
    # Risk metrics
    current_leverage = if (balance$equity > 0) 
      metrics$total_exposure / balance$equity else 0,
    portfolio_heat = calculate_portfolio_heat(positions, balance)
  )
  
  # Add calculated fields
  metrics$total_pnl_today <- metrics$realized_pnl_today + metrics$unrealized_pnl
  metrics$return_today <- if (balance$equity > 0) 
    metrics$total_pnl_today / balance$equity else 0
  
  # Store in global
  PERFORMANCE_DB$current_metrics <<- metrics
  
  return(metrics)
}

#' Store performance snapshot
store_performance_snapshot <- function(metrics) {
  
  # Add to snapshots
  PERFORMANCE_DB$position_snapshots[[length(PERFORMANCE_DB$position_snapshots) + 1]] <<- metrics
  
  # Maintain retention limit
  if (length(PERFORMANCE_DB$position_snapshots) > PERFORMANCE_CONFIG$retention$snapshots) {
    PERFORMANCE_DB$position_snapshots <<- tail(
      PERFORMANCE_DB$position_snapshots, 
      PERFORMANCE_CONFIG$retention$snapshots
    )
  }
}

# ==========================================================================================================
# 📈 TRADE ANALYTICS
# ==========================================================================================================

#' Analyze completed trades
analyze_trade_performance <- function(lookback_days = 30) {
  
  cat("\n📈 === TRADE PERFORMANCE ANALYSIS === 📈\n")
  cat("Period: Last", lookback_days, "days\n\n")
  
  # Get trade history
  trades <- get_trade_history(lookback_days)
  
  if (length(trades) == 0 || nrow(trades) == 0) {
    cat("No trades found in period\n")
    return(NULL)
  }
  
  # Basic statistics
  trade_stats <- calculate_trade_statistics(trades)
  
  # Win/Loss analysis
  win_loss_analysis <- analyze_win_loss_distribution(trades)
  
  # Strategy breakdown
  strategy_performance <- analyze_strategy_performance(trades)
  
  # Time analysis
  time_analysis <- analyze_trade_timing(trades)
  
  # Risk-adjusted metrics
  risk_metrics <- calculate_risk_adjusted_metrics(trades)
  
  # Display results
  display_trade_analysis(trade_stats, win_loss_analysis, 
                        strategy_performance, time_analysis, risk_metrics)
  
  return(list(
    stats = trade_stats,
    win_loss = win_loss_analysis,
    strategies = strategy_performance,
    timing = time_analysis,
    risk_metrics = risk_metrics
  ))
}

#' Calculate trade statistics
calculate_trade_statistics <- function(trades) {
  
  stats <- list(
    total_trades = nrow(trades),
    
    # P&L metrics
    total_pnl = sum(trades$pnl),
    avg_pnl_per_trade = mean(trades$pnl),
    median_pnl = median(trades$pnl),
    
    # Win/Loss metrics
    winning_trades = sum(trades$pnl > 0),
    losing_trades = sum(trades$pnl < 0),
    win_rate = sum(trades$pnl > 0) / nrow(trades),
    
    # Size metrics
    avg_trade_size = mean(trades$size * trades$exit_price),
    total_volume = sum(trades$size * trades$exit_price),
    
    # Duration metrics
    avg_hold_time = mean(trades$duration),
    median_hold_time = median(trades$duration),
    
    # Efficiency metrics
    profit_factor = calculate_profit_factor(trades),
    expectancy = calculate_expectancy(trades),
    
    # Risk metrics
    max_drawdown = calculate_max_drawdown(trades),
    recovery_factor = abs(sum(trades$pnl) / calculate_max_drawdown(trades))
  )
  
  # Add percentile analysis
  stats$pnl_percentiles <- quantile(trades$pnl, c(0.05, 0.25, 0.5, 0.75, 0.95))
  
  return(stats)
}

#' Analyze win/loss distribution
analyze_win_loss_distribution <- function(trades) {
  
  wins <- trades[trades$pnl > 0, ]
  losses <- trades[trades$pnl < 0, ]
  
  analysis <- list(
    # Win analysis
    wins = list(
      count = nrow(wins),
      total_profit = sum(wins$pnl),
      avg_win = mean(wins$pnl),
      max_win = max(wins$pnl),
      avg_win_size = mean(wins$pnl_percentage),
      avg_win_duration = mean(wins$duration)
    ),
    
    # Loss analysis
    losses = list(
      count = nrow(losses),
      total_loss = sum(losses$pnl),
      avg_loss = mean(losses$pnl),
      max_loss = min(losses$pnl),
      avg_loss_size = mean(losses$pnl_percentage),
      avg_loss_duration = mean(losses$duration)
    ),
    
    # Ratios
    ratios = list(
      win_loss_ratio = nrow(wins) / max(nrow(losses), 1),
      avg_win_loss_ratio = abs(mean(wins$pnl) / mean(losses$pnl)),
      profit_loss_ratio = abs(sum(wins$pnl) / sum(losses$pnl))
    )
  )
  
  # Distribution analysis
  analysis$distribution <- list(
    consecutive_wins = calculate_consecutive_wins(trades),
    consecutive_losses = calculate_consecutive_losses(trades),
    largest_win_streak = max(calculate_win_streaks(trades)),
    largest_loss_streak = max(calculate_loss_streaks(trades))
  )
  
  return(analysis)
}

#' Analyze strategy performance
analyze_strategy_performance <- function(trades) {
  
  # Group by strategy
  strategies <- unique(trades$strategy)
  
  strategy_analysis <- list()
  
  for (strategy in strategies) {
    strategy_trades <- trades[trades$strategy == strategy, ]
    
    strategy_analysis[[strategy]] <- list(
      trades = nrow(strategy_trades),
      total_pnl = sum(strategy_trades$pnl),
      avg_pnl = mean(strategy_trades$pnl),
      win_rate = sum(strategy_trades$pnl > 0) / nrow(strategy_trades),
      profit_factor = calculate_profit_factor(strategy_trades),
      sharpe_ratio = calculate_sharpe_ratio(strategy_trades$pnl),
      best_trade = max(strategy_trades$pnl),
      worst_trade = min(strategy_trades$pnl)
    )
  }
  
  return(strategy_analysis)
}

# ==========================================================================================================
# 📊 PORTFOLIO ANALYTICS
# ==========================================================================================================

#' Calculate portfolio performance metrics
calculate_portfolio_performance <- function(period_days = 30) {
  
  cat("\n📊 === PORTFOLIO PERFORMANCE === 📊\n")
  cat("Analysis period:", period_days, "days\n\n")
  
  # Get historical snapshots
  snapshots <- get_historical_snapshots(period_days)
  
  if (length(snapshots) < 2) {
    cat("Insufficient data for analysis\n")
    return(NULL)
  }
  
  # Extract equity curve
  equity_curve <- extract_equity_curve(snapshots)
  
  # Calculate returns
  returns <- calculate_returns_series(equity_curve)
  
  # Performance metrics
  performance_metrics <- list(
    # Return metrics
    total_return = calculate_total_return(equity_curve),
    annualized_return = calculate_annualized_return(returns, period_days),
    
    # Risk metrics
    volatility = calculate_volatility(returns),
    downside_deviation = calculate_downside_deviation(returns),
    max_drawdown = calculate_portfolio_max_drawdown(equity_curve),
    
    # Risk-adjusted returns
    sharpe_ratio = calculate_sharpe_ratio(returns),
    sortino_ratio = calculate_sortino_ratio(returns),
    calmar_ratio = calculate_calmar_ratio(returns, equity_curve),
    
    # Other metrics
    var_95 = calculate_var(returns, 0.95),
    cvar_95 = calculate_cvar(returns, 0.95),
    win_days = sum(returns > 0) / length(returns),
    best_day = max(returns),
    worst_day = min(returns)
  )
  
  # Display results
  display_portfolio_performance(performance_metrics, period_days)
  
  return(list(
    metrics = performance_metrics,
    equity_curve = equity_curve,
    returns = returns
  ))
}

#' Extract equity curve from snapshots
extract_equity_curve <- function(snapshots) {
  
  equity_data <- data.frame(
    timestamp = sapply(snapshots, function(x) x$timestamp),
    equity = sapply(snapshots, function(x) x$total_equity),
    stringsAsFactors = FALSE
  )
  
  # Convert timestamps
  equity_data$timestamp <- as.POSIXct(equity_data$timestamp, origin = "1970-01-01")
  
  # Sort by time
  equity_data <- equity_data[order(equity_data$timestamp), ]
  
  return(equity_data)
}

#' Calculate returns series
calculate_returns_series <- function(equity_curve) {
  
  if (nrow(equity_curve) < 2) return(numeric(0))
  
  # Calculate simple returns
  returns <- diff(equity_curve$equity) / head(equity_curve$equity, -1)
  
  # Remove any infinite or NA values
  returns <- returns[is.finite(returns)]
  
  return(returns)
}

# ==========================================================================================================
# 📈 RISK METRICS
# ==========================================================================================================

#' Calculate Value at Risk (VaR)
calculate_var <- function(returns, confidence = 0.95) {
  
  if (length(returns) < 10) return(NA)
  
  # Historical VaR
  var_historical <- quantile(returns, 1 - confidence)
  
  # Parametric VaR (assuming normal distribution)
  mean_return <- mean(returns)
  sd_return <- sd(returns)
  var_parametric <- mean_return + qnorm(1 - confidence) * sd_return
  
  # Return more conservative estimate
  return(min(var_historical, var_parametric))
}

#' Calculate Conditional Value at Risk (CVaR)
calculate_cvar <- function(returns, confidence = 0.95) {
  
  if (length(returns) < 10) return(NA)
  
  var_threshold <- calculate_var(returns, confidence)
  
  # CVaR is the average of returns below VaR
  cvar <- mean(returns[returns <= var_threshold])
  
  return(cvar)
}

#' Calculate Sharpe Ratio
calculate_sharpe_ratio <- function(returns, risk_free_rate = NULL) {
  
  if (length(returns) < 10) return(NA)
  
  if (is.null(risk_free_rate)) {
    risk_free_rate <- PERFORMANCE_CONFIG$benchmarks$risk_free_rate / 252  # Daily rate
  }
  
  excess_returns <- returns - risk_free_rate
  
  if (sd(returns) == 0) return(0)
  
  sharpe <- mean(excess_returns) / sd(returns) * sqrt(252)  # Annualized
  
  return(sharpe)
}

#' Calculate Sortino Ratio
calculate_sortino_ratio <- function(returns, target_return = 0) {
  
  if (length(returns) < 10) return(NA)
  
  excess_returns <- returns - target_return
  
  # Downside deviation
  downside_returns <- pmin(excess_returns, 0)
  downside_dev <- sqrt(mean(downside_returns^2))
  
  if (downside_dev == 0) return(0)
  
  sortino <- mean(excess_returns) / downside_dev * sqrt(252)  # Annualized
  
  return(sortino)
}

#' Calculate maximum drawdown
calculate_portfolio_max_drawdown <- function(equity_curve) {
  
  if (nrow(equity_curve) < 2) return(0)
  
  # Calculate running maximum
  running_max <- cummax(equity_curve$equity)
  
  # Calculate drawdowns
  drawdowns <- (equity_curve$equity - running_max) / running_max
  
  # Return maximum drawdown
  return(abs(min(drawdowns)))
}

#' Calculate portfolio heat (risk utilization)
calculate_portfolio_heat <- function(positions, balance) {
  
  if (nrow(positions) == 0) return(0)
  
  # Calculate total risk
  total_risk <- 0
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    
    # Estimate position risk (simplified)
    position_risk <- pos$size * pos$mark_price * 0.02  # 2% estimated move
    total_risk <- total_risk + position_risk
  }
  
  # Portfolio heat as percentage of equity
  portfolio_heat <- total_risk / balance$equity
  
  return(min(portfolio_heat, 1))  # Cap at 100%
}

# ==========================================================================================================
# 📊 BENCHMARK COMPARISON
# ==========================================================================================================

#' Compare performance against benchmarks
compare_to_benchmarks <- function(period_days = 30) {
  
  cat("\n📊 === BENCHMARK COMPARISON === 📊\n")
  
  # Get portfolio performance
  portfolio_perf <- calculate_portfolio_performance(period_days)
  
  if (is.null(portfolio_perf)) {
    cat("Cannot calculate portfolio performance\n")
    return(NULL)
  }
  
  # Get BTC benchmark performance
  btc_perf <- get_benchmark_performance("BTCUSDT", period_days)
  
  # Compare metrics
  comparison <- list(
    returns = list(
      portfolio = portfolio_perf$metrics$total_return,
      btc = btc_perf$total_return,
      outperformance = portfolio_perf$metrics$total_return - btc_perf$total_return
    ),
    
    risk = list(
      portfolio_vol = portfolio_perf$metrics$volatility,
      btc_vol = btc_perf$volatility,
      relative_vol = portfolio_perf$metrics$volatility / btc_perf$volatility
    ),
    
    risk_adjusted = list(
      portfolio_sharpe = portfolio_perf$metrics$sharpe_ratio,
      btc_sharpe = btc_perf$sharpe_ratio,
      sharpe_diff = portfolio_perf$metrics$sharpe_ratio - btc_perf$sharpe_ratio
    ),
    
    drawdown = list(
      portfolio_dd = portfolio_perf$metrics$max_drawdown,
      btc_dd = btc_perf$max_drawdown,
      dd_ratio = portfolio_perf$metrics$max_drawdown / btc_perf$max_drawdown
    )
  )
  
  # Display comparison
  display_benchmark_comparison(comparison)
  
  # Calculate correlation
  if (length(portfolio_perf$returns) > 10 && length(btc_perf$returns) > 10) {
    min_length <- min(length(portfolio_perf$returns), length(btc_perf$returns))
    comparison$correlation <- cor(
      head(portfolio_perf$returns, min_length),
      head(btc_perf$returns, min_length)
    )
  }
  
  return(comparison)
}

#' Get benchmark performance
get_benchmark_performance <- function(symbol, period_days) {
  
  # Get historical prices
  end_time <- Sys.time()
  start_time <- end_time - (period_days * 86400)
  
  # Fetch candles
  candles <- get_historical_candles(symbol, "1d", start_time, end_time)
  
  if (is.null(candles) || nrow(candles) < 2) {
    return(list(
      total_return = 0,
      volatility = 0,
      sharpe_ratio = 0,
      max_drawdown = 0,
      returns = numeric(0)
    ))
  }
  
  # Calculate returns
  prices <- as.numeric(candles$close)
  returns <- diff(prices) / head(prices, -1)
  
  # Calculate metrics
  metrics <- list(
    total_return = (tail(prices, 1) - head(prices, 1)) / head(prices, 1),
    volatility = sd(returns) * sqrt(252),
    sharpe_ratio = calculate_sharpe_ratio(returns),
    max_drawdown = calculate_benchmark_drawdown(prices),
    returns = returns
  )
  
  return(metrics)
}

# ==========================================================================================================
# 🚨 PERFORMANCE ALERTS
# ==========================================================================================================

#' Check performance alerts
check_performance_alerts <- function(metrics) {
  
  alerts <- list()
  
  # Drawdown alert
  if (exists("PERFORMANCE_DB$daily_summaries") && length(PERFORMANCE_DB$daily_summaries) > 0) {
    current_dd <- calculate_current_drawdown()
    
    if (current_dd > PERFORMANCE_CONFIG$risk_metrics$max_drawdown_alert) {
      alerts$drawdown <- list(
        type = "MAX_DRAWDOWN",
        severity = "HIGH",
        message = paste("Portfolio drawdown:", round(current_dd * 100, 1), "%"),
        value = current_dd
      )
    }
  }
  
  # Daily loss alert
  if (metrics$return_today < -0.05) {
    alerts$daily_loss <- list(
      type = "DAILY_LOSS",
      severity = "MEDIUM",
      message = paste("Daily loss:", round(metrics$return_today * 100, 1), "%"),
      value = metrics$return_today
    )
  }
  
  # Margin alert
  if (metrics$margin_ratio > 0.8) {
    alerts$margin <- list(
      type = "HIGH_MARGIN",
      severity = "HIGH",
      message = paste("Margin usage:", round(metrics$margin_ratio * 100, 0), "%"),
      value = metrics$margin_ratio
    )
  }
  
  # Process alerts
  if (length(alerts) > 0) {
    process_performance_alerts(alerts)
  }
  
  return(alerts)
}

#' Process performance alerts
process_performance_alerts <- function(alerts) {
  
  cat("\n🚨 PERFORMANCE ALERTS:\n")
  
  for (alert_name in names(alerts)) {
    alert <- alerts[[alert_name]]
    
    severity_icon <- if (alert$severity == "HIGH") "🔴" else "🟡"
    
    cat(severity_icon, alert$message, "\n")
  }
  
  # In production, could send notifications
}

# ==========================================================================================================
# 📊 REPORTING
# ==========================================================================================================

#' Generate performance report
generate_performance_report <- function(period = "daily") {
  
  cat("\n📋 === PERFORMANCE REPORT -", toupper(period), "=== 📋\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Get current metrics
  current <- PERFORMANCE_DB$current_metrics
  
  if (is.null(current)) {
    update_performance_metrics()
    current <- PERFORMANCE_DB$current_metrics
  }
  
  # Account summary
  cat("💰 ACCOUNT SUMMARY:\n")
  cat("├─ Total Equity: $", format(current$total_equity, big.mark = ",", digits = 2), "\n")
  cat("├─ Available: $", format(current$available_balance, big.mark = ",", digits = 2), "\n")
  cat("├─ Used Margin: $", format(current$used_margin, big.mark = ",", digits = 2), "\n")
  cat("└─ Open Positions:", current$open_positions, "\n")
  
  # Today's performance
  cat("\n📈 TODAY'S PERFORMANCE:\n")
  cat("├─ Realized P&L: $", format(current$realized_pnl_today, big.mark = ",", digits = 2), "\n")
  cat("├─ Unrealized P&L: $", format(current$unrealized_pnl, big.mark = ",", digits = 2), "\n")
  cat("├─ Total P&L: $", format(current$total_pnl_today, big.mark = ",", digits = 2), "\n")
  cat("└─ Return:", sprintf("%+.2f%%", current$return_today * 100), "\n")
  
  # Period analysis
  period_days <- switch(period,
    "daily" = 1,
    "weekly" = 7,
    "monthly" = 30,
    30
  )
  
  # Trade analysis
  trade_analysis <- analyze_trade_performance(period_days)
  
  # Portfolio metrics
  portfolio_metrics <- calculate_portfolio_performance(period_days)
  
  # Risk metrics
  if (!is.null(portfolio_metrics)) {
    cat("\n📊 RISK METRICS:\n")
    cat("├─ Volatility:", round(portfolio_metrics$metrics$volatility * 100, 1), "% annualized\n")
    cat("├─ Max Drawdown:", round(portfolio_metrics$metrics$max_drawdown * 100, 1), "%\n")
    cat("├─ VaR (95%):", round(portfolio_metrics$metrics$var_95 * 100, 2), "%\n")
    cat("├─ Sharpe Ratio:", round(portfolio_metrics$metrics$sharpe_ratio, 2), "\n")
    cat("└─ Win Days:", round(portfolio_metrics$metrics$win_days * 100, 1), "%\n")
  }
  
  # Benchmark comparison
  if (period_days >= 7) {
    benchmark_comp <- compare_to_benchmarks(period_days)
  }
  
  return(list(
    current = current,
    trades = trade_analysis,
    portfolio = portfolio_metrics,
    benchmark = if (exists("benchmark_comp")) benchmark_comp else NULL
  ))
}

#' Create performance dashboard
create_performance_dashboard <- function() {
  
  cat("\n")
  cat(rep("═", 70), "\n")
  cat("               📊 TRADING PERFORMANCE DASHBOARD 📊\n")
  cat(rep("═", 70), "\n")
  
  # Quick stats
  current <- PERFORMANCE_DB$current_metrics
  
  if (is.null(current)) {
    cat("No performance data available. Run update_performance_metrics() first.\n")
    return()
  }
  
  # Header info
  cat("\n🕐 Last Update:", format(PERFORMANCE_DB$last_update, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Three column layout
  cat("\n┌─────────────────────┬─────────────────────┬─────────────────────┐\n")
  cat(sprintf("│ %-19s │ %-19s │ %-19s │\n", 
              "📊 ACCOUNT", "📈 TODAY", "⚡ POSITIONS"))
  cat("├─────────────────────┼─────────────────────┼─────────────────────┤\n")
  
  # Account column
  cat(sprintf("│ Equity: $%-10.0f │ ", current$total_equity))
  
  # Today column
  pnl_color <- if (current$total_pnl_today >= 0) "+" else ""
  cat(sprintf("P&L: %s$%-11.2f │ ", pnl_color, current$total_pnl_today))
  
  # Positions column
  cat(sprintf("Open: %-14d │\n", current$open_positions))
  
  # Second row
  cat(sprintf("│ Avail: $%-11.0f │ ", current$available_balance))
  cat(sprintf("Return: %+6.2f%%     │ ", current$return_today * 100))
  cat(sprintf("Exposure: $%-9.0f │\n", current$total_exposure))
  
  # Third row
  cat(sprintf("│ Margin: %5.1f%%      │ ", current$margin_ratio * 100))
  cat(sprintf("Unreal: $%-11.2f │ ", current$unrealized_pnl))
  cat(sprintf("Leverage: %.1fx        │\n", current$current_leverage))
  
  cat("└─────────────────────┴─────────────────────┴─────────────────────┘\n")
  
  # Recent trades summary
  recent_trades <- get_recent_trades_summary(5)
  if (!is.null(recent_trades) && nrow(recent_trades) > 0) {
    cat("\n📜 RECENT TRADES:\n")
    cat("┌────────────┬──────┬──────┬────────┬─────────┬──────────┐\n")
    cat("│   Symbol   │ Side │ Size │  Entry │   Exit  │   P&L    │\n")
    cat("├────────────┼──────┼──────┼────────┼─────────┼──────────┤\n")
    
    for (i in 1:min(5, nrow(recent_trades))) {
      trade <- recent_trades[i, ]
      pnl_str <- sprintf("%+.2f", trade$pnl)
      
      cat(sprintf("│ %-10s │ %-4s │ %4.0f │ %6.2f │ %7.2f │ %8s │\n",
                  trade$symbol,
                  trade$side,
                  trade$size,
                  trade$entry_price,
                  trade$exit_price,
                  pnl_str))
    }
    
    cat("└────────────┴──────┴──────┴────────┴─────────┴──────────┘\n")
  }
  
  # Performance indicators
  cat("\n🎯 PERFORMANCE INDICATORS:\n")
  
  # Get weekly stats
  weekly_stats <- get_performance_summary(7)
  
  if (!is.null(weekly_stats)) {
    # Progress bars
    win_rate_bar <- create_progress_bar(weekly_stats$win_rate, 1, 20)
    sharpe_bar <- create_progress_bar(max(0, min(weekly_stats$sharpe_ratio / 3, 1)), 1, 20)
    
    cat("├─ Win Rate:    ", win_rate_bar, 
        sprintf(" %.1f%%\n", weekly_stats$win_rate * 100))
    cat("├─ Sharpe:      ", sharpe_bar, 
        sprintf(" %.2f\n", weekly_stats$sharpe_ratio))
    cat("├─ Profit Factor: ", sprintf("%.2f", weekly_stats$profit_factor), "\n")
    cat("└─ Max Drawdown:  ", sprintf("%.1f%%", weekly_stats$max_drawdown * 100), "\n")
  }
  
  cat("\n")
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

#' Get realized P&L for today
get_realized_pnl_today <- function() {
  
  # Get today's closed trades
  today_start <- as.POSIXct(format(Sys.Date(), "%Y-%m-%d 00:00:00"))
  
  trades <- PERFORMANCE_DB$trade_history
  
  if (length(trades) == 0) return(0)
  
  # Filter today's trades
  today_trades <- Filter(function(x) x$exit_time >= today_start, trades)
  
  if (length(today_trades) == 0) return(0)
  
  # Sum P&L
  total_pnl <- sum(sapply(today_trades, function(x) x$pnl))
  
  return(total_pnl)
}

#' Get trade history
get_trade_history <- function(days = 30) {
  
  if (length(PERFORMANCE_DB$trade_history) == 0) {
    # Try to load from position manager
    if (exists("POSITION_TRACKER")) {
      history <- get_position_history_summary(days * 24)  # Approximate
      if (!is.null(history)) {
        return(history$history)
      }
    }
    return(data.frame())
  }
  
  # Convert to data frame
  trades_df <- do.call(rbind, lapply(PERFORMANCE_DB$trade_history, as.data.frame))
  
  # Filter by date
  cutoff_date <- Sys.Date() - days
  recent_trades <- trades_df[as.Date(trades_df$exit_time) >= cutoff_date, ]
  
  return(recent_trades)
}

#' Calculate profit factor
calculate_profit_factor <- function(trades) {
  
  if (nrow(trades) == 0) return(0)
  
  gross_profit <- sum(trades$pnl[trades$pnl > 0])
  gross_loss <- abs(sum(trades$pnl[trades$pnl < 0]))
  
  if (gross_loss == 0) return(ifelse(gross_profit > 0, Inf, 0))
  
  return(gross_profit / gross_loss)
}

#' Calculate expectancy
calculate_expectancy <- function(trades) {
  
  if (nrow(trades) == 0) return(0)
  
  wins <- trades[trades$pnl > 0, ]
  losses <- trades[trades$pnl < 0, ]
  
  win_rate <- nrow(wins) / nrow(trades)
  avg_win <- if (nrow(wins) > 0) mean(wins$pnl) else 0
  avg_loss <- if (nrow(losses) > 0) abs(mean(losses$pnl)) else 0
  
  expectancy <- (win_rate * avg_win) - ((1 - win_rate) * avg_loss)
  
  return(expectancy)
}

#' Calculate consecutive wins
calculate_consecutive_wins <- function(trades) {
  
  if (nrow(trades) == 0) return(0)
  
  win_sequence <- trades$pnl > 0
  
  consecutive <- 0
  current_streak <- 0
  
  for (win in win_sequence) {
    if (win) {
      current_streak <- current_streak + 1
      consecutive <- max(consecutive, current_streak)
    } else {
      current_streak <- 0
    }
  }
  
  return(consecutive)
}

#' Create progress bar
create_progress_bar <- function(value, max_value, width = 20) {
  
  filled <- round(value / max_value * width)
  empty <- width - filled
  
  bar <- paste0(
    "[",
    paste(rep("█", filled), collapse = ""),
    paste(rep("░", empty), collapse = ""),
    "]"
  )
  
  return(bar)
}

#' Get recent trades summary
get_recent_trades_summary <- function(n = 5) {
  
  trades <- get_trade_history(7)  # Last 7 days
  
  if (nrow(trades) == 0) return(NULL)
  
  # Sort by exit time
  trades <- trades[order(trades$exit_time, decreasing = TRUE), ]
  
  # Return top n
  return(head(trades, n))
}

#' Get performance summary
get_performance_summary <- function(days = 7) {
  
  trades <- get_trade_history(days)
  
  if (nrow(trades) == 0) return(NULL)
  
  portfolio_perf <- calculate_portfolio_performance(days)
  
  summary <- list(
    win_rate = sum(trades$pnl > 0) / nrow(trades),
    profit_factor = calculate_profit_factor(trades),
    sharpe_ratio = if (!is.null(portfolio_perf)) portfolio_perf$metrics$sharpe_ratio else 0,
    max_drawdown = if (!is.null(portfolio_perf)) portfolio_perf$metrics$max_drawdown else 0
  )
  
  return(summary)
}

#' Display trade analysis
display_trade_analysis <- function(stats, win_loss, strategies, timing, risk_metrics) {
  
  cat("\n📊 TRADE STATISTICS:\n")
  cat("├─ Total Trades:", stats$total_trades, "\n")
  cat("├─ Win Rate:", round(stats$win_rate * 100, 1), "%\n")
  cat("├─ Profit Factor:", round(stats$profit_factor, 2), "\n")
  cat("├─ Expectancy: $", round(stats$expectancy, 2), "\n")
  cat("└─ Max Drawdown:", round(stats$max_drawdown * 100, 1), "%\n")
  
  cat("\n💰 P&L ANALYSIS:\n")
  cat("├─ Total P&L: $", format(stats$total_pnl, big.mark = ",", digits = 2), "\n")
  cat("├─ Avg per Trade: $", round(stats$avg_pnl_per_trade, 2), "\n")
  cat("├─ Avg Win: $", round(win_loss$wins$avg_win, 2), "\n")
  cat("├─ Avg Loss: $", round(win_loss$losses$avg_loss, 2), "\n")
  cat("└─ Win/Loss Ratio:", round(win_loss$ratios$avg_win_loss_ratio, 2), "\n")
}

#' Display portfolio performance
display_portfolio_performance <- function(metrics, period_days) {
  
  cat("\n📈 RETURNS:\n")
  cat("├─ Total Return:", sprintf("%+.2f%%", metrics$total_return * 100), "\n")
  cat("├─ Annualized:", sprintf("%+.2f%%", metrics$annualized_return * 100), "\n")
  cat("└─ Best/Worst Day:", sprintf("%+.2f%%", metrics$best_day * 100), 
      "/", sprintf("%+.2f%%", metrics$worst_day * 100), "\n")
  
  cat("\n📊 RISK-ADJUSTED:\n")
  cat("├─ Sharpe Ratio:", round(metrics$sharpe_ratio, 2), "\n")
  cat("├─ Sortino Ratio:", round(metrics$sortino_ratio, 2), "\n")
  cat("└─ Calmar Ratio:", round(metrics$calmar_ratio, 2), "\n")
}

#' Display benchmark comparison
display_benchmark_comparison <- function(comparison) {
  
  cat("\n🎯 vs BTC BENCHMARK:\n")
  cat("├─ Portfolio Return:", sprintf("%+.2f%%", comparison$returns$portfolio * 100), "\n")
  cat("├─ BTC Return:", sprintf("%+.2f%%", comparison$returns$btc * 100), "\n")
  cat("├─ Outperformance:", sprintf("%+.2f%%", comparison$returns$outperformance * 100), "\n")
  cat("└─ Relative Volatility:", round(comparison$risk$relative_vol, 2), "x\n")
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Performance tracking interface
PERFORMANCE_TRACKER <- list(
  update = update_performance_metrics,
  analyze_trades = analyze_trade_performance,
  portfolio_metrics = calculate_portfolio_performance,
  benchmark = compare_to_benchmarks,
  report = generate_performance_report,
  dashboard = create_performance_dashboard,
  get_current = function() PERFORMANCE_DB$current_metrics
)

cat("✅ PERFORMANCE_TRACKER.R loaded successfully!\n")
cat("📊 Performance analytics engine ready\n")
cat("📈 Trade and portfolio tracking enabled\n")
cat("🎯 Risk metrics calculation available\n")
cat("📋 Reporting system active\n")