# ==========================================================================================================
# 🔬 ADVANCED BACKTESTING ENGINE V1
# ==========================================================================================================
# Pfad: analytics/backtest_engine.r
# Umfassendes Backtesting-System für Strategy-Entwicklung
# Walk-Forward Analysis, Monte Carlo, Parameter-Optimierung
# ==========================================================================================================

cat("🔬 Loading Advanced Backtesting Engine V1...\n")

# ==========================================================================================================
# 🔧 DEPENDENCIES & CONFIGURATION
# ==========================================================================================================

# Load required packages
required_packages <- c("parallel", "doParallel", "foreach")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load system components
if (!exists("SYSTEM_CONFIG_LOADED")) {
  source("../core/config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
}

if (!exists("DATA_MANAGER")) {
  source("../utils/data_manager.r")
}

# ==========================================================================================================
# ⚙️ BACKTEST CONFIGURATION
# ==========================================================================================================

BACKTEST_CONFIG <- list(
  # Data settings
  data = list(
    min_history_days = 30,
    default_timeframe = "1h",
    tick_data_available = FALSE,
    slippage_ticks = 1,
    commission_rate = 0.0004  # 0.04% per side
  ),
  
  # Execution settings
  execution = list(
    initial_capital = 10000,
    position_sizing = "fixed_risk",  # fixed_risk, fixed_capital, kelly
    max_positions = 5,
    margin_requirement = 0.1,  # 10% margin
    use_leverage = TRUE,
    max_leverage = 20
  ),
  
  # Walk-forward settings
  walk_forward = list(
    in_sample_ratio = 0.7,      # 70% for training
    out_sample_ratio = 0.3,     # 30% for testing
    min_sample_days = 30,
    optimization_metric = "sharpe_ratio",
    anchored = FALSE            # Rolling vs anchored windows
  ),
  
  # Monte Carlo settings
  monte_carlo = list(
    simulations = 1000,
    confidence_levels = c(0.95, 0.99),
    randomize_trades = TRUE,
    randomize_returns = TRUE,
    bootstrap_samples = TRUE
  ),
  
  # Optimization settings
  optimization = list(
    method = "genetic",         # grid, random, genetic, bayesian
    max_iterations = 100,
    population_size = 50,
    convergence_threshold = 0.001,
    parallel_cores = parallel::detectCores() - 1
  ),
  
  # Performance metrics
  metrics = list(
    calculate_all = TRUE,
    custom_metrics = list(),
    risk_free_rate = 0.05,
    benchmark_symbol = "BTCUSDT"
  )
)

# ==========================================================================================================
# 📊 BACKTEST DATA MANAGEMENT
# ==========================================================================================================

#' Load historical data for backtesting
load_backtest_data <- function(symbols, start_date, end_date, 
                              timeframe = "1h", include_orderbook = FALSE) {
  
  cat("\n📊 Loading backtest data...\n")
  cat("Period:", format(start_date, "%Y-%m-%d"), "to", format(end_date, "%Y-%m-%d"), "\n")
  cat("Timeframe:", timeframe, "\n")
  
  backtest_data <- list()
  
  for (symbol in symbols) {
    cat("Loading", symbol, "...")
    
    # Try to load from database first
    market_data <- DATA_MANAGER$get_market_data(symbol, timeframe, start_date, end_date)
    
    if (nrow(market_data) == 0) {
      # Fetch from API if not in database
      market_data <- fetch_historical_data(symbol, start_date, end_date, timeframe)
      
      if (!is.null(market_data)) {
        # Save to database for future use
        DATA_MANAGER$save_market_data(symbol, timeframe, market_data)
      }
    }
    
    if (!is.null(market_data) && nrow(market_data) > 0) {
      backtest_data[[symbol]] <- prepare_backtest_data(market_data)
      cat(" ✅ (", nrow(market_data), "bars)\n")
    } else {
      cat(" ❌ No data\n")
    }
  }
  
  # Validate data alignment
  if (length(backtest_data) > 1) {
    backtest_data <- align_multi_symbol_data(backtest_data)
  }
  
  cat("\n✅ Data loaded for", length(backtest_data), "symbols\n")
  
  return(backtest_data)
}

#' Prepare data for backtesting
prepare_backtest_data <- function(raw_data) {
  
  # Ensure proper data types and sorting
  data <- data.frame(
    timestamp = as.POSIXct(raw_data$timestamp),
    open = as.numeric(raw_data$open),
    high = as.numeric(raw_data$high),
    low = as.numeric(raw_data$low),
    close = as.numeric(raw_data$close),
    volume = as.numeric(raw_data$volume),
    stringsAsFactors = FALSE
  )
  
  # Sort by timestamp
  data <- data[order(data$timestamp), ]
  
  # Add calculated fields
  data$returns <- c(0, diff(log(data$close)))
  data$range <- data$high - data$low
  data$typical_price <- (data$high + data$low + data$close) / 3
  
  # Add technical indicators if needed
  data <- add_basic_indicators(data)
  
  return(data)
}

#' Add basic technical indicators
add_basic_indicators <- function(data) {
  
  # Simple moving averages
  data$sma_20 <- calculate_sma(data$close, 20)
  data$sma_50 <- calculate_sma(data$close, 50)
  
  # RSI
  data$rsi_14 <- calculate_rsi_vector(data$close, 14)
  
  # ATR for position sizing
  data$atr_14 <- calculate_atr(data$high, data$low, data$close, 14)
  
  return(data)
}

# ==========================================================================================================
# 🎯 STRATEGY BACKTESTING ENGINE
# ==========================================================================================================

#' Run strategy backtest
run_backtest <- function(strategy_function, data, parameters = list(), 
                        config = BACKTEST_CONFIG) {
  
  cat("\n🔬 === RUNNING BACKTEST === 🔬\n")
  
  # Initialize backtest state
  backtest_state <- initialize_backtest_state(data, config)
  
  # Progress tracking
  total_bars <- nrow(data[[1]])
  pb <- txtProgressBar(min = 0, max = total_bars, style = 3)
  
  # Main backtest loop
  for (i in config$data$min_history_days:total_bars) {
    
    # Update current state
    backtest_state$current_bar <- i
    backtest_state$current_time <- data[[1]]$timestamp[i]
    
    # Get current market snapshot
    market_snapshot <- get_market_snapshot(data, i)
    
    # Generate signals
    signals <- strategy_function(
      market_snapshot, 
      backtest_state, 
      parameters
    )
    
    # Process signals
    if (!is.null(signals) && length(signals) > 0) {
      process_backtest_signals(signals, backtest_state, market_snapshot, config)
    }
    
    # Update open positions
    update_backtest_positions(backtest_state, market_snapshot, config)
    
    # Record metrics
    record_backtest_metrics(backtest_state, i)
    
    # Update progress
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # Calculate final metrics
  results <- calculate_backtest_results(backtest_state)
  
  # Display results
  display_backtest_results(results)
  
  return(results)
}

#' Initialize backtest state
initialize_backtest_state <- function(data, config) {
  
  state <- list(
    # Account state
    initial_capital = config$execution$initial_capital,
    capital = config$execution$initial_capital,
    available_capital = config$execution$initial_capital,
    
    # Position tracking
    positions = list(),
    position_history = list(),
    
    # Trade tracking
    trades = list(),
    trade_count = 0,
    
    # Performance tracking
    equity_curve = numeric(nrow(data[[1]])),
    drawdown_curve = numeric(nrow(data[[1]])),
    returns = numeric(nrow(data[[1]])),
    
    # Risk tracking
    max_drawdown = 0,
    current_drawdown = 0,
    consecutive_losses = 0,
    
    # Metadata
    start_time = data[[1]]$timestamp[1],
    current_bar = 1
  )
  
  return(state)
}

#' Get market snapshot at current bar
get_market_snapshot <- function(data, bar_index) {
  
  snapshot <- list()
  
  for (symbol in names(data)) {
    # Current bar data
    current <- data[[symbol]][bar_index, ]
    
    # Historical data (lookback)
    lookback_start <- max(1, bar_index - 100)
    history <- data[[symbol]][lookback_start:(bar_index-1), ]
    
    snapshot[[symbol]] <- list(
      current = current,
      history = history,
      last_price = current$close,
      bid = current$close - (current$close * 0.0001),  # Simulated spread
      ask = current$close + (current$close * 0.0001)
    )
  }
  
  return(snapshot)
}

#' Process backtest signals
process_backtest_signals <- function(signals, state, market_snapshot, config) {
  
  for (signal in signals) {
    
    # Check if can execute signal
    if (!validate_backtest_signal(signal, state, config)) {
      next
    }
    
    # Calculate position size
    position_size <- calculate_backtest_position_size(
      signal, state, market_snapshot, config
    )
    
    if (position_size <= 0) next
    
    # Simulate order execution
    execution <- simulate_order_execution(
      signal, position_size, market_snapshot, config
    )
    
    if (execution$success) {
      # Create position
      position <- create_backtest_position(signal, execution, state)
      
      # Update state
      state$positions[[signal$symbol]] <- position
      state$capital <- state$capital - execution$margin_required
      state$available_capital <- state$available_capital - execution$margin_required
      
      # Log trade entry
      log_backtest_trade(position, "ENTRY", state)
    }
  }
}

#' Update backtest positions
update_backtest_positions <- function(state, market_snapshot, config) {
  
  positions_to_close <- list()
  
  for (symbol in names(state$positions)) {
    position <- state$positions[[symbol]]
    
    # Update mark price
    position$mark_price <- market_snapshot[[symbol]]$last_price
    
    # Calculate P&L
    if (position$side == "long") {
      position$unrealized_pnl <- (position$mark_price - position$entry_price) * position$size
    } else {
      position$unrealized_pnl <- (position$entry_price - position$mark_price) * position$size
    }
    
    position$pnl_percentage <- position$unrealized_pnl / position$margin_used
    
    # Check exit conditions
    should_exit <- check_position_exit_conditions(position, market_snapshot[[symbol]], config)
    
    if (should_exit$exit) {
      positions_to_close[[symbol]] <- should_exit$reason
    }
    
    # Update position in state
    state$positions[[symbol]] <- position
  }
  
  # Close positions
  for (symbol in names(positions_to_close)) {
    close_backtest_position(symbol, positions_to_close[[symbol]], state, market_snapshot, config)
  }
}

#' Close backtest position
close_backtest_position <- function(symbol, reason, state, market_snapshot, config) {
  
  position <- state$positions[[symbol]]
  
  # Simulate exit execution
  exit_price <- simulate_exit_price(position, market_snapshot[[symbol]], config)
  
  # Calculate final P&L
  if (position$side == "long") {
    gross_pnl <- (exit_price - position$entry_price) * position$size
  } else {
    gross_pnl <- (position$entry_price - exit_price) * position$size
  }
  
  # Deduct commissions
  commission <- position$size * exit_price * config$data$commission_rate
  net_pnl <- gross_pnl - position$entry_commission - commission
  
  # Update position
  position$exit_price <- exit_price
  position$exit_time <- market_snapshot[[symbol]]$current$timestamp
  position$exit_reason <- reason
  position$realized_pnl <- net_pnl
  position$exit_commission <- commission
  
  # Update state
  state$capital <- state$capital + position$margin_used + net_pnl
  state$available_capital <- state$available_capital + position$margin_used
  
  # Log trade exit
  log_backtest_trade(position, "EXIT", state)
  
  # Move to history
  state$position_history[[length(state$position_history) + 1]] <- position
  state$positions[[symbol]] <- NULL
  
  # Update trade statistics
  state$trade_count <- state$trade_count + 1
  
  if (net_pnl > 0) {
    state$consecutive_losses <- 0
  } else {
    state$consecutive_losses <- state$consecutive_losses + 1
  }
}

# ==========================================================================================================
# 📈 PERFORMANCE ANALYSIS
# ==========================================================================================================

#' Calculate backtest results
calculate_backtest_results <- function(state) {
  
  # Convert trades to data frame
  trades_df <- do.call(rbind, lapply(state$position_history, function(pos) {
    data.frame(
      symbol = pos$symbol,
      side = pos$side,
      entry_time = pos$entry_time,
      exit_time = pos$exit_time,
      entry_price = pos$entry_price,
      exit_price = pos$exit_price,
      size = pos$size,
      pnl = pos$realized_pnl,
      pnl_percentage = pos$realized_pnl / pos$margin_used,
      exit_reason = pos$exit_reason,
      duration = as.numeric(difftime(pos$exit_time, pos$entry_time, units = "hours"))
    )
  }))
  
  # Calculate metrics
  if (nrow(trades_df) > 0) {
    results <- list(
      # Summary statistics
      total_trades = nrow(trades_df),
      winning_trades = sum(trades_df$pnl > 0),
      losing_trades = sum(trades_df$pnl < 0),
      win_rate = sum(trades_df$pnl > 0) / nrow(trades_df),
      
      # P&L metrics
      total_pnl = sum(trades_df$pnl),
      gross_profit = sum(trades_df$pnl[trades_df$pnl > 0]),
      gross_loss = sum(trades_df$pnl[trades_df$pnl < 0]),
      profit_factor = abs(sum(trades_df$pnl[trades_df$pnl > 0])) / 
                      abs(sum(trades_df$pnl[trades_df$pnl < 0])),
      
      # Average metrics
      avg_trade = mean(trades_df$pnl),
      avg_winner = mean(trades_df$pnl[trades_df$pnl > 0]),
      avg_loser = mean(trades_df$pnl[trades_df$pnl < 0]),
      avg_win_loss_ratio = abs(mean(trades_df$pnl[trades_df$pnl > 0])) / 
                           abs(mean(trades_df$pnl[trades_df$pnl < 0])),
      
      # Risk metrics
      max_drawdown = state$max_drawdown,
      max_consecutive_losses = max(rle(trades_df$pnl < 0)$lengths[rle(trades_df$pnl < 0)$values]),
      recovery_factor = sum(trades_df$pnl) / abs(state$max_drawdown),
      
      # Return metrics
      total_return = (state$capital - state$initial_capital) / state$initial_capital,
      annualized_return = calculate_annualized_return(state),
      
      # Risk-adjusted metrics
      sharpe_ratio = calculate_sharpe_ratio(state$returns),
      sortino_ratio = calculate_sortino_ratio(state$returns),
      calmar_ratio = calculate_calmar_ratio(state),
      
      # Additional data
      trades = trades_df,
      equity_curve = state$equity_curve,
      drawdown_curve = state$drawdown_curve
    )
  } else {
    results <- list(
      total_trades = 0,
      error = "No trades executed"
    )
  }
  
  return(results)
}

#' Display backtest results
display_backtest_results <- function(results) {
  
  cat("\n\n📊 === BACKTEST RESULTS === 📊\n")
  
  if (results$total_trades == 0) {
    cat("\n❌ No trades were executed during the backtest\n")
    return()
  }
  
  # Summary
  cat("\n📈 SUMMARY:\n")
  cat("├─ Total Trades:", results$total_trades, "\n")
  cat("├─ Win Rate:", sprintf("%.1f%%", results$win_rate * 100), "\n")
  cat("├─ Profit Factor:", round(results$profit_factor, 2), "\n")
  cat("└─ Total Return:", sprintf("%+.2f%%", results$total_return * 100), "\n")
  
  # P&L Analysis
  cat("\n💰 P&L ANALYSIS:\n")
  cat("├─ Total P&L: $", format(results$total_pnl, big.mark = ",", digits = 2), "\n")
  cat("├─ Average Trade: $", round(results$avg_trade, 2), "\n")
  cat("├─ Average Winner: $", round(results$avg_winner, 2), "\n")
  cat("├─ Average Loser: $", round(results$avg_loser, 2), "\n")
  cat("└─ Win/Loss Ratio:", round(results$avg_win_loss_ratio, 2), "\n")
  
  # Risk Metrics
  cat("\n🛡️ RISK METRICS:\n")
  cat("├─ Max Drawdown:", sprintf("%.2f%%", results$max_drawdown * 100), "\n")
  cat("├─ Max Consecutive Losses:", results$max_consecutive_losses, "\n")
  cat("├─ Recovery Factor:", round(results$recovery_factor, 2), "\n")
  cat("├─ Sharpe Ratio:", round(results$sharpe_ratio, 2), "\n")
  cat("├─ Sortino Ratio:", round(results$sortino_ratio, 2), "\n")
  cat("└─ Calmar Ratio:", round(results$calmar_ratio, 2), "\n")
  
  # Trade Analysis
  cat("\n📊 TRADE ANALYSIS:\n")
  cat("├─ Winning Trades:", results$winning_trades, 
      sprintf("(%.1f%%)", results$winning_trades / results$total_trades * 100), "\n")
  cat("├─ Losing Trades:", results$losing_trades,
      sprintf("(%.1f%%)", results$losing_trades / results$total_trades * 100), "\n")
  cat("├─ Best Trade: $", round(max(results$trades$pnl), 2), "\n")
  cat("├─ Worst Trade: $", round(min(results$trades$pnl), 2), "\n")
  cat("└─ Avg Duration:", round(mean(results$trades$duration), 1), "hours\n")
}

# ==========================================================================================================
# 🔄 WALK-FORWARD ANALYSIS
# ==========================================================================================================

#' Run walk-forward analysis
run_walk_forward_analysis <- function(strategy_function, data, 
                                    parameter_ranges, config = BACKTEST_CONFIG) {
  
  cat("\n🔄 === WALK-FORWARD ANALYSIS === 🔄\n")
  
  # Calculate window sizes
  total_bars <- nrow(data[[1]])
  in_sample_size <- floor(total_bars * config$walk_forward$in_sample_ratio)
  out_sample_size <- floor(total_bars * config$walk_forward$out_sample_ratio)
  step_size <- out_sample_size  # Non-overlapping windows
  
  # Calculate number of windows
  n_windows <- floor((total_bars - in_sample_size) / step_size)
  
  cat("Total periods:", n_windows, "\n")
  cat("In-sample size:", in_sample_size, "bars\n")
  cat("Out-sample size:", out_sample_size, "bars\n\n")
  
  # Results storage
  wf_results <- list()
  
  for (i in 1:n_windows) {
    cat("\n📍 Period", i, "of", n_windows, "\n")
    
    # Define window boundaries
    if (config$walk_forward$anchored) {
      in_start <- 1
      in_end <- in_sample_size + (i - 1) * step_size
    } else {
      in_start <- 1 + (i - 1) * step_size
      in_end <- in_start + in_sample_size - 1
    }
    
    out_start <- in_end + 1
    out_end <- min(out_start + out_sample_size - 1, total_bars)
    
    # Split data
    in_sample_data <- extract_data_window(data, in_start, in_end)
    out_sample_data <- extract_data_window(data, out_start, out_end)
    
    # Optimize on in-sample
    cat("  Optimizing on in-sample data...\n")
    optimal_params <- optimize_strategy_parameters(
      strategy_function, in_sample_data, parameter_ranges, config
    )
    
    cat("  Optimal parameters found:\n")
    print(optimal_params$parameters)
    
    # Test on out-sample
    cat("  Testing on out-sample data...\n")
    out_results <- run_backtest(
      strategy_function, out_sample_data, 
      optimal_params$parameters, config
    )
    
    # Store results
    wf_results[[i]] <- list(
      period = i,
      in_sample_period = c(in_start, in_end),
      out_sample_period = c(out_start, out_end),
      optimal_parameters = optimal_params$parameters,
      in_sample_performance = optimal_params$performance,
      out_sample_performance = out_results,
      efficiency = out_results$sharpe_ratio / optimal_params$performance
    )
  }
  
  # Analyze walk-forward results
  wf_analysis <- analyze_walk_forward_results(wf_results)
  
  # Display analysis
  display_walk_forward_analysis(wf_analysis)
  
  return(wf_analysis)
}

#' Analyze walk-forward results
analyze_walk_forward_results <- function(wf_results) {
  
  # Extract metrics
  in_sample_sharpe <- sapply(wf_results, function(x) x$in_sample_performance)
  out_sample_sharpe <- sapply(wf_results, function(x) x$out_sample_performance$sharpe_ratio)
  efficiency <- sapply(wf_results, function(x) x$efficiency)
  
  # Calculate statistics
  analysis <- list(
    n_periods = length(wf_results),
    
    # Performance consistency
    avg_in_sample_sharpe = mean(in_sample_sharpe, na.rm = TRUE),
    avg_out_sample_sharpe = mean(out_sample_sharpe, na.rm = TRUE),
    
    # Efficiency metrics
    avg_efficiency = mean(efficiency, na.rm = TRUE),
    efficiency_std = sd(efficiency, na.rm = TRUE),
    
    # Degradation analysis
    performance_degradation = 1 - mean(out_sample_sharpe, na.rm = TRUE) / 
                                 mean(in_sample_sharpe, na.rm = TRUE),
    
    # Stability analysis
    parameter_stability = calculate_parameter_stability(wf_results),
    
    # Success rate
    profitable_periods = sum(sapply(wf_results, function(x) 
      x$out_sample_performance$total_return > 0)),
    
    # Raw results
    raw_results = wf_results
  )
  
  return(analysis)
}

# ==========================================================================================================
# 🎲 MONTE CARLO SIMULATION
# ==========================================================================================================

#' Run Monte Carlo simulation
run_monte_carlo_simulation <- function(backtest_results, config = BACKTEST_CONFIG) {
  
  cat("\n🎲 === MONTE CARLO SIMULATION === 🎲\n")
  cat("Running", config$monte_carlo$simulations, "simulations...\n")
  
  # Extract trade returns
  trade_returns <- backtest_results$trades$pnl_percentage
  
  if (length(trade_returns) < 10) {
    cat("❌ Insufficient trades for Monte Carlo simulation\n")
    return(NULL)
  }
  
  # Initialize results storage
  mc_results <- list(
    equity_curves = matrix(NA, nrow = config$monte_carlo$simulations, 
                          ncol = length(trade_returns)),
    final_returns = numeric(config$monte_carlo$simulations),
    max_drawdowns = numeric(config$monte_carlo$simulations),
    sharpe_ratios = numeric(config$monte_carlo$simulations)
  )
  
  # Progress bar
  pb <- txtProgressBar(min = 0, max = config$monte_carlo$simulations, style = 3)
  
  # Run simulations
  for (sim in 1:config$monte_carlo$simulations) {
    
    # Generate random trade sequence
    if (config$monte_carlo$randomize_trades) {
      sim_returns <- sample(trade_returns, replace = config$monte_carlo$bootstrap_samples)
    } else {
      sim_returns <- trade_returns
    }
    
    # Add noise if configured
    if (config$monte_carlo$randomize_returns) {
      noise <- rnorm(length(sim_returns), 0, sd(trade_returns) * 0.1)
      sim_returns <- sim_returns + noise
    }
    
    # Calculate equity curve
    equity_curve <- cumprod(1 + sim_returns)
    mc_results$equity_curves[sim, ] <- equity_curve
    
    # Calculate metrics
    mc_results$final_returns[sim] <- tail(equity_curve, 1) - 1
    mc_results$max_drawdowns[sim] <- calculate_max_drawdown_from_returns(sim_returns)
    mc_results$sharpe_ratios[sim] <- calculate_sharpe_ratio(sim_returns)
    
    setTxtProgressBar(pb, sim)
  }
  
  close(pb)
  
  # Analyze results
  mc_analysis <- analyze_monte_carlo_results(mc_results, config)
  
  # Display analysis
  display_monte_carlo_analysis(mc_analysis, backtest_results)
  
  return(mc_analysis)
}

#' Analyze Monte Carlo results
analyze_monte_carlo_results <- function(mc_results, config) {
  
  analysis <- list(
    # Return distribution
    return_mean = mean(mc_results$final_returns),
    return_median = median(mc_results$final_returns),
    return_std = sd(mc_results$final_returns),
    
    # Confidence intervals
    return_ci = quantile(mc_results$final_returns, 
                        c((1 - config$monte_carlo$confidence_levels[1]) / 2,
                          1 - (1 - config$monte_carlo$confidence_levels[1]) / 2)),
    
    # Risk metrics
    avg_max_drawdown = mean(mc_results$max_drawdowns),
    worst_drawdown = max(mc_results$max_drawdowns),
    
    # Risk of ruin
    risk_of_ruin = sum(mc_results$final_returns < -0.5) / 
                   config$monte_carlo$simulations,
    
    # Probability of profit
    prob_profit = sum(mc_results$final_returns > 0) / 
                  config$monte_carlo$simulations,
    
    # Risk-adjusted returns
    avg_sharpe = mean(mc_results$sharpe_ratios),
    sharpe_ci = quantile(mc_results$sharpe_ratios, c(0.05, 0.95)),
    
    # Raw data
    raw_results = mc_results
  )
  
  return(analysis)
}

# ==========================================================================================================
# 🔧 OPTIMIZATION FUNCTIONS
# ==========================================================================================================

#' Optimize strategy parameters
optimize_strategy_parameters <- function(strategy_function, data, 
                                       parameter_ranges, config) {
  
  optimization_method <- config$optimization$method
  
  result <- switch(optimization_method,
    "grid" = optimize_grid_search(strategy_function, data, parameter_ranges, config),
    "random" = optimize_random_search(strategy_function, data, parameter_ranges, config),
    "genetic" = optimize_genetic_algorithm(strategy_function, data, parameter_ranges, config),
    "bayesian" = optimize_bayesian(strategy_function, data, parameter_ranges, config),
    optimize_grid_search(strategy_function, data, parameter_ranges, config)
  )
  
  return(result)
}

#' Grid search optimization
optimize_grid_search <- function(strategy_function, data, parameter_ranges, config) {
  
  # Create parameter grid
  param_grid <- expand.grid(parameter_ranges)
  n_combinations <- nrow(param_grid)
  
  cat("  Testing", n_combinations, "parameter combinations...\n")
  
  # Set up parallel processing
  if (config$optimization$parallel_cores > 1) {
    cl <- makeCluster(config$optimization$parallel_cores)
    registerDoParallel(cl)
    
    # Export required objects to workers
    clusterExport(cl, c("run_backtest", "strategy_function", "data", "config"),
                  envir = environment())
  }
  
  # Run backtests
  results <- foreach(i = 1:n_combinations, .combine = rbind) %dopar% {
    params <- as.list(param_grid[i, ])
    
    backtest_result <- run_backtest(strategy_function, data, params, config)
    
    # Return key metrics
    data.frame(
      param_set = i,
      sharpe_ratio = backtest_result$sharpe_ratio,
      total_return = backtest_result$total_return,
      max_drawdown = backtest_result$max_drawdown,
      win_rate = backtest_result$win_rate
    )
  }
  
  # Clean up parallel
  if (config$optimization$parallel_cores > 1) {
    stopCluster(cl)
  }
  
  # Find best parameters
  best_idx <- which.max(results$sharpe_ratio)
  best_params <- as.list(param_grid[best_idx, ])
  best_performance <- results$sharpe_ratio[best_idx]
  
  return(list(
    parameters = best_params,
    performance = best_performance,
    all_results = results
  ))
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

#' Calculate position size for backtest
calculate_backtest_position_size <- function(signal, state, market_snapshot, config) {
  
  method <- config$execution$position_sizing
  
  size <- switch(method,
    "fixed_risk" = {
      # Risk-based sizing
      risk_amount <- state$capital * 0.02  # 2% risk
      stop_distance <- abs(signal$stop_loss - signal$entry_price)
      
      if (stop_distance > 0) {
        risk_amount / stop_distance
      } else {
        0
      }
    },
    
    "fixed_capital" = {
      # Fixed percentage of capital
      (state$available_capital * 0.1) / signal$entry_price
    },
    
    "kelly" = {
      # Kelly criterion (simplified)
      win_rate <- 0.6  # Would be calculated from history
      win_loss_ratio <- 1.5
      
      kelly_fraction <- (win_rate * win_loss_ratio - (1 - win_rate)) / win_loss_ratio
      kelly_fraction <- min(kelly_fraction, 0.25)  # Cap at 25%
      
      (state$available_capital * kelly_fraction) / signal$entry_price
    },
    
    # Default
    (state$available_capital * 0.05) / signal$entry_price
  )
  
  # Apply leverage constraints
  margin_required <- size * signal$entry_price / config$execution$max_leverage
  
  if (margin_required > state$available_capital) {
    size <- state$available_capital * config$execution$max_leverage / signal$entry_price
  }
  
  return(size)
}

#' Simulate order execution with slippage
simulate_order_execution <- function(signal, size, market_snapshot, config) {
  
  market <- market_snapshot[[signal$symbol]]
  
  # Simulate slippage
  tick_size <- market$current$close * 0.0001  # 1 basis point
  slippage <- tick_size * config$data$slippage_ticks
  
  if (signal$direction == "long") {
    fill_price <- market$ask + slippage
  } else {
    fill_price <- market$bid - slippage
  }
  
  # Calculate commission
  commission <- size * fill_price * config$data$commission_rate
  
  # Calculate margin
  margin_required <- size * fill_price / config$execution$max_leverage
  
  return(list(
    success = TRUE,
    fill_price = fill_price,
    size = size,
    commission = commission,
    margin_required = margin_required
  ))
}

#' Check position exit conditions
check_position_exit_conditions <- function(position, market_data, config) {
  
  current_price <- market_data$last_price
  
  # Stop loss
  if (!is.null(position$stop_loss)) {
    if (position$side == "long" && current_price <= position$stop_loss) {
      return(list(exit = TRUE, reason = "STOP_LOSS"))
    } else if (position$side == "short" && current_price >= position$stop_loss) {
      return(list(exit = TRUE, reason = "STOP_LOSS"))
    }
  }
  
  # Take profit
  if (!is.null(position$take_profit)) {
    if (position$side == "long" && current_price >= position$take_profit) {
      return(list(exit = TRUE, reason = "TAKE_PROFIT"))
    } else if (position$side == "short" && current_price <= position$take_profit) {
      return(list(exit = TRUE, reason = "TAKE_PROFIT"))
    }
  }
  
  # Time-based exit
  if (!is.null(position$max_holding_time)) {
    hold_time <- as.numeric(difftime(market_data$current$timestamp, 
                                    position$entry_time, units = "hours"))
    if (hold_time > position$max_holding_time) {
      return(list(exit = TRUE, reason = "TIME_EXIT"))
    }
  }
  
  # Trailing stop
  if (!is.null(position$trailing_stop)) {
    update_trailing_stop(position, current_price)
  }
  
  return(list(exit = FALSE))
}

#' Calculate annualized return
calculate_annualized_return <- function(state) {
  
  total_days <- as.numeric(difftime(state$current_time, state$start_time, units = "days"))
  
  if (total_days <= 0) return(0)
  
  total_return <- (state$capital - state$initial_capital) / state$initial_capital
  annualized_return <- (1 + total_return) ^ (365 / total_days) - 1
  
  return(annualized_return)
}

#' Calculate Calmar ratio
calculate_calmar_ratio <- function(state) {
  
  annual_return <- calculate_annualized_return(state)
  
  if (state$max_drawdown == 0) return(0)
  
  return(annual_return / abs(state$max_drawdown))
}

#' Display walk-forward analysis
display_walk_forward_analysis <- function(analysis) {
  
  cat("\n\n📊 === WALK-FORWARD ANALYSIS RESULTS === 📊\n")
  
  cat("\n🔄 SUMMARY:\n")
  cat("├─ Periods Tested:", analysis$n_periods, "\n")
  cat("├─ Avg In-Sample Sharpe:", round(analysis$avg_in_sample_sharpe, 2), "\n")
  cat("├─ Avg Out-Sample Sharpe:", round(analysis$avg_out_sample_sharpe, 2), "\n")
  cat("├─ Efficiency:", sprintf("%.1f%%", analysis$avg_efficiency * 100), "\n")
  cat("├─ Performance Degradation:", sprintf("%.1f%%", analysis$performance_degradation * 100), "\n")
  cat("└─ Profitable Periods:", analysis$profitable_periods, "/", analysis$n_periods, "\n")
}

#' Display Monte Carlo analysis
display_monte_carlo_analysis <- function(analysis, original_results) {
  
  cat("\n\n🎲 === MONTE CARLO RESULTS === 🎲\n")
  
  cat("\n📊 RETURN DISTRIBUTION:\n")
  cat("├─ Mean Return:", sprintf("%.2f%%", analysis$return_mean * 100), "\n")
  cat("├─ Median Return:", sprintf("%.2f%%", analysis$return_median * 100), "\n")
  cat("├─ Std Deviation:", sprintf("%.2f%%", analysis$return_std * 100), "\n")
  cat("└─ 95% CI: [", sprintf("%.2f%%", analysis$return_ci[1] * 100), 
      ",", sprintf("%.2f%%", analysis$return_ci[2] * 100), "]\n")
  
  cat("\n🛡️ RISK ANALYSIS:\n")
  cat("├─ Avg Max Drawdown:", sprintf("%.2f%%", analysis$avg_max_drawdown * 100), "\n")
  cat("├─ Worst Drawdown:", sprintf("%.2f%%", analysis$worst_drawdown * 100), "\n")
  cat("├─ Risk of Ruin:", sprintf("%.2f%%", analysis$risk_of_ruin * 100), "\n")
  cat("└─ Probability of Profit:", sprintf("%.2f%%", analysis$prob_profit * 100), "\n")
  
  cat("\n📈 COMPARISON TO ORIGINAL:\n")
  cat("├─ Original Return:", sprintf("%.2f%%", original_results$total_return * 100), "\n")
  cat("├─ Percentile:", sprintf("%.1f%%", 
      sum(analysis$raw_results$final_returns < original_results$total_return) / 
      length(analysis$raw_results$final_returns) * 100), "\n")
  cat("└─ Original Sharpe:", round(original_results$sharpe_ratio, 2), 
      "vs Avg:", round(analysis$avg_sharpe, 2), "\n")
}

# ==========================================================================================================
# 📊 VISUALIZATION HELPERS
# ==========================================================================================================

#' Plot backtest results
plot_backtest_results <- function(results) {
  
  # This would create various plots:
  # 1. Equity curve
  # 2. Drawdown chart
  # 3. Monthly returns heatmap
  # 4. Trade distribution
  
  cat("\n📊 Visualization not implemented in text mode\n")
  cat("   Export results to CSV for external plotting\n")
}

#' Export backtest results
export_backtest_results <- function(results, filename = NULL) {
  
  if (is.null(filename)) {
    filename <- paste0("backtest_results_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  # Export trades
  write.csv(results$trades, paste0(filename, "_trades.csv"), row.names = FALSE)
  
  # Export summary
  summary_df <- data.frame(
    metric = c("Total Trades", "Win Rate", "Profit Factor", "Total Return",
               "Sharpe Ratio", "Max Drawdown"),
    value = c(results$total_trades, results$win_rate, results$profit_factor,
              results$total_return, results$sharpe_ratio, results$max_drawdown)
  )
  write.csv(summary_df, paste0(filename, "_summary.csv"), row.names = FALSE)
  
  cat("\n✅ Results exported to:", filename, "\n")
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Backtest engine interface
BACKTEST_ENGINE <- list(
  # Core functions
  run = run_backtest,
  load_data = load_backtest_data,
  
  # Advanced analysis
  walk_forward = run_walk_forward_analysis,
  monte_carlo = run_monte_carlo_simulation,
  
  # Optimization
  optimize = optimize_strategy_parameters,
  
  # Utilities
  export = export_backtest_results,
  plot = plot_backtest_results,
  
  # Configuration
  config = BACKTEST_CONFIG
)

cat("✅ BACKTEST_ENGINE.R loaded successfully!\n")
cat("🔬 Strategy backtesting ready\n")
cat("📊 Walk-forward analysis available\n")
cat("🎲 Monte Carlo simulation enabled\n")
cat("🔧 Parameter optimization configured\n")