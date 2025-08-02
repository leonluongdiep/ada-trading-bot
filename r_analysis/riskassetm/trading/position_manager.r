# ==========================================================================================================
# 📊 POSITION MANAGEMENT SYSTEM V3
# ==========================================================================================================
# Pfad: trading/position_manager.r
# Zentrale Positionsverwaltung mit erweiterten Tracking- und Analyse-Features
# ==========================================================================================================

cat("📊 Loading Position Management System V3...\n")

# ==========================================================================================================
# 🔧 SYSTEM INITIALIZATION
# ==========================================================================================================

# Load dependencies if not already loaded
if (!exists("SYSTEM_CONFIG_LOADED")) {
  source("../core/config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
}

if (!exists("API_ENGINE_LOADED")) {
  source("../core/api_engine.r")
  API_ENGINE_LOADED <- TRUE
}

# ==========================================================================================================
# 📈 POSITION TRACKING & MONITORING
# ==========================================================================================================

#' Enhanced position tracker with history
POSITION_TRACKER <- list(
  active_positions = list(),
  position_history = list(),
  performance_metrics = list(),
  last_update = NULL
)

#' Get detailed position information
get_position_details <- function(symbol = NULL) {
  tryCatch({
    positions <- get_current_positions()
    
    if (is.null(symbol)) {
      return(positions)
    }
    
    # Filter for specific symbol
    position <- positions[positions$symbol == symbol, ]
    
    if (nrow(position) == 0) {
      cat("ℹ️ No position found for", symbol, "\n")
      return(NULL)
    }
    
    # Enhance with additional metrics
    position$time_held <- Sys.time() - position$entry_time
    position$max_profit <- get_position_max_profit(symbol)
    position$max_loss <- get_position_max_loss(symbol)
    position$current_range <- (position$mark_price - position$avg_price) / position$avg_price
    
    return(position)
    
  }, error = function(e) {
    cat("❌ Error getting position details:", e$message, "\n")
    return(NULL)
  })
}

#' Update position tracker
update_position_tracker <- function() {
  tryCatch({
    current_positions <- get_current_positions()
    
    # Update active positions
    POSITION_TRACKER$active_positions <<- current_positions
    
    # Check for closed positions
    if (!is.null(POSITION_TRACKER$last_update)) {
      check_closed_positions(POSITION_TRACKER$last_update, current_positions)
    }
    
    # Update timestamp
    POSITION_TRACKER$last_update <<- Sys.time()
    
    # Calculate performance metrics
    if (nrow(current_positions) > 0) {
      POSITION_TRACKER$performance_metrics <<- calculate_performance_metrics(current_positions)
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error updating position tracker:", e$message, "\n")
    return(FALSE)
  })
}

# ==========================================================================================================
# 📊 POSITION ANALYTICS
# ==========================================================================================================

#' Calculate comprehensive position metrics
calculate_position_metrics <- function(position) {
  metrics <- list()
  
  # Basic metrics
  metrics$symbol <- position$symbol
  metrics$side <- position$side
  metrics$size <- position$size
  metrics$entry_price <- position$avg_price
  metrics$current_price <- position$mark_price
  
  # Performance metrics
  metrics$pnl <- position$unrealized_pnl
  metrics$pnl_percentage <- position$pnl_ratio * 100
  metrics$roi <- (position$unrealized_pnl / position$margin) * 100
  
  # Risk metrics
  metrics$exposure <- position$size * position$mark_price
  metrics$risk_score <- calculate_position_risk_score(position)
  metrics$leverage <- position$leverage
  
  # Price movement metrics
  price_change <- (position$mark_price - position$avg_price) / position$avg_price
  metrics$price_change_pct <- price_change * 100
  metrics$favorable_move <- (position$side == "long" && price_change > 0) || 
                           (position$side == "short" && price_change < 0)
  
  # Volatility estimate (simplified)
  metrics$volatility_estimate <- estimate_position_volatility(position$symbol)
  
  return(metrics)
}

#' Calculate performance metrics for all positions
calculate_performance_metrics <- function(positions) {
  if (nrow(positions) == 0) return(list())
  
  metrics <- list(
    total_positions = nrow(positions),
    total_exposure = sum(positions$size * positions$mark_price),
    total_pnl = sum(positions$unrealized_pnl),
    total_margin = sum(positions$margin),
    avg_leverage = mean(positions$leverage),
    winning_positions = sum(positions$unrealized_pnl > 0),
    losing_positions = sum(positions$unrealized_pnl < 0),
    win_rate = sum(positions$unrealized_pnl > 0) / nrow(positions) * 100
  )
  
  # Add per-symbol breakdown
  metrics$by_symbol <- lapply(unique(positions$symbol), function(sym) {
    pos <- positions[positions$symbol == sym, ]
    list(
      symbol = sym,
      pnl = sum(pos$unrealized_pnl),
      exposure = sum(pos$size * pos$mark_price),
      positions = nrow(pos)
    )
  })
  
  return(metrics)
}

# ==========================================================================================================
# 🎯 POSITION ENTRY & SIZING
# ==========================================================================================================

#' Calculate optimal position size based on risk
calculate_position_size <- function(symbol, entry_price, stop_loss_price, 
                                  risk_amount = NULL, max_leverage = 20) {
  
  # Get account balance
  balance <- get_account_balance()
  if (is.null(balance)) {
    cat("❌ Cannot fetch balance for position sizing\n")
    return(NULL)
  }
  
  # Use default risk if not specified
  if (is.null(risk_amount)) {
    risk_amount <- balance$available * RISK_PARAMS$max_position_risk
  }
  
  # Calculate risk per unit
  risk_per_unit <- abs(entry_price - stop_loss_price)
  
  # Calculate position size based on risk
  position_size <- risk_amount / risk_per_unit
  
  # Check leverage constraint
  position_value <- position_size * entry_price
  required_margin <- position_value / max_leverage
  
  if (required_margin > balance$available * 0.5) {
    # Adjust size to fit within margin limits
    max_margin <- balance$available * 0.5
    position_size <- (max_margin * max_leverage) / entry_price
    cat("⚠️ Position size adjusted to fit margin limits\n")
  }
  
  # Get contract specifications
  specs <- get_contract_specs(symbol)
  if (!is.null(specs)) {
    # Round to contract size
    position_size <- floor(position_size / specs$min_size) * specs$min_size
  }
  
  return(list(
    size = position_size,
    value = position_size * entry_price,
    margin_required = position_value / max_leverage,
    risk_amount = position_size * risk_per_unit,
    leverage_used = position_value / required_margin
  ))
}

#' Smart position entry with validation
smart_position_entry <- function(symbol, side, size = NULL, 
                               tp_ratio = 2, sl_ratio = 1,
                               use_market_order = TRUE) {
  
  cat("\n🎯 === SMART POSITION ENTRY === 🎯\n")
  cat("Symbol:", symbol, "| Side:", side, "\n")
  
  # Pre-entry checks
  if (!validate_entry_conditions(symbol, side)) {
    cat("❌ Entry conditions not met\n")
    return(FALSE)
  }
  
  # Get current market data
  ticker <- get_ticker(symbol)
  if (is.null(ticker)) {
    cat("❌ Cannot fetch market data\n")
    return(FALSE)
  }
  
  entry_price <- if (use_market_order) ticker$last else 
                 if (side == "buy") ticker$bid else ticker$ask
  
  # Calculate position size if not provided
  if (is.null(size)) {
    # Calculate stop loss price
    sl_price <- if (side == "buy") {
      entry_price * (1 - sl_ratio / 100)
    } else {
      entry_price * (1 + sl_ratio / 100)
    }
    
    size_calc <- calculate_position_size(symbol, entry_price, sl_price)
    if (is.null(size_calc)) {
      cat("❌ Cannot calculate position size\n")
      return(FALSE)
    }
    size <- size_calc$size
  }
  
  # Place entry order
  cat("📍 Placing", side, "order for", size, symbol, "at", entry_price, "\n")
  
  order_result <- place_order(
    symbol = symbol,
    side = side,
    size = size,
    order_type = if (use_market_order) "market" else "limit",
    price = if (!use_market_order) entry_price else NULL
  )
  
  if (!order_result$success) {
    cat("❌ Order placement failed\n")
    return(FALSE)
  }
  
  # Set up risk management orders
  Sys.sleep(2)  # Wait for position to be established
  
  setup_position_protection(symbol, side, size, entry_price, tp_ratio, sl_ratio)
  
  # Update tracker
  update_position_tracker()
  
  cat("✅ Position entry complete\n")
  return(TRUE)
}

# ==========================================================================================================
# 🛡️ POSITION PROTECTION
# ==========================================================================================================

#' Setup position protection orders (TP/SL)
setup_position_protection <- function(symbol, side, size, entry_price, 
                                    tp_ratio = 2, sl_ratio = 1) {
  
  cat("\n🛡️ Setting up position protection...\n")
  
  # Calculate TP/SL prices
  if (side == "buy" || side == "long") {
    tp_price <- entry_price * (1 + tp_ratio / 100)
    sl_price <- entry_price * (1 - sl_ratio / 100)
  } else {
    tp_price <- entry_price * (1 - tp_ratio / 100)
    sl_price <- entry_price * (1 + sl_ratio / 100)
  }
  
  # Place take profit order
  tp_result <- place_take_profit_order(symbol, side, size, tp_price)
  if (tp_result) {
    cat("✅ Take Profit set at", round(tp_price, 4), 
        "(", round(tp_ratio, 1), "% from entry)\n")
  } else {
    cat("❌ Failed to set Take Profit\n")
  }
  
  # Place stop loss order
  sl_result <- place_stop_loss_order(symbol, side, size, sl_price)
  if (sl_result) {
    cat("✅ Stop Loss set at", round(sl_price, 4), 
        "(", round(sl_ratio, 1), "% from entry)\n")
  } else {
    cat("❌ Failed to set Stop Loss\n")
  }
  
  return(list(tp = tp_result, sl = sl_result))
}

#' Adjust position protection dynamically
adjust_position_protection <- function(symbol, new_sl_price = NULL, new_tp_price = NULL) {
  
  position <- get_position_details(symbol)
  if (is.null(position)) return(FALSE)
  
  # Cancel existing orders
  cancel_position_orders(symbol)
  
  # Set new protection
  if (!is.null(new_sl_price)) {
    place_stop_loss_order(symbol, position$side, position$size, new_sl_price)
  }
  
  if (!is.null(new_tp_price)) {
    place_take_profit_order(symbol, position$side, position$size, new_tp_price)
  }
  
  return(TRUE)
}

# ==========================================================================================================
# 📉 POSITION EXIT MANAGEMENT
# ==========================================================================================================

#' Smart position exit with multiple strategies
smart_position_exit <- function(symbol, exit_strategy = "full", percentage = 100) {
  
  cat("\n📉 === SMART POSITION EXIT === 📉\n")
  
  position <- get_position_details(symbol)
  if (is.null(position)) {
    cat("❌ No position found for", symbol, "\n")
    return(FALSE)
  }
  
  cat("Current position:", position$side, position$size, symbol, "\n")
  cat("PnL:", round(position$unrealized_pnl, 2), "USDT",
      "(", round(position$pnl_ratio * 100, 2), "%)\n")
  
  exit_size <- position$size
  
  # Handle partial exits
  if (exit_strategy == "partial" && percentage < 100) {
    exit_size <- floor(position$size * percentage / 100)
    cat("Partial exit:", percentage, "% (", exit_size, "contracts)\n")
  }
  
  # Cancel protection orders first
  cancel_position_orders(symbol)
  
  # Execute exit
  exit_side <- if (position$side == "long") "sell" else "buy"
  
  exit_result <- place_order(
    symbol = symbol,
    side = exit_side,
    size = exit_size,
    order_type = "market"
  )
  
  if (exit_result$success) {
    cat("✅ Position exit successful\n")
    
    # Log to history
    log_position_closure(position, exit_result)
    
    # Update tracker
    update_position_tracker()
    
    return(TRUE)
  } else {
    cat("❌ Position exit failed\n")
    return(FALSE)
  }
}

#' Scale out of position gradually
scale_out_position <- function(symbol, levels = c(25, 50, 100), 
                             price_targets = NULL) {
  
  position <- get_position_details(symbol)
  if (is.null(position)) return(FALSE)
  
  cat("\n📊 Scaling out of", symbol, "position\n")
  
  results <- list()
  
  for (i in seq_along(levels)) {
    exit_pct <- if (i == 1) levels[i] else levels[i] - levels[i-1]
    
    cat("\nLevel", i, ": Exiting", exit_pct, "%")
    
    if (!is.null(price_targets) && i <= length(price_targets)) {
      cat(" at target price", price_targets[i])
      # Set limit order for scale-out
      # Implementation depends on your order system
    }
    
    result <- smart_position_exit(symbol, "partial", exit_pct)
    results[[paste0("level_", i)]] <- result
    
    if (!result) {
      cat("\n⚠️ Scale-out stopped at level", i, "\n")
      break
    }
  }
  
  return(results)
}

# ==========================================================================================================
# 📜 POSITION HISTORY & LOGGING
# ==========================================================================================================

#' Log closed position to history
log_position_closure <- function(position, exit_details) {
  
  closure_record <- list(
    symbol = position$symbol,
    side = position$side,
    entry_price = position$avg_price,
    exit_price = exit_details$price,
    size = position$size,
    pnl = position$unrealized_pnl,
    pnl_percentage = position$pnl_ratio * 100,
    entry_time = position$entry_time,
    exit_time = Sys.time(),
    duration = Sys.time() - position$entry_time,
    max_profit = position$max_profit,
    max_loss = position$max_loss
  )
  
  # Add to history
  POSITION_TRACKER$position_history[[length(POSITION_TRACKER$position_history) + 1]] <<- closure_record
  
  # Save to file if configured
  if (exists("POSITION_HISTORY_FILE")) {
    save_position_history(closure_record)
  }
}

#' Get position history summary
get_position_history_summary <- function(last_n = 20) {
  
  history <- POSITION_TRACKER$position_history
  
  if (length(history) == 0) {
    cat("ℹ️ No position history available\n")
    return(NULL)
  }
  
  # Convert to data frame
  history_df <- do.call(rbind, lapply(history, as.data.frame))
  
  # Limit to last N
  if (nrow(history_df) > last_n) {
    history_df <- tail(history_df, last_n)
  }
  
  # Calculate summary stats
  summary_stats <- list(
    total_trades = nrow(history_df),
    winning_trades = sum(history_df$pnl > 0),
    losing_trades = sum(history_df$pnl < 0),
    win_rate = sum(history_df$pnl > 0) / nrow(history_df) * 100,
    total_pnl = sum(history_df$pnl),
    avg_winner = mean(history_df$pnl[history_df$pnl > 0]),
    avg_loser = mean(history_df$pnl[history_df$pnl < 0]),
    profit_factor = abs(sum(history_df$pnl[history_df$pnl > 0])) / 
                    abs(sum(history_df$pnl[history_df$pnl < 0]))
  )
  
  return(list(
    history = history_df,
    summary = summary_stats
  ))
}

# ==========================================================================================================
# 🔍 POSITION VALIDATION & HELPERS
# ==========================================================================================================

#' Validate entry conditions
validate_entry_conditions <- function(symbol, side) {
  
  # Check if already in position
  current_pos <- get_position_details(symbol)
  if (!is.null(current_pos) && nrow(current_pos) > 0) {
    cat("⚠️ Already have position in", symbol, "\n")
    return(FALSE)
  }
  
  # Check account balance
  balance <- get_account_balance()
  if (is.null(balance) || balance$available < 100) {
    cat("⚠️ Insufficient balance\n")
    return(FALSE)
  }
  
  # Check market conditions
  ticker <- get_ticker(symbol)
  if (is.null(ticker)) {
    cat("⚠️ Cannot fetch market data\n")
    return(FALSE)
  }
  
  # Check spread
  spread_pct <- (ticker$ask - ticker$bid) / ticker$bid * 100
  if (spread_pct > 0.5) {
    cat("⚠️ High spread:", round(spread_pct, 2), "%\n")
  }
  
  return(TRUE)
}

#' Cancel all orders for a position
cancel_position_orders <- function(symbol) {
  
  orders <- get_open_orders()
  if (nrow(orders) == 0) return(TRUE)
  
  position_orders <- orders[orders$symbol == symbol, ]
  
  if (nrow(position_orders) > 0) {
    cat("Canceling", nrow(position_orders), "orders for", symbol, "\n")
    
    for (i in 1:nrow(position_orders)) {
      cancel_order(position_orders$order_id[i])
    }
  }
  
  return(TRUE)
}

#' Get contract specifications
get_contract_specs <- function(symbol) {
  # This would typically fetch from API
  # Simplified version with common specs
  
  specs <- list(
    "BTCUSDT" = list(min_size = 0.001, tick_size = 0.1),
    "ETHUSDT" = list(min_size = 0.01, tick_size = 0.01),
    "SOLUSDT" = list(min_size = 0.1, tick_size = 0.001)
  )
  
  return(specs[[symbol]])
}

#' Estimate position volatility
estimate_position_volatility <- function(symbol) {
  # Simplified volatility estimate
  # In production, would use historical data
  
  volatility_map <- list(
    "BTCUSDT" = 0.02,
    "ETHUSDT" = 0.025,
    "SOLUSDT" = 0.04
  )
  
  return(volatility_map[[symbol]] %||% 0.03)
}

# ==========================================================================================================
# 📊 POSITION DISPLAY FUNCTIONS
# ==========================================================================================================

#' Display detailed position information
display_position_details <- function(symbol = NULL) {
  
  positions <- get_position_details(symbol)
  
  if (is.null(positions) || nrow(positions) == 0) {
    cat("ℹ️ No positions to display\n")
    return()
  }
  
  cat("\n📊 === POSITION DETAILS === 📊\n")
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    metrics <- calculate_position_metrics(pos)
    
    cat("\n🔸", pos$symbol, "-", pos$side, "\n")
    cat("├─ Size:", pos$size, "contracts\n")
    cat("├─ Entry:", round(pos$avg_price, 4), "| Current:", round(pos$mark_price, 4), "\n")
    cat("├─ PnL:", round(metrics$pnl, 2), "USDT (", 
        sprintf("%+.2f%%", metrics$pnl_percentage), ")\n")
    cat("├─ ROI:", sprintf("%+.2f%%", metrics$roi), "\n")
    cat("├─ Exposure:", round(metrics$exposure, 2), "USDT\n")
    cat("├─ Leverage:", metrics$leverage, "x\n")
    cat("└─ Risk Score:", round(metrics$risk_score, 2), "\n")
  }
  
  # Show performance summary
  if (nrow(positions) > 1) {
    perf <- POSITION_TRACKER$performance_metrics
    if (!is.null(perf)) {
      cat("\n📈 Portfolio Summary:\n")
      cat("├─ Total Positions:", perf$total_positions, "\n")
      cat("├─ Total Exposure:", round(perf$total_exposure, 2), "USDT\n")
      cat("├─ Total PnL:", round(perf$total_pnl, 2), "USDT\n")
      cat("├─ Win Rate:", round(perf$win_rate, 1), "%\n")
      cat("└─ Avg Leverage:", round(perf$avg_leverage, 1), "x\n")
    }
  }
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Initialize position tracker on load
update_position_tracker()

# Export main functions
POSITION_MANAGER_FUNCTIONS <- list(
  get_details = get_position_details,
  update_tracker = update_position_tracker,
  calculate_size = calculate_position_size,
  smart_entry = smart_position_entry,
  smart_exit = smart_position_exit,
  scale_out = scale_out_position,
  display = display_position_details,
  history = get_position_history_summary
)

cat("✅ POSITION_MANAGER.R loaded successfully!\n")
cat("📊 Position tracking and management ready\n")
cat("🎯 Smart entry/exit strategies available\n")
cat("📜 Position history logging enabled\n")