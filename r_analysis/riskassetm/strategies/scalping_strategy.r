# ==========================================================================================================
# ⚡ HIGH-FREQUENCY SCALPING STRATEGY V1
# ==========================================================================================================
# Pfad: strategies/scalping_strategy.r
# Schnelle In-und-Out Trades basierend auf Mikrostrukturen
# Orderbook-Analyse, Spread-Trading, Momentum-Scalping
# ==========================================================================================================

cat("⚡ Loading High-Frequency Scalping Strategy V1...\n")

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

if (!exists("RISK_MANAGER_LOADED")) {
  source("../trading/risk_manager.r")
  RISK_MANAGER_LOADED <- TRUE
}

# ==========================================================================================================
# ⚙️ SCALPING CONFIGURATION
# ==========================================================================================================

SCALPING_CONFIG <- list(
  # Trade parameters
  trade_params = list(
    min_profit_ticks = 3,         # Minimum 3 ticks profit target
    max_loss_ticks = 2,           # Maximum 2 ticks stop loss
    position_hold_seconds = 300,   # Maximum 5 minutes hold time
    quick_exit_seconds = 60,       # Quick exit after 1 minute if no profit
    leverage = 10                  # Fixed leverage for scalping
  ),
  
  # Entry conditions
  entry_conditions = list(
    max_spread_percent = 0.05,     # Maximum 0.05% spread
    min_volume_ratio = 1.2,        # 20% above average volume
    min_order_book_imbalance = 0.6, # 60% order book imbalance
    momentum_threshold = 0.1,       # 0.1% momentum in 1 minute
    volatility_range = c(0.1, 0.5) # Volatility between 10-50% annualized
  ),
  
  # Risk management
  risk_params = list(
    max_positions = 3,             # Maximum 3 concurrent scalps
    position_size_pct = 5,         # 5% of capital per scalp
    daily_loss_limit_pct = 2,      # 2% daily loss limit
    win_rate_threshold = 0.55,     # Minimum 55% win rate to continue
    consecutive_losses_stop = 3     # Stop after 3 consecutive losses
  ),
  
  # Market microstructure
  microstructure = list(
    tick_window = 20,              # Analyze last 20 ticks
    orderbook_depth = 10,          # Analyze 10 levels of orderbook
    trade_flow_window = 100,       # Last 100 trades for flow analysis
    refresh_rate_ms = 100          # Update every 100ms
  ),
  
  # Signals and weights
  signal_weights = list(
    orderbook_pressure = 0.30,
    trade_flow = 0.25,
    momentum = 0.20,
    spread_quality = 0.15,
    volatility = 0.10
  )
)

# ==========================================================================================================
# 📊 MARKET MICROSTRUCTURE ANALYSIS
# ==========================================================================================================

#' Analyze orderbook microstructure
analyze_orderbook_microstructure <- function(symbol) {
  tryCatch({
    # Get orderbook data
    orderbook <- get_enhanced_orderbook(symbol, SCALPING_CONFIG$microstructure$orderbook_depth)
    
    if (is.null(orderbook)) return(NULL)
    
    # Calculate bid-ask imbalance
    bid_volume <- sum(orderbook$bids$size)
    ask_volume <- sum(orderbook$asks$size)
    total_volume <- bid_volume + ask_volume
    
    imbalance <- if (total_volume > 0) {
      (bid_volume - ask_volume) / total_volume
    } else {
      0
    }
    
    # Calculate weighted mid price
    weighted_mid <- calculate_weighted_mid(orderbook)
    
    # Analyze depth
    depth_analysis <- analyze_orderbook_depth(orderbook)
    
    # Detect walls
    walls <- detect_orderbook_walls(orderbook)
    
    # Calculate pressure score
    pressure_score <- calculate_orderbook_pressure(
      imbalance, depth_analysis, walls
    )
    
    return(list(
      imbalance = imbalance,
      weighted_mid = weighted_mid,
      spread = orderbook$spread,
      spread_pct = orderbook$spread_pct,
      depth_ratio = depth_analysis$depth_ratio,
      bid_wall = walls$bid_wall,
      ask_wall = walls$ask_wall,
      pressure_score = pressure_score,
      pressure_direction = if (pressure_score > 0.6) "BUY" 
                          else if (pressure_score < 0.4) "SELL" 
                          else "NEUTRAL"
    ))
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Calculate weighted mid price
calculate_weighted_mid <- function(orderbook) {
  
  # Weight by inverse distance from best bid/ask
  bid_weights <- 1 / (1:nrow(orderbook$bids))
  ask_weights <- 1 / (1:nrow(orderbook$asks))
  
  # Normalize weights
  bid_weights <- bid_weights / sum(bid_weights)
  ask_weights <- ask_weights / sum(ask_weights)
  
  # Calculate weighted prices
  weighted_bid <- sum(orderbook$bids$price * orderbook$bids$size * bid_weights) / 
                  sum(orderbook$bids$size * bid_weights)
  
  weighted_ask <- sum(orderbook$asks$price * orderbook$asks$size * ask_weights) / 
                  sum(orderbook$asks$size * ask_weights)
  
  return((weighted_bid + weighted_ask) / 2)
}

#' Analyze orderbook depth
analyze_orderbook_depth <- function(orderbook) {
  
  # Calculate cumulative volumes
  bid_cumsum <- cumsum(orderbook$bids$size)
  ask_cumsum <- cumsum(orderbook$asks$size)
  
  # Find depth at different price levels
  price_levels <- c(0.1, 0.2, 0.5)  # 0.1%, 0.2%, 0.5% from mid
  mid_price <- (orderbook$bids$price[1] + orderbook$asks$price[1]) / 2
  
  depth_at_levels <- list()
  
  for (level in price_levels) {
    bid_price_level <- mid_price * (1 - level / 100)
    ask_price_level <- mid_price * (1 + level / 100)
    
    bid_depth <- sum(orderbook$bids$size[orderbook$bids$price >= bid_price_level])
    ask_depth <- sum(orderbook$asks$size[orderbook$asks$price <= ask_price_level])
    
    depth_at_levels[[paste0(level, "%")]] <- list(
      bid = bid_depth,
      ask = ask_depth,
      ratio = bid_depth / (ask_depth + 1)
    )
  }
  
  return(list(
    total_bid_depth = sum(orderbook$bids$size),
    total_ask_depth = sum(orderbook$asks$size),
    depth_ratio = sum(orderbook$bids$size) / sum(orderbook$asks$size),
    depth_at_levels = depth_at_levels
  ))
}

#' Detect orderbook walls
detect_orderbook_walls <- function(orderbook) {
  
  # Wall detection threshold (3x average size)
  avg_bid_size <- mean(orderbook$bids$size)
  avg_ask_size <- mean(orderbook$asks$size)
  
  bid_wall <- NULL
  ask_wall <- NULL
  
  # Find bid walls
  bid_walls <- orderbook$bids[orderbook$bids$size > avg_bid_size * 3, ]
  if (nrow(bid_walls) > 0) {
    bid_wall <- bid_walls[1, ]  # Closest wall
  }
  
  # Find ask walls
  ask_walls <- orderbook$asks[orderbook$asks$size > avg_ask_size * 3, ]
  if (nrow(ask_walls) > 0) {
    ask_wall <- ask_walls[1, ]  # Closest wall
  }
  
  return(list(
    bid_wall = bid_wall,
    ask_wall = ask_wall,
    has_bid_support = !is.null(bid_wall),
    has_ask_resistance = !is.null(ask_wall)
  ))
}

#' Calculate orderbook pressure score
calculate_orderbook_pressure <- function(imbalance, depth_analysis, walls) {
  
  # Base score from imbalance
  base_score = (imbalance + 1) / 2  # Normalize to 0-1
  
  # Adjust for depth ratio
  depth_adjustment <- (depth_analysis$depth_ratio - 1) / (depth_analysis$depth_ratio + 1) * 0.2
  
  # Wall adjustments
  wall_adjustment <- 0
  if (walls$has_bid_support) wall_adjustment <- wall_adjustment + 0.1
  if (walls$has_ask_resistance) wall_adjustment <- wall_adjustment - 0.1
  
  # Final score
  pressure_score <- base_score + depth_adjustment + wall_adjustment
  
  return(max(0, min(1, pressure_score)))
}

# ==========================================================================================================
# 📈 TRADE FLOW ANALYSIS
# ==========================================================================================================

#' Analyze recent trade flow
analyze_trade_flow <- function(symbol) {
  tryCatch({
    # Get recent trades
    trades <- get_recent_trades(symbol, SCALPING_CONFIG$microstructure$trade_flow_window)
    
    if (is.null(trades) || nrow(trades) == 0) return(NULL)
    
    # Separate buy/sell trades
    buy_trades <- trades[trades$side == "buy", ]
    sell_trades <- trades[trades$side == "sell", ]
    
    # Volume analysis
    buy_volume <- sum(buy_trades$size * buy_trades$price)
    sell_volume <- sum(sell_trades$size * sell_trades$price)
    net_flow <- buy_volume - sell_volume
    
    # Trade size analysis
    avg_buy_size <- mean(buy_trades$size)
    avg_sell_size <- mean(sell_trades$size)
    
    # Large trade detection
    large_threshold <- quantile(trades$size, 0.9)
    large_buys <- sum(buy_trades$size > large_threshold)
    large_sells <- sum(sell_trades$size > large_threshold)
    
    # Trade intensity (trades per minute)
    time_span <- as.numeric(difftime(
      max(trades$timestamp), 
      min(trades$timestamp), 
      units = "mins"
    ))
    trade_intensity <- nrow(trades) / max(time_span, 1)
    
    # Calculate flow score
    flow_score <- calculate_trade_flow_score(
      buy_volume, sell_volume, large_buys, large_sells, trade_intensity
    )
    
    return(list(
      buy_volume = buy_volume,
      sell_volume = sell_volume,
      net_flow = net_flow,
      flow_ratio = buy_volume / (sell_volume + 1),
      avg_buy_size = avg_buy_size,
      avg_sell_size = avg_sell_size,
      large_buy_count = large_buys,
      large_sell_count = large_sells,
      trade_intensity = trade_intensity,
      flow_score = flow_score,
      flow_direction = if (flow_score > 0.6) "BULLISH"
                      else if (flow_score < 0.4) "BEARISH"
                      else "NEUTRAL"
    ))
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Calculate trade flow score
calculate_trade_flow_score <- function(buy_vol, sell_vol, large_buys, large_sells, intensity) {
  
  # Volume score
  total_vol <- buy_vol + sell_vol
  volume_score <- if (total_vol > 0) buy_vol / total_vol else 0.5
  
  # Large trade score
  large_total <- large_buys + large_sells
  large_score <- if (large_total > 0) large_buys / large_total else 0.5
  
  # Intensity adjustment
  intensity_mult <- min(intensity / 10, 1.5)  # Cap at 1.5x
  
  # Weighted score
  flow_score <- (volume_score * 0.6 + large_score * 0.4) * intensity_mult
  
  return(max(0, min(1, flow_score)))
}

# ==========================================================================================================
# ⚡ MOMENTUM DETECTION
# ==========================================================================================================

#' Detect micro momentum
detect_micro_momentum <- function(symbol) {
  tryCatch({
    # Get recent ticks
    ticks <- get_recent_ticks(symbol, SCALPING_CONFIG$microstructure$tick_window)
    
    if (is.null(ticks) || length(ticks) < 5) return(NULL)
    
    # Calculate momentum over different windows
    momentum_1m <- calculate_tick_momentum(ticks, 60)    # 1 minute
    momentum_30s <- calculate_tick_momentum(ticks, 30)   # 30 seconds
    momentum_10s <- calculate_tick_momentum(ticks, 10)   # 10 seconds
    
    # Acceleration
    acceleration <- momentum_10s - momentum_30s
    
    # Volume-weighted momentum
    vw_momentum <- calculate_volume_weighted_momentum(ticks)
    
    # Momentum quality
    momentum_consistency <- calculate_momentum_consistency(ticks)
    
    # Generate momentum score
    momentum_score <- score_momentum_quality(
      momentum_1m, momentum_30s, momentum_10s, 
      acceleration, momentum_consistency
    )
    
    return(list(
      momentum_1m = momentum_1m,
      momentum_30s = momentum_30s,
      momentum_10s = momentum_10s,
      acceleration = acceleration,
      vw_momentum = vw_momentum,
      consistency = momentum_consistency,
      momentum_score = momentum_score,
      signal = categorize_momentum_signal(momentum_score, acceleration)
    ))
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Calculate tick momentum
calculate_tick_momentum <- function(ticks, window_seconds) {
  
  current_time <- Sys.time()
  window_start <- current_time - window_seconds
  
  # Filter ticks within window
  window_ticks <- ticks[ticks$timestamp >= window_start, ]
  
  if (nrow(window_ticks) < 2) return(0)
  
  # Calculate return
  start_price <- window_ticks$price[1]
  end_price <- tail(window_ticks$price, 1)
  
  momentum <- (end_price - start_price) / start_price * 100
  
  return(momentum)
}

#' Score momentum quality
score_momentum_quality <- function(m1m, m30s, m10s, accel, consistency) {
  
  # Base score from 30s momentum
  base_score <- 0.5 + (m30s / 0.2)  # 0.1% = 0.5 score change
  
  # Acceleration bonus
  if (sign(accel) == sign(m30s) && abs(accel) > 0.02) {
    base_score <- base_score + 0.1
  }
  
  # Consistency bonus
  base_score <- base_score * (0.8 + consistency * 0.4)
  
  # Extreme momentum adjustment
  if (abs(m10s) > 0.15) {  # Very fast move
    base_score <- base_score * 1.2
  }
  
  return(max(0, min(1, base_score)))
}

# ==========================================================================================================
# 🎯 SCALPING SIGNAL GENERATION
# ==========================================================================================================

#' Generate scalping signals
generate_scalping_signals <- function(symbols = NULL) {
  
  if (is.null(symbols)) {
    symbols <- PORTFOLIO_ASSETS
  }
  
  cat("\n⚡ === SCALPING SIGNAL SCAN === ⚡\n")
  cat("Time:", format(Sys.time(), "%H:%M:%S"), "\n\n")
  
  signals <- list()
  
  for (symbol in symbols) {
    # Quick pre-check
    if (!check_scalping_viability(symbol)) {
      next
    }
    
    # Analyze components
    orderbook <- analyze_orderbook_microstructure(symbol)
    trade_flow <- analyze_trade_flow(symbol)
    momentum <- detect_micro_momentum(symbol)
    
    # Skip if any component fails
    if (is.null(orderbook) || is.null(trade_flow) || is.null(momentum)) {
      next
    }
    
    # Calculate composite signal
    signal <- calculate_scalping_signal(orderbook, trade_flow, momentum)
    
    if (signal$strength > 0.7) {
      signals[[symbol]] <- signal
      display_scalping_signal(symbol, signal)
    }
  }
  
  return(signals)
}

#' Check if asset is suitable for scalping
check_scalping_viability <- function(symbol) {
  
  # Get ticker
  ticker <- get_ticker(symbol)
  if (is.null(ticker)) return(FALSE)
  
  # Check spread
  spread_pct <- (ticker$ask - ticker$bid) / ticker$bid * 100
  if (spread_pct > SCALPING_CONFIG$entry_conditions$max_spread_percent) {
    return(FALSE)
  }
  
  # Check volume
  if (ticker$volume_24h_usdt < 1000000) {  # $1M minimum
    return(FALSE)
  }
  
  return(TRUE)
}

#' Calculate composite scalping signal
calculate_scalping_signal <- function(orderbook, trade_flow, momentum) {
  
  weights <- SCALPING_CONFIG$signal_weights
  
  # Component scores
  scores <- list(
    orderbook = orderbook$pressure_score,
    flow = trade_flow$flow_score,
    momentum = momentum$momentum_score,
    spread = 1 - (orderbook$spread_pct / SCALPING_CONFIG$entry_conditions$max_spread_percent),
    volatility = score_volatility_for_scalping(orderbook$weighted_mid)
  )
  
  # Weighted composite
  composite_score <- sum(
    scores$orderbook * weights$orderbook_pressure,
    scores$flow * weights$trade_flow,
    scores$momentum * weights$momentum,
    scores$spread * weights$spread_quality,
    scores$volatility * weights$volatility
  )
  
  # Determine direction
  direction_scores <- c(
    orderbook = if (orderbook$pressure_direction == "BUY") 1 
               else if (orderbook$pressure_direction == "SELL") -1 
               else 0,
    flow = if (trade_flow$flow_direction == "BULLISH") 1 
          else if (trade_flow$flow_direction == "BEARISH") -1 
          else 0,
    momentum = if (momentum$signal == "BULLISH") 1 
              else if (momentum$signal == "BEARISH") -1 
              else 0
  )
  
  net_direction <- sum(direction_scores)
  
  signal_direction <- if (net_direction >= 2) "LONG"
                     else if (net_direction <= -2) "SHORT"
                     else "NEUTRAL"
  
  # Entry price calculation
  entry_price <- calculate_scalp_entry_price(
    orderbook, signal_direction
  )
  
  # Target and stop calculation
  tick_size <- get_tick_size(orderbook$weighted_mid)
  
  targets <- calculate_scalp_targets(
    entry_price, signal_direction, tick_size
  )
  
  return(list(
    timestamp = Sys.time(),
    direction = signal_direction,
    strength = composite_score,
    components = scores,
    entry_price = entry_price,
    target_price = targets$target,
    stop_price = targets$stop,
    expected_profit = targets$expected_profit,
    risk_reward = targets$risk_reward,
    confidence = calculate_signal_confidence(scores, direction_scores)
  ))
}

#' Calculate scalp entry price
calculate_scalp_entry_price <- function(orderbook, direction) {
  
  if (direction == "LONG") {
    # Enter at or slightly above best ask
    entry <- orderbook$asks$price[1]
    
    # If strong signal, can pay up one tick
    if (orderbook$pressure_score > 0.8) {
      tick_size <- get_tick_size(entry)
      entry <- entry + tick_size
    }
    
  } else if (direction == "SHORT") {
    # Enter at or slightly below best bid
    entry <- orderbook$bids$price[1]
    
    # If strong signal, can sell down one tick
    if (orderbook$pressure_score < 0.2) {
      tick_size <- get_tick_size(entry)
      entry <- entry - tick_size
    }
    
  } else {
    # Neutral - use mid price
    entry <- (orderbook$bids$price[1] + orderbook$asks$price[1]) / 2
  }
  
  return(entry)
}

#' Calculate scalp targets
calculate_scalp_targets <- function(entry_price, direction, tick_size) {
  
  profit_ticks <- SCALPING_CONFIG$trade_params$min_profit_ticks
  loss_ticks <- SCALPING_CONFIG$trade_params$max_loss_ticks
  
  if (direction == "LONG") {
    target <- entry_price + (tick_size * profit_ticks)
    stop <- entry_price - (tick_size * loss_ticks)
  } else {
    target <- entry_price - (tick_size * profit_ticks)
    stop <- entry_price + (tick_size * loss_ticks)
  }
  
  expected_profit <- abs(target - entry_price) / entry_price * 100
  risk_reward <- profit_ticks / loss_ticks
  
  return(list(
    target = target,
    stop = stop,
    expected_profit = expected_profit,
    risk_reward = risk_reward
  ))
}

# ==========================================================================================================
# 🎯 SCALPING EXECUTION
# ==========================================================================================================

#' Execute scalping trade
execute_scalping_trade <- function(symbol, signal, dry_run = TRUE) {
  
  cat("\n⚡ === SCALPING EXECUTION === ⚡\n")
  cat("Symbol:", symbol, "\n")
  cat("Direction:", signal$direction, "\n")
  cat("Signal Strength:", round(signal$strength, 3), "\n")
  
  # Pre-trade checks
  if (!validate_scalping_conditions(symbol, signal)) {
    cat("❌ Validation failed\n")
    return(list(executed = FALSE, reason = "Validation failed"))
  }
  
  # Check daily performance
  daily_check <- check_daily_scalping_performance()
  if (!daily_check$can_trade) {
    cat("🛑", daily_check$reason, "\n")
    return(list(executed = FALSE, reason = daily_check$reason))
  }
  
  # Position sizing
  position_size <- calculate_scalping_position_size(symbol, signal)
  
  cat("\n📊 Trade Details:\n")
  cat("├─ Entry:", signal$entry_price, "\n")
  cat("├─ Target:", signal$target_price, 
      "(", round(signal$expected_profit, 2), "%)\n")
  cat("├─ Stop:", signal$stop_price, "\n")
  cat("├─ Size:", position_size, "\n")
  cat("└─ Risk/Reward:", round(signal$risk_reward, 2), ":1\n")
  
  if (dry_run) {
    cat("\n🔍 DRY RUN - No trade executed\n")
    return(list(
      executed = FALSE,
      mode = "dry_run",
      details = list(
        symbol = symbol,
        direction = signal$direction,
        entry = signal$entry_price,
        target = signal$target_price,
        stop = signal$stop_price,
        size = position_size
      )
    ))
  }
  
  # Execute trade
  trade_id <- paste0("SCALP_", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  # Place entry order
  entry_result <- place_scalping_entry(
    symbol, signal$direction, position_size, signal$entry_price
  )
  
  if (!entry_result$success) {
    cat("❌ Entry order failed\n")
    return(list(executed = FALSE, reason = "Entry failed"))
  }
  
  # Store scalp metadata
  ACTIVE_SCALPS[[symbol]] <- list(
    trade_id = trade_id,
    signal = signal,
    entry_time = Sys.time(),
    entry_order_id = entry_result$order_id,
    position_size = position_size,
    status = "ENTERING",
    target_order_id = NULL,
    stop_order_id = NULL
  )
  
  # Start position monitoring
  start_scalp_monitoring(symbol)
  
  cat("✅ Scalp trade initiated\n")
  
  return(list(
    executed = TRUE,
    trade_id = trade_id,
    order_id = entry_result$order_id
  ))
}

#' Validate scalping conditions
validate_scalping_conditions <- function(symbol, signal) {
  
  # Check if already in position
  positions <- get_current_positions()
  if (symbol %in% positions$symbol) {
    cat("Already in position\n")
    return(FALSE)
  }
  
  # Check active scalps
  if (length(ACTIVE_SCALPS) >= SCALPING_CONFIG$risk_params$max_positions) {
    cat("Max scalping positions reached\n")
    return(FALSE)
  }
  
  # Re-verify spread
  ticker <- get_ticker(symbol)
  current_spread <- (ticker$ask - ticker$bid) / ticker$bid * 100
  
  if (current_spread > SCALPING_CONFIG$entry_conditions$max_spread_percent) {
    cat("Spread too wide:", round(current_spread, 3), "%\n")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Calculate scalping position size
calculate_scalping_position_size <- function(symbol, signal) {
  
  balance <- get_account_balance()
  
  # Base size
  risk_capital <- balance$available * (SCALPING_CONFIG$risk_params$position_size_pct / 100)
  
  # Adjust for confidence
  confidence_mult <- 0.5 + (signal$confidence * 0.5)  # 50-100% of base
  
  # Adjust for daily performance
  daily_stats <- get_daily_scalping_stats()
  performance_mult <- if (daily_stats$win_rate > 0.7) 1.2
                     else if (daily_stats$win_rate < 0.4) 0.5
                     else 1.0
  
  # Final position value
  position_value <- risk_capital * confidence_mult * performance_mult
  
  # Convert to contracts
  contract_size = position_value * SCALPING_CONFIG$trade_params$leverage / signal$entry_price
  
  # Apply contract specifications
  specs <- get_contract_specs(symbol)
  if (!is.null(specs)) {
    contract_size <- floor(contract_size / specs$min_size) * specs$min_size
  }
  
  return(contract_size)
}

# ==========================================================================================================
# 🔍 SCALP MONITORING
# ==========================================================================================================

#' Monitor active scalps
monitor_active_scalps <- function() {
  
  if (length(ACTIVE_SCALPS) == 0) return()
  
  for (symbol in names(ACTIVE_SCALPS)) {
    scalp <- ACTIVE_SCALPS[[symbol]]
    
    # Check status and age
    age_seconds <- as.numeric(difftime(Sys.time(), scalp$entry_time, units = "secs"))
    
    # Get current price
    ticker <- get_ticker(symbol)
    current_price <- as.numeric(ticker$last)
    
    # Calculate P&L
    if (scalp$signal$direction == "LONG") {
      pnl_pct <- (current_price - scalp$signal$entry_price) / scalp$signal$entry_price * 100
    } else {
      pnl_pct <- (scalp$signal$entry_price - current_price) / scalp$signal$entry_price * 100
    }
    
    # Check exit conditions
    exit_reason <- check_scalp_exit_conditions(
      scalp, current_price, pnl_pct, age_seconds
    )
    
    if (!is.null(exit_reason)) {
      cat("\n⚡ Exiting", symbol, "scalp:", exit_reason, "\n")
      execute_scalp_exit(symbol, exit_reason)
    } else {
      # Update trailing stop if profitable
      if (pnl_pct > 0.05) {  # 5 basis points profit
        update_scalp_trailing_stop(symbol, current_price)
      }
    }
  }
}

#' Check scalp exit conditions
check_scalp_exit_conditions <- function(scalp, current_price, pnl_pct, age_seconds) {
  
  # Target hit
  if (scalp$signal$direction == "LONG" && current_price >= scalp$signal$target_price) {
    return("TARGET_HIT")
  } else if (scalp$signal$direction == "SHORT" && current_price <= scalp$signal$target_price) {
    return("TARGET_HIT")
  }
  
  # Stop hit
  if (scalp$signal$direction == "LONG" && current_price <= scalp$signal$stop_price) {
    return("STOP_HIT")
  } else if (scalp$signal$direction == "SHORT" && current_price >= scalp$signal$stop_price) {
    return("STOP_HIT")
  }
  
  # Time exit
  if (age_seconds > SCALPING_CONFIG$trade_params$position_hold_seconds) {
    return("TIME_EXIT")
  }
  
  # Quick exit if no profit
  if (age_seconds > SCALPING_CONFIG$trade_params$quick_exit_seconds && pnl_pct < 0.02) {
    return("QUICK_EXIT")
  }
  
  # Momentum reversal
  momentum <- detect_micro_momentum(scalp$signal$symbol)
  if (!is.null(momentum)) {
    if (scalp$signal$direction == "LONG" && momentum$signal == "BEARISH") {
      return("MOMENTUM_REVERSAL")
    } else if (scalp$signal$direction == "SHORT" && momentum$signal == "BULLISH") {
      return("MOMENTUM_REVERSAL")
    }
  }
  
  return(NULL)
}

#' Execute scalp exit
execute_scalp_exit <- function(symbol, reason) {
  
  scalp <- ACTIVE_SCALPS[[symbol]]
  if (is.null(scalp)) return()
  
  # Close position
  close_result <- smart_position_exit(symbol, "full")
  
  if (close_result) {
    # Log result
    log_scalp_result(scalp, reason)
    
    # Remove from active scalps
    ACTIVE_SCALPS[[symbol]] <- NULL
    
    # Update daily stats
    update_daily_scalping_stats()
  }
}

# ==========================================================================================================
# 📊 PERFORMANCE TRACKING
# ==========================================================================================================

#' Check daily scalping performance
check_daily_scalping_performance <- function() {
  
  stats <- get_daily_scalping_stats()
  
  # Check daily loss limit
  if (stats$total_pnl < -(SCALPING_CONFIG$risk_params$daily_loss_limit_pct / 100) * get_account_balance()$equity) {
    return(list(can_trade = FALSE, reason = "Daily loss limit reached"))
  }
  
  # Check consecutive losses
  if (stats$consecutive_losses >= SCALPING_CONFIG$risk_params$consecutive_losses_stop) {
    return(list(can_trade = FALSE, reason = "Too many consecutive losses"))
  }
  
  # Check win rate (after minimum trades)
  if (stats$total_trades > 10 && stats$win_rate < SCALPING_CONFIG$risk_params$win_rate_threshold) {
    return(list(can_trade = FALSE, reason = "Win rate too low"))
  }
  
  return(list(can_trade = TRUE))
}

#' Get daily scalping statistics
get_daily_scalping_stats <- function() {
  
  if (!exists("SCALPING_HISTORY") || length(SCALPING_HISTORY) == 0) {
    return(list(
      total_trades = 0,
      winning_trades = 0,
      losing_trades = 0,
      win_rate = 0,
      total_pnl = 0,
      consecutive_losses = 0
    ))
  }
  
  # Filter today's trades
  today_start <- as.POSIXct(format(Sys.Date(), "%Y-%m-%d 00:00:00"))
  today_trades <- Filter(function(x) x$exit_time >= today_start, SCALPING_HISTORY)
  
  if (length(today_trades) == 0) {
    return(list(
      total_trades = 0,
      winning_trades = 0,
      losing_trades = 0,
      win_rate = 0,
      total_pnl = 0,
      consecutive_losses = 0
    ))
  }
  
  # Calculate stats
  pnls <- sapply(today_trades, function(x) x$pnl)
  
  stats <- list(
    total_trades = length(today_trades),
    winning_trades = sum(pnls > 0),
    losing_trades = sum(pnls < 0),
    win_rate = sum(pnls > 0) / length(pnls),
    total_pnl = sum(pnls),
    consecutive_losses = calculate_current_loss_streak(today_trades)
  )
  
  return(stats)
}

#' Log scalp result
log_scalp_result <- function(scalp, exit_reason) {
  
  # Get final P&L
  position <- get_position_details(scalp$signal$symbol)
  
  result <- list(
    trade_id = scalp$trade_id,
    symbol = scalp$signal$symbol,
    direction = scalp$signal$direction,
    entry_time = scalp$entry_time,
    exit_time = Sys.time(),
    entry_price = scalp$signal$entry_price,
    exit_price = position$mark_price,
    size = scalp$position_size,
    pnl = position$unrealized_pnl,
    pnl_pct = position$pnl_ratio * 100,
    exit_reason = exit_reason,
    hold_seconds = as.numeric(difftime(Sys.time(), scalp$entry_time, units = "secs"))
  )
  
  # Add to history
  if (!exists("SCALPING_HISTORY")) {
    SCALPING_HISTORY <<- list()
  }
  
  SCALPING_HISTORY[[length(SCALPING_HISTORY) + 1]] <<- result
  
  # Maintain history limit
  if (length(SCALPING_HISTORY) > 1000) {
    SCALPING_HISTORY <<- tail(SCALPING_HISTORY, 1000)
  }
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

#' Get recent price ticks
get_recent_ticks <- function(symbol, n_ticks = 20) {
  # In production, this would maintain a tick buffer
  # Simplified version using recent trades
  
  trades <- get_recent_trades(symbol, n_ticks)
  
  if (is.null(trades)) return(NULL)
  
  ticks <- data.frame(
    timestamp = trades$timestamp,
    price = trades$price,
    size = trades$size,
    side = trades$side
  )
  
  return(ticks)
}

#' Get tick size for a price level
get_tick_size <- function(price) {
  # Simplified tick size calculation
  if (price < 1) return(0.0001)
  else if (price < 10) return(0.001)
  else if (price < 100) return(0.01)
  else if (price < 1000) return(0.1)
  else return(1)
}

#' Score volatility for scalping
score_volatility_for_scalping <- function(price) {
  # Placeholder - would calculate actual volatility
  # For now, return neutral score
  return(0.5)
}

#' Categorize momentum signal
categorize_momentum_signal <- function(score, acceleration) {
  if (score > 0.7 && acceleration > 0) return("BULLISH")
  else if (score < 0.3 && acceleration < 0) return("BEARISH")
  else return("NEUTRAL")
}

#' Calculate signal confidence
calculate_signal_confidence <- function(scores, direction_scores) {
  
  # Average component scores
  avg_score <- mean(unlist(scores))
  
  # Direction agreement
  direction_agreement <- abs(sum(direction_scores)) / 3
  
  # Combined confidence
  confidence <- avg_score * 0.6 + direction_agreement * 0.4
  
  return(confidence)
}

#' Display scalping signal
display_scalping_signal <- function(symbol, signal) {
  
  cat("⚡", symbol, "-", signal$direction, 
      "Strength:", round(signal$strength, 2),
      "| Entry:", signal$entry_price,
      "| Target: +", round(signal$expected_profit, 2), "%\n")
}

#' Place scalping entry order
place_scalping_entry <- function(symbol, direction, size, price) {
  
  side <- if (direction == "LONG") "buy" else "sell"
  
  # Use IOC (Immediate or Cancel) for scalping
  result <- place_order(
    symbol = symbol,
    side = side,
    size = size,
    order_type = "limit",
    price = price,
    time_in_force = "IOC"
  )
  
  return(result)
}

#' Start scalp monitoring loop
start_scalp_monitoring <- function(symbol) {
  # In production, would start a separate monitoring thread
  # For now, rely on periodic calls to monitor_active_scalps()
}

#' Calculate volume-weighted momentum
calculate_volume_weighted_momentum <- function(ticks) {
  if (nrow(ticks) < 2) return(0)
  
  # Weight by volume
  weights <- ticks$size / sum(ticks$size)
  
  # Calculate weighted price change
  price_changes <- diff(ticks$price)
  weighted_changes <- price_changes * weights[-1]
  
  vw_momentum <- sum(weighted_changes) / ticks$price[1] * 100
  
  return(vw_momentum)
}

#' Calculate momentum consistency
calculate_momentum_consistency <- function(ticks) {
  if (nrow(ticks) < 5) return(0)
  
  # Check if price moves are in same direction
  price_changes <- diff(ticks$price)
  
  # Count consistent moves
  positive_moves <- sum(price_changes > 0)
  negative_moves <- sum(price_changes < 0)
  
  consistency <- max(positive_moves, negative_moves) / length(price_changes)
  
  return(consistency)
}

#' Calculate current loss streak
calculate_current_loss_streak <- function(trades) {
  if (length(trades) == 0) return(0)
  
  # Sort by time
  sorted_trades <- trades[order(sapply(trades, function(x) x$exit_time), decreasing = TRUE)]
  
  streak <- 0
  for (trade in sorted_trades) {
    if (trade$pnl < 0) {
      streak <- streak + 1
    } else {
      break
    }
  }
  
  return(streak)
}

#' Update scalp trailing stop
update_scalp_trailing_stop <- function(symbol, current_price) {
  scalp <- ACTIVE_SCALPS[[symbol]]
  if (is.null(scalp)) return()
  
  # Simple trailing logic
  tick_size <- get_tick_size(current_price)
  
  if (scalp$signal$direction == "LONG") {
    new_stop <- current_price - (tick_size * SCALPING_CONFIG$trade_params$max_loss_ticks)
    if (new_stop > scalp$signal$stop_price) {
      scalp$signal$stop_price <- new_stop
      ACTIVE_SCALPS[[symbol]] <- scalp
    }
  } else {
    new_stop <- current_price + (tick_size * SCALPING_CONFIG$trade_params$max_loss_ticks)
    if (new_stop < scalp$signal$stop_price) {
      scalp$signal$stop_price <- new_stop
      ACTIVE_SCALPS[[symbol]] <- scalp
    }
  }
}

#' Update daily scalping statistics  
update_daily_scalping_stats <- function() {
  # Trigger recalculation on next call
  # In production, would update a cached version
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Initialize scalping state
ACTIVE_SCALPS <- list()
SCALPING_HISTORY <- list()

# Scalping strategy interface
SCALPING_STRATEGY <- list(
  scan = generate_scalping_signals,
  execute = execute_scalping_trade,
  monitor = monitor_active_scalps,
  get_active = function() ACTIVE_SCALPS,
  get_stats = get_daily_scalping_stats,
  get_config = function() SCALPING_CONFIG
)

cat("✅ SCALPING_STRATEGY.R loaded successfully!\n")
cat("⚡ High-frequency scalping engine ready\n")
cat("📊 Orderbook analysis enabled\n")
cat("🎯 Micro-momentum detection active\n")
cat("🔍 Real-time monitoring initialized\n")