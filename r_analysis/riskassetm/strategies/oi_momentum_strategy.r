# ==========================================================================================================
# 📈 OI MOMENTUM TRADING STRATEGY V1
# ==========================================================================================================
# Pfad: strategies/oi_momentum_strategy.r
# Open Interest basierte Momentum-Trading-Strategie
# Nutzt institutionelle OI-Flows für Entry/Exit Signale
# ==========================================================================================================

cat("📈 Loading OI Momentum Trading Strategy V1...\n")

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
# 📊 STRATEGY CONFIGURATION
# ==========================================================================================================

OI_MOMENTUM_CONFIG <- list(
  # Entry thresholds
  min_oi_change = 10,              # Minimum 10% OI change for signal
  min_institutional_score = 0.6,    # Minimum institutional activity score
  min_volume_ratio = 1.5,          # Volume must be 1.5x average
  
  # Position sizing
  base_position_pct = 2,           # 2% of capital per position
  max_position_pct = 5,            # Maximum 5% per position
  leverage_range = c(5, 20),       # Dynamic leverage 5x-20x
  
  # Risk management
  default_sl_pct = 1.5,            # Default 1.5% stop loss
  default_tp_pct = 3,              # Default 3% take profit
  trailing_activation = 2,         # Activate trailing at 2% profit
  trailing_distance = 1,           # Trail by 1%
  
  # Timing
  min_hold_minutes = 30,           # Minimum position hold time
  max_hold_hours = 24,             # Maximum position hold time
  
  # Market conditions
  avoid_news_minutes = 30,         # Avoid trading 30min around news
  min_liquidity_usdt = 1000000,    # $1M minimum daily volume
  
  # Signal weights
  weights = list(
    oi_flow = 0.35,
    institutional = 0.25,
    momentum = 0.20,
    volume = 0.10,
    structure = 0.10
  )
)

# ==========================================================================================================
# 🎯 SIGNAL GENERATION
# ==========================================================================================================

#' Generate OI momentum signals
generate_oi_momentum_signals <- function(symbols = NULL) {
  
  if (is.null(symbols)) {
    symbols <- PORTFOLIO_ASSETS
  }
  
  cat("\n🎯 === OI MOMENTUM SIGNAL GENERATION === 🎯\n")
  cat("Analyzing", length(symbols), "assets...\n\n")
  
  signals <- list()
  
  for (symbol in symbols) {
    cat("📊 Analyzing", symbol, "...")
    
    # Get OI analysis
    oi_analysis <- analyze_oi_momentum(symbol)
    
    # Get market structure
    market_structure <- analyze_market_structure(symbol)
    
    # Get volume profile
    volume_profile <- analyze_volume_profile(symbol)
    
    # Calculate composite signal
    signal <- calculate_composite_signal(
      oi_analysis, 
      market_structure, 
      volume_profile
    )
    
    signals[[symbol]] <- signal
    
    # Display signal
    display_signal_summary(symbol, signal)
    
    Sys.sleep(API_CONFIG$rate_limit_delay)
  }
  
  # Rank signals
  ranked_signals <- rank_trading_signals(signals)
  
  return(list(
    signals = signals,
    ranked = ranked_signals,
    timestamp = Sys.time()
  ))
}

#' Analyze OI momentum for a symbol
analyze_oi_momentum <- function(symbol) {
  tryCatch({
    # Get OI data
    oi_data <- get_oi_analysis(symbol)
    if (is.null(oi_data)) return(list(score = 0, signal = "NEUTRAL"))
    
    # Get historical OI
    oi_history <- get_oi_historical_data(symbol, "24h")
    if (is.null(oi_history)) return(list(score = 0, signal = "NEUTRAL"))
    
    # Calculate OI metrics
    current_oi <- as.numeric(tail(oi_history, 1)$amount)
    start_oi <- as.numeric(head(oi_history, 1)$amount)
    oi_change_pct <- ((current_oi - start_oi) / start_oi) * 100
    
    # Calculate OI velocity (rate of change)
    oi_velocity <- calculate_oi_velocity(oi_history)
    
    # Institutional activity score
    institutional_score <- calculate_institutional_activity(oi_history)
    
    # Determine signal
    signal <- "NEUTRAL"
    score <- 0.5
    
    if (oi_change_pct > OI_MOMENTUM_CONFIG$min_oi_change && 
        institutional_score > OI_MOMENTUM_CONFIG$min_institutional_score) {
      
      if (oi_velocity > 0) {
        signal <- "STRONG_BUY"
        score <- 0.8 + (min(oi_change_pct, 50) / 250)  # Max score 0.9
      } else {
        signal <- "BUY"
        score <- 0.65 + (min(oi_change_pct, 30) / 300)  # Max score 0.75
      }
      
    } else if (oi_change_pct < -OI_MOMENTUM_CONFIG$min_oi_change && 
               institutional_score > OI_MOMENTUM_CONFIG$min_institutional_score) {
      
      if (oi_velocity < 0) {
        signal <- "STRONG_SELL"
        score <- 0.2 - (min(abs(oi_change_pct), 50) / 250)  # Min score 0.1
      } else {
        signal <- "SELL"
        score <- 0.35 - (min(abs(oi_change_pct), 30) / 300)  # Min score 0.25
      }
    }
    
    return(list(
      score = score,
      signal = signal,
      oi_change_pct = oi_change_pct,
      oi_velocity = oi_velocity,
      institutional_score = institutional_score,
      metrics = list(
        current_oi = current_oi,
        oi_trend = if (oi_change_pct > 5) "INCREASING" else if (oi_change_pct < -5) "DECREASING" else "STABLE"
      )
    ))
    
  }, error = function(e) {
    cat(" ❌ Error:", e$message, "\n")
    return(list(score = 0.5, signal = "ERROR"))
  })
}

#' Analyze market structure
analyze_market_structure <- function(symbol) {
  tryCatch({
    # Get price data
    ticker <- get_ticker(symbol)
    if (is.null(ticker)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Get recent candles for structure analysis
    candles <- get_candles(symbol, "15m", 96)  # 24 hours of 15m candles
    if (is.null(candles) || nrow(candles) < 20) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Calculate structure metrics
    structure_analysis <- list(
      trend = calculate_trend_strength(candles),
      support_resistance = find_key_levels(candles),
      momentum = calculate_momentum_indicators(candles),
      volatility = calculate_volatility(candles)
    )
    
    # Score market structure
    structure_score <- score_market_structure(structure_analysis)
    
    return(list(
      score = structure_score,
      trend = structure_analysis$trend$direction,
      strength = structure_analysis$trend$strength,
      levels = structure_analysis$support_resistance,
      volatility = structure_analysis$volatility
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "NEUTRAL"))
  })
}

#' Analyze volume profile
analyze_volume_profile <- function(symbol) {
  tryCatch({
    # Get ticker for volume
    ticker <- get_ticker(symbol)
    if (is.null(ticker)) return(list(score = 0.5, profile = "NORMAL"))
    
    # Get asset config for typical volume
    config <- get_asset_config(symbol)
    typical_volume <- config$typical_volume_threshold %||% 1000000
    
    # Calculate volume metrics
    current_volume <- as.numeric(ticker$volume_24h_usdt)
    volume_ratio <- current_volume / typical_volume
    
    # Get recent trades for volume profile
    trades <- get_recent_trades(symbol, 100)
    
    # Analyze buy/sell volume ratio
    buy_sell_ratio <- if (!is.null(trades)) {
      analyze_trade_direction(trades)
    } else {
      0.5  # Neutral if no trade data
    }
    
    # Score volume
    volume_score <- 0.5
    profile <- "NORMAL"
    
    if (volume_ratio > OI_MOMENTUM_CONFIG$min_volume_ratio) {
      if (buy_sell_ratio > 0.6) {
        volume_score <- 0.7 + (min(volume_ratio - 1.5, 1) * 0.2)
        profile <- "BULLISH_SURGE"
      } else if (buy_sell_ratio < 0.4) {
        volume_score <- 0.3 - (min(volume_ratio - 1.5, 1) * 0.2)
        profile <- "BEARISH_SURGE"
      } else {
        volume_score <- 0.6
        profile <- "HIGH_ACTIVITY"
      }
    }
    
    return(list(
      score = volume_score,
      profile = profile,
      volume_ratio = volume_ratio,
      buy_sell_ratio = buy_sell_ratio,
      volume_24h = current_volume
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, profile = "ERROR"))
  })
}

# ==========================================================================================================
# 📊 SIGNAL CALCULATION
# ==========================================================================================================

#' Calculate composite trading signal
calculate_composite_signal <- function(oi_analysis, market_structure, volume_profile) {
  
  # Extract scores
  scores <- list(
    oi_flow = oi_analysis$score,
    institutional = oi_analysis$institutional_score,
    momentum = market_structure$score,
    volume = volume_profile$score,
    structure = if (market_structure$trend == oi_analysis$signal) 0.7 else 0.3
  )
  
  # Apply weights
  weights <- OI_MOMENTUM_CONFIG$weights
  weighted_score <- sum(
    scores$oi_flow * weights$oi_flow,
    scores$institutional * weights$institutional,
    scores$momentum * weights$momentum,
    scores$volume * weights$volume,
    scores$structure * weights$structure
  )
  
  # Determine signal strength
  signal_strength <- if (weighted_score > 0.75) "STRONG" 
                    else if (weighted_score > 0.65) "MODERATE"
                    else if (weighted_score < 0.25) "STRONG"
                    else if (weighted_score < 0.35) "MODERATE"
                    else "WEAK"
  
  # Determine direction
  direction <- if (weighted_score > 0.6) "BUY"
              else if (weighted_score < 0.4) "SELL"
              else "NEUTRAL"
  
  # Confidence calculation
  confidence <- calculate_signal_confidence(oi_analysis, market_structure, volume_profile)
  
  return(list(
    score = weighted_score,
    direction = direction,
    strength = signal_strength,
    confidence = confidence,
    components = scores,
    details = list(
      oi = oi_analysis,
      structure = market_structure,
      volume = volume_profile
    ),
    recommendation = generate_trade_recommendation(
      direction, signal_strength, confidence, oi_analysis, market_structure
    )
  ))
}

#' Calculate signal confidence
calculate_signal_confidence <- function(oi_analysis, market_structure, volume_profile) {
  
  confidence_factors <- c(
    # OI confidence
    if (abs(oi_analysis$oi_change_pct) > 20) 0.9 else 0.7,
    
    # Institutional confidence  
    oi_analysis$institutional_score,
    
    # Volume confidence
    if (volume_profile$volume_ratio > 2) 0.8 else 0.6,
    
    # Structure alignment
    if (market_structure$strength > 0.7) 0.8 else 0.5,
    
    # Volatility factor (lower vol = higher confidence)
    if (market_structure$volatility < 0.02) 0.8 else 0.6
  )
  
  return(mean(confidence_factors))
}

# ==========================================================================================================
# 🎯 TRADE EXECUTION
# ==========================================================================================================

#' Execute OI momentum trade
execute_oi_momentum_trade <- function(symbol, signal, dry_run = TRUE) {
  
  cat("\n🎯 === OI MOMENTUM TRADE EXECUTION === 🎯\n")
  cat("Symbol:", symbol, "\n")
  cat("Signal:", signal$direction, "-", signal$strength, "\n")
  cat("Confidence:", round(signal$confidence * 100, 1), "%\n")
  
  # Check if we should trade
  if (signal$direction == "NEUTRAL" || signal$confidence < 0.6) {
    cat("⚠️ Signal not strong enough to trade\n")
    return(list(executed = FALSE, reason = "Weak signal"))
  }
  
  # Risk check
  risk_check <- check_portfolio_risk()
  if (!risk_check$can_trade) {
    cat("🛡️ Risk limits prevent new trades\n")
    return(list(executed = FALSE, reason = risk_check$reason))
  }
  
  # Calculate position parameters
  position_params <- calculate_oi_position_params(symbol, signal)
  
  cat("\n📊 Position Parameters:\n")
  cat("├─ Size:", position_params$size, "\n")
  cat("├─ Leverage:", position_params$leverage, "x\n")
  cat("├─ Stop Loss:", round(position_params$sl_pct, 2), "%\n")
  cat("├─ Take Profit:", round(position_params$tp_pct, 2), "%\n")
  cat("└─ Risk Amount:", round(position_params$risk_amount, 2), "USDT\n")
  
  if (dry_run) {
    cat("\n🔍 DRY RUN - No actual trade executed\n")
    return(list(
      executed = FALSE,
      mode = "dry_run",
      would_execute = list(
        symbol = symbol,
        side = if (signal$direction == "BUY") "long" else "short",
        size = position_params$size,
        leverage = position_params$leverage,
        sl_pct = position_params$sl_pct,
        tp_pct = position_params$tp_pct
      )
    ))
  }
  
  # Execute trade
  trade_result <- smart_position_entry(
    symbol = symbol,
    side = if (signal$direction == "BUY") "long" else "short",
    size = position_params$size,
    tp_ratio = position_params$tp_pct,
    sl_ratio = position_params$sl_pct
  )
  
  if (trade_result) {
    # Set up advanced monitoring
    setup_oi_position_monitoring(symbol, signal, position_params)
    
    cat("✅ Trade executed successfully\n")
    return(list(
      executed = TRUE,
      symbol = symbol,
      params = position_params,
      signal = signal
    ))
  } else {
    cat("❌ Trade execution failed\n")
    return(list(executed = FALSE, reason = "Execution failed"))
  }
}

#' Calculate position parameters based on OI signal
calculate_oi_position_params <- function(symbol, signal) {
  
  # Get account info
  balance <- get_account_balance()
  available_balance <- balance$available
  
  # Base position size
  base_size_usdt <- available_balance * (OI_MOMENTUM_CONFIG$base_position_pct / 100)
  
  # Adjust size based on confidence
  size_multiplier <- 1 + ((signal$confidence - 0.6) * 1.5)  # 1x to 1.6x
  position_size_usdt <- base_size_usdt * size_multiplier
  
  # Cap at maximum
  max_size_usdt <- available_balance * (OI_MOMENTUM_CONFIG$max_position_pct / 100)
  position_size_usdt <- min(position_size_usdt, max_size_usdt)
  
  # Dynamic leverage based on volatility and signal strength
  base_leverage <- if (signal$strength == "STRONG") 15 else 10
  volatility_adjustment <- signal$details$structure$volatility
  leverage <- max(
    OI_MOMENTUM_CONFIG$leverage_range[1],
    min(
      base_leverage * (1 - volatility_adjustment * 5),
      OI_MOMENTUM_CONFIG$leverage_range[2]
    )
  )
  
  # Dynamic SL/TP based on OI metrics
  sl_pct <- OI_MOMENTUM_CONFIG$default_sl_pct
  tp_pct = OI_MOMENTUM_CONFIG$default_tp_pct
  
  # Adjust based on OI velocity
  if (abs(signal$details$oi$oi_velocity) > 0.5) {
    # Fast moving OI - wider stops
    sl_pct <- sl_pct * 1.2
    tp_pct <- tp_pct * 1.5
  }
  
  # Get current price
  ticker <- get_ticker(symbol)
  current_price <- as.numeric(ticker$last)
  
  # Convert USDT size to contracts
  contract_size <- (position_size_usdt * leverage) / current_price
  
  # Get contract specs and round
  specs <- get_contract_specs(symbol)
  if (!is.null(specs)) {
    contract_size <- floor(contract_size / specs$min_size) * specs$min_size
  }
  
  return(list(
    size = contract_size,
    size_usdt = contract_size * current_price,
    leverage = round(leverage),
    sl_pct = sl_pct,
    tp_pct = tp_pct,
    risk_amount = (contract_size * current_price * sl_pct) / (100 * leverage),
    entry_price = current_price
  ))
}

# ==========================================================================================================
# 🔍 POSITION MONITORING
# ==========================================================================================================

#' Setup OI-based position monitoring
setup_oi_position_monitoring <- function(symbol, signal, position_params) {
  
  # Store position metadata
  OI_POSITION_METADATA[[symbol]] <- list(
    entry_signal = signal,
    entry_params = position_params,
    entry_time = Sys.time(),
    entry_oi = signal$details$oi$metrics$current_oi,
    monitoring = list(
      oi_reversal_threshold = 0.7,  # Exit if OI reverses 70% of entry move
      momentum_exit = TRUE,          # Exit on momentum loss
      trailing_enabled = TRUE,       # Enable trailing stops
      max_hold_time = OI_MOMENTUM_CONFIG$max_hold_hours * 3600
    )
  )
  
  cat("📍 Position monitoring configured\n")
}

#' Monitor OI positions for exit signals
monitor_oi_positions <- function() {
  
  positions <- get_current_positions()
  if (nrow(positions) == 0) return()
  
  cat("\n🔍 === OI POSITION MONITORING === 🔍\n")
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    
    # Check if this is an OI strategy position
    if (!pos$symbol %in% names(OI_POSITION_METADATA)) next
    
    metadata <- OI_POSITION_METADATA[[pos$symbol]]
    
    # Get current OI state
    current_oi_analysis <- analyze_oi_momentum(pos$symbol)
    
    # Check exit conditions
    exit_signals <- check_oi_exit_conditions(
      pos, metadata, current_oi_analysis
    )
    
    if (exit_signals$should_exit) {
      cat("🚨", pos$symbol, "- Exit signal:", exit_signals$reason, "\n")
      
      # Execute exit
      if (exit_signals$urgency == "HIGH") {
        smart_position_exit(pos$symbol, "full")
      } else {
        scale_out_position(pos$symbol, levels = c(50, 100))
      }
      
      # Clear metadata
      OI_POSITION_METADATA[[pos$symbol]] <- NULL
    } else {
      # Update trailing stops if needed
      if (metadata$monitoring$trailing_enabled && 
          pos$pnl_ratio > OI_MOMENTUM_CONFIG$trailing_activation / 100) {
        update_trailing_stop(pos$symbol, OI_MOMENTUM_CONFIG$trailing_distance)
      }
    }
  }
}

#' Check OI exit conditions
check_oi_exit_conditions <- function(position, metadata, current_oi) {
  
  exit_reasons <- list()
  
  # 1. OI Reversal Check
  entry_oi_change <- metadata$entry_signal$details$oi$oi_change_pct
  current_oi_change <- current_oi$oi_change_pct
  
  if (sign(entry_oi_change) != sign(current_oi_change) &&
      abs(current_oi_change) > abs(entry_oi_change) * metadata$monitoring$oi_reversal_threshold) {
    exit_reasons$oi_reversal <- list(
      triggered = TRUE,
      urgency = "HIGH",
      message = "OI flow reversed significantly"
    )
  }
  
  # 2. Momentum Loss Check
  if (metadata$monitoring$momentum_exit) {
    if ((position$side == "long" && current_oi$signal %in% c("SELL", "STRONG_SELL")) ||
        (position$side == "short" && current_oi$signal %in% c("BUY", "STRONG_BUY"))) {
      exit_reasons$momentum_loss <- list(
        triggered = TRUE,
        urgency = "MEDIUM",
        message = "OI momentum turned against position"
      )
    }
  }
  
  # 3. Time-based Exit
  hold_time <- as.numeric(difftime(Sys.time(), metadata$entry_time, units = "secs"))
  if (hold_time > metadata$monitoring$max_hold_time) {
    exit_reasons$time_limit <- list(
      triggered = TRUE,
      urgency = "LOW",
      message = "Maximum hold time reached"
    )
  }
  
  # 4. Profit Target Hit (beyond normal TP)
  if (position$pnl_ratio > metadata$entry_params$tp_pct * 1.5 / 100) {
    exit_reasons$extended_profit <- list(
      triggered = TRUE,
      urgency = "LOW",
      message = "Extended profit target reached"
    )
  }
  
  # Determine if should exit
  should_exit <- any(sapply(exit_reasons, function(x) x$triggered))
  
  # Get highest urgency
  urgency <- if (should_exit) {
    urgencies <- sapply(exit_reasons[sapply(exit_reasons, function(x) x$triggered)], 
                       function(x) x$urgency)
    if ("HIGH" %in% urgencies) "HIGH"
    else if ("MEDIUM" %in% urgencies) "MEDIUM"
    else "LOW"
  } else {
    "NONE"
  }
  
  # Get exit reason
  reason <- if (should_exit) {
    triggered_reasons <- exit_reasons[sapply(exit_reasons, function(x) x$triggered)]
    paste(sapply(triggered_reasons, function(x) x$message), collapse = "; ")
  } else {
    "None"
  }
  
  return(list(
    should_exit = should_exit,
    urgency = urgency,
    reason = reason,
    details = exit_reasons
  ))
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

#' Calculate OI velocity (rate of change)
calculate_oi_velocity <- function(oi_history) {
  if (length(oi_history) < 3) return(0)
  
  # Get last 3 data points
  recent_oi <- tail(oi_history, 3)
  
  # Calculate rate of change
  changes <- diff(as.numeric(recent_oi$amount))
  velocity <- mean(changes) / mean(as.numeric(recent_oi$amount)) * 100
  
  return(velocity)
}

#' Calculate institutional activity score
calculate_institutional_activity <- function(oi_history) {
  if (length(oi_history) < 10) return(0.5)
  
  # Look for large volume spikes (institutional footprint)
  oi_values <- as.numeric(oi_history$amount)
  oi_changes <- abs(diff(oi_values))
  
  # Calculate percentile of changes
  large_changes <- oi_changes > quantile(oi_changes, 0.8)
  institutional_score <- sum(large_changes) / length(oi_changes)
  
  # Adjust for consistency
  if (sd(oi_values) / mean(oi_values) > 0.3) {
    institutional_score <- institutional_score * 1.2
  }
  
  return(min(institutional_score, 1))
}

#' Calculate trend strength from candles
calculate_trend_strength <- function(candles) {
  
  # Simple trend calculation using price action
  closes <- as.numeric(candles$close)
  
  # Linear regression for trend
  time_index <- 1:length(closes)
  trend_model <- lm(closes ~ time_index)
  slope <- coef(trend_model)[2]
  
  # Normalize slope
  avg_price <- mean(closes)
  normalized_slope <- slope / avg_price * 100
  
  # Calculate R-squared for trend strength
  r_squared <- summary(trend_model)$r.squared
  
  return(list(
    direction = if (normalized_slope > 0.1) "UPTREND" 
               else if (normalized_slope < -0.1) "DOWNTREND" 
               else "SIDEWAYS",
    strength = r_squared,
    slope = normalized_slope
  ))
}

#' Find support and resistance levels
find_key_levels <- function(candles) {
  
  highs <- as.numeric(candles$high)
  lows <- as.numeric(candles$low)
  
  # Simple pivot identification
  resistance <- quantile(highs, c(0.75, 0.9, 0.95))
  support <- quantile(lows, c(0.05, 0.1, 0.25))
  
  return(list(
    resistance = resistance,
    support = support,
    current_position = (tail(candles$close, 1) - min(lows)) / (max(highs) - min(lows))
  ))
}

#' Calculate momentum indicators
calculate_momentum_indicators <- function(candles) {
  
  closes <- as.numeric(candles$close)
  
  # RSI calculation (simplified)
  price_changes <- diff(closes)
  gains <- price_changes[price_changes > 0]
  losses <- abs(price_changes[price_changes < 0])
  
  avg_gain <- mean(gains, na.rm = TRUE)
  avg_loss <- mean(losses, na.rm = TRUE)
  
  rs <- if (avg_loss > 0) avg_gain / avg_loss else 100
  rsi <- 100 - (100 / (1 + rs))
  
  # Momentum score
  momentum_score <- if (rsi > 70) 0.8
                   else if (rsi > 60) 0.65
                   else if (rsi < 30) 0.2
                   else if (rsi < 40) 0.35
                   else 0.5
  
  return(list(
    rsi = rsi,
    score = momentum_score
  ))
}

#' Calculate volatility
calculate_volatility <- function(candles) {
  
  # Calculate returns
  closes <- as.numeric(candles$close)
  returns <- diff(log(closes))
  
  # Annualized volatility (simplified)
  volatility <- sd(returns, na.rm = TRUE) * sqrt(365 * 96)  # 96 15-min candles per day
  
  return(volatility)
}

#' Score market structure
score_market_structure <- function(structure_analysis) {
  
  score <- 0.5  # Neutral base
  
  # Trend alignment
  if (structure_analysis$trend$direction == "UPTREND") {
    score <- score + 0.2 * structure_analysis$trend$strength
  } else if (structure_analysis$trend$direction == "DOWNTREND") {
    score <- score - 0.2 * structure_analysis$trend$strength
  }
  
  # Momentum
  score <- score + (structure_analysis$momentum$score - 0.5) * 0.3
  
  # Position relative to S/R
  position <- structure_analysis$support_resistance$current_position
  if (position < 0.3) score <- score + 0.1  # Near support
  if (position > 0.7) score <- score - 0.1  # Near resistance
  
  # Volatility adjustment
  if (structure_analysis$volatility < 0.2) {
    score <- score * 1.1  # Low volatility bonus
  } else if (structure_analysis$volatility > 0.5) {
    score <- score * 0.9  # High volatility penalty
  }
  
  return(max(0, min(1, score)))
}

#' Analyze trade direction from recent trades
analyze_trade_direction <- function(trades) {
  if (is.null(trades) || nrow(trades) == 0) return(0.5)
  
  # Count buy vs sell trades
  buy_volume <- sum(trades$size[trades$side == "buy"], na.rm = TRUE)
  sell_volume <- sum(trades$size[trades$side == "sell"], na.rm = TRUE)
  total_volume <- buy_volume + sell_volume
  
  if (total_volume == 0) return(0.5)
  
  return(buy_volume / total_volume)
}

#' Generate trade recommendation
generate_trade_recommendation <- function(direction, strength, confidence, oi_analysis, market_structure) {
  
  if (direction == "NEUTRAL") {
    return(list(
      action = "WAIT",
      message = "No clear signal - waiting for better setup"
    ))
  }
  
  # Build recommendation
  action <- paste(strength, direction)
  
  # Risk level based on volatility
  risk_level <- if (market_structure$volatility > 0.4) "HIGH"
               else if (market_structure$volatility > 0.25) "MEDIUM"
               else "LOW"
  
  # Entry timing
  entry_timing <- if (strength == "STRONG" && confidence > 0.75) "IMMEDIATE"
                 else if (confidence > 0.65) "ON_PULLBACK"
                 else "WAIT_CONFIRMATION"
  
  # Size recommendation
  size_recommendation <- if (confidence > 0.8 && risk_level != "HIGH") "FULL"
                        else if (confidence > 0.7) "NORMAL"
                        else "REDUCED"
  
  return(list(
    action = action,
    entry_timing = entry_timing,
    size = size_recommendation,
    risk_level = risk_level,
    targets = list(
      primary = OI_MOMENTUM_CONFIG$default_tp_pct,
      extended = OI_MOMENTUM_CONFIG$default_tp_pct * 1.5
    ),
    message = sprintf(
      "%s signal with %s confidence. OI change: %.1f%%, Institutional: %.2f",
      action, 
      if (confidence > 0.75) "HIGH" else if (confidence > 0.65) "MODERATE" else "LOW",
      oi_analysis$oi_change_pct,
      oi_analysis$institutional_score
    )
  ))
}

# ==========================================================================================================
# 📊 DISPLAY FUNCTIONS
# ==========================================================================================================

#' Display signal summary
display_signal_summary <- function(symbol, signal) {
  
  if (signal$direction == "NEUTRAL") {
    cat(" → No signal\n")
    return()
  }
  
  # Signal indicator
  indicator <- if (signal$direction == "BUY") "📈" else "📉"
  
  cat(" ", indicator, signal$direction, "-", signal$strength,
      "(", round(signal$confidence * 100), "% confidence)\n")
  
  # Key metrics
  cat("     OI Change:", round(signal$details$oi$oi_change_pct, 1), "%",
      "| Institutional:", round(signal$details$oi$institutional_score, 2),
      "| Volume:", round(signal$details$volume$volume_ratio, 1), "x\n")
}

#' Rank trading signals
rank_trading_signals <- function(signals) {
  
  # Filter out neutral signals
  active_signals <- signals[sapply(signals, function(s) s$direction != "NEUTRAL")]
  
  if (length(active_signals) == 0) {
    return(list())
  }
  
  # Sort by score and confidence
  sorted_signals <- active_signals[order(
    sapply(active_signals, function(s) s$score * s$confidence),
    decreasing = TRUE
  )]
  
  # Add ranking
  for (i in seq_along(sorted_signals)) {
    sorted_signals[[i]]$rank <- i
  }
  
  return(sorted_signals)
}

#' Display strategy performance summary
display_strategy_performance <- function() {
  
  cat("\n📊 === OI MOMENTUM STRATEGY PERFORMANCE === 📊\n")
  
  # Get position history
  history <- get_position_history_summary()
  
  if (is.null(history)) {
    cat("No trading history available\n")
    return()
  }
  
  # Filter OI strategy trades
  oi_trades <- history$history[grep("OI_", history$history$strategy), ]
  
  if (nrow(oi_trades) == 0) {
    cat("No OI momentum trades in history\n")
    return()
  }
  
  # Calculate metrics
  total_trades <- nrow(oi_trades)
  winning_trades <- sum(oi_trades$pnl > 0)
  win_rate <- winning_trades / total_trades * 100
  
  total_pnl <- sum(oi_trades$pnl)
  avg_win <- mean(oi_trades$pnl[oi_trades$pnl > 0])
  avg_loss <- mean(oi_trades$pnl[oi_trades$pnl <= 0])
  
  profit_factor <- abs(sum(oi_trades$pnl[oi_trades$pnl > 0])) / 
                   abs(sum(oi_trades$pnl[oi_trades$pnl <= 0]))
  
  # Display metrics
  cat("\n📈 Strategy Metrics:\n")
  cat("├─ Total Trades:", total_trades, "\n")
  cat("├─ Win Rate:", round(win_rate, 1), "%\n")
  cat("├─ Total PnL:", round(total_pnl, 2), "USDT\n")
  cat("├─ Avg Win:", round(avg_win, 2), "USDT\n")
  cat("├─ Avg Loss:", round(avg_loss, 2), "USDT\n")
  cat("└─ Profit Factor:", round(profit_factor, 2), "\n")
  
  # Recent performance
  recent_trades <- tail(oi_trades, 10)
  recent_win_rate <- sum(recent_trades$pnl > 0) / nrow(recent_trades) * 100
  
  cat("\n📊 Recent Performance (last 10 trades):\n")
  cat("├─ Win Rate:", round(recent_win_rate, 1), "%\n")
  cat("└─ PnL:", round(sum(recent_trades$pnl), 2), "USDT\n")
}

# ==========================================================================================================
# 🚀 STRATEGY INITIALIZATION
# ==========================================================================================================

# Initialize position metadata storage
OI_POSITION_METADATA <- list()

# Main strategy functions
OI_MOMENTUM_STRATEGY <- list(
  generate_signals = generate_oi_momentum_signals,
  execute_trade = execute_oi_momentum_trade,
  monitor_positions = monitor_oi_positions,
  get_config = function() OI_MOMENTUM_CONFIG,
  performance = display_strategy_performance
)

cat("✅ OI_MOMENTUM_STRATEGY.R loaded successfully!\n")
cat("📈 OI-based momentum trading ready\n")
cat("🎯 Signal generation and execution enabled\n")
cat("🔍 Position monitoring active\n")