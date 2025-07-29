# ========================================================================================================== 
# 🛡️ PORTFOLIO RISK MANAGER V2 - UNIFIED RISK MANAGEMENT SYSTEM
# ==========================================================================================================
# Konsolidiert: trailing_sl_system.r + emergency_trailing_sl.r
# Comprehensive portfolio risk management with advanced trailing stop-loss systems
# ==========================================================================================================

# Load central configuration if not already loaded
if (!exists("SYSTEM_CONFIG_LOADED")) {
  source("system_config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
}

# ==========================================================================================================
# 📊 POSITION MANAGEMENT & MONITORING
# ==========================================================================================================

#' Enhanced position monitoring with risk assessment
get_current_positions <- function(include_risk_metrics = TRUE) {
  tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/position/all-position",
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(response) || length(response$data) == 0) {
      cat("ℹ️ No open positions found\n")
      return(data.frame())
    }
    
    positions <- response$data
    positions_df <- do.call(rbind, lapply(positions, function(pos) {
      data.frame(
        symbol = pos$symbol,
        side = pos$holdSide,
        size = as.numeric(pos$total),
        available = as.numeric(pos$available),
        avg_price = as.numeric(pos$averageOpenPrice),
        mark_price = as.numeric(pos$markPrice),
        unrealized_pnl = as.numeric(pos$unrealizedPL),
        pnl_ratio = as.numeric(pos$unrealizedPL) / as.numeric(pos$total),
        leverage = as.numeric(pos$leverage),
        margin = as.numeric(pos$im),
        stringsAsFactors = FALSE
      )
    }))
    
    # Add risk metrics if requested
    if (include_risk_metrics && nrow(positions_df) > 0) {
      positions_df <- add_risk_metrics(positions_df)
    }
    
    return(positions_df)
    
  }, error = function(e) {
    cat("❌ Error fetching positions:", e$message, "\n")
    return(data.frame())
  })
}

#' Get current planned orders (TP/SL orders)
get_current_plan_orders <- function() {
  tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/order/orders-plan-pending",
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(response) || length(response$data) == 0) {
      return(data.frame())
    }
    
    orders <- response$data
    orders_df <- do.call(rbind, lapply(orders, function(order) {
      data.frame(
        order_id = order$orderId,
        symbol = order$symbol,
        side = order$side,
        order_type = order$orderType,
        size = as.numeric(order$size),
        trigger_price = as.numeric(order$triggerPrice),
        trigger_type = order$triggerType,
        plan_type = order$planType,
        status = order$status,
        stringsAsFactors = FALSE
      )
    }))
    
    return(orders_df)
    
  }, error = function(e) {
    cat("❌ Error fetching plan orders:", e$message, "\n")
    return(data.frame())
  })
}

#' Monitor portfolio positions with advanced risk alerts
monitor_portfolio_positions <- function(symbols = PORTFOLIO_ASSETS, alert_threshold = 0.05) {
  cat("\n🛡️ === PORTFOLIO RISK MONITORING === 🛡️\n")
  
  positions <- get_current_positions(include_risk_metrics = TRUE)
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No positions to monitor\n")
    return(list())
  }
  
  risk_alerts <- list()
  total_portfolio_risk <- 0
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    cat("\n📊", pos$symbol, "Position Status:\n")
    cat("   Size:", pos$size, pos$side, "| PnL:", round(pos$unrealized_pnl, 2), "USDT\n")
    cat("   Risk Score:", round(pos$risk_score, 3), "| Volatility:", round(pos$volatility_risk, 3), "\n")
    
    # Check for risk alerts
    if (pos$pnl_ratio < -alert_threshold) {
      alert <- list(
        symbol = pos$symbol,
        type = "HIGH_LOSS",
        severity = if (pos$pnl_ratio < -0.1) "CRITICAL" else "WARNING",
        message = paste("Loss exceeds threshold:", round(pos$pnl_ratio * 100, 2), "%")
      )
      risk_alerts[[length(risk_alerts) + 1]] <- alert
      cat("   🚨", alert$severity, ":", alert$message, "\n")
    }
    
    if (pos$risk_score > 0.8) {
      alert <- list(
        symbol = pos$symbol,
        type = "HIGH_RISK",
        severity = "WARNING",
        message = paste("High risk score:", round(pos$risk_score, 3))
      )
      risk_alerts[[length(risk_alerts) + 1]] <- alert
      cat("   ⚠️ WARNING:", alert$message, "\n")
    }
    
    total_portfolio_risk <- total_portfolio_risk + pos$risk_score * abs(pos$unrealized_pnl)
  }
  
  # Portfolio-level risk assessment
  portfolio_risk_score <- calculate_portfolio_risk(positions)
  cat("\n📈 Portfolio Risk Score:", round(portfolio_risk_score, 3), "\n")
  
  if (portfolio_risk_score > 0.7) {
    cat("🚨 HIGH PORTFOLIO RISK DETECTED - Consider risk reduction measures\n")
  }
  
  return(list(
    positions = positions,
    risk_alerts = risk_alerts,
    portfolio_risk_score = portfolio_risk_score
  ))
}

# ==========================================================================================================
# 🎯 ADVANCED TRAILING STOP-LOSS SYSTEM
# ==========================================================================================================

#' Percentage-based trailing stop loss with enhanced features
place_trailing_sl_percent <- function(symbol, sl_percent = NULL, trailing_percent = NULL, 
                                     max_attempts = 3, dry_run = FALSE) {
  tryCatch({
    # Get asset config for symbol-specific settings
    asset_config <- get_asset_config(symbol)
    if (is.null(asset_config)) {
      cat("❌ Unknown symbol:", symbol, "\n")
      return(FALSE)
    }
    
    # Use provided percentages or defaults from config
    sl_percent <- sl_percent %||% asset_config$default_sl_percent %||% DEFAULT_SL_PERCENT
    trailing_percent <- trailing_percent %||% asset_config$trailing_sl_percent %||% (sl_percent * 0.5)
    
    # Get current position
    positions <- get_current_positions()
    position <- positions[positions$symbol == symbol, ]
    
    if (nrow(position) == 0) {
      cat("❌ No position found for", symbol, "\n")
      return(FALSE)
    }
    
    # Get current market price
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) {
      cat("❌ Failed to get market price for", symbol, "\n")
      return(FALSE)
    }
    
    current_price <- as.numeric(ticker_data$lastPr)
    side <- position$side[1]
    size <- position$size[1]
    
    # Calculate trailing stop price
    if (side == "long") {
      trigger_price <- current_price * (1 - sl_percent / 100)
      trailing_stop_price <- current_price * (1 - trailing_percent / 100)
    } else {
      trigger_price <- current_price * (1 + sl_percent / 100)
      trailing_stop_price <- current_price * (1 + trailing_percent / 100)
    }
    
    # Validate prices
    if (!validate_order_parameters(symbol, trigger_price, side, size)) {
      cat("❌ Invalid order parameters for", symbol, "\n")
      return(FALSE)
    }
    
    if (dry_run) {
      cat("🔍 DRY RUN - Trailing SL for", symbol, ":\n")
      cat("   Trigger Price:", round(trigger_price, asset_config$price_precision), "\n")
      cat("   Trailing Stop:", round(trailing_stop_price, asset_config$price_precision), "\n")
      return(TRUE)
    }
    
    # Place trailing stop order with retry logic
    for (attempt in 1:max_attempts) {
      order_result <- place_trailing_stop_order(symbol, side, size, trigger_price, trailing_stop_price)
      
      if (!is.null(order_result) && order_result$success) {
        cat("✅ Trailing SL placed for", symbol, "| Trigger:", round(trigger_price, asset_config$price_precision), "\n")
        return(TRUE)
      }
      
      if (attempt < max_attempts) {
        cat("⏳ Retry", attempt, "failed, attempting again...\n")
        Sys.sleep(1)
      }
    }
    
    cat("❌ Failed to place trailing SL for", symbol, "after", max_attempts, "attempts\n")
    return(FALSE)
    
  }, error = function(e) {
    cat("❌ Error in place_trailing_sl_percent:", e$message, "\n")
    return(FALSE)
  })
}

#' Batch trailing stop-loss for multiple positions
place_batch_trailing_sl <- function(symbols = NULL, sl_percent = NULL, dry_run = FALSE) {
  if (is.null(symbols)) {
    positions <- get_current_positions()
    symbols <- unique(positions$symbol)
  }
  
  if (length(symbols) == 0) {
    cat("ℹ️ No symbols to process\n")
    return(list())
  }
  
  cat("\n🔄 === BATCH TRAILING SL PLACEMENT === 🔄\n")
  
  results <- list()
  
  for (symbol in symbols) {
    cat("\n📍 Processing", symbol, "...\n")
    
    # Get symbol-specific SL percentage if not provided
    asset_config <- get_asset_config(symbol)
    symbol_sl_percent <- sl_percent %||% asset_config$default_sl_percent %||% DEFAULT_SL_PERCENT
    
    success <- place_trailing_sl_percent(symbol, symbol_sl_percent, dry_run = dry_run)
    results[[symbol]] <- success
    
    # Brief pause between orders to avoid rate limits
    if (!dry_run) Sys.sleep(0.5)
  }
  
  # Summary
  successful <- sum(unlist(results))
  total <- length(results)
  cat("\n📊 Batch Results:", successful, "successful out of", total, "attempts\n")
  
  return(results)
}

#' Dynamic trailing stop-loss with market volatility adjustment
place_dynamic_trailing_sl <- function(symbol, volatility_factor = 1.0, dry_run = FALSE) {
  tryCatch({
    # Calculate current market volatility
    volatility <- calculate_market_volatility(symbol)
    
    if (is.null(volatility)) {
      cat("⚠️ Unable to calculate volatility, using default SL\n")
      return(place_trailing_sl_percent(symbol, dry_run = dry_run))
    }
    
    # Adjust SL percentage based on volatility
    asset_config <- get_asset_config(symbol)
    base_sl_percent <- asset_config$default_sl_percent %||% DEFAULT_SL_PERCENT
    
    # Higher volatility = wider stop loss
    adjusted_sl_percent <- base_sl_percent * (1 + volatility * volatility_factor)
    adjusted_sl_percent <- min(adjusted_sl_percent, base_sl_percent * 2) # Cap at 2x base
    
    cat("🔄 Dynamic SL for", symbol, "| Volatility:", round(volatility, 3), 
        "| Adjusted SL:", round(adjusted_sl_percent, 2), "%\n")
    
    return(place_trailing_sl_percent(symbol, adjusted_sl_percent, dry_run = dry_run))
    
  }, error = function(e) {
    cat("❌ Error in dynamic trailing SL:", e$message, "\n")
    return(FALSE)
  })
}

#' Portfolio-wide trailing stop-loss management
place_portfolio_trailing_sl <- function(risk_based = TRUE, dry_run = FALSE) {
  cat("\n🛡️ === PORTFOLIO TRAILING SL MANAGEMENT === 🛡️\n")
  
  positions <- get_current_positions(include_risk_metrics = TRUE)
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No positions to protect\n")
    return(list())
  }
  
  results <- list()
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    symbol <- pos$symbol
    
    if (risk_based) {
      # Adjust SL based on position risk
      risk_multiplier <- if (pos$risk_score > 0.7) 0.8 else if (pos$risk_score > 0.5) 0.9 else 1.0
      
      asset_config <- get_asset_config(symbol)
      adjusted_sl <- (asset_config$default_sl_percent %||% DEFAULT_SL_PERCENT) * risk_multiplier
      
      cat("📊", symbol, "- Risk Score:", round(pos$risk_score, 3), 
          "| Adjusted SL:", round(adjusted_sl, 2), "%\n")
      
      success <- place_trailing_sl_percent(symbol, adjusted_sl, dry_run = dry_run)
    } else {
      success <- place_trailing_sl_percent(symbol, dry_run = dry_run)
    }
    
    results[[symbol]] <- success
    
    if (!dry_run) Sys.sleep(0.5)
  }
  
  return(results)
}

# ==========================================================================================================
# ⚠️ EMERGENCY PROTECTION SYSTEM
# ==========================================================================================================

#' Emergency portfolio protection triggers
emergency_protection_triggers <- function(max_portfolio_loss = 0.1, max_position_loss = 0.15) {
  cat("\n🚨 === EMERGENCY PROTECTION CHECK === 🚨\n")
  
  positions <- get_current_positions(include_risk_metrics = TRUE)
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No positions to monitor\n")
    return(list(triggered = FALSE))
  }
  
  emergency_actions <- list()
  total_unrealized_pnl <- sum(positions$unrealized_pnl)
  total_margin <- sum(positions$margin)
  
  # Portfolio-level emergency check
  portfolio_loss_ratio <- abs(total_unrealized_pnl) / total_margin
  
  if (total_unrealized_pnl < 0 && portfolio_loss_ratio > max_portfolio_loss) {
    emergency_actions[["PORTFOLIO_EMERGENCY"]] <- list(
      type = "PORTFOLIO_STOP",
      severity = "CRITICAL",
      message = paste("Portfolio loss exceeds threshold:", round(portfolio_loss_ratio * 100, 2), "%"),
      action = "CLOSE_ALL_POSITIONS"
    )
    
    cat("🚨 PORTFOLIO EMERGENCY TRIGGERED!\n")
    cat("   Total PnL:", round(total_unrealized_pnl, 2), "USDT\n")
    cat("   Loss Ratio:", round(portfolio_loss_ratio * 100, 2), "%\n")
  }
  
  # Position-level emergency checks
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    
    if (pos$pnl_ratio < -max_position_loss) {
      emergency_actions[[pos$symbol]] <- list(
        type = "POSITION_EMERGENCY",
        severity = "CRITICAL",
        message = paste("Position loss exceeds threshold:", round(pos$pnl_ratio * 100, 2), "%"),
        action = "CLOSE_POSITION",
        symbol = pos$symbol
      )
      
      cat("🚨", pos$symbol, "EMERGENCY TRIGGERED!\n")
      cat("   Position PnL:", round(pos$unrealized_pnl, 2), "USDT\n")
      cat("   Loss Ratio:", round(pos$pnl_ratio * 100, 2), "%\n")
    }
  }
  
  return(list(
    triggered = length(emergency_actions) > 0,
    actions = emergency_actions,
    portfolio_status = list(
      total_pnl = total_unrealized_pnl,
      loss_ratio = portfolio_loss_ratio
    )
  ))
}

#' Execute emergency position closure
execute_emergency_closure <- function(symbol = NULL, close_all = FALSE, dry_run = TRUE) {
  if (dry_run) {
    cat("🔍 DRY RUN - Emergency closure simulation\n")
  }
  
  if (close_all) {
    cat("🚨 EXECUTING EMERGENCY PORTFOLIO CLOSURE\n")
    positions <- get_current_positions()
    
    if (nrow(positions) == 0) {
      cat("ℹ️ No positions to close\n")
      return(TRUE)
    }
    
    results <- list()
    for (i in 1:nrow(positions)) {
      pos <- positions[i, ]
      if (dry_run) {
        cat("   Would close:", pos$symbol, pos$side, pos$size, "\n")
        results[[pos$symbol]] <- TRUE
      } else {
        results[[pos$symbol]] <- close_position_market(pos$symbol, pos$side, pos$size)
      }
    }
    
    return(results)
    
  } else if (!is.null(symbol)) {
    cat("🚨 EXECUTING EMERGENCY CLOSURE FOR", symbol, "\n")
    
    positions <- get_current_positions()
    position <- positions[positions$symbol == symbol, ]
    
    if (nrow(position) == 0) {
      cat("❌ No position found for", symbol, "\n")
      return(FALSE)
    }
    
    if (dry_run) {
      cat("   Would close:", symbol, position$side[1], position$size[1], "\n")
      return(TRUE)
    } else {
      return(close_position_market(symbol, position$side[1], position$size[1]))
    }
  }
  
  return(FALSE)
}

# ==========================================================================================================
# 📋 ORDER MANAGEMENT & VALIDATION
# ==========================================================================================================

#' Place simple take profit order
place_tp_simple <- function(symbol, tp_percent = NULL, dry_run = FALSE) {
  tryCatch({
    asset_config <- get_asset_config(symbol)
    tp_percent <- tp_percent %||% asset_config$default_tp_percent %||% DEFAULT_TP_PERCENT
    
    positions <- get_current_positions()
    position <- positions[positions$symbol == symbol, ]
    
    if (nrow(position) == 0) {
      cat("❌ No position found for", symbol, "\n")
      return(FALSE)
    }
    
    side <- position$side[1]
    size <- position$size[1]
    avg_price <- position$avg_price[1]
    
    # Calculate TP price
    if (side == "long") {
      tp_price <- avg_price * (1 + tp_percent / 100)
    } else {
      tp_price <- avg_price * (1 - tp_percent / 100)
    }
    
    if (dry_run) {
      cat("🔍 DRY RUN - TP for", symbol, ":", round(tp_price, asset_config$price_precision), "\n")
      return(TRUE)
    }
    
    return(place_take_profit_order(symbol, side, size, tp_price))
    
  }, error = function(e) {
    cat("❌ Error placing TP:", e$message, "\n")
    return(FALSE)
  })
}

#' Place simple stop loss order
place_sl_simple <- function(symbol, sl_percent = NULL, dry_run = FALSE) {
  tryCatch({
    asset_config <- get_asset_config(symbol)
    sl_percent <- sl_percent %||% asset_config$default_sl_percent %||% DEFAULT_SL_PERCENT
    
    positions <- get_current_positions()
    position <- positions[positions$symbol == symbol, ]
    
    if (nrow(position) == 0) {
      cat("❌ No position found for", symbol, "\n")
      return(FALSE)
    }
    
    side <- position$side[1]
    size <- position$size[1]
    avg_price <- position$avg_price[1]
    
    # Calculate SL price
    if (side == "long") {
      sl_price <- avg_price * (1 - sl_percent / 100)
    } else {
      sl_price <- avg_price * (1 + sl_percent / 100)
    }
    
    if (dry_run) {
      cat("🔍 DRY RUN - SL for", symbol, ":", round(sl_price, asset_config$price_precision), "\n")
      return(TRUE)
    }
    
    return(place_stop_loss_order(symbol, side, size, sl_price))
    
  }, error = function(e) {
    cat("❌ Error placing SL:", e$message, "\n")
    return(FALSE)
  })
}

#' Intelligent TP/SL placement with risk analysis
place_intelligent_tp_sl <- function(symbol, risk_level = "moderate", dry_run = FALSE) {
  asset_config <- get_asset_config(symbol)
  
  # Adjust percentages based on risk level
  risk_multipliers <- list(
    conservative = list(tp = 0.8, sl = 0.8),
    moderate = list(tp = 1.0, sl = 1.0),
    aggressive = list(tp = 1.5, sl = 1.2)
  )
  
  multiplier <- risk_multipliers[[risk_level]]
  
  tp_percent <- (asset_config$default_tp_percent %||% DEFAULT_TP_PERCENT) * multiplier$tp
  sl_percent <- (asset_config$default_sl_percent %||% DEFAULT_SL_PERCENT) * multiplier$sl
  
  cat("🧠 Intelligent TP/SL for", symbol, "| Risk Level:", risk_level, "\n")
  cat("   TP:", round(tp_percent, 2), "% | SL:", round(sl_percent, 2), "%\n")
  
  tp_result <- place_tp_simple(symbol, tp_percent, dry_run)
  sl_result <- place_sl_simple(symbol, sl_percent, dry_run)
  
  return(list(tp = tp_result, sl = sl_result))
}

#' Validate order parameters before submission
validate_order_parameters <- function(symbol, price, side, size) {
  tryCatch({
    asset_config <- get_asset_config(symbol)
    
    # Check minimum order size
    if (size < asset_config$min_order_size) {
      cat("❌ Order size too small:", size, "< minimum:", asset_config$min_order_size, "\n")
      return(FALSE)
    }
    
    # Check price precision
    price_precision <- asset_config$price_precision
    if (length(strsplit(as.character(price), "\\.")[[1]]) > 1) {
      decimal_places <- nchar(strsplit(as.character(price), "\\.")[[1]][2])
      if (decimal_places > price_precision) {
        cat("❌ Price precision too high:", decimal_places, "> maximum:", price_precision, "\n")
        return(FALSE)
      }
    }
    
    # Check if price is reasonable (not too far from current market)
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (!is.null(ticker_data)) {
      current_price <- as.numeric(ticker_data$lastPr)
      price_diff_percent <- abs(price - current_price) / current_price
      
      if (price_diff_percent > 0.5) {  # 50% difference threshold
        cat("⚠️ Warning: Price significantly differs from market:", round(price_diff_percent * 100, 2), "%\n")
      }
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error validating order parameters:", e$message, "\n")
    return(FALSE)
  })
}

# ==========================================================================================================
# 📊 RISK ASSESSMENT & METRICS
# ==========================================================================================================

#' Calculate portfolio-wide risk score
calculate_portfolio_risk <- function(positions) {
  if (nrow(positions) == 0) return(0)
  
  # Weighted risk calculation based on position sizes and individual risk scores
  total_exposure <- sum(abs(positions$unrealized_pnl))
  
  if (total_exposure == 0) return(0)
  
  weighted_risk <- sum(positions$risk_score * abs(positions$unrealized_pnl)) / total_exposure
  
  # Adjust for portfolio concentration
  concentration_penalty <- 1 + (length(unique(positions$symbol)) - 1) * 0.1  # Diversification bonus
  concentration_penalty <- min(concentration_penalty, 1.5)  # Cap bonus
  
  portfolio_risk <- weighted_risk / concentration_penalty
  
  return(min(max(portfolio_risk, 0), 1))  # Clamp between 0 and 1
}

#' Assess individual position exposure
assess_position_exposure <- function(position) {
  # Calculate multiple risk factors
  pnl_risk <- abs(position$pnl_ratio)
  leverage_risk <- position$leverage / 50  # Normalize leverage (50x = 1.0)
  size_risk <- position$size / 1000  # Normalize size
  
  # Combine risk factors
  overall_risk <- (pnl_risk * 0.4 + leverage_risk * 0.3 + size_risk * 0.3)
  
  return(min(max(overall_risk, 0), 1))
}

#' Add risk metrics to positions dataframe
add_risk_metrics <- function(positions_df) {
  if (nrow(positions_df) == 0) return(positions_df)
  
  # Calculate risk metrics for each position
  positions_df$risk_score <- sapply(1:nrow(positions_df), function(i) {
    assess_position_exposure(positions_df[i, ])
  })
  
  # Add volatility risk (simplified calculation)
  positions_df$volatility_risk <- sapply(positions_df$symbol, function(symbol) {
    volatility <- calculate_market_volatility(symbol)
    return(volatility %||% 0.5)  # Default moderate volatility
  })
  
  return(positions_df)
}

# ==========================================================================================================
# 🛠️ HELPER FUNCTIONS
# ==========================================================================================================

#' Calculate market volatility for dynamic SL adjustment
calculate_market_volatility <- function(symbol, periods = 24) {
  tryCatch({
    # Fetch recent price data
    kline_data <- get_kline_data(symbol, "1h", periods)
    
    if (is.null(kline_data) || length(kline_data) < 5) {
      return(NULL)
    }
    
    # Calculate hourly returns
    prices <- sapply(kline_data, function(k) as.numeric(k[5]))  # Close prices
    returns <- diff(log(prices))
    
    # Calculate volatility (standard deviation of returns)
    volatility <- sd(returns, na.rm = TRUE)
    
    # Normalize to 0-1 scale (typical crypto volatility range)
    normalized_volatility <- min(max(volatility / 0.1, 0), 1)
    
    return(normalized_volatility)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Place trailing stop order via API
place_trailing_stop_order <- function(symbol, side, size, trigger_price, trailing_stop_price) {
  tryCatch({
    # Determine order side for closing position
    order_side <- if (side == "long") "sell" else "buy"
    
    response <- bitget_request(
      method = "POST",
      endpoint = "/api/v2/mix/order/place-plan-order",
      params = list(
        symbol = symbol,
        productType = "USDT-FUTURES",
        marginMode = "crossed",
        marginCoin = "USDT",
        side = order_side,
        orderType = "market",
        size = as.character(size),
        triggerPrice = as.character(trigger_price),
        triggerType = "mark_price",
        planType = "moving_plan"
      )
    )
    
    if (!is.null(response) && !is.null(response$data) && !is.null(response$data$orderId)) {
      return(list(success = TRUE, order_id = response$data$orderId))
    } else {
      return(list(success = FALSE, error = "API response error"))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

#' Close position at market price (emergency closure)
close_position_market <- function(symbol, side, size) {
  tryCatch({
    order_side <- if (side == "long") "sell" else "buy"
    
    response <- bitget_request(
      method = "POST",
      endpoint = "/api/v2/mix/order/place-order",
      params = list(
        symbol = symbol,
        productType = "USDT-FUTURES",
        marginMode = "crossed",
        marginCoin = "USDT",
        side = order_side,
        orderType = "market",
        size = as.character(size),
        reduceOnly = "true"
      )
    )
    
    if (!is.null(response) && !is.null(response$data) && !is.null(response$data$orderId)) {
      cat("✅ Emergency closure executed for", symbol, "| Order ID:", response$data$orderId, "\n")
      return(TRUE)
    } else {
      cat("❌ Failed to execute emergency closure for", symbol, "\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("❌ Error in emergency closure:", e$message, "\n")
    return(FALSE)
  })
}

#' Place take profit order
place_take_profit_order <- function(symbol, side, size, tp_price) {
  order_side <- if (side == "long") "sell" else "buy"
  
  response <- bitget_request(
    method = "POST",
    endpoint = "/api/v2/mix/order/place-plan-order",
    params = list(
      symbol = symbol,
      productType = "USDT-FUTURES",
      marginMode = "crossed", 
      marginCoin = "USDT",
      side = order_side,
      orderType = "limit",
      size = as.character(size),
      price = as.character(tp_price),
      triggerPrice = as.character(tp_price),
      triggerType = "mark_price",
      planType = "profit_plan"
    )
  )
  
  return(!is.null(response) && !is.null(response$data) && !is.null(response$data$orderId))
}

#' Place stop loss order
place_stop_loss_order <- function(symbol, side, size, sl_price) {
  order_side <- if (side == "long") "sell" else "buy"
  
  response <- bitget_request(
    method = "POST",
    endpoint = "/api/v2/mix/order/place-plan-order", 
    params = list(
      symbol = symbol,
      productType = "USDT-FUTURES",
      marginMode = "crossed",
      marginCoin = "USDT", 
      side = order_side,
      orderType = "market",
      size = as.character(size),
      triggerPrice = as.character(sl_price),
      triggerType = "mark_price",
      planType = "loss_plan"
    )
  )
  
  return(!is.null(response) && !is.null(response$data) && !is.null(response$data$orderId))
}

cat("✅ PORTFOLIO_RISK_MANAGER.R loaded successfully!\n")
cat("🛡️ Advanced trailing stop-loss system ready\n")
cat("🚨 Emergency protection protocols activated\n")
cat("📊 Portfolio risk monitoring enabled\n")