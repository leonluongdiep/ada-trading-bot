# ==========================================================================================================
# 🛡️ RISK MANAGEMENT SYSTEM V3
# ==========================================================================================================
# Pfad: C:\freeding\tbot202506\r_analysis\riskassetm\trading\risk_manager.r
# Konsolidiert aus: portfolio_risk_manager.r + trailing_sl_system.r + emergency_trailing_sl.r
# ==========================================================================================================

cat("🛡️ Loading Risk Management System V3...\n")

# ==========================================================================================================
# 📊 RISK ASSESSMENT
# ==========================================================================================================

#' Analyze portfolio risk
analyze_portfolio_risk <- function(symbols = NULL) {
  
  if (is.null(symbols)) {
    symbols <- PORTFOLIO_ASSETS
  }
  
  cat("\n🛡️ === PORTFOLIO RISK ANALYSIS === 🛡️\n")
  
  # Get current positions
  positions <- get_current_positions()
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No open positions\n")
    return(list(
      has_positions = FALSE,
      risk_level = "NONE",
      alerts = list()
    ))
  }
  
  # Calculate risk metrics
  risk_metrics <- calculate_risk_metrics(positions)
  
  # Check risk limits
  risk_alerts <- check_risk_limits(positions, risk_metrics)
  
  # Determine overall risk level
  risk_level <- determine_risk_level(risk_metrics, risk_alerts)
  
  # Display risk summary
  display_risk_summary(risk_metrics, risk_level, risk_alerts)
  
  return(list(
    has_positions = TRUE,
    positions = positions,
    metrics = risk_metrics,
    risk_level = risk_level,
    alerts = risk_alerts
  ))
}

#' Calculate risk metrics
calculate_risk_metrics <- function(positions) {
  
  # Portfolio metrics
  total_exposure <- sum(positions$size * positions$mark_price)
  total_pnl <- sum(positions$unrealized_pnl)
  total_margin <- sum(positions$margin)
  
  # Risk ratios
  portfolio_pnl_ratio <- if (total_margin > 0) total_pnl / total_margin else 0
  
  # Position concentration
  position_values <- positions$size * positions$mark_price
  max_position_value <- max(position_values)
  concentration_ratio <- if (total_exposure > 0) max_position_value / total_exposure else 0
  
  # Average leverage
  avg_leverage <- mean(positions$leverage)
  
  # Winning/Losing positions
  winning_positions <- sum(positions$unrealized_pnl > 0)
  losing_positions <- sum(positions$unrealized_pnl < 0)
  
  return(list(
    total_exposure = total_exposure,
    total_pnl = total_pnl,
    total_margin = total_margin,
    portfolio_pnl_ratio = portfolio_pnl_ratio,
    concentration_ratio = concentration_ratio,
    avg_leverage = avg_leverage,
    winning_positions = winning_positions,
    losing_positions = losing_positions,
    position_count = nrow(positions)
  ))
}

#' Check risk limits
check_risk_limits <- function(positions, metrics) {
  
  alerts <- list()
  
  # Check portfolio exposure
  balance <- get_account_balance()
  if (!is.null(balance)) {
    exposure_ratio <- metrics$total_exposure / balance$equity
    
    if (exposure_ratio > RISK_LIMITS$max_portfolio_exposure) {
      alerts[["HIGH_EXPOSURE"]] <- list(
        type = "PORTFOLIO_EXPOSURE",
        severity = "HIGH",
        message = paste("Portfolio exposure", round(exposure_ratio * 100), 
                        "% exceeds limit of", RISK_LIMITS$max_portfolio_exposure * 100, "%")
      )
    }
  }
  
  # Check position concentration
  if (metrics$concentration_ratio > RISK_LIMITS$max_single_position) {
    alerts[["HIGH_CONCENTRATION"]] <- list(
      type = "POSITION_CONCENTRATION",
      severity = "MEDIUM",
      message = paste("Largest position is", round(metrics$concentration_ratio * 100), 
                      "% of portfolio")
    )
  }
  
  # Check individual position losses
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    
    if (pos$pnl_ratio < -RISK_LIMITS$emergency_position_loss) {
      alert_id <- paste0("POSITION_LOSS_", pos$symbol)
      alerts[[alert_id]] <- list(
        type = "POSITION_LOSS",
        severity = "HIGH",
        symbol = pos$symbol,
        message = paste(pos$symbol, "loss:", round(pos$pnl_ratio * 100, 1), "%")
      )
    }
  }
  
  # Check portfolio loss
  if (metrics$portfolio_pnl_ratio < -RISK_LIMITS$emergency_portfolio_loss) {
    alerts[["PORTFOLIO_LOSS"]] <- list(
      type = "PORTFOLIO_LOSS",
      severity = "CRITICAL",
      message = paste("Portfolio loss:", round(metrics$portfolio_pnl_ratio * 100, 1), "%")
    )
  }
  
  return(alerts)
}

#' Determine overall risk level
determine_risk_level <- function(metrics, alerts) {
  
  # Count alert severities
  critical_count <- sum(sapply(alerts, function(a) a$severity == "CRITICAL"))
  high_count <- sum(sapply(alerts, function(a) a$severity == "HIGH"))
  medium_count <- sum(sapply(alerts, function(a) a$severity == "MEDIUM"))
  
  # Determine level
  if (critical_count > 0) {
    return("CRITICAL")
  } else if (high_count > 0) {
    return("HIGH")
  } else if (medium_count > 0 || metrics$avg_leverage > 15) {
    return("MEDIUM")
  } else {
    return("LOW")
  }
}

#' Display risk summary
display_risk_summary <- function(metrics, risk_level, alerts) {
  
  # Risk level icon
  risk_icon <- switch(risk_level,
                      "CRITICAL" = "🔴",
                      "HIGH" = "🟠",
                      "MEDIUM" = "🟡",
                      "LOW" = "🟢",
                      "⚪"
  )
  
  cat("\n📊 Portfolio Metrics:\n")
  cat("   Positions:", metrics$position_count, 
      "(", metrics$winning_positions, "W /", metrics$losing_positions, "L)\n")
  cat("   Total Exposure:", round(metrics$total_exposure, 2), "USDT\n")
  cat("   Total PnL:", sprintf("%+.2f USDT (%.1f%%)", 
                               metrics$total_pnl, 
                               metrics$portfolio_pnl_ratio * 100), "\n")
  cat("   Avg Leverage:", round(metrics$avg_leverage, 1), "x\n")
  cat("   Concentration:", round(metrics$concentration_ratio * 100, 1), "%\n")
  
  cat("\n", risk_icon, "Overall Risk Level:", risk_level, "\n")
  
  # Display alerts
  if (length(alerts) > 0) {
    cat("\n⚠️ Risk Alerts:\n")
    for (alert in alerts) {
      severity_icon <- switch(alert$severity,
                              "CRITICAL" = "🚨",
                              "HIGH" = "❗",
                              "MEDIUM" = "⚡",
                              "ℹ️"
      )
      cat("   ", severity_icon, alert$message, "\n")
    }
  }
}

# ==========================================================================================================
# 🛡️ TRAILING STOP LOSS SYSTEM
# ==========================================================================================================

#' Place trailing stop loss
place_trailing_sl <- function(symbol, trailing_percent = NULL, dry_run = TRUE) {
  
  # Get position
  positions <- get_current_positions()
  position <- positions[positions$symbol == symbol, ]
  
  if (nrow(position) == 0) {
    cat("❌ No position found for", symbol, "\n")
    return(list(success = FALSE, error = "No position"))
  }
  
  # Get trailing percentage
  config <- get_asset_config(symbol)
  trailing_percent <- trailing_percent %||% config$trailing_sl_percent %||% 2.5
  
  cat("🛡️ Trailing Stop Loss:", symbol, "at", trailing_percent, "%\n")
  
  pos <- position[1, ]
  ticker <- get_enhanced_ticker_data(symbol)
  
  if (is.null(ticker)) {
    cat("❌ Cannot get current price\n")
    return(list(success = FALSE, error = "No price data"))
  }
  
  current_price <- ticker$last_price
  
  # Calculate trailing stop price
  if (pos$side == "long") {
    trigger_price := current_price * (1 - trailing_percent / 100)
  } else {
    trigger_price := current_price * (1 + trailing_percent / 100)
  }
  
  cat("   📊 Position:", pos$side, pos$size, "\n")
  cat("   💰 Current Price:", round(current_price, 4), "USDT\n")
  cat("   🛡️ Trigger Price:", round(trigger_price, 4), "USDT\n")
  
  if (dry_run) {
    cat("🔍 DRY RUN MODE\n")
    return(list(success = TRUE, mode = "dry_run", trigger_price = trigger_price))
  }
  
  # Place trailing stop order
  order_side <- if (pos$side == "long") "sell" else "buy"
  
  tryCatch({
    response <- bitget_request(
      method = "POST",
      endpoint = API_CONFIG$endpoints$place_plan,
      params = list(
        symbol = symbol,
        productType = "USDT-FUTURES",
        marginMode = "crossed",
        marginCoin = "USDT",
        side = order_side,
        orderType = "market",
        size = as.character(pos$size),
        triggerPrice = as.character(trigger_price),
        triggerType = "mark_price",
        planType = "moving_plan",
        callbackRatio = as.character(trailing_percent / 100)
      )
    )
    
    if (!is.null(response) && response$code == "00000") {
      cat("✅ Trailing stop placed successfully!\n")
      return(list(success = TRUE, order_id = response$data$orderId))
    } else {
      cat("❌ Failed to place trailing stop\n")
      return(list(success = FALSE, error = response$msg %||% "Unknown error"))
    }
    
  }, error = function(e) {
    cat("❌ Error:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

#' Batch trailing stop loss for all positions
batch_trailing_sl <- function(trailing_percent = NULL, dry_run = TRUE) {
  
  cat("\n🔄 === BATCH TRAILING STOP LOSS === 🔄\n")
  
  positions <- get_current_positions()
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No positions to protect\n")
    return(list())
  }
  
  results <- list()
  
  for (i in 1:nrow(positions)) {
    symbol <- positions$symbol[i]
    cat("\n📍 Processing", symbol, "...\n")
    
    result <- place_trailing_sl(symbol, trailing_percent, dry_run)
    results[[symbol]] <- result
    
    if (!dry_run) Sys.sleep(0.5)  # Rate limiting
  }
  
  # Summary
  successful <- sum(sapply(results, function(r) r$success))
  cat("\n📊 Results:", successful, "/", length(results), "successful\n")
  
  return(results)
}

#' Dynamic trailing stop with volatility adjustment
place_dynamic_trailing_sl <- function(symbol, volatility_factor = 1.0, dry_run = TRUE) {
  
  cat("🔄 Dynamic Trailing SL for", symbol, "\n")
  
  # Calculate volatility
  volatility <- calculate_volatility(symbol)
  
  if (is.null(volatility)) {
    cat("⚠️ Cannot calculate volatility, using default\n")
    return(place_trailing_sl(symbol, dry_run = dry_run))
  }
  
  # Adjust trailing percentage based on volatility
  config <- get_asset_config(symbol)
  base_trailing <- config$trailing_sl_percent %||% 2.5
  
  # Higher volatility = wider trailing stop
  adjusted_trailing <- base_trailing * (1 + volatility * volatility_factor)
  adjusted_trailing <- min(adjusted_trailing, base_trailing * 2)  # Cap at 2x
  
  cat("   📊 Volatility:", round(volatility, 3), "\n")
  cat("   🔧 Adjusted Trailing:", round(adjusted_trailing, 2), "%\n")
  
  return(place_trailing_sl(symbol, adjusted_trailing, dry_run))
}

# ==========================================================================================================
# 🚨 EMERGENCY PROTECTION
# ==========================================================================================================

#' Check emergency conditions
check_emergency_conditions <- function() {
  
  cat("\n🚨 === EMERGENCY CHECK === 🚨\n")
  
  positions <- get_current_positions()
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No positions to monitor\n")
    return(list(triggered = FALSE))
  }
  
  balance <- get_account_balance()
  if (is.null(balance)) {
    cat("❌ Cannot fetch balance\n")
    return(list(triggered = FALSE))
  }
  
  emergency_actions <- list()
  
  # Calculate metrics
  total_pnl <- sum(positions$unrealized_pnl)
  portfolio_loss_ratio <- abs(total_pnl) / balance$equity
  
  # Check portfolio emergency
  if (total_pnl < 0 && portfolio_loss_ratio > RISK_LIMITS$emergency_portfolio_loss) {
    emergency_actions[["PORTFOLIO"]] <- list(
      type = "PORTFOLIO_EMERGENCY",
      severity = "CRITICAL",
      message = paste("Portfolio loss:", round(portfolio_loss_ratio * 100, 1), "%"),
      action = "CLOSE_ALL_POSITIONS"
    )
    
    cat("🚨 PORTFOLIO EMERGENCY TRIGGERED!\n")
    cat("   Total Loss:", round(total_pnl, 2), "USDT\n")
    cat("   Loss Ratio:", round(portfolio_loss_ratio * 100, 1), "%\n")
  }
  
  # Check individual positions
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    
    if (pos$pnl_ratio < -RISK_LIMITS$emergency_position_loss) {
      emergency_actions[[pos$symbol]] <- list(
        type = "POSITION_EMERGENCY",
        severity = "HIGH",
        symbol = pos$symbol,
        message = paste(pos$symbol, "loss:", round(pos$pnl_ratio * 100, 1), "%"),
        action = "CLOSE_POSITION"
      )
      
      cat("🚨", pos$symbol, "EMERGENCY!\n")
      cat("   Loss:", round(pos$unrealized_pnl, 2), "USDT",
          "(", round(pos$pnl_ratio * 100, 1), "%)\n")
    }
  }
  
  return(list(
    triggered = length(emergency_actions) > 0,
    actions = emergency_actions
  ))
}

#' Execute emergency closure
execute_emergency_closure <- function(symbol = NULL, close_all = FALSE, dry_run = TRUE) {
  
  if (dry_run) {
    cat("🔍 DRY RUN - Emergency closure simulation\n")
  }
  
  # Load order manager for closing positions
  source("C:/freeding/tbot202506/r_analysis/riskassetm/trading/order_manager.r")
  
  if (close_all) {
    cat("🚨 EMERGENCY: CLOSING ALL POSITIONS\n")
    return(close_all_positions(dry_run))
  } else if (!is.null(symbol)) {
    cat("🚨 EMERGENCY: CLOSING", symbol, "\n")
    return(quick_sell_all(symbol))
  }
  
  return(list(success = FALSE, error = "No action specified"))
}

# ==========================================================================================================
# 📊 POSITION MONITORING
# ==========================================================================================================

#' Monitor positions with alerts
monitor_positions <- function(alert_threshold = 0.05) {
  
  cat("\n📊 === POSITION MONITORING === 📊\n")
  
  positions <- get_current_positions()
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No positions to monitor\n")
    return(list())
  }
  
  alerts <- list()
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    
    cat("\n🔸", pos$symbol, ":\n")
    cat("   Position:", pos$side, pos$size, "@ Entry:", round(pos$avg_price, 4), "\n")
    cat("   Current:", round(pos$mark_price, 4), "| PnL:", 
        sprintf("%+.2f USDT (%.1f%%)", pos$unrealized_pnl, pos$pnl_ratio * 100), "\n")
    cat("   Risk Score:", round(pos$risk_score, 3), "| Leverage:", pos$leverage, "x\n")
    
    # Check for alerts
    if (abs(pos$pnl_ratio) > alert_threshold) {
      alert_type <- if (pos$pnl_ratio > 0) "PROFIT" else "LOSS"
      alerts[[pos$symbol]] <- list(
        type = alert_type,
        pnl_ratio = pos$pnl_ratio,
        message = paste(pos$symbol, alert_type, round(pos$pnl_ratio * 100, 1), "%")
      )
      
      if (alert_type == "PROFIT") {
        cat("   💰 PROFIT ALERT: Consider taking profits\n")
      } else {
        cat("   ⚠️ LOSS ALERT: Consider risk management\n")
      }
    }
  }
  
  return(list(
    positions = positions,
    alerts = alerts
  ))
}

# ==========================================================================================================
# 🔧 UTILITY FUNCTIONS
# ==========================================================================================================

#' Calculate market volatility
calculate_volatility <- function(symbol, periods = 24) {
  
  tryCatch({
    # Get kline data
    klines <- get_kline_data(symbol, "1h", periods)
    
    if (is.null(klines) || nrow(klines) < 5) {
      return(NULL)
    }
    
    # Calculate returns
    returns <- diff(log(klines$close))
    
    # Calculate volatility
    volatility <- sd(returns, na.rm = TRUE)
    
    # Normalize (0-1 scale)
    normalized_volatility <- min(max(volatility / 0.1, 0), 1)
    
    return(normalized_volatility)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Assess portfolio health
assess_portfolio_health <- function() {
  
  cat("\n🏥 === PORTFOLIO HEALTH CHECK === 🏥\n")
  
  # Get balance
  balance <- get_account_balance()
  if (!is.null(balance)) {
    cat("💰 Account Equity:", round(balance$equity, 2), "USDT\n")
    cat("   Available:", round(balance$available, 2), "USDT\n")
    cat("   Margin Used:", round(balance$locked, 2), "USDT\n")
    cat("   Unrealized PnL:", sprintf("%+.2f", balance$unrealized_pnl), "USDT\n")
  }
  
  # Risk analysis
  risk_analysis <- analyze_portfolio_risk()
  
  # Emergency check
  emergency_check <- check_emergency_conditions()
  
  # Overall health
  health_status <- "HEALTHY"
  
  if (emergency_check$triggered) {
    health_status <- "CRITICAL"
  } else if (risk_analysis$risk_level %in% c("HIGH", "CRITICAL")) {
    health_status <- "UNHEALTHY"
  } else if (risk_analysis$risk_level == "MEDIUM") {
    health_status <- "CAUTION"
  }
  
  health_icon <- switch(health_status,
                        "HEALTHY" = "🟢",
                        "CAUTION" = "🟡",
                        "UNHEALTHY" = "🟠",
                        "CRITICAL" = "🔴",
                        "⚪"
  )
  
  cat("\n", health_icon, "Portfolio Health:", health_status, "\n")
  
  return(list(
    health_status = health_status,
    balance = balance,
    risk_analysis = risk_analysis,
    emergency_status = emergency_check
  ))
}

# ==========================================================================================================
# 🚀 QUICK ACCESS FUNCTIONS
# ==========================================================================================================

#' Quick risk check
quick_risk_check <- function() {
  analyze_portfolio_risk()
}

#' Quick protection for all positions
quick_protect_all <- function() {
  batch_trailing_sl(dry_run = FALSE)
}

#' Emergency stop all
emergency_stop_all <- function() {
  emergency_check <- check_emergency_conditions()
  
  if (emergency_check$triggered) {
    cat("🚨 EMERGENCY CONDITIONS DETECTED!\n")
    confirm <- readline("Execute emergency closure? (yes/no): ")
    
    if (tolower(confirm) == "yes") {
      execute_emergency_closure(close_all = TRUE, dry_run = FALSE)
    }
  } else {
    cat("✅ No emergency conditions detected\n")
  }
}

cat("✅ RISK_MANAGER.R loaded successfully!\n")
cat("🛡️ Risk management functions ready\n")
cat("🚨 Emergency protection enabled\n")