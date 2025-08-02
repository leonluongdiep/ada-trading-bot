# ==========================================================================================================
# 🔍 ADVANCED MARKET SCANNER V1
# ==========================================================================================================
# Pfad: analytics/market_scanner.r
# Kontinuierliche Marktüberwachung und Opportunity Detection
# Multi-Asset Scanner mit ML-basierten Signalen
# ==========================================================================================================

cat("🔍 Loading Advanced Market Scanner V1...\n")

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

if (!exists("RISK_MANAGER_LOADED")) {
  source("../trading/risk_manager.r")
  RISK_MANAGER_LOADED <- TRUE
}

# ==========================================================================================================
# 📊 SCANNER CONFIGURATION
# ==========================================================================================================

SCANNER_CONFIG <- list(
  # Scan parameters
  scan_interval_seconds = 60,      # Scan every minute
  quick_scan_symbols = 10,         # Top 10 for quick scans
  full_scan_symbols = 50,          # Top 50 for full scans
  
  # Alert thresholds
  alerts = list(
    volume_spike = 3,              # 3x average volume
    price_breakout = 2,            # 2% price move
    oi_surge = 15,                 # 15% OI change
    volatility_spike = 2.5,        # 2.5x normal volatility
    momentum_threshold = 70        # RSI > 70 or < 30
  ),
  
  # Opportunity scoring
  opportunity_weights = list(
    technical = 0.25,
    volume = 0.20,
    oi_flow = 0.30,
    momentum = 0.15,
    volatility = 0.10
  ),
  
  # Market regimes
  regimes = list(
    trending = list(min_trend_strength = 0.7, volatility_cap = 0.3),
    ranging = list(max_trend_strength = 0.3, min_volatility = 0.1),
    volatile = list(min_volatility = 0.3, volume_multiplier = 1.5),
    quiet = list(max_volatility = 0.1, max_volume_ratio = 0.7)
  )
)

# ==========================================================================================================
# 🌐 MARKET UNIVERSE MANAGEMENT
# ==========================================================================================================

#' Get trading universe
get_trading_universe <- function(top_n = 50) {
  tryCatch({
    # In production, this would fetch from API
    # For now, use predefined universe
    universe <- c(
      # Major coins
      "BTCUSDT", "ETHUSDT", "BNBUSDT", "SOLUSDT", "XRPUSDT",
      # DeFi
      "AVAXUSDT", "LINKUSDT", "UNIUSDT", "AAVEUSDT", "SUSHIUSDT",
      # Layer 1s
      "ADAUSDT", "DOTUSDT", "MATICUSDT", "NEARUSDT", "ATOMUSDT",
      # Layer 2s
      "ARBUSDT", "OPUSDT", "IMXUSDT",
      # Meme/Gaming
      "DOGEUSDT", "SHIBUSDT", "SANDUSDT", "MANAUSDT", "AXSUSDT",
      # Others
      "FTMUSDT", "ALGOUSDT", "VETUSDT", "ICPUSDT", "FILUSDT",
      "HBARUSDT", "APTUSDT", "LDOUSDT", "STXUSDT", "INJUSDT"
    )
    
    return(head(universe, top_n))
    
  }, error = function(e) {
    # Fallback to core assets
    return(PORTFOLIO_ASSETS)
  })
}

#' Update market universe based on volume
update_market_universe <- function() {
  cat("\n🌐 Updating market universe...\n")
  
  # Get all available symbols
  all_symbols <- get_all_tradeable_symbols()
  
  if (length(all_symbols) == 0) {
    return(get_trading_universe())
  }
  
  # Fetch volume data for all symbols
  volume_data <- list()
  
  for (symbol in all_symbols) {
    ticker <- get_ticker(symbol)
    if (!is.null(ticker)) {
      volume_data[[symbol]] <- as.numeric(ticker$volume_24h_usdt)
    }
    
    # Rate limit
    if (length(volume_data) %% 10 == 0) {
      Sys.sleep(1)
    }
  }
  
  # Sort by volume
  sorted_symbols <- names(sort(unlist(volume_data), decreasing = TRUE))
  
  # Save to cache
  MARKET_UNIVERSE_CACHE <<- list(
    symbols = sorted_symbols,
    volumes = volume_data,
    updated = Sys.time()
  )
  
  return(sorted_symbols)
}

# ==========================================================================================================
# 🔍 MARKET SCANNING ENGINE
# ==========================================================================================================

#' Run comprehensive market scan
run_market_scan <- function(scan_type = "quick", symbols = NULL) {
  
  cat("\n🔍 === MARKET SCAN -", toupper(scan_type), "MODE === 🔍\n")
  cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Determine symbols to scan
  if (is.null(symbols)) {
    n_symbols <- if (scan_type == "quick") {
      SCANNER_CONFIG$quick_scan_symbols
    } else {
      SCANNER_CONFIG$full_scan_symbols
    }
    symbols <- get_trading_universe(n_symbols)
  }
  
  cat("Scanning", length(symbols), "assets...\n")
  
  # Initialize results
  scan_results <- list()
  opportunities <- list()
  alerts <- list()
  
  # Progress tracking
  pb <- txtProgressBar(min = 0, max = length(symbols), style = 3)
  
  for (i in seq_along(symbols)) {
    symbol <- symbols[i]
    
    # Scan individual asset
    asset_scan <- scan_single_asset(symbol, scan_type)
    
    if (!is.null(asset_scan)) {
      scan_results[[symbol]] <- asset_scan
      
      # Check for opportunities
      if (asset_scan$opportunity_score > 0.7) {
        opportunities[[symbol]] <- asset_scan
      }
      
      # Check for alerts
      if (length(asset_scan$alerts) > 0) {
        alerts[[symbol]] <- asset_scan$alerts
      }
    }
    
    # Update progress
    setTxtProgressBar(pb, i)
    
    # Rate limiting
    Sys.sleep(API_CONFIG$rate_limit_delay)
  }
  
  close(pb)
  
  # Analyze market regime
  market_regime <- analyze_market_regime(scan_results)
  
  # Rank opportunities
  ranked_opportunities <- rank_opportunities(opportunities)
  
  # Generate report
  scan_report <- generate_scan_report(
    scan_results, ranked_opportunities, alerts, market_regime
  )
  
  # Display results
  display_scan_results(scan_report)
  
  return(scan_report)
}

#' Scan single asset
scan_single_asset <- function(symbol, scan_type = "quick") {
  tryCatch({
    # Get market data
    market_data <- fetch_asset_market_data(symbol)
    
    if (is.null(market_data)) return(NULL)
    
    # Run technical analysis
    technical_signals <- analyze_technicals(market_data)
    
    # Analyze volume
    volume_analysis <- analyze_volume_profile(symbol)
    
    # Get OI analysis (if available)
    oi_analysis <- if (scan_type == "full") {
      analyze_oi_momentum(symbol)
    } else {
      list(score = 0.5, signal = "NEUTRAL")
    }
    
    # Calculate momentum
    momentum_analysis <- calculate_momentum_metrics(market_data)
    
    # Detect patterns
    patterns <- if (scan_type == "full") {
      detect_chart_patterns(market_data)
    } else {
      list()
    }
    
    # Check for alerts
    asset_alerts <- check_alert_conditions(
      symbol, market_data, volume_analysis, oi_analysis, momentum_analysis
    )
    
    # Calculate opportunity score
    opportunity_score <- calculate_opportunity_score(
      technical_signals, volume_analysis, oi_analysis, 
      momentum_analysis, market_data$volatility
    )
    
    return(list(
      symbol = symbol,
      timestamp = Sys.time(),
      price = market_data$current_price,
      change_24h = market_data$change_24h_pct,
      technical = technical_signals,
      volume = volume_analysis,
      oi = oi_analysis,
      momentum = momentum_analysis,
      patterns = patterns,
      alerts = asset_alerts,
      opportunity_score = opportunity_score,
      market_data = market_data
    ))
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Fetch comprehensive market data
fetch_asset_market_data <- function(symbol) {
  tryCatch({
    # Get ticker data
    ticker <- get_ticker(symbol)
    if (is.null(ticker)) return(NULL)
    
    # Get recent candles
    candles_1h <- get_candles(symbol, "1h", 24)
    candles_15m <- get_candles(symbol, "15m", 96)
    
    if (is.null(candles_1h) || nrow(candles_1h) < 10) return(NULL)
    
    # Calculate metrics
    current_price <- as.numeric(ticker$last)
    high_24h <- as.numeric(ticker$high_24h)
    low_24h <- as.numeric(ticker$low_24h)
    volume_24h <- as.numeric(ticker$volume_24h_usdt)
    
    # Price metrics
    change_24h_pct <- as.numeric(ticker$change_24h_pct)
    range_24h <- (high_24h - low_24h) / low_24h * 100
    
    # Volatility calculation
    returns_1h <- diff(log(as.numeric(candles_1h$close)))
    volatility <- sd(returns_1h, na.rm = TRUE) * sqrt(24 * 365)
    
    # Trend calculation
    trend <- calculate_trend_metrics(candles_1h)
    
    return(list(
      current_price = current_price,
      high_24h = high_24h,
      low_24h = low_24h,
      volume_24h = volume_24h,
      change_24h_pct = change_24h_pct,
      range_24h = range_24h,
      volatility = volatility,
      trend = trend,
      candles_1h = candles_1h,
      candles_15m = candles_15m
    ))
    
  }, error = function(e) {
    return(NULL)
  })
}

# ==========================================================================================================
# 📈 TECHNICAL ANALYSIS
# ==========================================================================================================

#' Comprehensive technical analysis
analyze_technicals <- function(market_data) {
  
  candles <- market_data$candles_1h
  closes <- as.numeric(candles$close)
  highs <- as.numeric(candles$high)
  lows <- as.numeric(candles$low)
  
  # Moving averages
  ma_analysis <- calculate_ma_signals(closes)
  
  # Support/Resistance
  sr_levels <- calculate_support_resistance(highs, lows, closes)
  
  # Trend strength
  trend_strength <- market_data$trend$strength
  
  # Price position
  price_position <- (market_data$current_price - market_data$low_24h) / 
                   (market_data$high_24h - market_data$low_24h)
  
  # Technical score
  tech_score <- calculate_technical_score(
    ma_analysis, sr_levels, trend_strength, price_position
  )
  
  return(list(
    score = tech_score,
    ma_signals = ma_analysis,
    sr_levels = sr_levels,
    trend_strength = trend_strength,
    price_position = price_position,
    signal = generate_technical_signal(tech_score, trend_strength)
  ))
}

#' Calculate MA signals
calculate_ma_signals <- function(prices) {
  
  # Calculate MAs
  ma_7 <- mean(tail(prices, 7))
  ma_14 <- mean(tail(prices, 14))
  ma_21 <- mean(tail(prices, 21))
  
  current_price <- tail(prices, 1)
  
  # MA alignment
  bullish_alignment <- ma_7 > ma_14 && ma_14 > ma_21
  bearish_alignment <- ma_7 < ma_14 && ma_14 < ma_21
  
  # Price vs MAs
  above_mas <- current_price > ma_7 && current_price > ma_14
  below_mas <- current_price < ma_7 && current_price < ma_14
  
  return(list(
    ma_7 = ma_7,
    ma_14 = ma_14,
    ma_21 = ma_21,
    bullish_alignment = bullish_alignment,
    bearish_alignment = bearish_alignment,
    price_above_mas = above_mas,
    price_below_mas = below_mas
  ))
}

#' Calculate support and resistance
calculate_support_resistance <- function(highs, lows, closes) {
  
  # Find local extremes
  n <- length(closes)
  
  # Resistance levels (local highs)
  resistance_levels <- c()
  for (i in 3:(n-2)) {
    if (highs[i] > highs[i-1] && highs[i] > highs[i-2] &&
        highs[i] > highs[i+1] && highs[i] > highs[i+2]) {
      resistance_levels <- c(resistance_levels, highs[i])
    }
  }
  
  # Support levels (local lows)
  support_levels <- c()
  for (i in 3:(n-2)) {
    if (lows[i] < lows[i-1] && lows[i] < lows[i-2] &&
        lows[i] < lows[i+1] && lows[i] < lows[i+2]) {
      support_levels <- c(support_levels, lows[i])
    }
  }
  
  # Get strongest levels
  current_price <- tail(closes, 1)
  
  nearest_resistance <- if (length(resistance_levels) > 0) {
    min(resistance_levels[resistance_levels > current_price], na.rm = TRUE)
  } else {
    current_price * 1.05
  }
  
  nearest_support <- if (length(support_levels) > 0) {
    max(support_levels[support_levels < current_price], na.rm = TRUE)
  } else {
    current_price * 0.95
  }
  
  return(list(
    resistance = nearest_resistance,
    support = nearest_support,
    resistance_distance = (nearest_resistance - current_price) / current_price * 100,
    support_distance = (current_price - nearest_support) / current_price * 100
  ))
}

# ==========================================================================================================
# 📊 MOMENTUM ANALYSIS
# ==========================================================================================================

#' Calculate momentum metrics
calculate_momentum_metrics <- function(market_data) {
  
  closes <- as.numeric(market_data$candles_15m$close)
  
  # RSI
  rsi <- calculate_rsi(closes, 14)
  
  # MACD (simplified)
  macd_data <- calculate_macd(closes)
  
  # Momentum oscillator
  momentum <- (tail(closes, 1) - closes[length(closes) - 10]) / closes[length(closes) - 10] * 100
  
  # Volume momentum
  volumes <- as.numeric(market_data$candles_15m$volume)
  volume_momentum <- mean(tail(volumes, 4)) / mean(volumes) 
  
  # Momentum score
  momentum_score <- score_momentum(rsi, macd_data, momentum, volume_momentum)
  
  return(list(
    rsi = rsi,
    macd = macd_data,
    price_momentum = momentum,
    volume_momentum = volume_momentum,
    score = momentum_score,
    signal = categorize_momentum(momentum_score, rsi)
  ))
}

#' Calculate RSI
calculate_rsi <- function(prices, period = 14) {
  
  if (length(prices) < period + 1) return(50)
  
  # Calculate price changes
  changes <- diff(prices)
  
  # Separate gains and losses
  gains <- ifelse(changes > 0, changes, 0)
  losses <- ifelse(changes < 0, -changes, 0)
  
  # Calculate average gains and losses
  avg_gain <- mean(tail(gains, period))
  avg_loss <- mean(tail(losses, period))
  
  # Calculate RS and RSI
  if (avg_loss == 0) {
    rsi <- 100
  } else {
    rs <- avg_gain / avg_loss
    rsi <- 100 - (100 / (1 + rs))
  }
  
  return(rsi)
}

#' Calculate MACD
calculate_macd <- function(prices) {
  
  if (length(prices) < 26) {
    return(list(macd = 0, signal = 0, histogram = 0))
  }
  
  # EMA calculation helper
  ema <- function(x, n) {
    alpha <- 2 / (n + 1)
    ema_values <- numeric(length(x))
    ema_values[1] <- x[1]
    
    for (i in 2:length(x)) {
      ema_values[i] <- alpha * x[i] + (1 - alpha) * ema_values[i-1]
    }
    
    return(ema_values)
  }
  
  # Calculate MACD
  ema_12 <- tail(ema(prices, 12), 1)
  ema_26 <- tail(ema(prices, 26), 1)
  
  macd_line <- ema_12 - ema_26
  
  # For signal line, we'd need historical MACD values
  # Simplified version
  signal_line <- macd_line * 0.9
  histogram <- macd_line - signal_line
  
  return(list(
    macd = macd_line,
    signal = signal_line,
    histogram = histogram
  ))
}

# ==========================================================================================================
# 🚨 ALERT DETECTION
# ==========================================================================================================

#' Check alert conditions
check_alert_conditions <- function(symbol, market_data, volume_analysis, oi_analysis, momentum_analysis) {
  
  alerts <- list()
  
  # Volume spike alert
  if (volume_analysis$volume_ratio > SCANNER_CONFIG$alerts$volume_spike) {
    alerts$volume_spike <- list(
      type = "VOLUME_SPIKE",
      severity = "HIGH",
      message = paste("Volume", round(volume_analysis$volume_ratio, 1), "x average"),
      value = volume_analysis$volume_ratio
    )
  }
  
  # Price breakout alert
  if (abs(market_data$change_24h_pct) > SCANNER_CONFIG$alerts$price_breakout) {
    alerts$price_breakout <- list(
      type = "PRICE_BREAKOUT",
      severity = if (abs(market_data$change_24h_pct) > 5) "HIGH" else "MEDIUM",
      message = paste("Price moved", round(market_data$change_24h_pct, 1), "% in 24h"),
      value = market_data$change_24h_pct
    )
  }
  
  # OI surge alert
  if (!is.null(oi_analysis$oi_change_pct) && 
      abs(oi_analysis$oi_change_pct) > SCANNER_CONFIG$alerts$oi_surge) {
    alerts$oi_surge <- list(
      type = "OI_SURGE",
      severity = "HIGH",
      message = paste("OI changed", round(oi_analysis$oi_change_pct, 1), "%"),
      value = oi_analysis$oi_change_pct
    )
  }
  
  # Volatility spike alert
  normal_volatility <- 0.3  # 30% annualized
  if (market_data$volatility > normal_volatility * SCANNER_CONFIG$alerts$volatility_spike) {
    alerts$volatility_spike <- list(
      type = "VOLATILITY_SPIKE",
      severity = "MEDIUM",
      message = paste("Volatility at", round(market_data$volatility * 100, 0), "%"),
      value = market_data$volatility
    )
  }
  
  # Momentum alert (overbought/oversold)
  if (momentum_analysis$rsi > SCANNER_CONFIG$alerts$momentum_threshold) {
    alerts$overbought <- list(
      type = "OVERBOUGHT",
      severity = "MEDIUM",
      message = paste("RSI at", round(momentum_analysis$rsi, 0)),
      value = momentum_analysis$rsi
    )
  } else if (momentum_analysis$rsi < (100 - SCANNER_CONFIG$alerts$momentum_threshold)) {
    alerts$oversold <- list(
      type = "OVERSOLD",
      severity = "MEDIUM",
      message = paste("RSI at", round(momentum_analysis$rsi, 0)),
      value = momentum_analysis$rsi
    )
  }
  
  return(alerts)
}

# ==========================================================================================================
# 📊 OPPORTUNITY SCORING
# ==========================================================================================================

#' Calculate opportunity score
calculate_opportunity_score <- function(technical, volume, oi, momentum, volatility) {
  
  weights <- SCANNER_CONFIG$opportunity_weights
  
  # Normalize volatility score (lower is better for opportunities)
  volatility_score <- 1 - min(volatility / 0.5, 1)
  
  # Calculate weighted score
  opportunity_score <- (
    technical$score * weights$technical +
    volume$score * weights$volume +
    oi$score * weights$oi_flow +
    momentum$score * weights$momentum +
    volatility_score * weights$volatility
  )
  
  # Boost score for aligned signals
  alignment_bonus <- 0
  
  # Check signal alignment
  signals <- c(technical$signal, volume$profile, oi$signal, momentum$signal)
  buy_signals <- sum(grepl("BUY|BULLISH", signals, ignore.case = TRUE))
  sell_signals <- sum(grepl("SELL|BEARISH", signals, ignore.case = TRUE))
  
  if (buy_signals >= 3 || sell_signals >= 3) {
    alignment_bonus <- 0.1
  }
  
  return(min(opportunity_score + alignment_bonus, 1))
}

# ==========================================================================================================
# 🌍 MARKET REGIME ANALYSIS
# ==========================================================================================================

#' Analyze overall market regime
analyze_market_regime <- function(scan_results) {
  
  if (length(scan_results) == 0) {
    return(list(regime = "UNKNOWN", confidence = 0))
  }
  
  # Aggregate metrics
  avg_volatility <- mean(sapply(scan_results, function(x) x$market_data$volatility), na.rm = TRUE)
  avg_volume_ratio <- mean(sapply(scan_results, function(x) x$volume$volume_ratio), na.rm = TRUE)
  
  # Trend alignment
  bullish_count <- sum(sapply(scan_results, function(x) 
    x$technical$signal %in% c("BUY", "STRONG_BUY")), na.rm = TRUE)
  bearish_count <- sum(sapply(scan_results, function(x) 
    x$technical$signal %in% c("SELL", "STRONG_SELL")), na.rm = TRUE)
  
  total_assets <- length(scan_results)
  trend_strength <- abs(bullish_count - bearish_count) / total_assets
  
  # Determine regime
  regime <- "NORMAL"
  confidence <- 0.5
  
  if (trend_strength > SCANNER_CONFIG$regimes$trending$min_trend_strength &&
      avg_volatility < SCANNER_CONFIG$regimes$trending$volatility_cap) {
    regime <- if (bullish_count > bearish_count) "TRENDING_UP" else "TRENDING_DOWN"
    confidence <- 0.8
  } else if (trend_strength < SCANNER_CONFIG$regimes$ranging$max_trend_strength &&
             avg_volatility > SCANNER_CONFIG$regimes$ranging$min_volatility) {
    regime <- "RANGING"
    confidence <- 0.7
  } else if (avg_volatility > SCANNER_CONFIG$regimes$volatile$min_volatility) {
    regime <- "VOLATILE"
    confidence <- 0.75
  } else if (avg_volatility < SCANNER_CONFIG$regimes$quiet$max_volatility &&
             avg_volume_ratio < SCANNER_CONFIG$regimes$quiet$max_volume_ratio) {
    regime <- "QUIET"
    confidence <- 0.7
  }
  
  return(list(
    regime = regime,
    confidence = confidence,
    metrics = list(
      avg_volatility = avg_volatility,
      avg_volume_ratio = avg_volume_ratio,
      trend_strength = trend_strength,
      bullish_percentage = bullish_count / total_assets * 100,
      bearish_percentage = bearish_count / total_assets * 100
    )
  ))
}

# ==========================================================================================================
# 📊 RANKING & REPORTING
# ==========================================================================================================

#' Rank opportunities
rank_opportunities <- function(opportunities) {
  
  if (length(opportunities) == 0) return(list())
  
  # Sort by opportunity score
  sorted_opps <- opportunities[order(
    sapply(opportunities, function(x) x$opportunity_score),
    decreasing = TRUE
  )]
  
  # Add ranking and categorization
  for (i in seq_along(sorted_opps)) {
    sorted_opps[[i]]$rank <- i
    
    # Categorize opportunity
    score <- sorted_opps[[i]]$opportunity_score
    sorted_opps[[i]]$category <- if (score > 0.85) "HOT"
                                 else if (score > 0.75) "STRONG"
                                 else if (score > 0.65) "MODERATE"
                                 else "WATCH"
  }
  
  return(sorted_opps)
}

#' Generate scan report
generate_scan_report <- function(scan_results, opportunities, alerts, market_regime) {
  
  report <- list(
    timestamp = Sys.time(),
    assets_scanned = length(scan_results),
    market_regime = market_regime,
    
    opportunities = list(
      count = length(opportunities),
      top_5 = head(opportunities, 5),
      by_category = table(sapply(opportunities, function(x) x$category))
    ),
    
    alerts = list(
      count = length(alerts),
      by_type = table(unlist(lapply(alerts, function(x) 
        sapply(x, function(y) y$type)))),
      high_severity = Filter(function(x) 
        any(sapply(x, function(y) y$severity == "HIGH")), alerts)
    ),
    
    summary_stats = list(
      avg_change_24h = mean(sapply(scan_results, function(x) 
        x$change_24h), na.rm = TRUE),
      avg_opportunity_score = mean(sapply(scan_results, function(x) 
        x$opportunity_score), na.rm = TRUE),
      bullish_signals = sum(sapply(scan_results, function(x) 
        x$technical$signal %in% c("BUY", "STRONG_BUY")), na.rm = TRUE),
      bearish_signals = sum(sapply(scan_results, function(x) 
        x$technical$signal %in% c("SELL", "STRONG_SELL")), na.rm = TRUE)
    ),
    
    raw_results = scan_results
  )
  
  return(report)
}

# ==========================================================================================================
# 🖥️ DISPLAY FUNCTIONS
# ==========================================================================================================

#' Display scan results
display_scan_results <- function(report) {
  
  cat("\n🌍 === MARKET SCAN REPORT === 🌍\n")
  cat("Timestamp:", format(report$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Assets Scanned:", report$assets_scanned, "\n")
  
  # Market regime
  cat("\n📊 Market Regime:", report$market_regime$regime, 
      "(", round(report$market_regime$confidence * 100), "% confidence)\n")
  cat("├─ Avg Volatility:", round(report$market_regime$metrics$avg_volatility * 100, 1), "%\n")
  cat("├─ Bullish Assets:", round(report$market_regime$metrics$bullish_percentage, 1), "%\n")
  cat("└─ Bearish Assets:", round(report$market_regime$metrics$bearish_percentage, 1), "%\n")
  
  # Top opportunities
  if (report$opportunities$count > 0) {
    cat("\n🎯 Top Trading Opportunities:\n")
    
    for (i in seq_len(min(5, length(report$opportunities$top_5)))) {
      opp <- report$opportunities$top_5[[i]]
      
      cat(sprintf("\n%d. %s %s [%s] - Score: %.2f\n",
                  i,
                  if (opp$technical$signal %in% c("BUY", "STRONG_BUY")) "📈" else "📉",
                  names(report$opportunities$top_5)[i],
                  opp$category,
                  opp$opportunity_score))
      
      cat(sprintf("   Price: $%.4f | 24h: %+.1f%% | Vol: %.1fx | RSI: %.0f\n",
                  opp$price,
                  opp$change_24h,
                  opp$volume$volume_ratio,
                  opp$momentum$rsi))
      
      # Show signals
      cat("   Signals:", opp$technical$signal, "|",
          opp$volume$profile, "|",
          opp$momentum$signal, "\n")
    }
  } else {
    cat("\n📊 No strong opportunities detected\n")
  }
  
  # Alerts summary
  if (report$alerts$count > 0) {
    cat("\n🚨 Active Alerts:", report$alerts$count, "\n")
    print(report$alerts$by_type)
    
    # Show high severity alerts
    if (length(report$alerts$high_severity) > 0) {
      cat("\n⚠️ High Severity Alerts:\n")
      
      for (symbol in names(report$alerts$high_severity)) {
        alerts <- report$alerts$high_severity[[symbol]]
        high_alerts <- Filter(function(x) x$severity == "HIGH", alerts)
        
        for (alert in high_alerts) {
          cat("  •", symbol, "-", alert$message, "\n")
        }
      }
    }
  }
  
  # Summary
  cat("\n📊 Market Summary:\n")
  cat("├─ Avg 24h Change:", round(report$summary_stats$avg_change_24h, 2), "%\n")
  cat("├─ Avg Opportunity Score:", round(report$summary_stats$avg_opportunity_score, 3), "\n")
  cat("├─ Bullish Signals:", report$summary_stats$bullish_signals, "\n")
  cat("└─ Bearish Signals:", report$summary_stats$bearish_signals, "\n")
}

#' Create visual market heatmap (text-based)
create_market_heatmap <- function(scan_results, top_n = 20) {
  
  cat("\n🗺️ === MARKET HEATMAP === 🗺️\n\n")
  
  # Sort by absolute price change
  sorted_results <- scan_results[order(
    abs(sapply(scan_results, function(x) x$change_24h)),
    decreasing = TRUE
  )]
  
  # Take top N
  display_results <- head(sorted_results, top_n)
  
  # Create heatmap
  for (result in display_results) {
    symbol <- result$symbol
    change <- result$change_24h
    score <- result$opportunity_score
    
    # Color coding (emoji-based)
    color <- if (change > 5) "🟩🟩🟩"
            else if (change > 2) "🟩🟩"
            else if (change > 0) "🟩"
            else if (change > -2) "🟥"
            else if (change > -5) "🟥🟥"
            else "🟥🟥🟥"
    
    # Opportunity indicator
    opp_indicator <- if (score > 0.8) "⭐"
                    else if (score > 0.7) "🔸"
                    else ""
    
    cat(sprintf("%-10s %s %+6.2f%% %s\n",
                symbol, color, change, opp_indicator))
  }
  
  cat("\n🟩 Gains | 🟥 Losses | ⭐ High Opportunity | 🔸 Good Opportunity\n")
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

#' Calculate trend metrics
calculate_trend_metrics <- function(candles) {
  
  closes <- as.numeric(candles$close)
  
  # Linear regression trend
  x <- 1:length(closes)
  trend_fit <- lm(closes ~ x)
  
  slope <- coef(trend_fit)[2]
  r_squared <- summary(trend_fit)$r.squared
  
  # Normalize slope
  avg_price <- mean(closes)
  normalized_slope <- slope / avg_price * 100 * length(closes)
  
  # Determine trend
  trend_direction <- if (normalized_slope > 1) "UPTREND"
                    else if (normalized_slope < -1) "DOWNTREND"
                    else "SIDEWAYS"
  
  return(list(
    direction = trend_direction,
    strength = r_squared,
    slope = normalized_slope
  ))
}

#' Calculate technical score
calculate_technical_score <- function(ma_analysis, sr_levels, trend_strength, price_position) {
  
  score <- 0.5  # Neutral base
  
  # MA alignment bonus
  if (ma_analysis$bullish_alignment) score <- score + 0.15
  if (ma_analysis$bearish_alignment) score <- score - 0.15
  
  # Price vs MA bonus
  if (ma_analysis$price_above_mas) score <- score + 0.1
  if (ma_analysis$price_below_mas) score <- score - 0.1
  
  # Trend strength bonus
  score <- score + (trend_strength - 0.5) * 0.2
  
  # S/R proximity
  if (sr_levels$support_distance < 2) score <- score + 0.1
  if (sr_levels$resistance_distance < 2) score <- score - 0.1
  
  # Price position bonus
  if (price_position > 0.8) score <- score + 0.05
  if (price_position < 0.2) score <- score - 0.05
  
  return(max(0, min(1, score)))
}

#' Generate technical signal
generate_technical_signal <- function(tech_score, trend_strength) {
  
  if (tech_score > 0.75 && trend_strength > 0.6) return("STRONG_BUY")
  if (tech_score > 0.65) return("BUY")
  if (tech_score < 0.25 && trend_strength > 0.6) return("STRONG_SELL")
  if (tech_score < 0.35) return("SELL")
  
  return("NEUTRAL")
}

#' Score momentum
score_momentum <- function(rsi, macd_data, price_momentum, volume_momentum) {
  
  score <- 0.5
  
  # RSI scoring
  if (rsi > 70) score <- score + 0.2
  else if (rsi > 60) score <- score + 0.1
  else if (rsi < 30) score <- score - 0.2
  else if (rsi < 40) score <- score - 0.1
  
  # MACD scoring
  if (macd_data$histogram > 0) score <- score + 0.1
  else score <- score - 0.1
  
  # Price momentum
  if (abs(price_momentum) > 5) {
    score <- score + sign(price_momentum) * 0.15
  }
  
  # Volume momentum
  if (volume_momentum > 1.5) score <- score + 0.1
  
  return(max(0, min(1, score)))
}

#' Categorize momentum
categorize_momentum <- function(momentum_score, rsi) {
  
  if (momentum_score > 0.7 || rsi > 70) return("OVERBOUGHT")
  if (momentum_score > 0.6) return("BULLISH")
  if (momentum_score < 0.3 || rsi < 30) return("OVERSOLD")
  if (momentum_score < 0.4) return("BEARISH")
  
  return("NEUTRAL")
}

#' Detect chart patterns (simplified)
detect_chart_patterns <- function(market_data) {
  
  # This is a simplified pattern detection
  # In production, would use more sophisticated algorithms
  
  patterns <- list()
  
  # Check for breakout
  if (market_data$current_price > market_data$high_24h * 0.98) {
    patterns$breakout_high <- TRUE
  }
  
  if (market_data$current_price < market_data$low_24h * 1.02) {
    patterns$breakout_low <- TRUE
  }
  
  # Check for range
  if (market_data$range_24h < 3) {
    patterns$tight_range <- TRUE
  }
  
  return(patterns)
}

# ==========================================================================================================
# ⏰ AUTOMATED SCANNING
# ==========================================================================================================

#' Start automated scanner
start_automated_scanner <- function(interval_seconds = NULL, scan_type = "quick") {
  
  if (is.null(interval_seconds)) {
    interval_seconds <- SCANNER_CONFIG$scan_interval_seconds
  }
  
  cat("\n⏰ Starting automated market scanner...\n")
  cat("Scan interval:", interval_seconds, "seconds\n")
  cat("Scan type:", scan_type, "\n")
  cat("Press Ctrl+C to stop\n\n")
  
  SCANNER_ACTIVE <<- TRUE
  scan_count <- 0
  
  while (SCANNER_ACTIVE) {
    scan_count <- scan_count + 1
    
    cat("\n", rep("=", 60), "\n", sep = "")
    cat("Scan #", scan_count, "\n")
    
    # Run scan
    report <- run_market_scan(scan_type)
    
    # Save report
    save_scan_report(report)
    
    # Check for urgent alerts
    if (report$alerts$count > 0) {
      urgent_alerts <- Filter(function(x) 
        any(sapply(x, function(y) y$severity == "HIGH")), 
        report$alerts$high_severity)
      
      if (length(urgent_alerts) > 0) {
        cat("\n🚨 URGENT ALERTS DETECTED! 🚨\n")
        # In production, could send notifications
      }
    }
    
    # Wait for next scan
    cat("\nNext scan in", interval_seconds, "seconds...\n")
    Sys.sleep(interval_seconds)
  }
  
  cat("\n⏹️ Scanner stopped\n")
}

#' Stop automated scanner
stop_automated_scanner <- function() {
  SCANNER_ACTIVE <<- FALSE
  cat("\n⏹️ Stopping scanner...\n")
}

#' Save scan report
save_scan_report <- function(report) {
  
  if (!exists("SCAN_HISTORY")) {
    SCAN_HISTORY <<- list()
  }
  
  # Add to history
  SCAN_HISTORY[[length(SCAN_HISTORY) + 1]] <<- report
  
  # Keep only last 100 scans
  if (length(SCAN_HISTORY) > 100) {
    SCAN_HISTORY <<- tail(SCAN_HISTORY, 100)
  }
  
  # Save to file if configured
  if (exists("SCANNER_LOG_FILE")) {
    tryCatch({
      saveRDS(report, file = paste0(
        SCANNER_LOG_FILE, 
        format(report$timestamp, "%Y%m%d_%H%M%S"),
        ".rds"
      ))
    }, error = function(e) {
      cat("Warning: Could not save scan report to file\n")
    })
  }
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Initialize scanner state
SCANNER_ACTIVE <- FALSE
MARKET_UNIVERSE_CACHE <- list()
SCAN_HISTORY <- list()

# Scanner interface
MARKET_SCANNER <- list(
  scan = run_market_scan,
  quick_scan = function() run_market_scan("quick"),
  full_scan = function() run_market_scan("full"),
  start_auto = start_automated_scanner,
  stop_auto = stop_automated_scanner,
  heatmap = function() create_market_heatmap(tail(SCAN_HISTORY, 1)[[1]]$raw_results),
  get_history = function() SCAN_HISTORY,
  update_universe = update_market_universe
)

cat("✅ MARKET_SCANNER.R loaded successfully!\n")
cat("🔍 Market scanning engine ready\n")
cat("🎯 Opportunity detection enabled\n")
cat("🚨 Alert monitoring active\n")
cat("⏰ Automated scanning available\n")