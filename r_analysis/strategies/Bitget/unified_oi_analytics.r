# ========================================================================================================== 
# 📊 UNIFIED OPEN INTEREST ANALYTICS ENGINE V2 
# ==========================================================================================================
# Konsolidiert: dynamic_ada_oi_heatmap.r + algo_oi_heatmap.r + oi_table_dashboard.r
# Universal OI-Analytics für alle Assets mit enhanced institutional analysis
# ==========================================================================================================

# Load central configuration if not already loaded
if (!exists("SYSTEM_CONFIG_LOADED")) {
  source("system_config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
}

# ==========================================================================================================
# 🎯 UNIVERSAL OI HEATMAP FUNCTIONS (REPLACES ASSET-SPECIFIC VERSIONS)
# ==========================================================================================================

#' Universal OI Heatmap Generator - Replaces generate_dynamic_ada_oi_heatmap() and generate_dynamic_algo_oi_heatmap()
generate_universal_oi_heatmap <- function(symbol, custom_config = NULL) {
  tryCatch({
    # Get asset config from central configuration
    asset_config <- get_asset_config(symbol)
    if (is.null(asset_config)) {
      cat("❌ Unknown symbol:", symbol, "\n")
      return(NULL)
    }
    
    # Use custom config if provided, otherwise use asset default
    config <- if (!is.null(custom_config)) custom_config else asset_config
    
    # Fetch live price via enhanced ticker data
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) {
      cat("❌ Failed to fetch live price for", symbol, "\n")
      return(NULL)
    }
    
    current_price <- as.numeric(ticker_data$lastPr)
    
    # Calculate dynamic price range based on asset characteristics
    range_percent <- config$oi_range_percent %||% 0.15  # Default 15%
    price_min <- current_price * (1 - range_percent)
    price_max <- current_price * (1 + range_percent)
    
    # Generate OI concentration using universal algorithm
    oi_data <- calculate_oi_concentration(symbol, current_price, price_min, price_max, config)
    
    if (is.null(oi_data)) {
      cat("❌ Failed to calculate OI concentration for", symbol, "\n")
      return(NULL)
    }
    
    # Apply asset-specific adjustments (tick size, volatility)
    oi_data <- apply_asset_specific_adjustments(oi_data, config)
    
    # Generate and display heatmap
    plot_data <- create_oi_heatmap_plot(oi_data, symbol, current_price, config)
    
    # Return standardized result structure
    return(list(
      symbol = symbol,
      current_price = current_price,
      price_range = c(price_min, price_max),
      oi_data = oi_data,
      plot_data = plot_data,
      timestamp = Sys.time(),
      config_used = config
    ))
    
  }, error = function(e) {
    cat("❌ Error in generate_universal_oi_heatmap:", e$message, "\n")
    return(NULL)
  })
}

#' Universal Live Asset Price Fetcher - Replaces get_live_ada_price() and get_live_algo_price()
get_live_asset_price <- function(symbol) {
  ticker_data <- get_enhanced_ticker_data(symbol)
  if (!is.null(ticker_data) && !is.null(ticker_data$lastPr)) {
    return(as.numeric(ticker_data$lastPr))
  }
  return(NULL)
}

#' Calculate OI Concentration for any symbol
calculate_oi_concentration <- function(symbol, current_price, price_min, price_max, config) {
  tryCatch({
    # Fetch open interest data
    oi_response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/market/open-interest",
      params = list(
        symbol = symbol,
        granularity = config$oi_granularity %||% "1H"
      )
    )
    
    if (is.null(oi_response) || length(oi_response$data) == 0) {
      return(NULL)
    }
    
    # Process OI data
    oi_data <- oi_response$data[[1]]
    
    # Calculate price levels within range
    num_levels <- config$oi_price_levels %||% 50
    price_levels <- seq(price_min, price_max, length.out = num_levels)
    
    # Calculate OI intensity at each price level
    oi_intensity <- sapply(price_levels, function(price) {
      # Distance-weighted OI calculation
      distance_factor <- abs(price - current_price) / current_price
      weight <- exp(-distance_factor * config$distance_decay %||% 10)
      return(as.numeric(oi_data$amount) * weight)
    })
    
    # Create concentration matrix
    concentration_data <- data.frame(
      price_level = price_levels,
      oi_intensity = oi_intensity,
      distance_from_current = abs(price_levels - current_price),
      concentration_score = oi_intensity / max(oi_intensity, na.rm = TRUE)
    )
    
    return(concentration_data)
    
  }, error = function(e) {
    cat("❌ Error calculating OI concentration:", e$message, "\n")
    return(NULL)
  })
}

# ==========================================================================================================
# 🏦 INSTITUTIONAL ANALYTICS (FROM oi_table_dashboard.r)
# ==========================================================================================================

#' Run comprehensive institutional OI analysis
run_institutional_oi_analysis <- function(symbols = NULL) {
  if (is.null(symbols)) {
    symbols <- PORTFOLIO_ASSETS
  }
  
  cat("\n🏦 === INSTITUTIONAL OI ANALYSIS === 🏦\n")
  
  results <- list()
  
  for (symbol in symbols) {
    cat("\n📊 Analyzing", symbol, "...\n")
    
    # Generate OI flow analysis
    oi_flow <- generate_oi_flow_analysis(symbol)
    
    # Calculate 24h OI magnets
    oi_magnets <- calculate_24h_oi_magnets(symbol)
    
    # Extract price context
    price_context <- extract_price_context(symbol)
    
    # Synthesize trading signals
    trading_signals <- synthesize_trading_signals(oi_flow, oi_magnets, price_context)
    
    results[[symbol]] <- list(
      oi_flow = oi_flow,
      oi_magnets = oi_magnets,
      price_context = price_context,
      trading_signals = trading_signals
    )
    
    # Display formatted results
    display_institutional_analysis(symbol, results[[symbol]])
  }
  
  # Cross-asset comparative analysis
  if (length(symbols) > 1) {
    comparative_analysis <- compare_multi_asset_oi(results)
    display_comparative_oi_analysis(comparative_analysis)
  }
  
  return(results)
}

#' Generate OI flow analysis for institutional patterns
generate_oi_flow_analysis <- function(symbol) {
  tryCatch({
    # Fetch historical OI data for trend analysis
    oi_history <- get_oi_historical_data(symbol, "24h")
    
    if (is.null(oi_history) || length(oi_history) == 0) {
      return(list(
        trend = "UNKNOWN",
        change_24h = 0,
        volume_trend = "UNKNOWN",
        institutional_score = 0
      ))
    }
    
    # Calculate OI trend and institutional indicators
    current_oi <- as.numeric(oi_history[length(oi_history)]$amount)
    start_oi <- as.numeric(oi_history[1]$amount)
    change_percent <- ((current_oi - start_oi) / start_oi) * 100
    
    # Determine institutional activity level
    institutional_score <- calculate_institutional_score(oi_history)
    
    return(list(
      trend = if (change_percent > 5) "INCREASING" else if (change_percent < -5) "DECREASING" else "STABLE",
      change_24h = round(change_percent, 2),
      volume_trend = analyze_volume_trend(symbol),
      institutional_score = institutional_score
    ))
    
  }, error = function(e) {
    cat("❌ Error in OI flow analysis:", e$message, "\n")
    return(list(trend = "ERROR", change_24h = 0, volume_trend = "ERROR", institutional_score = 0))
  })
}

#' Calculate 24h OI magnets (price levels with high OI concentration)
calculate_24h_oi_magnets <- function(symbol) {
  tryCatch({
    current_price <- get_live_asset_price(symbol)
    if (is.null(current_price)) return(list())
    
    # Analyze OI distribution around current price
    asset_config <- get_asset_config(symbol)
    oi_data <- calculate_oi_concentration(symbol, current_price, 
                                         current_price * 0.9, current_price * 1.1, asset_config)
    
    if (is.null(oi_data)) return(list())
    
    # Find local maxima (OI magnets)
    magnets <- find_oi_magnets(oi_data, current_price)
    
    return(magnets)
    
  }, error = function(e) {
    cat("❌ Error calculating OI magnets:", e$message, "\n")
    return(list())
  })
}

#' Extract current price context for institutional analysis
extract_price_context <- function(symbol) {
  tryCatch({
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) return(list())
    
    return(list(
      current_price = as.numeric(ticker_data$lastPr),
      change_24h = as.numeric(ticker_data$change24h),
      volume_24h = as.numeric(ticker_data$baseVolume),
      high_24h = as.numeric(ticker_data$high24h),
      low_24h = as.numeric(ticker_data$low24h)
    ))
    
  }, error = function(e) {
    return(list())
  })
}

#' Synthesize trading signals from institutional analysis
synthesize_trading_signals <- function(oi_flow, oi_magnets, price_context) {
  signals <- list()
  
  # OI trend signals
  if (oi_flow$trend == "INCREASING" && oi_flow$change_24h > 10) {
    signals$oi_trend <- "BULLISH_STRONG"
  } else if (oi_flow$trend == "DECREASING" && oi_flow$change_24h < -10) {
    signals$oi_trend <- "BEARISH_STRONG"
  } else {
    signals$oi_trend <- "NEUTRAL"
  }
  
  # Institutional activity signal
  if (oi_flow$institutional_score > 0.7) {
    signals$institutional <- "HIGH_ACTIVITY"
  } else if (oi_flow$institutional_score > 0.4) {
    signals$institutional <- "MODERATE_ACTIVITY"
  } else {
    signals$institutional <- "LOW_ACTIVITY"
  }
  
  # Overall recommendation
  if (signals$oi_trend == "BULLISH_STRONG" && signals$institutional == "HIGH_ACTIVITY") {
    signals$recommendation <- "STRONG_BUY"
  } else if (signals$oi_trend == "BEARISH_STRONG" && signals$institutional == "HIGH_ACTIVITY") {
    signals$recommendation <- "STRONG_SELL"
  } else {
    signals$recommendation <- "HOLD"
  }
  
  return(signals)
}

# ==========================================================================================================
# 🔄 COMPARATIVE ANALYSIS (NEW FUNCTIONALITY)
# ==========================================================================================================

#' Compare OI patterns across multiple assets
compare_multi_asset_oi <- function(analysis_results) {
  comparison <- list()
  
  # Extract key metrics for comparison
  for (symbol in names(analysis_results)) {
    result <- analysis_results[[symbol]]
    comparison[[symbol]] <- list(
      oi_change_24h = result$oi_flow$change_24h,
      institutional_score = result$oi_flow$institutional_score,
      recommendation = result$trading_signals$recommendation,
      price_change_24h = result$price_context$change_24h %||% 0
    )
  }
  
  # Rank assets by institutional activity
  institutional_ranking <- sort(sapply(comparison, function(x) x$institutional_score), decreasing = TRUE)
  
  # Find correlations
  correlations <- calculate_oi_correlations(comparison)
  
  return(list(
    asset_comparison = comparison,
    institutional_ranking = institutional_ranking,
    correlations = correlations
  ))
}

#' Cross-asset correlation analysis
cross_asset_correlation_analysis <- function(symbols = PORTFOLIO_ASSETS) {
  correlations <- list()
  
  for (i in 1:(length(symbols)-1)) {
    for (j in (i+1):length(symbols)) {
      symbol1 <- symbols[i]
      symbol2 <- symbols[j]
      
      # Calculate OI correlation between assets
      correlation <- calculate_pair_oi_correlation(symbol1, symbol2)
      correlations[[paste(symbol1, symbol2, sep = "_")]] <- correlation
    }
  }
  
  return(correlations)
}

# ==========================================================================================================
# 🎨 DISPLAY & VISUALIZATION FUNCTIONS
# ==========================================================================================================

#' Display formatted institutional analysis results
display_institutional_analysis <- function(symbol, analysis) {
  cat("\n📈", symbol, "Institutional Analysis:\n")
  cat("   OI Trend:", analysis$oi_flow$trend, "(", analysis$oi_flow$change_24h, "%)\n")
  cat("   Institutional Score:", round(analysis$oi_flow$institutional_score, 3), "\n")
  cat("   Recommendation:", analysis$trading_signals$recommendation, "\n")
  
  if (length(analysis$oi_magnets) > 0) {
    cat("   🧲 OI Magnets found:", length(analysis$oi_magnets), "\n")
  }
}

#' Display comparative OI analysis across assets
display_comparative_oi_analysis <- function(comparative_analysis) {
  cat("\n🔍 === CROSS-ASSET OI COMPARISON === 🔍\n")
  
  cat("\n🏆 Institutional Activity Ranking:\n")
  for (i in 1:length(comparative_analysis$institutional_ranking)) {
    symbol <- names(comparative_analysis$institutional_ranking)[i]
    score <- comparative_analysis$institutional_ranking[i]
    cat("   ", i, ".", symbol, "- Score:", round(score, 3), "\n")
  }
}

# ==========================================================================================================
# 🛠️ HELPER FUNCTIONS
# ==========================================================================================================

#' Apply asset-specific adjustments to OI data
apply_asset_specific_adjustments <- function(oi_data, config) {
  # Apply tick size adjustments
  if (!is.null(config$tick_size)) {
    oi_data$price_level <- round(oi_data$price_level / config$tick_size) * config$tick_size
  }
  
  # Apply volatility adjustments
  if (!is.null(config$volatility_factor)) {
    oi_data$concentration_score <- oi_data$concentration_score * config$volatility_factor
  }
  
  return(oi_data)
}

#' Create OI heatmap plot data
create_oi_heatmap_plot <- function(oi_data, symbol, current_price, config) {
  # Generate plot structure (simplified - full implementation would use ggplot2)
  plot_data <- list(
    x = oi_data$price_level,
    y = oi_data$oi_intensity,
    colors = scales::col_numeric("RdYlGn", domain = range(oi_data$concentration_score))(oi_data$concentration_score),
    title = paste(symbol, "OI Heatmap -", Sys.time()),
    current_price_line = current_price
  )
  
  return(plot_data)
}

#' Find OI magnets (local maxima in OI concentration)
find_oi_magnets <- function(oi_data, current_price) {
  if (nrow(oi_data) < 3) return(list())
  
  # Simple peak detection
  magnets <- list()
  for (i in 2:(nrow(oi_data)-1)) {
    if (oi_data$oi_intensity[i] > oi_data$oi_intensity[i-1] && 
        oi_data$oi_intensity[i] > oi_data$oi_intensity[i+1] &&
        oi_data$concentration_score[i] > 0.5) {
      
      magnets[[length(magnets) + 1]] <- list(
        price = oi_data$price_level[i],
        intensity = oi_data$oi_intensity[i],
        distance_from_current = abs(oi_data$price_level[i] - current_price)
      )
    }
  }
  
  return(magnets)
}

#' Calculate institutional activity score
calculate_institutional_score <- function(oi_history) {
  if (length(oi_history) < 2) return(0)
  
  # Calculate volatility and trend strength
  oi_values <- sapply(oi_history, function(x) as.numeric(x$amount))
  volatility <- sd(oi_values, na.rm = TRUE) / mean(oi_values, na.rm = TRUE)
  trend_strength <- abs(cor(1:length(oi_values), oi_values, use = "complete.obs"))
  
  # Combine metrics for institutional score
  score <- (volatility * 0.4 + trend_strength * 0.6) 
  return(min(max(score, 0), 1))  # Clamp between 0 and 1
}

#' Calculate OI correlations between assets
calculate_oi_correlations <- function(comparison_data) {
  if (length(comparison_data) < 2) return(list())
  
  # Extract OI changes for correlation calculation
  oi_changes <- sapply(comparison_data, function(x) x$oi_change_24h)
  
  # Calculate correlation matrix
  correlations <- list()
  symbols <- names(comparison_data)
  
  for (i in 1:(length(symbols)-1)) {
    for (j in (i+1):length(symbols)) {
      pair_name <- paste(symbols[i], symbols[j], sep = "_")
      correlations[[pair_name]] <- cor(oi_changes[i], oi_changes[j], use = "complete.obs")
    }
  }
  
  return(correlations)
}

cat("✅ UNIFIED_OI_ANALYTICS.R loaded successfully!\n")
cat("📊 Universal OI Analytics available for all assets\n")
cat("🏦 Institutional analysis functions ready\n")
cat("🔄 Cross-asset comparative analysis enabled\n")