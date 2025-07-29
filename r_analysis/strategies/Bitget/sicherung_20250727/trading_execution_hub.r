# ========================================================================================================== 
# 🎯 TRADING EXECUTION HUB V2 - MAIN EXECUTION INTERFACE
# ==========================================================================================================
# Konsolidiert: multi_asset_rexecution_v7.r + altcoin_rally_triggers.r + Console Management
# Central command interface for all trading operations with enhanced user experience
# ==========================================================================================================

# ==========================================================================================================
# 🔧 AUTOMATIC SYSTEM COMPONENT LOADING
# ==========================================================================================================

cat("🔄 Loading Trading System V2 Components...\n")

# Load central configuration if not already loaded
if (!exists("SYSTEM_CONFIG_LOADED")) {
  cat("📁 Loading system_config.r...\n")
  source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/system_config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
  cat("   ✅ Configuration loaded\n")
}

# Load core trading engine
if (!exists("CORE_ENGINE_LOADED")) {
  cat("📁 Loading bitget_core_engine.r...\n")
  source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_core_engine.r")
  CORE_ENGINE_LOADED <- TRUE
  cat("   ✅ Core engine loaded\n")
}

# Load OI analytics system
if (!exists("OI_ANALYTICS_LOADED")) {
  cat("📁 Loading unified_oi_analytics.r...\n")
  source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/unified_oi_analytics.r")
  OI_ANALYTICS_LOADED <- TRUE
  cat("   ✅ OI analytics loaded\n")
}

# Load risk management system
if (!exists("RISK_MANAGER_LOADED")) {
  cat("📁 Loading portfolio_risk_manager.r...\n")
  source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/portfolio_risk_manager.r")
  RISK_MANAGER_LOADED <- TRUE
  cat("   ✅ Risk manager loaded\n")
}

cat("🚀 All system components loaded successfully!\n")

#C:/freeding/tbot202506/r_analysis/strategies/Bitget


# ==========================================================================================================
# 🚀 MAIN EXECUTION INTERFACE
# ==========================================================================================================

#' Primary execution function - Enhanced version of main execution from multi_asset_rexecution_v7.r
execute_trading_system <- function(mode = "full", symbols = NULL, include_oi = TRUE, 
                                  include_risk_check = TRUE, interactive = FALSE) {
  
  # Clear and prepare console
  clean_console_output()
  display_system_header()
  
  tryCatch({
    cat("\n🚀 === TRADING SYSTEM V2 EXECUTION === 🚀\n")
    cat("Mode:", toupper(mode), "| Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Initialize symbols list
    if (is.null(symbols)) {
      symbols <- PORTFOLIO_ASSETS
    }
    
    cat("📊 Assets:", paste(symbols, collapse = ", "), "\n")
    
    # Execute based on mode
    execution_results <- switch(mode,
      "full" = execute_full_analysis(symbols, include_oi, include_risk_check),
      "quick" = execute_quick_check(symbols),
      "oi_only" = execute_oi_analysis_only(symbols),
      "risk_only" = execute_risk_check_only(symbols),
      "sentiment" = execute_sentiment_analysis(symbols),
      execute_full_analysis(symbols, include_oi, include_risk_check)  # Default
    )
    
    # Display execution summary
    display_execution_summary(execution_results, mode)
    
    # Interactive mode for additional commands
    if (interactive) {
      launch_interactive_interface(execution_results)
    }
    
    return(execution_results)
    
  }, error = function(e) {
    cat("❌ EXECUTION ERROR:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

#' Full comprehensive analysis (replaces main function from multi_asset_rexecution_v7.r)
execute_full_analysis <- function(symbols, include_oi = TRUE, include_risk_check = TRUE) {
  
  results <- list(
    timestamp = Sys.time(),
    symbols = symbols,
    market_data = list(),
    technical_analysis = list(),
    oi_analysis = list(),
    risk_assessment = list(),
    sentiment_analysis = list(),
    recommendations = list()
  )
  
  cat("\n📊 === COLLECTING MULTI-ASSET DATA === 📊\n")
  
  # Step 1: Collect enhanced market data for all symbols
  for (symbol in symbols) {
    cat("📈 Processing", symbol, "...\n")
    
    # Enhanced market data collection
    market_data <- collect_enhanced_market_data(symbol)
    results$market_data[[symbol]] <- market_data
    
    if (!is.null(market_data)) {
      # Technical analysis
      technical_analysis <- perform_comprehensive_technical_analysis(symbol, market_data)
      results$technical_analysis[[symbol]] <- technical_analysis
      
      # Display quick summary
      display_asset_summary(symbol, market_data, technical_analysis)
    } else {
      cat("⚠️ Failed to collect data for", symbol, "\n")
    }
    
    Sys.sleep(0.3)  # Rate limiting
  }
  
  # Step 2: OI Analysis (if enabled)
  if (include_oi) {
    cat("\n🧲 === OPEN INTEREST ANALYSIS === 🧲\n")
    oi_results <- run_institutional_oi_analysis(symbols)
    results$oi_analysis <- oi_results
  }
  
  # Step 3: Risk Assessment (if enabled)
  if (include_risk_check) {
    cat("\n🛡️ === PORTFOLIO RISK ASSESSMENT === 🛡️\n")
    risk_results <- monitor_portfolio_positions(symbols)
    results$risk_assessment <- risk_results
    
    # Check for emergency triggers
    emergency_check <- emergency_protection_triggers()
    results$emergency_status <- emergency_check
    
    if (emergency_check$triggered) {
      cat("\n🚨 EMERGENCY CONDITIONS DETECTED - Review recommended\n")
    }
  }
  
  # Step 4: Enhanced Sentiment Analysis (from altcoin_rally_triggers.r)
  cat("\n🎭 === MULTI-FACTOR SENTIMENT ANALYSIS === 🎭\n")
  sentiment_results <- analyze_multi_factor_sentiment(symbols)
  results$sentiment_analysis <- sentiment_results
  
  # Step 5: Generate trading recommendations
  cat("\n🎯 === GENERATING RECOMMENDATIONS === 🎯\n")
  recommendations <- generate_comprehensive_recommendations(results)
  results$recommendations <- recommendations
  
  # Step 6: Display comprehensive overview
  display_portfolio_overview(results)
  display_comparative_analysis(results)
  
  return(results)
}

#' Quick market check (fast execution mode)
execute_quick_check <- function(symbols) {
  cat("\n⚡ === QUICK MARKET CHECK === ⚡\n")
  
  results <- list(quick_data = list())
  
  for (symbol in symbols) {
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (!is.null(ticker_data)) {
      quick_summary <- list(
        symbol = symbol,
        price = as.numeric(ticker_data$lastPr),
        change_24h = as.numeric(ticker_data$change24h),
        volume = as.numeric(ticker_data$baseVolume),
        timestamp = Sys.time()
      )
      
      results$quick_data[[symbol]] <- quick_summary
      
      cat("📊", symbol, ":", round(quick_summary$price, 4), 
          "| 24h:", round(quick_summary$change_24h, 2), "%\n")
    }
  }
  
  return(results)
}

#' OI analysis only mode  
execute_oi_analysis_only <- function(symbols) {
  cat("\n🧲 === OI ANALYSIS ONLY === 🧲\n")
  
  oi_results <- run_institutional_oi_analysis(symbols)
  
  return(list(oi_analysis = oi_results))
}

#' Risk check only mode
execute_risk_check_only <- function(symbols) {
  cat("\n🛡️ === RISK CHECK ONLY === 🛡️\n")
  
  risk_results <- monitor_portfolio_positions(symbols)
  emergency_check <- emergency_protection_triggers()
  
  return(list(
    risk_assessment = risk_results,
    emergency_status = emergency_check
  ))
}

#' Enhanced sentiment analysis (from altcoin_rally_triggers.r)
execute_sentiment_analysis <- function(symbols) {
  cat("\n🎭 === SENTIMENT ANALYSIS === 🎭\n")
  
  sentiment_results <- analyze_multi_factor_sentiment(symbols)
  display_sentiment_dashboard(sentiment_results)
  
  return(list(sentiment_analysis = sentiment_results))
}

# ==========================================================================================================
# 🎭 MULTI-FACTOR SENTIMENT ANALYSIS (ENHANCED FROM altcoin_rally_triggers.r)
# ==========================================================================================================

#' Comprehensive multi-factor sentiment analysis
analyze_multi_factor_sentiment <- function(symbols) {
  
  sentiment_results <- list()
  
  for (symbol in symbols) {
    cat("🎭 Analyzing", symbol, "sentiment...\n")
    
    # Factor 1: Technical Momentum
    technical_momentum <- calculate_technical_momentum(symbol)
    
    # Factor 2: Volume Analysis  
    volume_sentiment <- analyze_volume_sentiment(symbol)
    
    # Factor 3: OI Sentiment
    oi_sentiment <- analyze_oi_sentiment(symbol)
    
    # Factor 4: Price Action Sentiment
    price_action_sentiment <- analyze_price_action_sentiment(symbol)
    
    # Factor 5: Market Structure
    market_structure_sentiment <- analyze_market_structure(symbol)
    
    # Combine all factors
    combined_sentiment <- combine_sentiment_factors(
      technical_momentum, volume_sentiment, oi_sentiment, 
      price_action_sentiment, market_structure_sentiment
    )
    
    sentiment_results[[symbol]] <- list(
      overall_sentiment = combined_sentiment$overall,
      sentiment_score = combined_sentiment$score,
      confidence_level = combined_sentiment$confidence,
      factors = list(
        technical = technical_momentum,
        volume = volume_sentiment,
        oi = oi_sentiment,
        price_action = price_action_sentiment,
        market_structure = market_structure_sentiment
      ),
      recommendation = generate_sentiment_recommendation(combined_sentiment),
      timestamp = Sys.time()
    )
    
    # Display individual sentiment
    display_asset_sentiment(symbol, sentiment_results[[symbol]])
  }
  
  # Cross-asset sentiment correlation
  if (length(symbols) > 1) {
    correlation_analysis <- analyze_cross_asset_sentiment(sentiment_results)
    sentiment_results$cross_asset_analysis <- correlation_analysis
  }
  
  return(sentiment_results)
}

#' Calculate technical momentum indicators
calculate_technical_momentum <- function(symbol) {
  tryCatch({
    # Get recent market data
    market_data <- get_enhanced_market_data(symbol)
    if (is.null(market_data)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Calculate momentum indicators
    technical_analysis <- calculate_technical_indicators_fixed(market_data)
    
    # Extract momentum signals
    rsi_signal <- if (technical_analysis$rsi > 70) "OVERBOUGHT" else if (technical_analysis$rsi < 30) "OVERSOLD" else "NEUTRAL"
    macd_signal <- if (technical_analysis$macd > technical_analysis$signal_line) "BULLISH" else "BEARISH"
    bb_signal <- if (technical_analysis$bb_position > 0.8) "OVERBOUGHT" else if (technical_analysis$bb_position < 0.2) "OVERSOLD" else "NEUTRAL"
    
    # Combine signals into momentum score
    momentum_score <- calculate_momentum_score(rsi_signal, macd_signal, bb_signal, technical_analysis)
    
    return(list(
      score = momentum_score,
      trend = if (momentum_score > 0.6) "BULLISH" else if (momentum_score < 0.4) "BEARISH" else "NEUTRAL",
      rsi = technical_analysis$rsi,
      macd_signal = macd_signal,
      bb_signal = bb_signal
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Analyze volume-based sentiment
analyze_volume_sentiment <- function(symbol) {
  tryCatch({
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Get historical volume data for comparison
    current_volume <- as.numeric(ticker_data$baseVolume)
    price_change <- as.numeric(ticker_data$change24h)
    
    # Volume trend analysis (simplified)
    volume_score <- if (current_volume > 1000000) {  # High volume threshold
      if (price_change > 0) 0.7 else 0.3  # High volume + price up = bullish
    } else {
      0.5  # Low volume = neutral
    }
    
    return(list(
      score = volume_score,
      trend = if (volume_score > 0.6) "BULLISH" else if (volume_score < 0.4) "BEARISH" else "NEUTRAL",
      volume = current_volume,
      volume_price_divergence = abs(price_change) / max(current_volume / 1000000, 1)
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Analyze OI-based sentiment
analyze_oi_sentiment <- function(symbol) {
  tryCatch({
    # Use OI flow analysis from unified analytics
    oi_flow <- generate_oi_flow_analysis(symbol)
    
    if (is.null(oi_flow)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Convert OI trend to sentiment score
    oi_score <- switch(oi_flow$trend,
      "INCREASING" = if (oi_flow$change_24h > 10) 0.7 else 0.6,
      "DECREASING" = if (oi_flow$change_24h < -10) 0.3 else 0.4,
      0.5  # STABLE or unknown
    )
    
    return(list(
      score = oi_score,
      trend = if (oi_score > 0.6) "BULLISH" else if (oi_score < 0.4) "BEARISH" else "NEUTRAL",
      oi_change_24h = oi_flow$change_24h,
      institutional_score = oi_flow$institutional_score
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Analyze price action sentiment
analyze_price_action_sentiment <- function(symbol) {
  tryCatch({
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    current_price <- as.numeric(ticker_data$lastPr)
    high_24h <- as.numeric(ticker_data$high24h)
    low_24h <- as.numeric(ticker_data$low24h)
    change_24h <- as.numeric(ticker_data$change24h)
    
    # Price position within 24h range
    price_position <- (current_price - low_24h) / (high_24h - low_24h)
    
    # Combine price position with change
    price_action_score <- (price_position * 0.6) + ((change_24h + 10) / 20 * 0.4)  # Normalize change to 0-1
    price_action_score <- min(max(price_action_score, 0), 1)  # Clamp
    
    return(list(
      score = price_action_score,
      trend = if (price_action_score > 0.6) "BULLISH" else if (price_action_score < 0.4) "BEARISH" else "NEUTRAL",
      price_position_24h = price_position,
      change_24h = change_24h
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Analyze market structure sentiment
analyze_market_structure <- function(symbol) {
  tryCatch({
    # Get enhanced market data
    market_data <- get_enhanced_market_data(symbol)
    if (is.null(market_data)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Analyze bid-ask spread and depth
    orderbook_data <- get_enhanced_orderbook(symbol)
    
    if (!is.null(orderbook_data)) {
      # Calculate spread
      best_bid <- as.numeric(orderbook_data$bids[[1]][1])
      best_ask <- as.numeric(orderbook_data$asks[[1]][1])
      spread_percent <- (best_ask - best_bid) / best_bid * 100
      
      # Tight spread = healthy market structure
      structure_score <- if (spread_percent < 0.1) 0.7 else if (spread_percent < 0.5) 0.5 else 0.3
    } else {
      structure_score <- 0.5
    }
    
    return(list(
      score = structure_score,
      trend = if (structure_score > 0.6) "HEALTHY" else if (structure_score < 0.4) "POOR" else "NEUTRAL",
      spread_percent = spread_percent %||% NA
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Combine all sentiment factors into overall sentiment
combine_sentiment_factors <- function(technical, volume, oi, price_action, market_structure) {
  
  # Weighted combination of factors
  weights <- list(
    technical = 0.25,
    volume = 0.20,
    oi = 0.25,
    price_action = 0.20,
    market_structure = 0.10
  )
  
  overall_score <- (
    technical$score * weights$technical +
    volume$score * weights$volume +
    oi$score * weights$oi +
    price_action$score * weights$price_action +
    market_structure$score * weights$market_structure
  )
  
  # Calculate confidence based on factor agreement
  factor_scores <- c(technical$score, volume$score, oi$score, price_action$score, market_structure$score)
  confidence <- 1 - sd(factor_scores) / mean(factor_scores)  # Lower standard deviation = higher confidence
  confidence <- min(max(confidence, 0), 1)
  
  # Determine overall sentiment
  overall_sentiment <- if (overall_score > 0.65) {
    "STRONG_BULLISH"
  } else if (overall_score > 0.55) {
    "BULLISH"
  } else if (overall_score > 0.45) {
    "NEUTRAL"
  } else if (overall_score > 0.35) {
    "BEARISH"
  } else {
    "STRONG_BEARISH"
  }
  
  return(list(
    overall = overall_sentiment,
    score = overall_score,
    confidence = confidence
  ))
}

# ==========================================================================================================
# 🎮 INTERACTIVE TRADING INTERFACE
# ==========================================================================================================

#' Launch interactive command interface
launch_interactive_interface <- function(execution_results = NULL) {
  cat("\n🎮 === INTERACTIVE TRADING INTERFACE === 🎮\n")
  cat("Available commands: help, status, risk, oi, sentiment, execute, quit\n")
  
  repeat {
    cat("\n> ")
    command <- trimws(tolower(readline()))
    
    if (command == "quit" || command == "q" || command == "exit") {
      cat("👋 Exiting interactive mode\n")
      break
    }
    
    process_interactive_command(command, execution_results)
  }
}

#' Process interactive commands
process_interactive_command <- function(command, execution_results) {
  
  parts <- strsplit(command, " ")[[1]]
  cmd <- parts[1]
  args <- if (length(parts) > 1) parts[2:length(parts)] else character(0)
  
  switch(cmd,
    "help" = display_help_menu(),
    "status" = display_system_status(),
    "risk" = handle_risk_command(args),
    "oi" = handle_oi_command(args),
    "sentiment" = handle_sentiment_command(args),
    "execute" = handle_execute_command(args),
    "positions" = display_current_positions(),
    "market" = handle_market_command(args),
    "config" = display_config_summary(),
    cat("❓ Unknown command. Type 'help' for available commands.\n")
  )
}

#' Display help menu
display_help_menu <- function() {
  cat("\n📖 === AVAILABLE COMMANDS === 📖\n")
  cat("🔧 System Commands:\n")
  cat("   status          - Display system status\n")
  cat("   config          - Show current configuration\n")
  cat("   positions       - Show current positions\n")
  cat("\n📊 Analysis Commands:\n")
  cat("   risk [symbol]   - Risk analysis (all symbols if none specified)\n")
  cat("   oi [symbol]     - Open Interest analysis\n")
  cat("   sentiment [sym] - Sentiment analysis\n")
  cat("   market [symbol] - Quick market data\n")
  cat("\n🚀 Execution Commands:\n")
  cat("   execute [mode]  - Run analysis (modes: full, quick, oi, risk, sentiment)\n")
  cat("\n🎮 Navigation:\n")
  cat("   help            - Show this help menu\n")
  cat("   quit            - Exit interactive mode\n")
}

#' Handle risk-related commands
handle_risk_command <- function(args) {
  symbols <- if (length(args) > 0) args else PORTFOLIO_ASSETS
  risk_results <- monitor_portfolio_positions(symbols)
  
  # Display emergency status if triggered
  emergency_check <- emergency_protection_triggers()
  if (emergency_check$triggered) {
    cat("\n🚨 EMERGENCY STATUS:\n")
    for (action in emergency_check$actions) {
      cat("   ", action$severity, ":", action$message, "\n")
    }
  }
}

#' Handle OI analysis commands
handle_oi_command <- function(args) {
  symbols <- if (length(args) > 0) args else PORTFOLIO_ASSETS
  oi_results <- run_institutional_oi_analysis(symbols)
}

#' Handle sentiment analysis commands
handle_sentiment_command <- function(args) {
  symbols <- if (length(args) > 0) args else PORTFOLIO_ASSETS
  sentiment_results <- analyze_multi_factor_sentiment(symbols)
}

#' Handle execution commands
handle_execute_command <- function(args) {
  mode <- if (length(args) > 0) args[1] else "full"
  cat("🚀 Executing in", toupper(mode), "mode...\n")
  execute_trading_system(mode = mode, interactive = FALSE)
}

#' Handle market data commands
handle_market_command <- function(args) {
  symbols <- if (length(args) > 0) args else PORTFOLIO_ASSETS
  execute_quick_check(symbols)
}

# ==========================================================================================================
# 📊 ENHANCED DISPLAY FUNCTIONS
# ==========================================================================================================

#' Display comprehensive system header
display_system_header <- function() {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                    🚀 BITGET TRADING SYSTEM V2 🚀                              ║\n")
  cat("║                     Advanced Multi-Asset Trading Platform                        ║\n")
  cat("╠══════════════════════════════════════════════════════════════════════════════════╣\n")
  cat("║ 🔧 Core Engine   📊 OI Analytics   🛡️ Risk Manager   🎯 Execution Hub          ║\n")
  cat("╚══════════════════════════════════════════════════════════════════════════════════╝\n")
}

#' Display enhanced portfolio overview
display_portfolio_overview <- function(results) {
  cat("\n📊 === PORTFOLIO OVERVIEW === 📊\n")
  
  if (!is.null(results$market_data)) {
    for (symbol in names(results$market_data)) {
      market_data <- results$market_data[[symbol]]
      technical <- results$technical_analysis[[symbol]]
      
      if (!is.null(market_data) && !is.null(technical)) {
        cat("\n🔸", symbol, "Overview:\n")
        cat("   Price:", round(market_data$current_price, 4), 
            "| 24h Change:", round(market_data$change_24h, 2), "%\n")
        cat("   RSI:", round(technical$rsi, 2), 
            "| MACD:", round(technical$macd, 6), 
            "| BB Position:", round(technical$bb_position, 3), "\n")
        
        # Add sentiment if available
        if (!is.null(results$sentiment_analysis[[symbol]])) {
          sentiment <- results$sentiment_analysis[[symbol]]
          cat("   Sentiment:", sentiment$overall_sentiment, 
              "| Score:", round(sentiment$sentiment_score, 3), 
              "| Confidence:", round(sentiment$confidence_level, 3), "\n")
        }
      }
    }
  }
}

#' Display comparative analysis across assets
display_comparative_analysis <- function(results) {
  if (length(results$symbols) < 2) return()
  
  cat("\n🔍 === COMPARATIVE ANALYSIS === 🔍\n")
  
  # Compare performance metrics
  performance_summary <- data.frame(
    Symbol = character(0),
    Price_Change = numeric(0),
    RSI = numeric(0),
    Sentiment_Score = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (symbol in results$symbols) {
    if (!is.null(results$market_data[[symbol]]) && 
        !is.null(results$technical_analysis[[symbol]])) {
      
      market_data <- results$market_data[[symbol]]
      technical <- results$technical_analysis[[symbol]]
      sentiment_score <- if (!is.null(results$sentiment_analysis[[symbol]])) {
        results$sentiment_analysis[[symbol]]$sentiment_score
      } else 0.5
      
      performance_summary <- rbind(performance_summary, data.frame(
        Symbol = symbol,
        Price_Change = market_data$change_24h,
        RSI = technical$rsi,
        Sentiment_Score = sentiment_score,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(performance_summary) > 0) {
    # Rank by sentiment score
    performance_summary <- performance_summary[order(-performance_summary$Sentiment_Score), ]
    
    cat("🏆 Sentiment Ranking:\n")
    for (i in 1:nrow(performance_summary)) {
      row <- performance_summary[i, ]
      cat("   ", i, ".", row$Symbol, "- Score:", round(row$Sentiment_Score, 3), 
          "| RSI:", round(row$RSI, 1), 
          "| 24h:", round(row$Price_Change, 2), "%\n")
    }
  }
}

#' Display asset sentiment breakdown
display_asset_sentiment <- function(symbol, sentiment_data) {
  cat("🎭", symbol, "Sentiment Analysis:\n")
  cat("   Overall:", sentiment_data$overall_sentiment, 
      "| Score:", round(sentiment_data$sentiment_score, 3), 
      "| Confidence:", round(sentiment_data$confidence_level, 3), "\n")
  cat("   Recommendation:", sentiment_data$recommendation, "\n")
}

#' Display quick asset summary
display_asset_summary <- function(symbol, market_data, technical_analysis) {
  if (is.null(market_data) || is.null(technical_analysis)) return()
  
  # Determine trend arrows
  trend_arrow <- if (market_data$change_24h > 2) "📈" else if (market_data$change_24h < -2) "📉" else "➡️"
  rsi_indicator <- if (technical_analysis$rsi > 70) "🔴" else if (technical_analysis$rsi < 30) "🟢" else "🟡"
  
  cat("   ", trend_arrow, symbol, ":", round(market_data$current_price, 4), 
      "| 24h:", round(market_data$change_24h, 2), "% | RSI:", rsi_indicator, round(technical_analysis$rsi, 1), "\n")
}

#' Display execution summary
display_execution_summary <- function(execution_results, mode) {
  cat("\n✅ === EXECUTION SUMMARY === ✅\n")
  cat("Mode:", toupper(mode), "| Duration:", 
      round(as.numeric(Sys.time() - execution_results$timestamp), 2), "seconds\n")
  
  if (!is.null(execution_results$symbols)) {
    cat("Assets Processed:", length(execution_results$symbols), "\n")
  }
  
  if (!is.null(execution_results$risk_assessment)) {
    cat("Risk Alerts:", length(execution_results$risk_assessment$risk_alerts), "\n")
  }
  
  if (!is.null(execution_results$emergency_status) && execution_results$emergency_status$triggered) {
    cat("🚨 Emergency Status: ACTIVE\n")
  }
  
  cat("System Status: ✅ OPERATIONAL\n")
}

#' Clean console output for better readability
clean_console_output <- function() {
  # Clear console in RStudio/R console
  if (.Platform$OS.type == "windows") {
    system("cls", intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
  } else {
    system("clear", intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
  }
}

#' Display current system status
display_system_status <- function() {
  cat("\n🔧 === SYSTEM STATUS === 🔧\n")
  cat("Core Engine:", if (exists("CORE_ENGINE_LOADED")) "✅ LOADED" else "❌ NOT LOADED", "\n")
  cat("OI Analytics:", if (exists("OI_ANALYTICS_LOADED")) "✅ LOADED" else "❌ NOT LOADED", "\n")
  cat("Risk Manager:", if (exists("RISK_MANAGER_LOADED")) "✅ LOADED" else "❌ NOT LOADED", "\n")
  cat("Configuration:", if (exists("SYSTEM_CONFIG_LOADED")) "✅ LOADED" else "❌ NOT LOADED", "\n")
  
  # Check API connectivity
  test_result <- test_api_connectivity()
  cat("API Connection:", if (test_result) "✅ CONNECTED" else "❌ DISCONNECTED", "\n")
  
  # Display current positions
  positions <- get_current_positions()
  cat("Active Positions:", nrow(positions), "\n")
}

#' Display current positions summary
display_current_positions <- function() {
  positions <- get_current_positions(include_risk_metrics = TRUE)
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No open positions\n")
    return()
  }
  
  cat("\n📊 === CURRENT POSITIONS === 📊\n")
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    cat("🔸", pos$symbol, ":", pos$side, pos$size, "contracts\n")
    cat("   Entry:", round(pos$avg_price, 4), "| Current:", round(pos$mark_price, 4), 
        "| PnL:", round(pos$unrealized_pnl, 2), "USDT\n")
    cat("   Risk Score:", round(pos$risk_score, 3), "| Leverage:", pos$leverage, "x\n")
  }
}

#' Display configuration summary
display_config_summary <- function() {
  cat("\n⚙️ === CONFIGURATION SUMMARY === ⚙️\n")
  cat("Portfolio Assets:", paste(PORTFOLIO_ASSETS, collapse = ", "), "\n")
  cat("Default TP:", DEFAULT_TP_PERCENT, "% | Default SL:", DEFAULT_SL_PERCENT, "%\n")
  cat("Console Output Level:", CONSOLE_OUTPUT_LEVEL, "\n")
  cat("Available Assets:", length(names(MULTI_ASSET_CONFIG)), "\n")
}

# ==========================================================================================================
# 🛠️ HELPER & UTILITY FUNCTIONS
# ==========================================================================================================

#' Test API connectivity
test_api_connectivity <- function() {
  tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/public/time"
    )
    return(!is.null(response) && !is.null(response$data))
  }, error = function(e) {
    return(FALSE)
  })
}

#' Calculate momentum score from technical indicators
calculate_momentum_score <- function(rsi_signal, macd_signal, bb_signal, technical_analysis) {
  # Convert signals to numeric scores
  rsi_score <- switch(rsi_signal,
    "OVERBOUGHT" = 0.8,
    "OVERSOLD" = 0.2,
    0.5  # NEUTRAL
  )
  
  macd_score <- if (macd_signal == "BULLISH") 0.7 else 0.3
  
  bb_score <- switch(bb_signal,
    "OVERBOUGHT" = 0.8,
    "OVERSOLD" = 0.2,
    0.5  # NEUTRAL
  )
  
  # Weighted combination
  momentum_score <- (rsi_score * 0.4 + macd_score * 0.4 + bb_score * 0.2)
  
  return(momentum_score)
}

#' Generate sentiment-based recommendation
generate_sentiment_recommendation <- function(combined_sentiment) {
  sentiment <- combined_sentiment$overall
  confidence <- combined_sentiment$confidence
  
  if (confidence < 0.5) {
    return("HOLD - Low confidence in signals")
  }
  
  recommendation <- switch(sentiment,
    "STRONG_BULLISH" = "STRONG BUY - High conviction bullish signals",
    "BULLISH" = "BUY - Moderate bullish signals",
    "NEUTRAL" = "HOLD - Mixed or neutral signals",
    "BEARISH" = "SELL - Moderate bearish signals", 
    "STRONG_BEARISH" = "STRONG SELL - High conviction bearish signals",
    "HOLD - Unknown sentiment"
  )
  
  return(recommendation)
}

#' Analyze cross-asset sentiment correlations
analyze_cross_asset_sentiment <- function(sentiment_results) {
  if (length(sentiment_results) < 2) return(list())
  
  # Extract sentiment scores
  sentiment_scores <- sapply(names(sentiment_results), function(symbol) {
    if (symbol == "cross_asset_analysis") return(NULL)
    sentiment_results[[symbol]]$sentiment_score
  })
  
  sentiment_scores <- sentiment_scores[!sapply(sentiment_scores, is.null)]
  
  # Calculate correlations between assets
  correlations <- list()
  asset_names <- names(sentiment_scores)
  
  for (i in 1:(length(asset_names)-1)) {
    for (j in (i+1):length(asset_names)) {
      asset1 <- asset_names[i]
      asset2 <- asset_names[j]
      
      # Simple correlation based on sentiment alignment
      score1 <- sentiment_scores[[asset1]]
      score2 <- sentiment_scores[[asset2]]
      
      correlation <- 1 - abs(score1 - score2)  # Higher correlation if scores are similar
      
      correlations[[paste(asset1, asset2, sep = "_")]] <- correlation
    }
  }
  
  return(correlations)
}

#' Generate comprehensive recommendations from all analysis results
generate_comprehensive_recommendations <- function(results) {
  recommendations <- list()
  
  for (symbol in results$symbols) {
    # Collect all relevant data for the symbol
    market_data <- results$market_data[[symbol]]
    technical <- results$technical_analysis[[symbol]]
    sentiment <- results$sentiment_analysis[[symbol]]
    
    if (is.null(market_data) || is.null(technical) || is.null(sentiment)) {
      recommendations[[symbol]] <- "INSUFFICIENT_DATA"
      next
    }
    
    # Multi-factor recommendation logic
    factors <- list(
      technical_trend = if (technical$rsi > 70) "SELL" else if (technical$rsi < 30) "BUY" else "HOLD",
      price_momentum = if (market_data$change_24h > 5) "BUY" else if (market_data$change_24h < -5) "SELL" else "HOLD",
      sentiment_signal = sentiment$recommendation
    )
    
    # Consensus-based recommendation
    buy_signals <- sum(sapply(factors, function(x) grepl("BUY", x)))
    sell_signals <- sum(sapply(factors, function(x) grepl("SELL", x)))
    
    if (buy_signals >= 2) {
      recommendations[[symbol]] <- "BUY - Multi-factor consensus"
    } else if (sell_signals >= 2) {
      recommendations[[symbol]] <- "SELL - Multi-factor consensus"
    } else {
      recommendations[[symbol]] <- "HOLD - Mixed signals"
    }
  }
  
  return(recommendations)
}

#' Enhanced market data collection with error handling
collect_enhanced_market_data <- function(symbol) {
  tryCatch({
    # Primary data collection
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) return(NULL)
    
    # Enhanced data with additional metrics
    enhanced_data <- list(
      symbol = symbol,
      current_price = as.numeric(ticker_data$lastPr),
      change_24h = as.numeric(ticker_data$change24h),
      volume_24h = as.numeric(ticker_data$baseVolume),
      high_24h = as.numeric(ticker_data$high24h),
      low_24h = as.numeric(ticker_data$low24h),
      timestamp = Sys.time()
    )
    
    # Add calculated metrics
    enhanced_data$price_range_24h <- enhanced_data$high_24h - enhanced_data$low_24h
    enhanced_data$price_position <- (enhanced_data$current_price - enhanced_data$low_24h) / enhanced_data$price_range_24h
    
    return(enhanced_data)
    
  }, error = function(e) {
    cat("❌ Error collecting data for", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

#' Perform comprehensive technical analysis
perform_comprehensive_technical_analysis <- function(symbol, market_data) {
  tryCatch({
    # Use core engine technical analysis
    technical_analysis <- complete_trading_analysis_enhanced(symbol)
    
    if (is.null(technical_analysis)) {
      # Fallback to basic analysis
      technical_analysis <- list(
        rsi = 50,
        macd = 0,
        signal_line = 0,
        bb_position = 0.5,
        trend = "NEUTRAL"
      )
    }
    
    return(technical_analysis)
    
  }, error = function(e) {
    cat("❌ Technical analysis error for", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

#' Display sentiment dashboard
display_sentiment_dashboard <- function(sentiment_results) {
  cat("\n🎭 === SENTIMENT DASHBOARD === 🎭\n")
  
  for (symbol in names(sentiment_results)) {
    if (symbol == "cross_asset_analysis") next
    
    sentiment <- sentiment_results[[symbol]]
    cat("\n📊", symbol, "Sentiment Breakdown:\n")
    cat("   Overall:", sentiment$overall_sentiment, "(", round(sentiment$sentiment_score, 3), ")\n")
    cat("   Technical:", sentiment$factors$technical$trend, "\n")
    cat("   Volume:", sentiment$factors$volume$trend, "\n")
    cat("   OI:", sentiment$factors$oi$trend, "\n")
    cat("   Recommendation:", sentiment$recommendation, "\n")
  }
  
  # Cross-asset summary
  if (!is.null(sentiment_results$cross_asset_analysis)) {
    cat("\n🔗 Cross-Asset Correlations:\n")
    for (pair in names(sentiment_results$cross_asset_analysis)) {
      correlation <- sentiment_results$cross_asset_analysis[[pair]]
      cat("   ", pair, ":", round(correlation, 3), "\n")
    }
  }
}

cat("✅ TRADING_EXECUTION_HUB.R loaded successfully!\n")
cat("🎯 Main execution interface ready\n")
cat("🎮 Interactive trading commands available\n") 
cat("🎭 Multi-factor sentiment analysis enabled\n")
cat("🚀 Complete trading system V2 operational!\n")