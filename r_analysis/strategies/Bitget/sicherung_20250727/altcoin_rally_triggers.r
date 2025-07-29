# ==========================================================================================================
# 🚀 ALTCOIN RALLY TRIGGERS MONITORING SYSTEM
# ==========================================================================================================
# 
# ZWECK: Überwacht kritische Indikatoren für Altcoin-Rally Potentiale
# INTEGRATION: Für rexecution.r Script
# DATEN: Bitget API + berechnete Indikatoren
# OUTPUT: Professional Trading Signals für Altcoin-Timing
# 
# ==========================================================================================================

cat("🚀 Loading Altcoin Rally Triggers Monitoring System...\n")

# ==========================================================================================================
# 📊 CORE RALLY TRIGGER FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ BITCOIN DOMINANCE CALCULATION - Kern-Indikator für Alt-Season                                       │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
calculate_bitcoin_dominance_proxy <- function() {
  cat("📊 Calculating Bitcoin Dominance Proxy...\n")
  
  # Sammle Multi-Asset Daten
  btc_data <- NULL
  eth_data <- NULL
  ada_data <- NULL
  algo_data <- NULL
  
  # BTC Daten
  if (exists("get_enhanced_ticker_data")) {
    tryCatch({
      btc_data <- get_enhanced_ticker_data("BTCUSDT_UMCBL")
    }, error = function(e) {
      cat("⚠️ BTC data unavailable\n")
    })
  }
  
  # ETH Daten  
  tryCatch({
    eth_data <- get_enhanced_ticker_data("ETHUSDT_UMCBL")
  }, error = function(e) {
    cat("⚠️ ETH data unavailable\n")
  })
  
  # ADA Daten
  tryCatch({
    ada_data <- get_enhanced_ticker_data("ADAUSDT_UMCBL")
  }, error = function(e) {
    cat("⚠️ ADA data unavailable\n")
  })
  
  # ALGO Daten
  tryCatch({
    algo_data <- get_enhanced_ticker_data("ALGOUSDT_UMCBL")
  }, error = function(e) {
    cat("⚠️ ALGO data unavailable\n")
  })
  
  # Proxy-Berechnung basierend auf verfügbaren Daten
  dominance_data <- list(
    timestamp = Sys.time(),
    btc_price = if(!is.null(btc_data)) btc_data$last_price else 61000,
    eth_price = if(!is.null(eth_data)) eth_data$last_price else 3400,
    ada_price = if(!is.null(ada_data)) ada_data$last_price else 0.73,
    algo_price = if(!is.null(algo_data)) algo_data$last_price else 0.29
  )
  
  # Simplified Market Cap Proxy (Preis * geschätzte Supply)
  btc_mcap_proxy <- dominance_data$btc_price * 19700000  # ~19.7M BTC
  eth_mcap_proxy <- dominance_data$eth_price * 120000000 # ~120M ETH
  ada_mcap_proxy <- dominance_data$ada_price * 35000000000 # ~35B ADA
  algo_mcap_proxy <- dominance_data$algo_price * 8000000000 # ~8B ALGO
  
  total_mcap_proxy <- btc_mcap_proxy + eth_mcap_proxy + ada_mcap_proxy + algo_mcap_proxy
  
  # Bitcoin Dominance Proxy
  btc_dominance_proxy <- (btc_mcap_proxy / total_mcap_proxy) * 100
  
  # ETH/BTC Ratio (Wichtiger Alt-Indikator)
  eth_btc_ratio <- dominance_data$eth_price / dominance_data$btc_price
  
  dominance_result <- list(
    btc_dominance_proxy = round(btc_dominance_proxy, 2),
    eth_btc_ratio = round(eth_btc_ratio, 6),
    btc_mcap_proxy = btc_mcap_proxy,
    total_mcap_proxy = total_mcap_proxy,
    calculation_time = Sys.time(),
    
    # Altcoin Season Signals
    alt_season_signal = if(btc_dominance_proxy < 50) "STRONG_ALT_SEASON" 
                       else if(btc_dominance_proxy < 55) "ALT_SEASON_STARTING" 
                       else if(btc_dominance_proxy > 65) "BTC_DOMINANCE_HIGH" 
                       else "NEUTRAL",
    
    eth_strength_signal = if(eth_btc_ratio > 0.055) "ETH_VERY_STRONG"
                         else if(eth_btc_ratio > 0.050) "ETH_STRONG" 
                         else if(eth_btc_ratio < 0.040) "ETH_WEAK"
                         else "ETH_NEUTRAL"
  )
  
  cat(sprintf("📊 BTC Dominance Proxy: %.1f%% (%s)\n", 
              btc_dominance_proxy, dominance_result$alt_season_signal))
  cat(sprintf("📊 ETH/BTC Ratio: %.5f (%s)\n", 
              eth_btc_ratio, dominance_result$eth_strength_signal))
  
  return(dominance_result)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ RELATIVE STRENGTH ANALYSIS - ADA/BTC und ALGO/BTC Performance                                       │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
calculate_altcoin_relative_strength <- function() {
  cat("📈 Calculating Altcoin Relative Strength...\n")
  
  # Hole aktuelle Preise
  btc_price <- 61000  # Fallback
  ada_price <- 0.73   # Fallback
  algo_price <- 0.29  # Fallback
  
  btc_change_24h <- 0
  ada_change_24h <- 0
  algo_change_24h <- 0
  
  # Live Daten holen
  tryCatch({
    btc_data <- get_enhanced_ticker_data("BTCUSDT_UMCBL")
    if (!is.null(btc_data)) {
      btc_price <- btc_data$last_price
      btc_change_24h <- btc_data$change_24h_pct
    }
  }, error = function(e) NULL)
  
  tryCatch({
    ada_data <- get_enhanced_ticker_data("ADAUSDT_UMCBL")
    if (!is.null(ada_data)) {
      ada_price <- ada_data$last_price
      ada_change_24h <- ada_data$change_24h_pct
    }
  }, error = function(e) NULL)
  
  tryCatch({
    algo_data <- get_enhanced_ticker_data("ALGOUSDT_UMCBL")
    if (!is.null(algo_data)) {
      algo_price <- algo_data$last_price
      algo_change_24h <- algo_data$change_24h_pct
    }
  }, error = function(e) NULL)
  
  # Relative Strength Berechnungen
  ada_btc_ratio <- ada_price / btc_price
  algo_btc_ratio <- algo_price / btc_price
  
  # Relative Performance (24h)
  ada_rel_performance <- ada_change_24h - btc_change_24h
  algo_rel_performance <- algo_change_24h - btc_change_24h
  
  # Strength Classification
  classify_strength <- function(rel_perf) {
    if (rel_perf > 5) "VERY_STRONG"
    else if (rel_perf > 2) "STRONG" 
    else if (rel_perf > -2) "NEUTRAL"
    else if (rel_perf > -5) "WEAK"
    else "VERY_WEAK"
  }
  
  strength_result <- list(
    ada_btc_ratio = round(ada_btc_ratio, 8),
    algo_btc_ratio = round(algo_btc_ratio, 8),
    ada_rel_performance = round(ada_rel_performance, 2),
    algo_rel_performance = round(algo_rel_performance, 2),
    ada_strength = classify_strength(ada_rel_performance),
    algo_strength = classify_strength(algo_rel_performance),
    
    # Overall Altcoin Strength
    overall_alt_strength = if(ada_rel_performance > 0 && algo_rel_performance > 0) "ALTS_OUTPERFORMING"
                          else if(ada_rel_performance < -3 && algo_rel_performance < -3) "ALTS_UNDERPERFORMING" 
                          else "MIXED_PERFORMANCE",
    
    calculation_time = Sys.time()
  )
  
  cat(sprintf("📈 ADA vs BTC: %+.2f%% (%s)\n", 
              ada_rel_performance, strength_result$ada_strength))
  cat(sprintf("📈 ALGO vs BTC: %+.2f%% (%s)\n", 
              algo_rel_performance, strength_result$algo_strength))
  cat(sprintf("🎯 Overall: %s\n", strength_result$overall_alt_strength))
  
  return(strength_result)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ FUNDING RATE DIVERGENCE - Sentiment-Indikator für Long/Short Bias                                   │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
analyze_funding_rate_divergence <- function() {
  cat("💰 Analyzing Funding Rate Divergence...\n")
  
  funding_data <- list()
  symbols <- c("BTCUSDT_UMCBL", "ETHUSDT_UMCBL", "ADAUSDT_UMCBL", "ALGOUSDT_UMCBL")
  
  for (symbol in symbols) {
    tryCatch({
      ticker <- get_enhanced_ticker_data(symbol)
      if (!is.null(ticker) && !is.null(ticker$funding_rate)) {
        asset_name <- switch(symbol,
                           "BTCUSDT_UMCBL" = "BTC",
                           "ETHUSDT_UMCBL" = "ETH", 
                           "ADAUSDT_UMCBL" = "ADA",
                           "ALGOUSDT_UMCBL" = "ALGO")
        
        funding_data[[asset_name]] <- list(
          funding_rate = ticker$funding_rate,
          funding_rate_pct = ticker$funding_rate * 100
        )
      }
    }, error = function(e) {
      cat(sprintf("⚠️ Funding rate unavailable for %s\n", symbol))
    })
  }
  
  # Divergence Analysis
  if (length(funding_data) >= 2) {
    btc_funding <- funding_data$BTC$funding_rate_pct %||% 0.01
    avg_alt_funding <- mean(c(
      funding_data$ETH$funding_rate_pct %||% 0.01,
      funding_data$ADA$funding_rate_pct %||% 0.01,
      funding_data$ALGO$funding_rate_pct %||% 0.01
    ), na.rm = TRUE)
    
    funding_divergence <- avg_alt_funding - btc_funding
    
    divergence_signal <- if (funding_divergence > 0.005) "ALTS_MORE_BULLISH"
                        else if (funding_divergence < -0.005) "BTC_MORE_BULLISH" 
                        else "FUNDING_NEUTRAL"
  } else {
    funding_divergence <- 0
    divergence_signal <- "INSUFFICIENT_DATA"
  }
  
  funding_result <- list(
    funding_rates = funding_data,
    funding_divergence = round(funding_divergence, 4),
    divergence_signal = divergence_signal,
    interpretation = switch(divergence_signal,
                          "ALTS_MORE_BULLISH" = "Altcoins haben höhere Funding Rates = Mehr Long-Interesse",
                          "BTC_MORE_BULLISH" = "BTC hat höhere Funding Rate = BTC-favorisiert",
                          "FUNDING_NEUTRAL" = "Ausgeglichene Funding Rates",
                          "Nicht genug Daten"),
    calculation_time = Sys.time()
  )
  
  cat(sprintf("💰 Funding Divergence: %+.3f%% (%s)\n", 
              funding_divergence, divergence_signal))
  
  return(funding_result)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ VOLUME MOMENTUM ANALYSIS - Volume-based Rally Indicators                                            │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
analyze_volume_momentum <- function() {
  cat("📊 Analyzing Volume Momentum...\n")
  
  volume_data <- list()
  symbols <- c("BTCUSDT_UMCBL", "ADAUSDT_UMCBL", "ALGOUSDT_UMCBL")
  
  for (symbol in symbols) {
    tryCatch({
      ticker <- get_enhanced_ticker_data(symbol)
      if (!is.null(ticker)) {
        asset_name <- switch(symbol,
                           "BTCUSDT_UMCBL" = "BTC",
                           "ADAUSDT_UMCBL" = "ADA", 
                           "ALGOUSDT_UMCBL" = "ALGO")
        
        volume_data[[asset_name]] <- list(
          volume_24h = ticker$volume_24h_usdt,
          change_24h = ticker$change_24h_pct,
          price = ticker$last_price
        )
      }
    }, error = function(e) {
      cat(sprintf("⚠️ Volume data unavailable for %s\n", symbol))
    })
  }
  
  # Volume Ratios berechnen
  if (length(volume_data) >= 2) {
    btc_volume <- volume_data$BTC$volume_24h %||% 1000000000
    ada_volume <- volume_data$ADA$volume_24h %||% 200000000
    algo_volume <- volume_data$ALGO$volume_24h %||% 50000000
    
    total_alt_volume <- ada_volume + algo_volume
    alt_btc_volume_ratio <- total_alt_volume / btc_volume
    
    # Volume vs Price Momentum
    volume_momentum_signals <- list()
    
    for (asset in names(volume_data)) {
      data <- volume_data[[asset]]
      if (!is.null(data$volume_24h) && !is.null(data$change_24h)) {
        # High Volume + Positive Price = Bullish
        # High Volume + Negative Price = Bearish/Distribution
        volume_millions <- data$volume_24h / 1000000
        
        momentum_signal <- if (data$change_24h > 2 && volume_millions > 100) "STRONG_BULLISH"
                          else if (data$change_24h > 0 && volume_millions > 50) "BULLISH"
                          else if (data$change_24h < -2 && volume_millions > 100) "STRONG_BEARISH" 
                          else if (data$change_24h < 0 && volume_millions > 50) "BEARISH"
                          else "NEUTRAL"
        
        volume_momentum_signals[[asset]] <- list(
          volume_millions = round(volume_millions, 1),
          change_24h = data$change_24h,
          signal = momentum_signal
        )
      }
    }
  } else {
    alt_btc_volume_ratio <- 0.2  # Fallback
    volume_momentum_signals <- list()
  }
  
  # Overall Volume Assessment
  overall_volume_signal <- if (alt_btc_volume_ratio > 0.4) "HIGH_ALT_VOLUME"
                          else if (alt_btc_volume_ratio > 0.25) "MODERATE_ALT_VOLUME"
                          else "LOW_ALT_VOLUME"
  
  volume_result <- list(
    alt_btc_volume_ratio = round(alt_btc_volume_ratio, 3),
    overall_volume_signal = overall_volume_signal,
    individual_signals = volume_momentum_signals,
    calculation_time = Sys.time()
  )
  
  cat(sprintf("📊 Alt/BTC Volume Ratio: %.3f (%s)\n", 
              alt_btc_volume_ratio, overall_volume_signal))
  
  return(volume_result)
}

# ==========================================================================================================
# 🎯 MASTER ALTCOIN RALLY TRIGGERS FUNCTION
# ==========================================================================================================

analyze_altcoin_rally_triggers <- function(display_summary = TRUE) {
  cat("\n🚀 ALTCOIN RALLY TRIGGERS ANALYSIS\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf("📅 Analysis Time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat(strrep("=", 70), "\n")
  
  # Sammle alle Trigger-Daten
  trigger_data <- list()
  
  # 1. Bitcoin Dominance
  cat("\n📊 1/4 Analyzing Bitcoin Dominance...\n")
  trigger_data$dominance <- calculate_bitcoin_dominance_proxy()
  
  # 2. Relative Strength
  cat("\n📈 2/4 Analyzing Relative Strength...\n")
  trigger_data$relative_strength <- calculate_altcoin_relative_strength()
  
  # 3. Funding Rate Divergence
  cat("\n💰 3/4 Analyzing Funding Rate Divergence...\n")
  trigger_data$funding <- analyze_funding_rate_divergence()
  
  # 4. Volume Momentum
  cat("\n📊 4/4 Analyzing Volume Momentum...\n")
  trigger_data$volume <- analyze_volume_momentum()
  
  # Master Signal Berechnung
  trigger_score <- calculate_rally_trigger_score(trigger_data)
  trigger_data$master_signal <- trigger_score
  
  # Display Summary
  if (display_summary) {
    display_rally_triggers_summary(trigger_data)
  }
  
  return(trigger_data)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ RALLY TRIGGER SCORE CALCULATION - Kombiniert alle Indikatoren zu einem Score                       │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
calculate_rally_trigger_score <- function(trigger_data) {
  cat("🎯 Calculating Master Rally Trigger Score...\n")
  
  score <- 0
  max_score <- 0
  signals <- list()
  
  # Dominance Score (25% Gewichtung)
  if (!is.null(trigger_data$dominance)) {
    dominance_score <- switch(trigger_data$dominance$alt_season_signal,
                             "STRONG_ALT_SEASON" = 25,
                             "ALT_SEASON_STARTING" = 15,
                             "NEUTRAL" = 5,
                             "BTC_DOMINANCE_HIGH" = -10,
                             0)
    score <- score + dominance_score
    max_score <- max_score + 25
    signals$dominance <- trigger_data$dominance$alt_season_signal
  }
  
  # Relative Strength Score (25% Gewichtung)
  if (!is.null(trigger_data$relative_strength)) {
    rel_score <- switch(trigger_data$relative_strength$overall_alt_strength,
                       "ALTS_OUTPERFORMING" = 25,
                       "MIXED_PERFORMANCE" = 10,
                       "ALTS_UNDERPERFORMING" = -15,
                       0)
    score <- score + rel_score
    max_score <- max_score + 25
    signals$relative_strength <- trigger_data$relative_strength$overall_alt_strength
  }
  
  # Funding Score (25% Gewichtung)
  if (!is.null(trigger_data$funding)) {
    funding_score <- switch(trigger_data$funding$divergence_signal,
                           "ALTS_MORE_BULLISH" = 25,
                           "FUNDING_NEUTRAL" = 10,
                           "BTC_MORE_BULLISH" = -10,
                           0)
    score <- score + funding_score
    max_score <- max_score + 25
    signals$funding <- trigger_data$funding$divergence_signal
  }
  
  # Volume Score (25% Gewichtung)
  if (!is.null(trigger_data$volume)) {
    volume_score <- switch(trigger_data$volume$overall_volume_signal,
                          "HIGH_ALT_VOLUME" = 25,
                          "MODERATE_ALT_VOLUME" = 15,
                          "LOW_ALT_VOLUME" = 5,
                          0)
    score <- score + volume_score
    max_score <- max_score + 25
    signals$volume <- trigger_data$volume$overall_volume_signal
  }
  
  # Normalize Score
  score_percentage <- if (max_score > 0) (score / max_score) * 100 else 0
  
  # Master Signal Classification
  master_signal <- if (score_percentage >= 75) "STRONG_ALTCOIN_RALLY_SIGNAL"
                   else if (score_percentage >= 50) "MODERATE_ALTCOIN_RALLY_SIGNAL"
                   else if (score_percentage >= 25) "WEAK_ALTCOIN_RALLY_SIGNAL"
                   else if (score_percentage >= 0) "NEUTRAL_ALTCOIN_SIGNAL"
                   else "BEARISH_ALTCOIN_SIGNAL"
  
  master_result <- list(
    score = score,
    max_score = max_score,
    score_percentage = round(score_percentage, 1),
    master_signal = master_signal,
    individual_signals = signals,
    calculation_time = Sys.time(),
    
    # Trading Recommendations
    recommendation = switch(master_signal,
                          "STRONG_ALTCOIN_RALLY_SIGNAL" = "AGGRESSIVE BUY - Alt-Season likely starting",
                          "MODERATE_ALTCOIN_RALLY_SIGNAL" = "MODERATE BUY - Positive signals emerging", 
                          "WEAK_ALTCOIN_RALLY_SIGNAL" = "WATCH CLOSELY - Some positive signs",
                          "NEUTRAL_ALTCOIN_SIGNAL" = "HOLD - No clear direction",
                          "BEARISH_ALTCOIN_SIGNAL" = "CAUTION - Consider reducing positions")
  )
  
  cat(sprintf("🎯 Rally Trigger Score: %.1f%% (%s)\n", 
              score_percentage, master_signal))
  
  return(master_result)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ SUMMARY DISPLAY FUNCTION - Professional Output für rexecution.r                                    │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
display_rally_triggers_summary <- function(trigger_data) {
  cat("\n🎯 ALTCOIN RALLY TRIGGERS SUMMARY\n")
  cat("┌────────────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                        🚀 ALTCOIN RALLY TRIGGERS 🚀                          │\n")
  cat("├────────────────────────────────────────────────────────────────────────────────┤\n")
  
  # Master Signal
  master <- trigger_data$master_signal
  signal_icon <- switch(master$master_signal,
                       "STRONG_ALTCOIN_RALLY_SIGNAL" = "🟢🚀",
                       "MODERATE_ALTCOIN_RALLY_SIGNAL" = "🟡📈",
                       "WEAK_ALTCOIN_RALLY_SIGNAL" = "🟠⚡",
                       "NEUTRAL_ALTCOIN_SIGNAL" = "⚪⚖️",
                       "BEARISH_ALTCOIN_SIGNAL" = "🔴📉")
  
  cat(sprintf("│ %s MASTER SIGNAL: %-45s │\n", 
              signal_icon, master$master_signal))
  cat(sprintf("│ 🎯 Score: %.1f%% | 💡 %s │\n", 
              master$score_percentage, master$recommendation))
  cat("├────────────────────────────────────────────────────────────────────────────────┤\n")
  
  # Individual Triggers
  cat("│ 📊 INDIVIDUAL TRIGGERS:                                                       │\n")
  
  # Dominance
  if (!is.null(trigger_data$dominance)) {
    dom <- trigger_data$dominance
    cat(sprintf("│ 📊 BTC Dominance: %.1f%% (%s) │\n", 
                dom$btc_dominance_proxy, dom$alt_season_signal))
    cat(sprintf("│ 📊 ETH/BTC Ratio: %.5f (%s) │\n", 
                dom$eth_btc_ratio, dom$eth_strength_signal))
  }
  
  # Relative Strength
  if (!is.null(trigger_data$relative_strength)) {
    rel <- trigger_data$relative_strength
    cat(sprintf("│ 📈 ADA vs BTC: %+.2f%% (%s) │\n", 
                rel$ada_rel_performance, rel$ada_strength))
    cat(sprintf("│ 📈 ALGO vs BTC: %+.2f%% (%s) │\n", 
                rel$algo_rel_performance, rel$algo_strength))
  }
  
  # Funding
  if (!is.null(trigger_data$funding)) {
    fund <- trigger_data$funding
    cat(sprintf("│ 💰 Funding Divergence: %+.3f%% (%s) │\n", 
                fund$funding_divergence, fund$divergence_signal))
  }
  
  # Volume
  if (!is.null(trigger_data$volume)) {
    vol <- trigger_data$volume
    cat(sprintf("│ 📊 Alt/BTC Volume: %.3f (%s) │\n", 
                vol$alt_btc_volume_ratio, vol$overall_volume_signal))
  }
  
  cat("└────────────────────────────────────────────────────────────────────────────────┘\n")
  
  # Action Items
  cat("\n💡 IMMEDIATE ACTION ITEMS:\n")
  cat("=========================\n")
  
  if (master$score_percentage >= 60) {
    cat("🟢 BULLISH SIGNALS DETECTED:\n")
    cat("   • Consider increasing altcoin positions\n")
    cat("   • Watch for breakouts in ADA (>0.75) and ALGO (>0.30)\n")
    cat("   • Set alerts for BTC dominance <50%\n")
  } else if (master$score_percentage >= 25) {
    cat("🟡 MIXED SIGNALS:\n")
    cat("   • Maintain current positions\n") 
    cat("   • Prepare for potential rally\n")
    cat("   • Monitor key resistance levels\n")
  } else {
    cat("🔴 BEARISH/NEUTRAL SIGNALS:\n")
    cat("   • Consider defensive positioning\n")
    cat("   • Set tight stop-losses\n")
    cat("   • Wait for clearer bullish signals\n")
  }
  
  cat(sprintf("\n📅 Next Analysis: Recommended in 4-6 hours\n"))
  cat(sprintf("⏰ Analysis Time: %s\n", format(Sys.time(), "%H:%M:%S")))
}

# ==========================================================================================================
# ✅ INTEGRATION FUNCTION FÜR REXECUTION.R
# ==========================================================================================================

# Diese Funktion wird in dein rexecution.r Script eingebaut
run_altcoin_rally_monitoring <- function() {
  cat("\n")
  cat(strrep("█", 80), "\n")
  cat("🚀 ALTCOIN RALLY TRIGGERS MONITORING\n")
  cat(strrep("█", 80), "\n")
  
  # Führe komplette Analyse durch
  trigger_results <- analyze_altcoin_rally_triggers(display_summary = TRUE)
  
  # Return für weitere Verarbeitung
  return(trigger_results)
}

# ==========================================================================================================
# 🎯 SYSTEM READY MESSAGE
# ==========================================================================================================

cat("✅ ALTCOIN RALLY TRIGGERS MONITORING SYSTEM LOADED!\n")
cat(strrep("=", 70), "\n")
cat("🎯 INTEGRATION COMMAND FOR rexecution.r:\n")
cat("\n")
cat("# Add this line to your rexecution.r:\n")
cat("altcoin_triggers <- run_altcoin_rally_monitoring()\n")
cat("\n")
cat("🔍 AVAILABLE FUNCTIONS:\n")
cat("   analyze_altcoin_rally_triggers()           # Full analysis\n")
cat("   calculate_bitcoin_dominance_proxy()        # BTC dominance\n")
cat("   calculate_altcoin_relative_strength()      # Relative performance\n")
cat("   analyze_funding_rate_divergence()          # Funding analysis\n")
cat("   analyze_volume_momentum()                  # Volume analysis\n")
cat("\n")
cat("🚀 Ready for Altcoin Rally Detection!\n")
cat(strrep("=", 70), "\n")