# ==========================================================================================================
# 🚀 OPTIMIZED ALTCOIN RALLY TRIGGERS - PRODUCTION VERSION
# ==========================================================================================================
# 
# ZWECK: Überwacht kritische Indikatoren für Altcoin-Rally Potentiale mit verfügbaren Bitget Daten
# INTEGRATION: Für rexecution.r Script  
# FOCUS: Maximale Nutzung verfügbarer APIs + intelligente Proxies für Elite-Daten
# OUTPUT: Professional Trading Signals mit robusten Indikatoren
# 
# ==========================================================================================================

cat("🚀 Loading Optimized Altcoin Rally Triggers (Production Version)...\n")

# ==========================================================================================================
# 📊 ENHANCED MARKET DATA ANALYSIS - Mit robusten verfügbaren Daten
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ ENHANCED TAKER ANALYSIS - Basierend auf verfügbaren Trades-Daten                                    │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
analyze_enhanced_taker_behavior <- function(symbol = "ADAUSDT_UMCBL") {
  cat("💹 Analyzing Enhanced Taker Behavior for", symbol, "...\n")
  
  # Hole umfangreiche Trades-Daten (mehr Samples für bessere Genauigkeit)
  trades_data <- get_enhanced_trades(symbol, 200)  # Größere Sample-Size
  
  if (!is.null(trades_data) && trades_data$total_trades > 50) {
    # Erweiterte Taker-Analyse
    buy_ratio <- trades_data$buy_volume / (trades_data$buy_volume + trades_data$sell_volume)
    sell_ratio <- 1 - buy_ratio
    
    # Momentum-basierte Klassifikation
    taker_ratio <- buy_ratio / max(sell_ratio, 0.01)
    
    # Enhanced Aggression Levels
    aggression_level <- if(taker_ratio > 1.4) "EXTREME_BUY_AGGRESSION"
                       else if(taker_ratio > 1.2) "HIGH_BUY_AGGRESSION"
                       else if(taker_ratio > 1.05) "MODERATE_BUY_AGGRESSION"
                       else if(taker_ratio > 0.95) "BALANCED_AGGRESSION"
                       else if(taker_ratio > 0.8) "MODERATE_SELL_AGGRESSION"
                       else if(taker_ratio > 0.6) "HIGH_SELL_AGGRESSION"
                       else "EXTREME_SELL_AGGRESSION"
    
    # Trade Count Analysis (zusätzlicher Indikator)
    buy_trade_count <- trades_data$buy_trades_count %||% (trades_data$total_trades * buy_ratio)
    sell_trade_count <- trades_data$sell_trades_count %||% (trades_data$total_trades * sell_ratio)
    trade_count_ratio <- buy_trade_count / max(sell_trade_count, 1)
    
    # Volume vs Count Divergence (Whales vs Retail)
    volume_count_divergence <- taker_ratio - trade_count_ratio
    whale_activity <- if(volume_count_divergence > 0.2) "WHALE_BUYING"
                     else if(volume_count_divergence < -0.2) "WHALE_SELLING"
                     else "RETAIL_DOMINATED"
    
    enhanced_taker_data <- list(
      symbol = symbol,
      timestamp = Sys.time(),
      buy_ratio = round(buy_ratio, 3),
      sell_ratio = round(sell_ratio, 3),
      taker_ratio = round(taker_ratio, 3),
      aggression_level = aggression_level,
      buy_volume = trades_data$buy_volume,
      sell_volume = trades_data$sell_volume,
      trade_count_ratio = round(trade_count_ratio, 3),
      whale_activity = whale_activity,
      total_trades_analyzed = trades_data$total_trades,
      data_quality = "ENHANCED_TRADES"
    )
    
    cat(sprintf("💹 Enhanced Taker: %.2f (%s) | Whale: %s\n", 
                taker_ratio, aggression_level, whale_activity))
    
  } else {
    # Fallback mit Sentiment-Proxy
    market_data <- get_enhanced_market_data(symbol)
    sentiment_pct <- market_data$sentiment$percentage %||% 50
    
    # Convert Sentiment to Taker Behavior
    taker_ratio_est <- 0.7 + (sentiment_pct / 100) * 0.6  # Range: 0.7-1.3
    
    aggression_level <- if(taker_ratio_est > 1.1) "MODERATE_BUY_AGGRESSION"
                       else if(taker_ratio_est < 0.9) "MODERATE_SELL_AGGRESSION"
                       else "BALANCED_AGGRESSION"
    
    enhanced_taker_data <- list(
      symbol = symbol,
      timestamp = Sys.time(),
      buy_ratio = taker_ratio_est / (1 + taker_ratio_est),
      sell_ratio = 1 / (1 + taker_ratio_est),
      taker_ratio = round(taker_ratio_est, 3),
      aggression_level = aggression_level,
      whale_activity = "UNKNOWN",
      total_trades_analyzed = 0,
      data_quality = "SENTIMENT_PROXY"
    )
    
    cat(sprintf("💹 Estimated Taker: %.2f (%s)\n", 
                taker_ratio_est, aggression_level))
  }
  
  return(enhanced_taker_data)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ INTELLIGENT LONG/SHORT PROXY - Basierend auf Funding Rate + Market Sentiment                       │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
calculate_intelligent_ls_proxy <- function(symbol = "ADAUSDT_UMCBL") {
  cat("⚖️ Calculating Intelligent L/S Proxy for", symbol, "...\n")
  
  # Sammle verfügbare Daten
  market_data <- get_enhanced_market_data(symbol)
  
  funding_rate <- market_data$ticker$funding_rate %||% 0.01
  sentiment_pct <- market_data$sentiment$percentage %||% 50
  price_change_24h <- market_data$ticker$change_24h_pct %||% 0
  
  # SOPHISTICATED L/S RATIO CALCULATION
  # Funding Rate Impact: Positive = Long bias, Negative = Short bias
  funding_factor <- if(funding_rate > 0.03) 0.15      # Very high funding = extreme long bias
                   else if(funding_rate > 0.015) 0.08  # High funding = long bias
                   else if(funding_rate > 0) 0.03      # Positive funding = slight long bias
                   else if(funding_rate > -0.015) -0.03 # Negative funding = slight short bias
                   else -0.08                          # Very negative = short bias
  
  # Sentiment Impact
  sentiment_factor <- (sentiment_pct - 50) / 100 * 0.2  # Max ±0.2 impact
  
  # Price Action Impact (momentum)
  momentum_factor <- if(price_change_24h > 5) 0.1
                    else if(price_change_24h > 2) 0.05
                    else if(price_change_24h > 0) 0.02
                    else if(price_change_24h > -2) -0.02
                    else if(price_change_24h > -5) -0.05
                    else -0.1
  
  # Combined L/S Ratio (base 1.0 = balanced)
  base_ratio <- 1.0
  adjusted_ratio <- base_ratio + funding_factor + sentiment_factor + momentum_factor
  adjusted_ratio <- max(0.3, min(2.5, adjusted_ratio))  # Clamp to realistic range
  
  # Calculate Long/Short percentages
  long_ratio <- adjusted_ratio / (1 + adjusted_ratio)
  short_ratio <- 1 - long_ratio
  
  # Classification
  ratio_signal <- if(adjusted_ratio > 1.6) "STRONGLY_LONG_BIASED"
                 else if(adjusted_ratio > 1.3) "MODERATELY_LONG_BIASED"
                 else if(adjusted_ratio > 0.8) "BALANCED"
                 else if(adjusted_ratio > 0.6) "MODERATELY_SHORT_BIASED"
                 else "STRONGLY_SHORT_BIASED"
  
  ls_proxy_data <- list(
    symbol = symbol,
    timestamp = Sys.time(),
    long_ratio = round(long_ratio, 3),
    short_ratio = round(short_ratio, 3),
    long_short_ratio = round(adjusted_ratio, 3),
    ratio_signal = ratio_signal,
    
    # Component Analysis
    funding_component = funding_factor,
    sentiment_component = sentiment_factor,
    momentum_component = momentum_factor,
    
    # Source Data
    funding_rate_pct = funding_rate * 100,
    sentiment_pct = sentiment_pct,
    price_change_24h = price_change_24h,
    data_quality = "INTELLIGENT_PROXY"
  )
  
  cat(sprintf("⚖️ Intelligent L/S Ratio: %.2f (%s)\n", 
              adjusted_ratio, ratio_signal))
  cat(sprintf("   📊 Components: Funding: %+.2f | Sentiment: %+.2f | Momentum: %+.2f\n",
              funding_factor, sentiment_factor, momentum_factor))
  
  return(ls_proxy_data)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ ELITE TRADER PROXY - Basierend auf Volume, Funding und Orderbook Patterns                          │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
estimate_elite_trader_sentiment <- function(symbol = "ADAUSDT_UMCBL") {
  cat("🏦 Estimating Elite Trader Sentiment for", symbol, "...\n")
  
  # Sammle alle verfügbaren Daten
  market_data <- get_enhanced_market_data(symbol)
  
  # ELITE TRADER INDICATORS
  # 1. Funding Rate (Elite Traders drive funding)
  funding_rate <- market_data$ticker$funding_rate %||% 0.01
  funding_signal <- if(funding_rate > 0.025) 2        # Very bullish elite sentiment
                   else if(funding_rate > 0.015) 1     # Bullish elite sentiment
                   else if(funding_rate > 0) 0.5       # Slightly bullish
                   else if(funding_rate > -0.015) -0.5 # Slightly bearish
                   else if(funding_rate > -0.025) -1   # Bearish
                   else -2                             # Very bearish
  
  # 2. Volume Analysis (Elite traders create volume spikes)
  volume_24h <- market_data$ticker$volume_24h_usdt %||% 100000000
  volume_millions <- volume_24h / 1000000
  
  # Symbol-specific volume thresholds
  volume_threshold <- if(symbol == "ADAUSDT_UMCBL") 200    # ADA typical volume
                     else if(symbol == "ALGOUSDT_UMCBL") 50 # ALGO typical volume
                     else 100                               # Default
  
  volume_signal <- if(volume_millions > volume_threshold * 2) 2      # Extreme volume = elite activity
                  else if(volume_millions > volume_threshold * 1.5) 1 # High volume
                  else if(volume_millions > volume_threshold) 0.5     # Above average
                  else if(volume_millions > volume_threshold * 0.5) 0 # Normal
                  else -1                                             # Low volume = elite exit
  
  # 3. Orderbook Imbalance (Elite orders create imbalances)
  bid_ask_ratio <- market_data$orderbook$bid_ask_ratio %||% 1.0
  orderbook_signal <- if(bid_ask_ratio > 1.5) 1.5          # Strong bid pressure
                     else if(bid_ask_ratio > 1.2) 1         # Moderate bid pressure
                     else if(bid_ask_ratio > 0.8) 0         # Balanced
                     else if(bid_ask_ratio > 0.6) -1        # Ask pressure
                     else -1.5                              # Strong ask pressure
  
  # 4. Price Action Quality (Elite traders create clean moves)
  price_change := market_data$ticker$change_24h_pct %||% 0
  quality_signal <- if(abs(price_change) > 5 && volume_signal > 0) 1    # Strong move with volume
                   else if(abs(price_change) > 2 && volume_signal > 0) 0.5 # Moderate move
                   else if(abs(price_change) < 1) -0.5                   # Choppy/indecision
                   else 0                                                # Normal
  
  # COMPOSITE ELITE SENTIMENT SCORE
  elite_score <- funding_signal + volume_signal + orderbook_signal + quality_signal
  max_possible_score <- 6.5  # Maximum theoretical score
  
  # Normalize to Long/Short Dominance
  long_dominance_raw <- elite_score / max_possible_score
  long_dominance <- max(-0.3, min(0.3, long_dominance_raw))  # Clamp realistic range
  
  # Elite Sentiment Classification
  elite_sentiment <- if(long_dominance > 0.15) "ELITE_VERY_BULLISH"
                    else if(long_dominance > 0.08) "ELITE_BULLISH"
                    else if(long_dominance > -0.08) "ELITE_NEUTRAL"
                    else if(long_dominance > -0.15) "ELITE_BEARISH"
                    else "ELITE_VERY_BEARISH"
  
  # Calculate position ratios
  base_long_ratio <- 0.55  # Baseline assumption
  adjusted_long_ratio <- base_long_ratio + long_dominance
  adjusted_short_ratio := 1 - adjusted_long_ratio
  
  elite_data <- list(
    symbol = symbol,
    timestamp = Sys.time(),
    elite_sentiment = elite_sentiment,
    long_dominance = round(long_dominance, 4),
    long_position_ratio = round(adjusted_long_ratio, 3),
    short_position_ratio = round(adjusted_short_ratio, 3),
    
    # Component Scores
    funding_signal = funding_signal,
    volume_signal = volume_signal,
    orderbook_signal = orderbook_signal,
    quality_signal = quality_signal,
    composite_score = round(elite_score, 2),
    
    # Source Data
    funding_rate_pct = funding_rate * 100,
    volume_millions = round(volume_millions, 1),
    bid_ask_ratio = round(bid_ask_ratio, 3),
    price_change_24h = price_change,
    data_quality = "ELITE_PROXY"
  )
  
  cat(sprintf("🏦 Elite Sentiment: %s (Dominance: %+.1f%%)\n", 
              elite_sentiment, long_dominance * 100))
  cat(sprintf("   📊 Signals: F: %+.1f | V: %+.1f | OB: %+.1f | Q: %+.1f\n",
              funding_signal, volume_signal, orderbook_signal, quality_signal))
  
  return(elite_data)
}

# ==========================================================================================================
# 📊 ENHANCED CORE RALLY TRIGGER FUNCTIONS
# ==========================================================================================================

# Original Bitcoin Dominance Function (optimiert)
calculate_bitcoin_dominance_proxy <- function() {
  cat("📊 Calculating Bitcoin Dominance Proxy...\n")
  
  # Sammle Multi-Asset Daten
  symbols <- c("BTCUSDT_UMCBL", "ETHUSDT_UMCBL", "ADAUSDT_UMCBL", "ALGOUSDT_UMCBL")
  prices <- list()
  
  for (symbol in symbols) {
    tryCatch({
      ticker <- get_enhanced_ticker_data(symbol)
      asset_name <- strsplit(symbol, "USDT")[[1]][1]
      prices[[asset_name]] <- ticker$last_price
    }, error = function(e) {
      # Fallback prices
      fallback_prices <- list(BTC = 119000, ETH = 3350, ADA = 0.76, ALGO = 0.29)
      asset_name := strsplit(symbol, "USDT")[[1]][1]
      prices[[asset_name]] <<- fallback_prices[[asset_name]]
    })
  }
  
  # Enhanced Market Cap Proxy Calculation
  estimated_supplies <- list(
    BTC = 19700000,      # ~19.7M BTC
    ETH = 120000000,     # ~120M ETH  
    ADA = 35000000000,   # ~35B ADA
    ALGO = 8000000000    # ~8B ALGO
  )
  
  mcaps <- mapply(function(price, supply) price * supply, prices, estimated_supplies)
  total_mcap <- sum(mcaps)
  btc_dominance := (mcaps[["BTC"]] / total_mcap) * 100
  
  # Enhanced ALT-Season Detection
  eth_btc_ratio := prices[["ETH"]] / prices[["BTC"]]
  
  # Multi-factor Alt-Season Signal
  dominance_factor := if(btc_dominance < 45) "STRONG_ALT_SEASON"
                     else if(btc_dominance < 55) "ALT_SEASON_STARTING"
                     else if(btc_dominance < 70) "NEUTRAL"
                     else if(btc_dominance < 80) "BTC_DOMINANCE_HIGH"
                     else "BTC_DOMINANCE_EXTREME"
  
  eth_factor := if(eth_btc_ratio > 0.06) "ETH_VERY_STRONG"
               else if(eth_btc_ratio > 0.05) "ETH_STRONG"
               else if(eth_btc_ratio > 0.035) "ETH_NEUTRAL"
               else if(eth_btc_ratio > 0.025) "ETH_WEAK"
               else "ETH_VERY_WEAK"
  
  dominance_result := list(
    btc_dominance_proxy = round(btc_dominance, 2),
    eth_btc_ratio = round(eth_btc_ratio, 6),
    alt_season_signal = dominance_factor,
    eth_strength_signal = eth_factor,
    mcap_breakdown = mcaps,
    calculation_time = Sys.time()
  )
  
  cat(sprintf("📊 BTC Dominance: %.1f%% (%s)\n", btc_dominance, dominance_factor))
  cat(sprintf("📊 ETH/BTC Ratio: %.5f (%s)\n", eth_btc_ratio, eth_factor))
  
  return(dominance_result)
}

# Enhanced Relative Strength Function
calculate_enhanced_altcoin_relative_strength <- function() {
  cat("📈 Calculating Enhanced Altcoin Relative Strength...\n")
  
  symbols := c("BTCUSDT_UMCBL", "ADAUSDT_UMCBL", "ALGOUSDT_UMCBL")
  asset_data := list()
  
  # Sammle Daten für alle Assets
  for (symbol in symbols) {
    tryCatch({
      ticker := get_enhanced_ticker_data(symbol)
      asset_name := strsplit(symbol, "USDT")[[1]][1]
      asset_data[[asset_name]] := list(
        price = ticker$last_price,
        change_24h = ticker$change_24h_pct,
        volume_24h = ticker$volume_24h_usdt
      )
    }, error = function(e) {
      cat(sprintf("⚠️ %s data unavailable\n", symbol))
    })
  }
  
  if (length(asset_data) >= 2) {
    btc_change := asset_data[["BTC"]]$change_24h %||% 0
    ada_change := asset_data[["ADA"]]$change_24h %||% 0
    algo_change := asset_data[["ALGO"]]$change_24h %||% 0
    
    # Enhanced Relative Performance
    ada_rel_perf := ada_change - btc_change
    algo_rel_perf := algo_change - btc_change
    
    # Volume-Weighted Strength (berücksichtigt Volume)
    ada_volume := asset_data[["ADA"]]$volume_24h %||% 200000000
    algo_volume := asset_data[["ALGO"]]$volume_24h %||% 50000000
    btc_volume := asset_data[["BTC"]]$volume_24h %||% 2000000000
    
    ada_volume_strength := if(ada_volume > 300000000) 1.2    # High volume boost
                          else if(ada_volume > 150000000) 1.0 # Normal
                          else 0.8                            # Low volume penalty
    
    algo_volume_strength := if(algo_volume > 80000000) 1.2
                           else if(algo_volume > 40000000) 1.0
                           else 0.8
    
    # Adjusted strength with volume
    ada_adjusted_strength := ada_rel_perf * ada_volume_strength
    algo_adjusted_strength := algo_rel_perf * algo_volume_strength
    
    # Enhanced Classification
    classify_enhanced_strength := function(adj_strength) {
      if (adj_strength > 8) "EXTREMELY_STRONG"
      else if (adj_strength > 4) "VERY_STRONG"
      else if (adj_strength > 2) "STRONG"
      else if (adj_strength > -1) "NEUTRAL"
      else if (adj_strength > -3) "WEAK"
      else if (adj_strength > -6) "VERY_WEAK"
      else "EXTREMELY_WEAK"
    }
    
    # Overall Assessment
    both_positive := ada_adjusted_strength > 1 && algo_adjusted_strength > 1
    both_negative := ada_adjusted_strength < -2 && algo_adjusted_strength < -2
    
    overall_strength := if(both_positive) "ALTS_STRONGLY_OUTPERFORMING"
                       else if(ada_adjusted_strength > 0 && algo_adjusted_strength > 0) "ALTS_OUTPERFORMING"
                       else if(both_negative) "ALTS_STRONGLY_UNDERPERFORMING"
                       else if(ada_adjusted_strength < -1 && algo_adjusted_strength < -1) "ALTS_UNDERPERFORMING"
                       else "MIXED_PERFORMANCE"
    
    strength_result := list(
      ada_rel_performance = round(ada_rel_perf, 2),
      algo_rel_performance = round(algo_rel_perf, 2),
      ada_adjusted_strength = round(ada_adjusted_strength, 2),
      algo_adjusted_strength = round(algo_adjusted_strength, 2),
      ada_strength = classify_enhanced_strength(ada_adjusted_strength),
      algo_strength = classify_enhanced_strength(algo_adjusted_strength),
      overall_alt_strength = overall_strength,
      volume_factors = list(
        ada_volume_strength = ada_volume_strength,
        algo_volume_strength = algo_volume_strength
      ),
      calculation_time = Sys.time()
    )
    
    cat(sprintf("📈 ADA: %+.2f%% (Adj: %+.2f) - %s\n", 
                ada_rel_perf, ada_adjusted_strength, strength_result$ada_strength))
    cat(sprintf("📈 ALGO: %+.2f%% (Adj: %+.2f) - %s\n", 
                algo_rel_perf, algo_adjusted_strength, strength_result$algo_strength))
    cat(sprintf("🎯 Overall: %s\n", overall_strength))
    
  } else {
    # Fallback
    strength_result := list(
      ada_rel_performance = 0,
      algo_rel_performance = 0,
      overall_alt_strength = "INSUFFICIENT_DATA",
      calculation_time = Sys.time()
    )
  }
  
  return(strength_result)
}

# ==========================================================================================================
# 🎯 OPTIMIZED MASTER ALTCOIN RALLY TRIGGERS
# ==========================================================================================================

analyze_optimized_altcoin_rally_triggers <- function(display_summary = TRUE) {
  cat("\n🚀 OPTIMIZED ALTCOIN RALLY TRIGGERS ANALYSIS\n")
  cat(strrep("=", 80), "\n")
  cat(sprintf("📅 Analysis Time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat(strrep("=", 80), "\n")
  
  # Sammle alle verfügbaren Daten
  trigger_data := list()
  
  # === CORE INDICATORS ===
  cat("\n📊 1/6 Analyzing Bitcoin Dominance...\n")
  trigger_data$dominance := calculate_bitcoin_dominance_proxy()
  
  cat("\n📈 2/6 Analyzing Enhanced Relative Strength...\n")
  trigger_data$relative_strength := calculate_enhanced_altcoin_relative_strength()
  
  # === PROXY ELITE INDICATORS ===
  cat("\n🏦 3/6 Estimating Elite Trader Sentiment...\n")
  trigger_data$elite_ada := estimate_elite_trader_sentiment("ADAUSDT_UMCBL")
  trigger_data$elite_algo := estimate_elite_trader_sentiment("ALGOUSDT_UMCBL")
  
  cat("\n⚖️ 4/6 Calculating Intelligent L/S Proxies...\n")
  trigger_data$ls_proxy_ada := calculate_intelligent_ls_proxy("ADAUSDT_UMCBL")
  trigger_data$ls_proxy_algo := calculate_intelligent_ls_proxy("ALGOUSDT_UMCBL")
  
  cat("\n💹 5/6 Analyzing Enhanced Taker Behavior...\n")
  trigger_data$taker_ada := analyze_enhanced_taker_behavior("ADAUSDT_UMCBL")
  trigger_data$taker_algo := analyze_enhanced_taker_behavior("ALGOUSDT_UMCBL")
  
  # === OPTIMIZED MASTER SIGNAL ===
  cat("\n🎯 6/6 Calculating Optimized Master Signal...\n")
  trigger_score := calculate_optimized_rally_trigger_score(trigger_data)
  trigger_data$master_signal := trigger_score
  
  # Display Summary
  if (display_summary) {
    display_optimized_rally_triggers_summary(trigger_data)
  }
  
  return(trigger_data)
}

# Enhanced Rally Trigger Score Calculation
calculate_optimized_rally_trigger_score <- function(trigger_data) {
  cat("🎯 Calculating Optimized Rally Trigger Score...\n")
  
  score := 0
  max_score := 0
  signals := list()
  data_quality_points := 0
  
  # === 1. DOMINANCE SCORE (20% Gewichtung) ===
  if (!is.null(trigger_data$dominance)) {
    dominance_score := switch(trigger_data$dominance$alt_season_signal,
                             "STRONG_ALT_SEASON" = 20,
                             "ALT_SEASON_STARTING" = 12,
                             "NEUTRAL" = 4,
                             "BTC_DOMINANCE_HIGH" = -8,
                             "BTC_DOMINANCE_EXTREME" = -16,
                             0)
    score := score + dominance_score
    max_score := max_score + 20
    signals$dominance := trigger_data$dominance$alt_season_signal
    data_quality_points := data_quality_points + 1
  }
  
  # === 2. ENHANCED RELATIVE STRENGTH (20% Gewichtung) ===
  if (!is.null(trigger_data$relative_strength)) {
    rel_score := switch(trigger_data$relative_strength$overall_alt_strength,
                       "ALTS_STRONGLY_OUTPERFORMING" = 20,
                       "ALTS_OUTPERFORMING" = 12,
                       "MIXED_PERFORMANCE" = 6,
                       "ALTS_UNDERPERFORMING" = -8,
                       "ALTS_STRONGLY_UNDERPERFORMING" = -16,
                       0)
    score := score + rel_score
    max_score := max_score + 20
    signals$relative_strength := trigger_data$relative_strength$overall_alt_strength
    data_quality_points := data_quality_points + 1
  }
  
  # === 3. ELITE SENTIMENT PROXIES (20% Gewichtung) ===
  elite_score := 0
  elite_signals := c()
  
  if (!is.null(trigger_data$elite_ada)) {
    ada_elite_score := switch(trigger_data$elite_ada$elite_sentiment,
                             "ELITE_VERY_BULLISH" = 10,
                             "ELITE_BULLISH" = 6,
                             "ELITE_NEUTRAL" = 2,
                             "ELITE_BEARISH" = -4,
                             "ELITE_VERY_BEARISH" = -8,
                             0)
    elite_score := elite_score + ada_elite_score
    elite_signals := c(elite_signals, paste("ADA:", trigger_data$elite_ada$elite_sentiment))
    data_quality_points := data_quality_points + 1
  }
  
  if (!is.null(trigger_data$elite_algo)) {
    algo_elite_score := switch(trigger_data$elite_algo$elite_sentiment,
                              "ELITE_VERY_BULLISH" = 10,
                              "ELITE_BULLISH" = 6,
                              "ELITE_NEUTRAL" = 2,
                              "ELITE_BEARISH" = -4,
                              "ELITE_VERY_BEARISH" = -8,
                              0)
    elite_score := elite_score + algo_elite_score
    elite_signals := c(elite_signals, paste("ALGO:", trigger_data$elite_algo$elite_sentiment))
    data_quality_points := data_quality_points + 1
  }
  
  score := score + elite_score
  max_score := max_score + 20
  signals$elite_sentiment := paste(elite_signals, collapse = " | ")
  
  # === 4. L/S PROXY SCORE (15% Gewichtung) ===
  ls_score := 0
  ls_signals := c()
  
  if (!is.null(trigger_data$ls_proxy_ada)) {
    ada_ls_score := switch(trigger_data$ls_proxy_ada$ratio_signal,
                          "STRONGLY_LONG_BIASED" = 8,
                          "MODERATELY_LONG_BIASED" = 5,
                          "BALANCED" = 2,
                          "MODERATELY_SHORT_BIASED" = -3,
                          "STRONGLY_SHORT_BIASED" = -6,
                          0)
    ls_score := ls_score + ada_ls_score
    ls_signals := c(ls_signals, paste("ADA:", trigger_data$ls_proxy_ada$ratio_signal))
    data_quality_points := data_quality_points + 1
  }
  
  if (!is.null(trigger_data$ls_proxy_algo)) {
    algo_ls_score := switch(trigger_data$ls_proxy_algo$ratio_signal,
                           "STRONGLY_LONG_BIASED" = 7,
                           "MODERATELY_LONG_BIASED" = 4,
                           "BALANCED" = 2,
                           "MODERATELY_SHORT_BIASED" = -3,
                           "STRONGLY_SHORT_BIASED" = -6,
                           0)
    ls_score := ls_score + algo_ls_score
    ls_signals := c(ls_signals, paste("ALGO:", trigger_data$ls_proxy_algo$ratio_signal))
    data_quality_points := data_quality_points + 1
  }
  
  score := score + ls_score
  max_score := max_score + 15
  signals$ls_proxies := paste(ls_signals, collapse = " | ")
  
  # === 5. ENHANCED TAKER BEHAVIOR (15% Gewichtung) ===
  taker_score := 0
  taker_signals := c()
  
  if (!is.null(trigger_data$taker_ada)) {
    ada_taker_score := switch(trigger_data$taker_ada$aggression_level,
                             "EXTREME_BUY_AGGRESSION" = 8,
                             "HIGH_BUY_AGGRESSION" = 6,
                             "MODERATE_BUY_AGGRESSION" = 4,
                             "BALANCED_AGGRESSION" = 2,
                             "MODERATE_SELL_AGGRESSION" = -3,
                             "HIGH_SELL_AGGRESSION" = -6,
                             "EXTREME_SELL_AGGRESSION" = -8,
                             0)
    taker_score := taker_score + ada_taker_score
    taker_signals := c(taker_signals, paste("ADA:", trigger_data$taker_ada$aggression_level))
    data_quality_points := data_quality_points + 1
  }
  
  if (!is.null(trigger_data$taker_algo)) {
    algo_taker_score := switch(trigger_data$taker_algo$aggression_level,
                              "EXTREME_BUY_AGGRESSION" = 7,
                              "HIGH_BUY_AGGRESSION" = 5,
                              "MODERATE_BUY_AGGRESSION" = 3,
                              "BALANCED_AGGRESSION" = 2,
                              "MODERATE_SELL_AGGRESSION" = -3,
                              "HIGH_SELL_AGGRESSION" = -5,
                              "EXTREME_SELL_AGGRESSION" = -7,
                              0)
    taker_score := taker_score + algo_taker_score
    taker_signals := c(taker_signals, paste("ALGO:", trigger_data$taker_algo$aggression_level))
    data_quality_points := data_quality_points + 1
  }
  
  score := score + taker_score
  max_score := max_score + 15
  signals$taker_behavior := paste(taker_signals, collapse = " | ")
  
  # === 6. DATA QUALITY BONUS (10% Gewichtung) ===
  quality_score := min(data_quality_points * 1.25, 10)  # Max 10 points
  score := score + quality_score
  max_score := max_score + 10
  signals$data_quality := paste(data_quality_points, "quality data sources")
  
  # Normalize Score
  score_percentage := if (max_score > 0) (score / max_score) * 100 else 0
  score_percentage := max(0, min(100, score_percentage))  # Clamp 0-100
  
  # Enhanced Master Signal Classification
  master_signal := if (score_percentage >= 75) "STRONG_ALTCOIN_RALLY_SIGNAL"
                   else if (score_percentage >= 60) "MODERATE_ALTCOIN_RALLY_SIGNAL"
                   else if (score_percentage >= 40) "WEAK_ALTCOIN_RALLY_SIGNAL"
                   else if (score_percentage >= 25) "NEUTRAL_ALTCOIN_SIGNAL"
                   else "BEARISH_ALTCOIN_SIGNAL"
  
  master_result := list(
    score = score,
    max_score = max_score,
    score_percentage = round(score_percentage, 1),
    master_signal = master_signal,
    individual_signals = signals,
    data_quality_sources = data_quality_points,
    calculation_time = Sys.time(),
    
    # Enhanced Trading Recommendations
    recommendation = switch(master_signal,
                          "STRONG_ALTCOIN_RALLY_SIGNAL" = "🟢 STRONG BUY - Multiple bullish indicators aligned",
                          "MODERATE_ALTCOIN_RALLY_SIGNAL" = "🟡 MODERATE BUY - Positive signals emerging", 
                          "WEAK_ALTCOIN_RALLY_SIGNAL" = "🟠 WATCH CLOSELY - Some positive signs",
                          "NEUTRAL_ALTCOIN_SIGNAL" = "⚪ HOLD - No clear direction",
                          "BEARISH_ALTCOIN_SIGNAL" = "🔴 CAUTION - Consider reducing positions")
  )
  
  cat(sprintf("🎯 Optimized Rally Score: %.1f%% (%s)\n", 
              score_percentage, master_signal))
  cat(sprintf("📊 Data Quality: %d/8 sources available\n", data_quality_points))
  
  return(master_result)
}

# Enhanced Summary Display
display_optimized_rally_triggers_summary <- function(trigger_data) {
  cat("\n🎯 OPTIMIZED ALTCOIN RALLY TRIGGERS SUMMARY\n")
  cat("┌────────────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                    🚀 OPTIMIZED ALTCOIN RALLY TRIGGERS 🚀                    │\n")
  cat("├────────────────────────────────────────────────────────────────────────────────┤\n")
  
  # Master Signal
  master := trigger_data$master_signal
  signal_icon := switch(master$master_signal,
                       "STRONG_ALTCOIN_RALLY_SIGNAL" = "🟢🚀",
                       "MODERATE_ALTCOIN_RALLY_SIGNAL" = "🟡📈",
                       "WEAK_ALTCOIN_RALLY_SIGNAL" = "🟠⚡",
                       "NEUTRAL_ALTCOIN_SIGNAL" = "⚪⚖️",
                       "BEARISH_ALTCOIN_SIGNAL" = "🔴📉")
  
  cat(sprintf("│ %s MASTER SIGNAL: %-43s │\n", 
              signal_icon, master$master_signal))
  cat(sprintf("│ 🎯 Score: %.1f%% | 📊 Sources: %d/8 | 💡 %s │\n", 
              master$score_percentage, master$data_quality_sources, 
              substr(master$recommendation, 1, 15)))
  cat("├────────────────────────────────────────────────────────────────────────────────┤\n")
  
  # Core Indicators
  cat("│ 📊 CORE INDICATORS:                                                           │\n")
  
  if (!is.null(trigger_data$dominance)) {
    dom := trigger_data$dominance
    cat(sprintf("│ 📊 BTC Dominance: %.1f%% (%s)                          │\n", 
                dom$btc_dominance_proxy, substr(dom$alt_season_signal, 1, 20)))
    cat(sprintf("│ 📊 ETH/BTC Ratio: %.5f (%s)                               │\n", 
                dom$eth_btc_ratio, substr(dom$eth_strength_signal, 1, 15)))
  }
  
  if (!is.null(trigger_data$relative_strength)) {
    rel := trigger_data$relative_strength  
    cat(sprintf("│ 📈 ADA vs BTC: %+.2f%% | ALGO vs BTC: %+.2f%% (%s)       │\n", 
                rel$ada_rel_performance, rel$algo_rel_performance, 
                substr(rel$overall_alt_strength, 1, 15)))
  }
  
  cat("├────────────────────────────────────────────────────────────────────────────────┤\n")
  cat("│ 🏦 PROXY ELITE INDICATORS:                                                    │\n")
  
  # Elite Proxies
  if (!is.null(trigger_data$elite_ada)) {
    ada_elite := trigger_data$elite_ada
    cat(sprintf("│ 🏦 ADA Elite Proxy: %s (Dom: %+.1f%%)                      │\n", 
                substr(ada_elite$elite_sentiment, 1, 15), ada_elite$long_dominance * 100))
  }
  
  if (!is.null(trigger_data$elite_algo)) {
    algo_elite := trigger_data$elite_algo
    cat(sprintf("│ 🏦 ALGO Elite Proxy: %s (Dom: %+.1f%%)                     │\n", 
                substr(algo_elite$elite_sentiment, 1, 15), algo_elite$long_dominance * 100))
  }
  
  # L/S Proxies
  if (!is.null(trigger_data$ls_proxy_ada)) {
    ada_ls := trigger_data$ls_proxy_ada
    cat(sprintf("│ ⚖️ ADA L/S Proxy: %.2f (%s)                               │\n", 
                ada_ls$long_short_ratio, substr(ada_ls$ratio_signal, 1, 15)))
  }
  
  if (!is.null(trigger_data$ls_proxy_algo)) {
    algo_ls := trigger_data$ls_proxy_algo
    cat(sprintf("│ ⚖️ ALGO L/S Proxy: %.2f (%s)                              │\n", 
                algo_ls$long_short_ratio, substr(algo_ls$ratio_signal, 1, 15)))
  }
  
  # Enhanced Taker Behavior
  if (!is.null(trigger_data$taker_ada)) {
    ada_taker := trigger_data$taker_ada
    cat(sprintf("│ 💹 ADA Taker: %.2f (%s) | Whale: %s           │\n", 
                ada_taker$taker_ratio, substr(ada_taker$aggression_level, 1, 10),
                substr(ada_taker$whale_activity, 1, 10)))
  }
  
  if (!is.null(trigger_data$taker_algo)) {
    algo_taker := trigger_data$taker_algo
    cat(sprintf("│ 💹 ALGO Taker: %.2f (%s) | Whale: %s          │\n", 
                algo_taker$taker_ratio, substr(algo_taker$aggression_level, 1, 10),
                substr(algo_taker$whale_activity, 1, 10)))
  }
  
  cat("└────────────────────────────────────────────────────────────────────────────────┘\n")
  
  # Enhanced Action Items
  cat("\n💡 OPTIMIZED ACTION ITEMS:\n")
  cat("=========================\n")
  
  if (master$score_percentage >= 60) {
    cat("🟢 BULLISH SIGNALS DETECTED:\n")
    cat("   • 📊 Multiple indicators showing strength\n")
    cat("   • 🚀 Consider increasing altcoin exposure\n")
    cat("   • 📈 Watch for breakout confirmations\n")
    cat("   • ⚖️ Long bias in proxy calculations\n")
  } else if (master$score_percentage >= 40) {
    cat("🟡 MIXED SIGNALS:\n")
    cat("   • 📊 Some positive indicators present\n")
    cat("   • 🎯 Maintain current positions\n") 
    cat("   • 📈 Wait for stronger confirmation\n")
    cat("   • 🔍 Monitor for trend changes\n")
  } else if (master$score_percentage >= 25) {
    cat("🟠 WEAK SIGNALS:\n")
    cat("   • 📊 Limited bullish momentum\n")
    cat("   • 🏦 Elite proxies show uncertainty\n")
    cat("   • 📊 Hold current positions\n")
    cat("   • 🔍 Prepare for defensive measures\n")
  } else {
    cat("🔴 BEARISH SIGNALS:\n")
    cat("   • 📊 Multiple bearish indicators\n")
    cat("   • 🏦 Elite proxies suggest distribution\n")
    cat("   • 📉 Consider reducing exposure\n")
    cat("   • ⚖️ Short bias in calculations\n")
  }
  
  cat(sprintf("\n📊 Data Quality: %d/8 proxy sources available\n", master$data_quality_sources))
  cat(sprintf("📅 Next Analysis: Recommended in 3-6 hours\n"))
  cat(sprintf("⏰ Analysis Time: %s\n", format(Sys.time(), "%H:%M:%S")))
}

# ==========================================================================================================
# ✅ OPTIMIZED INTEGRATION FUNCTION FÜR REXECUTION.R
# ==========================================================================================================

run_optimized_altcoin_rally_monitoring <- function() {
  cat("\n")
  cat(strrep("█", 80), "\n")
  cat("🚀 OPTIMIZED ALTCOIN RALLY TRIGGERS - PRODUCTION VERSION\n")
  cat(strrep("█", 80), "\n")
  
  # Führe optimierte Analyse durch
  trigger_results := analyze_optimized_altcoin_rally_triggers(display_summary = TRUE)
  
  # Return für weitere Verarbeitung
  return(trigger_results)
}

# ==========================================================================================================
# 🎯 OPTIMIZED SYSTEM READY MESSAGE
# ==========================================================================================================

cat("✅ OPTIMIZED ALTCOIN RALLY TRIGGERS LOADED (PRODUCTION VERSION)!\n")
cat(strrep("=", 80), "\n")
cat("🎯 OPTIMIZED FEATURES:\n")
cat("   ✅ Enhanced Taker Analysis (200+ trades sample)\n")
cat("   ✅ Intelligent L/S Proxies (multi-factor calculation)\n")
cat("   ✅ Elite Trader Proxies (volume + funding + orderbook)\n")
cat("   ✅ Volume-Weighted Relative Strength\n")
cat("   ✅ Enhanced BTC Dominance Detection\n")
cat("   ✅ Robust Fallback Systems\n")
cat("\n")
cat("🎯 OPTIMIZED INTEGRATION COMMAND FOR rexecution.r:\n")
cat("\n")
cat("# Replace old command with:\n")
cat("optimized_altcoin_triggers <- run_optimized_altcoin_rally_monitoring()\n")
cat("\n")
cat("📊 AVAILABLE OPTIMIZED FUNCTIONS:\n")
cat("   analyze_optimized_altcoin_rally_triggers()   # Full optimized analysis\n")
cat("   analyze_enhanced_taker_behavior(symbol)      # Enhanced taker analysis\n")
cat("   calculate_intelligent_ls_proxy(symbol)       # Smart L/S calculation\n")
cat("   estimate_elite_trader_sentiment(symbol)      # Elite proxy estimation\n")
cat("   calculate_enhanced_altcoin_relative_strength() # Volume-weighted strength\n")
cat("\n")
cat("🚀 Optimized for Available Bitget APIs + Intelligent Proxies!\n")
cat(strrep("=", 80), "\n")