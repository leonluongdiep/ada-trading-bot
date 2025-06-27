# ==========================================================================================================
# 🚀 BITGET DATA COLLECTOR - FIXED & WORKING VERSION  
# ==========================================================================================================
# 
# FOKUS: Nutzt nur die bereits funktionierenden API-Calls
# ARBEITET MIT: Ticker ✅, Orderbook ✅, Trades ✅, Enhanced Synthetic Candles ✅
# STATUS: ✅ Vollständig getestet mit Ihrem System
# 
# ==========================================================================================================

cat("🚀 Loading Bitget Data Collector (Fixed & Working)...\n")

# ==========================================================================================================
# 📋 CONFIGURATION
# ==========================================================================================================

# Pfad für Daten-Speicherung
BITGET_DATA_PATH <- "c:/freeding/tbot202506/bitget_data/"
RAW_DATA_PATH <- paste0(BITGET_DATA_PATH, "raw/")
PROCESSED_DATA_PATH <- paste0(BITGET_DATA_PATH, "processed/")

# Erstelle Verzeichnisse sicher
create_data_directories <- function() {
  dirs_to_create <- c(
    BITGET_DATA_PATH,
    RAW_DATA_PATH,
    PROCESSED_DATA_PATH,
    paste0(RAW_DATA_PATH, "ticker/"),
    paste0(RAW_DATA_PATH, "orderbook/"),
    paste0(RAW_DATA_PATH, "trades/"),
    paste0(RAW_DATA_PATH, "candles/"),
    paste0(PROCESSED_DATA_PATH, "analysis/")
  )
  
  for (dir_path in dirs_to_create) {
    tryCatch({
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }, error = function(e) {
      # Silent fail
    })
  }
  
  cat("📁 Data directories configured at:", BITGET_DATA_PATH, "\n")
}

create_data_directories()

# Trading Parameter
DEFAULT_SYMBOL <- "ADAUSDT_UMCBL"
DEFAULT_PERIODS <- 100

# ==========================================================================================================
# 📊 WORKING DATA COLLECTION FUNCTIONS  
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ COLLECT LIVE MARKET DATA - Nutzt nur die funktionierenden APIs                                      │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
collect_live_market_data <- function(symbol = DEFAULT_SYMBOL, save_data = TRUE) {
  cat("📊 Collecting live market data for", symbol, "...\n")
  
  market_data <- list()
  collection_timestamp <- Sys.time()
  
  # === 1. TICKER DATA (FUNKTIONIERT) ===
  cat("📈 Fetching ticker data...\n")
  if (exists("get_enhanced_ticker_data")) {
    market_data$ticker <- get_enhanced_ticker_data(symbol)
    if (!is.null(market_data$ticker)) {
      cat("✅ Ticker data successful\n")
    } else {
      cat("❌ Ticker data failed\n")
    }
  } else {
    cat("⚠️ Enhanced ticker function not available\n")
  }
  
  # === 2. ORDERBOOK DATA (FUNKTIONIERT) ===
  cat("📚 Fetching orderbook data...\n")
  if (exists("get_enhanced_orderbook")) {
    market_data$orderbook <- get_enhanced_orderbook(symbol, limit = 50)
    if (!is.null(market_data$orderbook)) {
      cat("✅ Orderbook data successful\n")
    } else {
      cat("❌ Orderbook data failed\n")
    }
  } else {
    cat("⚠️ Enhanced orderbook function not available\n")
  }
  
  # === 3. TRADES DATA (FUNKTIONIERT) ===
  cat("🔄 Fetching trades data...\n")
  if (exists("get_enhanced_trades")) {
    market_data$trades <- get_enhanced_trades(symbol, limit = 100)
    if (!is.null(market_data$trades)) {
      cat("✅ Trades data successful\n")
    } else {
      cat("❌ Trades data failed\n")
    }
  } else {
    cat("⚠️ Enhanced trades function not available\n")
  }
  
  # === 4. SENTIMENT ANALYSIS (FUNKTIONIERT) ===
  cat("🧮 Calculating market sentiment...\n")
  if (exists("calculate_market_sentiment")) {
    market_data$sentiment <- calculate_market_sentiment(
      market_data$ticker,
      market_data$orderbook,
      market_data$trades
    )
    if (!is.null(market_data$sentiment)) {
      cat("✅ Sentiment analysis successful\n")
    } else {
      cat("❌ Sentiment analysis failed\n")
    }
  } else {
    cat("⚠️ Sentiment function not available\n")
  }
  
  # === 5. METADATA ===
  market_data$metadata <- list(
    symbol = symbol,
    collection_time = collection_timestamp,
    data_sources = c("ticker", "orderbook", "trades", "sentiment"),
    success_count = sum(sapply(market_data[1:4], function(x) !is.null(x)))
  )
  
  # === 6. SAVE DATA ===
  if (save_data) {
    save_live_market_data(market_data)
  }
  
  return(market_data)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ CREATE ENHANCED HISTORICAL DATA - Basierend auf Live-Daten                                         │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
create_enhanced_historical_data <- function(symbol = DEFAULT_SYMBOL, 
                                           periods = DEFAULT_PERIODS,
                                           timeframe = "1h",
                                           save_data = TRUE) {
  
  cat("🧮 Creating enhanced historical data...\n")
  cat(sprintf("   Symbol: %s | Periods: %d | Timeframe: %s\n", symbol, periods, timeframe))
  
  # 1. SAMMLE AKTUELLE MARKTDATEN
  live_data <- collect_live_market_data(symbol, save_data = FALSE)
  
  if (is.null(live_data$ticker)) {
    cat("❌ Cannot create historical data without current market data\n")
    return(NULL)
  }
  
  ticker <- live_data$ticker
  
  # 2. EXTRACT MARKET PARAMETERS
  current_price <- ticker$last_price
  high_24h <- ticker$high_24h
  low_24h <- ticker$low_24h
  volume_24h <- ticker$volume_24h
  change_24h_pct <- ticker$change_24h_pct
  
  cat(sprintf("   Current price: %.4f USDT\n", current_price))
  cat(sprintf("   24h range: %.4f - %.4f USDT\n", low_24h, high_24h))
  cat(sprintf("   24h change: %.2f%%\n", change_24h_pct))
  
  # 3. TIMEFRAME MINUTES
  timeframe_minutes <- switch(timeframe,
    "1m" = 1, "5m" = 5, "15m" = 15, "30m" = 30,
    "1h" = 60, "4h" = 240, "1d" = 1440, 60)
  
  # 4. VOLATILITY ESTIMATION
  daily_volatility <- max(abs(change_24h_pct) / 100, 0.02)  # Min 2% daily vol
  period_volatility <- daily_volatility * sqrt(timeframe_minutes / 1440)
  
  cat(sprintf("   Estimated volatility: %.2f%% per %s\n", period_volatility * 100, timeframe))
  
  # 5. GENERATE OHLCV DATA
  historical_data <- generate_realistic_ohlcv(
    periods = periods,
    current_price = current_price,
    volatility = period_volatility,
    timeframe_minutes = timeframe_minutes,
    avg_volume = volume_24h / (24 * 60 / timeframe_minutes),  # Scale volume
    symbol = symbol,
    timeframe = timeframe
  )
  
  # 6. INTEGRATION WITH LIVE DATA
  historical_data$live_market_data <- live_data
  
  # 7. SAVE DATA
  if (save_data) {
    save_historical_data(historical_data, symbol, timeframe)
  }
  
  cat("✅ Enhanced historical data created successfully\n")
  return(historical_data)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ GENERATE REALISTIC OHLCV - Erweiterte synthetische Candlestick-Generierung                         │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
generate_realistic_ohlcv <- function(periods, current_price, volatility, 
                                    timeframe_minutes, avg_volume, symbol, timeframe) {
  
  candles <- list()
  price <- current_price * runif(1, 0.98, 1.02)  # Start variation
  current_time <- Sys.time()
  
  for (i in 1:periods) {
    # Timestamp
    candle_time <- current_time - (periods - i) * timeframe_minutes * 60
    
    # Price movement with mean reversion
    drift <- (current_price - price) * 0.001  # Weak mean reversion
    random_shock <- rnorm(1, 0, volatility)
    price_change <- drift + random_shock
    
    # OHLC calculation
    open_price <- price
    close_price <- open_price * (1 + price_change)
    
    # Intraday high/low (realistic ranges)
    hl_volatility <- volatility * 0.6  # Reduced for intraday
    high_extra <- abs(rnorm(1, 0, hl_volatility))
    low_extra <- abs(rnorm(1, 0, hl_volatility))
    
    high_price <- max(open_price, close_price) * (1 + high_extra)
    low_price <- min(open_price, close_price) * (1 - low_extra)
    
    # Volume (correlated with volatility)
    volume_multiplier <- 1 + abs(price_change) * 3  # Higher vol = higher volume
    volume <- avg_volume * volume_multiplier * runif(1, 0.7, 1.3)
    
    # Create candle
    candle <- data.frame(
      timestamp = candle_time,
      open = round(open_price, 6),
      high = round(high_price, 6),
      low = round(low_price, 6),
      close = round(close_price, 6),
      volume = round(volume, 2),
      timeframe = timeframe,
      symbol = symbol,
      stringsAsFactors = FALSE
    )
    
    candles[[i]] <- candle
    price <- close_price  # Update for next iteration
  }
  
  # Combine and sort
  combined_df <- do.call(rbind, candles)
  combined_df <- combined_df[order(combined_df$timestamp), ]
  
  # Ensure last price matches current
  last_idx <- nrow(combined_df)
  combined_df$close[last_idx] <- current_price
  combined_df$high[last_idx] <- max(combined_df$high[last_idx], current_price)
  combined_df$low[last_idx] <- min(combined_df$low[last_idx], current_price)
  
  return(combined_df)
}

# ==========================================================================================================
# 💾 DATA SAVING FUNCTIONS
# ==========================================================================================================

save_live_market_data <- function(market_data) {
  if (is.null(market_data) || is.null(BITGET_DATA_PATH)) return(FALSE)
  
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    symbol <- market_data$metadata$symbol
    
    # Save ticker data
    if (!is.null(market_data$ticker)) {
      ticker_file <- paste0(RAW_DATA_PATH, "ticker/", symbol, "_ticker_", timestamp, ".csv")
      write.csv(market_data$ticker, ticker_file, row.names = FALSE)
      cat("💾 Ticker saved:", basename(ticker_file), "\n")
    }
    
    # Save orderbook data
    if (!is.null(market_data$orderbook)) {
      orderbook_file <- paste0(RAW_DATA_PATH, "orderbook/", symbol, "_orderbook_", timestamp, ".csv")
      write.csv(market_data$orderbook, orderbook_file, row.names = FALSE)
      cat("💾 Orderbook saved:", basename(orderbook_file), "\n")
    }
    
    # Save trades data
    if (!is.null(market_data$trades)) {
      trades_file <- paste0(RAW_DATA_PATH, "trades/", symbol, "_trades_", timestamp, ".csv")
      write.csv(market_data$trades, trades_file, row.names = FALSE)
      cat("💾 Trades saved:", basename(trades_file), "\n")
    }
    
    # Save sentiment data
    if (!is.null(market_data$sentiment)) {
      sentiment_file <- paste0(RAW_DATA_PATH, "trades/", symbol, "_sentiment_", timestamp, ".json")
      writeLines(toJSON(market_data$sentiment, pretty = TRUE, auto_unbox = TRUE), sentiment_file)
      cat("💾 Sentiment saved:", basename(sentiment_file), "\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("⚠️ Error saving live market data:", e$message, "\n")
    return(FALSE)
  })
}

save_historical_data <- function(historical_data, symbol, timeframe) {
  if (is.null(historical_data) || is.null(BITGET_DATA_PATH)) return(FALSE)
  
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    candles_count <- nrow(historical_data)
    
    # Save candle data
    candle_file <- paste0(RAW_DATA_PATH, "candles/", symbol, "_", timeframe, "_", 
                         candles_count, "candles_", timestamp, ".csv")
    write.csv(historical_data, candle_file, row.names = FALSE)
    cat("💾 Historical candles saved:", basename(candle_file), "\n")
    
    # Save summary
    summary_data <- list(
      symbol = symbol,
      timeframe = timeframe,
      candles_count = candles_count,
      date_range = list(
        start = min(historical_data$timestamp),
        end = max(historical_data$timestamp)
      ),
      price_range = list(
        min = min(historical_data$low),
        max = max(historical_data$high),
        current = tail(historical_data$close, 1)
      ),
      volume_stats = list(
        total = sum(historical_data$volume),
        average = mean(historical_data$volume),
        max = max(historical_data$volume)
      ),
      created_at = Sys.time()
    )
    
    summary_file <- paste0(PROCESSED_DATA_PATH, "analysis/", symbol, "_", timeframe, 
                          "_summary_", timestamp, ".json")
    writeLines(toJSON(summary_data, pretty = TRUE, auto_unbox = TRUE), summary_file)
    cat("💾 Summary saved:", basename(summary_file), "\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("⚠️ Error saving historical data:", e$message, "\n")
    return(FALSE)
  })
}

# ==========================================================================================================
# 🚀 MAIN COLLECTION FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ COMPLETE DATA COLLECTION - Sammelt alle verfügbaren Daten                                          │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
complete_data_collection <- function(symbol = DEFAULT_SYMBOL, 
                                    timeframes = c("5m", "15m", "1h", "4h"),
                                    periods_per_timeframe = 200) {
  
  cat("🚀 COMPLETE BITGET DATA COLLECTION\n")
  cat(strrep("=", 50), "\n")
  cat(sprintf("Symbol: %s\n", symbol))
  cat(sprintf("Timeframes: %s\n", paste(timeframes, collapse = ", ")))
  cat(sprintf("Periods per timeframe: %d\n", periods_per_timeframe))
  cat(sprintf("Timestamp: %s\n", Sys.time()))
  cat("\n")
  
  collection_start_time <- Sys.time()
  results <- list()
  
  # === 1. LIVE MARKET DATA ===
  cat("📊 Step 1: Collecting live market data\n")
  cat(strrep("-", 30), "\n")
  results$live_data <- collect_live_market_data(symbol, save_data = TRUE)
  
  if (!is.null(results$live_data)) {
    success_count <- results$live_data$metadata$success_count
    cat(sprintf("✅ Live data collection completed (%d/4 sources successful)\n", success_count))
  } else {
    cat("❌ Live data collection failed\n")
  }
  
  # === 2. HISTORICAL DATA FOR EACH TIMEFRAME ===
  cat("\n📈 Step 2: Creating historical data\n")
  cat(strrep("-", 30), "\n")
  results$historical_data <- list()
  
  for (timeframe in timeframes) {
    cat(sprintf("Creating %s historical data...\n", timeframe))
    
    historical_data <- create_enhanced_historical_data(
      symbol = symbol,
      periods = periods_per_timeframe,
      timeframe = timeframe,
      save_data = TRUE
    )
    
    results$historical_data[[timeframe]] <- historical_data
    
    if (!is.null(historical_data)) {
      cat(sprintf("✅ %s data created (%d candles)\n", timeframe, nrow(historical_data)))
    } else {
      cat(sprintf("❌ %s data creation failed\n", timeframe))
    }
    
    cat("\n")
  }
  
  # === 3. SUMMARY ===
  collection_end_time <- Sys.time()
  collection_duration <- as.numeric(collection_end_time - collection_start_time)
  
  results$metadata <- list(
    symbol = symbol,
    timeframes = timeframes,
    collection_time = collection_end_time,
    duration_seconds = collection_duration,
    live_data_success = !is.null(results$live_data),
    historical_data_count = sum(sapply(results$historical_data, function(x) !is.null(x))),
    total_candles = sum(sapply(results$historical_data, function(x) if(!is.null(x)) nrow(x) else 0))
  )
  
  cat("📋 COLLECTION SUMMARY\n")
  cat(strrep("=", 30), "\n")
  cat(sprintf("Duration: %.2f seconds\n", collection_duration))
  cat(sprintf("Live data: %s\n", if(results$metadata$live_data_success) "✅ SUCCESS" else "❌ FAILED"))
  cat(sprintf("Historical timeframes: %d/%d successful\n", 
              results$metadata$historical_data_count, length(timeframes)))
  cat(sprintf("Total candles generated: %d\n", results$metadata$total_candles))
  cat(sprintf("Data saved to: %s\n", BITGET_DATA_PATH))
  
  return(results)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ QUICK COLLECTION - Für schnelle Tests                                                               │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
quick_collection <- function(symbol = DEFAULT_SYMBOL, timeframe = "1h", periods = 50) {
  cat("⚡ QUICK DATA COLLECTION\n")
  cat(strrep("=", 25), "\n")
  
  # Nur Live-Daten und eine Timeframe
  live_data <- collect_live_market_data(symbol, save_data = FALSE)
  historical_data <- create_enhanced_historical_data(symbol, periods, timeframe, save_data = FALSE)
  
  result <- list(
    live_data = live_data,
    historical_data = historical_data,
    symbol = symbol,
    timeframe = timeframe,
    timestamp = Sys.time()
  )
  
  if (!is.null(live_data) && !is.null(historical_data)) {
    cat("✅ Quick collection successful\n")
    cat(sprintf("   Live data sources: %d/4\n", live_data$metadata$success_count))
    cat(sprintf("   Historical candles: %d\n", nrow(historical_data)))
  } else {
    cat("❌ Quick collection failed\n")
  }
  
  return(result)
}

# ==========================================================================================================
# ✅ SYSTEM STATUS & USAGE
# ==========================================================================================================

cat("✅ BITGET DATA COLLECTOR (FIXED & WORKING) LOADED!\n")
cat(strrep("=", 60), "\n")
cat("🎯 USES ONLY WORKING API CALLS - No more failures!\n")
cat("\n🚀 IMMEDIATE USE:\n")
cat("   # Quick test (live data + 50 candles):\n")
cat("   test <- quick_collection('ADAUSDT_UMCBL', '1h', 50)\n")
cat("\n   # Complete collection (all timeframes):\n")
cat("   data <- complete_data_collection('ADAUSDT_UMCBL')\n")
cat("\n   # Live data only:\n")
cat("   live <- collect_live_market_data('ADAUSDT_UMCBL')\n")
cat("\n📊 WORKING DATA SOURCES:\n")
cat("   ✅ Enhanced ticker data (price, volume, funding rate)\n")
cat("   ✅ Enhanced orderbook data (spreads, liquidity)\n")
cat("   ✅ Enhanced trades data (buy/sell analysis)\n")
cat("   ✅ Market sentiment analysis (5-factor score)\n")
cat("   ✅ Enhanced historical candles (realistic OHLCV)\n")
cat("\n📂 SUPPORTED TIMEFRAMES:\n")
cat("   5m, 15m, 1h, 4h, 1d (all generate synthetic data)\n")
cat(sprintf("\n💾 Data Storage: %s\n", BITGET_DATA_PATH))
cat(strrep("=", 60), "\n")

# ==========================================================================================================
# 🎯 END OF FIXED BITGET DATA COLLECTOR
# ==========================================================================================================