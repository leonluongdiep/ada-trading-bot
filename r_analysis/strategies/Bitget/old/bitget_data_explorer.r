# ==========================================================================================================
# 📊 BITGET DATA EXPLORER - DESCRIPTIVE STATISTICS & ANALYSIS
# ==========================================================================================================
# 
# ZWECK: Comprehensive Exploration der gesammelten Bitget-Daten
# FUNKTIONEN: Descriptive Stats, Visualisierungen, Market Analysis, Reports
# STATUS: ✅ Ready to use mit Ihren gesammelten Daten
# 
# ==========================================================================================================

cat("📊 Loading Bitget Data Explorer...\n")

# ==========================================================================================================
# 📋 CONFIGURATION & LIBRARIES
# ==========================================================================================================

# Data Path Configuration
BITGET_DATA_PATH <- "c:/freeding/tbot202506/bitget_data/"
RAW_DATA_PATH <- paste0(BITGET_DATA_PATH, "raw/")
PROCESSED_DATA_PATH <- paste0(BITGET_DATA_PATH, "processed/")
REPORTS_PATH <- paste0(BITGET_DATA_PATH, "reports/")

# Create reports directory
tryCatch({
  dir.create(REPORTS_PATH, recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(REPORTS_PATH, "charts/"), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0(REPORTS_PATH, "tables/"), recursive = TRUE, showWarnings = FALSE)
}, error = function(e) {
  cat("⚠️ Could not create reports directory\n")
})

# Load required libraries
load_libraries <- function() {
  required_libs <- c("jsonlite")
  
  # Optional libraries for enhanced analysis
  optional_libs <- c("ggplot2", "dplyr", "lubridate", "corrplot", "VIM")
  
  # Load required
  for (lib in required_libs) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      cat("⚠️ Required library missing:", lib, "\n")
    } else {
      library(lib, quietly = TRUE, character.only = TRUE)
    }
  }
  
  # Load optional
  advanced_features <- TRUE
  for (lib in optional_libs) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      advanced_features <- FALSE
    } else {
      library(lib, quietly = TRUE, character.only = TRUE)
    }
  }
  
  if (!advanced_features) {
    cat("📊 Using base R for analysis (install ggplot2, dplyr for enhanced features)\n")
  } else {
    cat("📊 Enhanced analysis libraries loaded\n")
  }
  
  return(advanced_features)
}

ADVANCED_FEATURES <- load_libraries()

# ==========================================================================================================
# 📁 DATA LOADING FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ LOAD ALL AVAILABLE DATA - Scannt alle gespeicherten Dateien und lädt sie                           │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
load_all_bitget_data <- function(symbol = "ADAUSDT_UMCBL") {
  cat("📁 Loading all available Bitget data for", symbol, "...\n")
  
  data_collection <- list()
  
  # === 1. LOAD TICKER DATA ===
  ticker_files <- list.files(paste0(RAW_DATA_PATH, "ticker/"), 
                             pattern = paste0(symbol, "_ticker_.*\\.csv$"), 
                             full.names = TRUE)
  
  if (length(ticker_files) > 0) {
    # Load most recent ticker file
    latest_ticker <- tail(ticker_files, 1)
    data_collection$ticker <- read.csv(latest_ticker, stringsAsFactors = FALSE)
    data_collection$ticker$timestamp <- as.POSIXct(data_collection$ticker$timestamp)
    cat(sprintf("✅ Ticker data loaded: %s\n", basename(latest_ticker)))
  } else {
    cat("⚠️ No ticker data found\n")
  }
  
  # === 2. LOAD ORDERBOOK DATA ===
  orderbook_files <- list.files(paste0(RAW_DATA_PATH, "orderbook/"), 
                                pattern = paste0(symbol, "_orderbook_.*\\.csv$"), 
                                full.names = TRUE)
  
  if (length(orderbook_files) > 0) {
    # Load all orderbook files for trend analysis
    orderbook_list <- lapply(orderbook_files, function(f) {
      df <- read.csv(f, stringsAsFactors = FALSE)
      df$timestamp <- as.POSIXct(df$timestamp)
      df$file_timestamp <- basename(f)
      return(df)
    })
    data_collection$orderbook <- do.call(rbind, orderbook_list)
    cat(sprintf("✅ Orderbook data loaded: %d files, %d total observations\n", 
                length(orderbook_files), nrow(data_collection$orderbook)))
  } else {
    cat("⚠️ No orderbook data found\n")
  }
  
  # === 3. LOAD TRADES DATA ===
  trades_files <- list.files(paste0(RAW_DATA_PATH, "trades/"), 
                             pattern = paste0(symbol, "_trades_.*\\.csv$"), 
                             full.names = TRUE)
  
  if (length(trades_files) > 0) {
    # Load all trades files
    trades_list <- lapply(trades_files, function(f) {
      df <- read.csv(f, stringsAsFactors = FALSE)
      df$timestamp <- as.POSIXct(df$timestamp)
      df$file_timestamp <- basename(f)
      return(df)
    })
    data_collection$trades <- do.call(rbind, trades_list)
    cat(sprintf("✅ Trades data loaded: %d files, %d total observations\n", 
                length(trades_files), nrow(data_collection$trades)))
  } else {
    cat("⚠️ No trades data found\n")
  }
  
  # === 4. LOAD SENTIMENT DATA ===
  sentiment_files <- list.files(paste0(RAW_DATA_PATH, "trades/"), 
                                pattern = paste0(symbol, "_sentiment_.*\\.json$"), 
                                full.names = TRUE)
  
  if (length(sentiment_files) > 0) {
    sentiment_list <- lapply(sentiment_files, function(f) {
      tryCatch({
        content <- readLines(f, warn = FALSE)
        sentiment_data <- fromJSON(content)
        data.frame(
          file_timestamp = basename(f),
          overall_sentiment = sentiment_data$overall_sentiment,
          sentiment_percentage = sentiment_data$sentiment_percentage,
          factors_count = sentiment_data$max_factors,
          stringsAsFactors = FALSE
        )
      }, error = function(e) NULL)
    })
    
    sentiment_list <- sentiment_list[!sapply(sentiment_list, is.null)]
    if (length(sentiment_list) > 0) {
      data_collection$sentiment <- do.call(rbind, sentiment_list)
      cat(sprintf("✅ Sentiment data loaded: %d files\n", length(sentiment_files)))
    }
  } else {
    cat("⚠️ No sentiment data found\n")
  }
  
  # === 5. LOAD HISTORICAL CANDLES ===
  candle_files <- list.files(paste0(RAW_DATA_PATH, "candles/"), 
                             pattern = paste0(symbol, "_.*_.*candles_.*\\.csv$"), 
                             full.names = TRUE)
  
  if (length(candle_files) > 0) {
    data_collection$candles <- list()
    
    for (file in candle_files) {
      # Extract timeframe from filename
      timeframe_match <- regmatches(file, regexpr("_(\\d+[mhd]|\\dH)_", file))
      if (length(timeframe_match) > 0) {
        timeframe <- gsub("_", "", timeframe_match)
        
        # Safe CSV reading with error handling
        tryCatch({
          df <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
          
          # Check if data was read properly
          if (nrow(df) > 0 && "timestamp" %in% names(df)) {
            df$timestamp <- as.POSIXct(df$timestamp)
            
            # Only keep properly loaded data
            if (nrow(df) > 10) {  # Only use files with sufficient data
              data_collection$candles[[timeframe]] <- df
            }
          }
        }, error = function(e) {
          cat("⚠️ Could not load candle file:", basename(file), "\n")
        })
      }
    }
    
    if (length(data_collection$candles) > 0) {
      cat(sprintf("✅ Historical candles loaded: %d timeframes (%s)\n", 
                  length(data_collection$candles), 
                  paste(names(data_collection$candles), collapse = ", ")))
    } else {
      cat("⚠️ No valid historical candle data found\n")
    }
  } else {
    cat("⚠️ No historical candle data found\n")
  }
  
  # === 6. METADATA ===
  data_collection$metadata <- list(
    symbol = symbol,
    load_time = Sys.time(),
    data_types = names(data_collection)[names(data_collection) != "metadata"],
    total_files_loaded = length(ticker_files) + length(orderbook_files) + 
                        length(trades_files) + length(sentiment_files) + length(candle_files)
  )
  
  cat(sprintf("📊 Data loading complete: %d data types, %d total files\n", 
              length(data_collection$metadata$data_types), 
              data_collection$metadata$total_files_loaded))
  
  return(data_collection)
}

# ==========================================================================================================
# 📊 DESCRIPTIVE STATISTICS FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ TICKER DESCRIPTIVE STATISTICS                                                                        │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
analyze_ticker_data <- function(ticker_data) {
  if (is.null(ticker_data) || nrow(ticker_data) == 0) {
    cat("⚠️ No ticker data available for analysis\n")
    return(NULL)
  }
  
  cat("📈 TICKER DATA ANALYSIS\n")
  cat(strrep("=", 40), "\n")
  
  # Basic Info
  cat(sprintf("Symbol: %s\n", ticker_data$symbol[1]))
  cat(sprintf("Data timestamp: %s\n", ticker_data$timestamp[1]))
  cat(sprintf("Current price: %.4f USDT\n", ticker_data$last_price[1]))
  
  # Price Statistics
  price_stats <- list(
    current_price = ticker_data$last_price[1],
    high_24h = ticker_data$high_24h[1],
    low_24h = ticker_data$low_24h[1],
    change_24h_pct = ticker_data$change_24h_pct[1],
    price_range_24h = ticker_data$high_24h[1] - ticker_data$low_24h[1],
    price_position = (ticker_data$last_price[1] - ticker_data$low_24h[1]) / 
                    (ticker_data$high_24h[1] - ticker_data$low_24h[1])
  )
  
  cat("\n💰 PRICE ANALYSIS:\n")
  cat(sprintf("   24h High: %.4f USDT\n", price_stats$high_24h))
  cat(sprintf("   24h Low: %.4f USDT\n", price_stats$low_24h))
  cat(sprintf("   24h Range: %.4f USDT (%.2f%%)\n", 
              price_stats$price_range_24h, 
              (price_stats$price_range_24h / price_stats$current_price) * 100))
  cat(sprintf("   24h Change: %.2f%%\n", price_stats$change_24h_pct))
  cat(sprintf("   Position in Range: %.1f%% %s\n", 
              price_stats$price_position * 100,
              if (price_stats$price_position > 0.7) "(Near High)" 
              else if (price_stats$price_position < 0.3) "(Near Low)" 
              else "(Mid-Range)"))
  
  # Volume Statistics
  volume_stats <- list(
    volume_24h = ticker_data$volume_24h[1],
    volume_24h_usdt = ticker_data$volume_24h_usdt[1],
    avg_price_24h = ticker_data$volume_24h_usdt[1] / ticker_data$volume_24h[1]
  )
  
  cat("\n📊 VOLUME ANALYSIS:\n")
  cat(sprintf("   24h Volume: %.0f %s\n", volume_stats$volume_24h, 
              substr(ticker_data$symbol[1], 1, 3)))
  cat(sprintf("   24h Volume (USDT): %.0f USDT (%.1fM)\n", 
              volume_stats$volume_24h_usdt, volume_stats$volume_24h_usdt / 1000000))
  cat(sprintf("   Average Price: %.4f USDT\n", volume_stats$avg_price_24h))
  
  # Futures-specific
  if ("funding_rate" %in% names(ticker_data)) {
    funding_stats <- list(
      funding_rate = ticker_data$funding_rate[1],
      funding_rate_pct = ticker_data$funding_rate[1] * 100,
      open_interest = ticker_data$open_interest[1]
    )
    
    cat("\n🔮 FUTURES ANALYSIS:\n")
    cat(sprintf("   Funding Rate: %.6f (%.4f%%)\n", 
                funding_stats$funding_rate, funding_stats$funding_rate_pct))
    cat(sprintf("   Direction: %s\n", 
                if (funding_stats$funding_rate > 0) "Longs pay Shorts (Bullish pressure)" 
                else "Shorts pay Longs (Bearish pressure)"))
    
    if (!is.na(funding_stats$open_interest)) {
      cat(sprintf("   Open Interest: %.0f contracts\n", funding_stats$open_interest))
    }
  }
  
  # Bid-Ask Analysis
  if ("best_bid" %in% names(ticker_data) && "best_ask" %in% names(ticker_data)) {
    spread_stats <- list(
      best_bid = ticker_data$best_bid[1],
      best_ask = ticker_data$best_ask[1],
      spread = ticker_data$best_ask[1] - ticker_data$best_bid[1],
      spread_pct = ((ticker_data$best_ask[1] - ticker_data$best_bid[1]) / 
                   ticker_data$last_price[1]) * 100
    )
    
    cat("\n💱 SPREAD ANALYSIS:\n")
    cat(sprintf("   Best Bid: %.4f USDT\n", spread_stats$best_bid))
    cat(sprintf("   Best Ask: %.4f USDT\n", spread_stats$best_ask))
    cat(sprintf("   Spread: %.6f USDT (%.4f%%)\n", 
                spread_stats$spread, spread_stats$spread_pct))
    cat(sprintf("   Liquidity: %s\n", 
                if (spread_stats$spread_pct < 0.01) "Excellent" 
                else if (spread_stats$spread_pct < 0.05) "Good" 
                else "Poor"))
  }
  
  return(list(
    price_stats = price_stats,
    volume_stats = volume_stats,
    funding_stats = if (exists("funding_stats")) funding_stats else NULL,
    spread_stats = if (exists("spread_stats")) spread_stats else NULL
  ))
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ ORDERBOOK DESCRIPTIVE STATISTICS                                                                     │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
analyze_orderbook_data <- function(orderbook_data) {
  if (is.null(orderbook_data) || nrow(orderbook_data) == 0) {
    cat("⚠️ No orderbook data available for analysis\n")
    return(NULL)
  }
  
  cat("\n📚 ORDERBOOK DATA ANALYSIS\n")
  cat(strrep("=", 40), "\n")
  
  cat(sprintf("Total observations: %d\n", nrow(orderbook_data)))
  cat(sprintf("Time range: %s to %s\n", 
              min(orderbook_data$timestamp), max(orderbook_data$timestamp)))
  
  # Spread Statistics
  spread_stats <- list(
    mean_spread_pct = mean(orderbook_data$spread_pct, na.rm = TRUE),
    median_spread_pct = median(orderbook_data$spread_pct, na.rm = TRUE),
    min_spread_pct = min(orderbook_data$spread_pct, na.rm = TRUE),
    max_spread_pct = max(orderbook_data$spread_pct, na.rm = TRUE),
    sd_spread_pct = sd(orderbook_data$spread_pct, na.rm = TRUE)
  )
  
  cat("\n💱 SPREAD STATISTICS:\n")
  cat(sprintf("   Mean Spread: %.4f%%\n", spread_stats$mean_spread_pct))
  cat(sprintf("   Median Spread: %.4f%%\n", spread_stats$median_spread_pct))
  cat(sprintf("   Min Spread: %.4f%%\n", spread_stats$min_spread_pct))
  cat(sprintf("   Max Spread: %.4f%%\n", spread_stats$max_spread_pct))
  cat(sprintf("   Std Dev: %.4f%%\n", spread_stats$sd_spread_pct))
  
  # Bid-Ask Ratio Statistics (if available)
  if ("bid_ask_ratio" %in% names(orderbook_data)) {
    ratio_stats <- list(
      mean_ratio = mean(orderbook_data$bid_ask_ratio, na.rm = TRUE),
      median_ratio = median(orderbook_data$bid_ask_ratio, na.rm = TRUE),
      bullish_ratio = mean(orderbook_data$bid_ask_ratio > 1, na.rm = TRUE),
      bearish_ratio = mean(orderbook_data$bid_ask_ratio < 1, na.rm = TRUE)
    )
    
    cat("\n⚖️ BID-ASK RATIO STATISTICS:\n")
    cat(sprintf("   Mean Ratio: %.3f\n", ratio_stats$mean_ratio))
    cat(sprintf("   Median Ratio: %.3f\n", ratio_stats$median_ratio))
    cat(sprintf("   Bullish Periods: %.1f%% (Ratio > 1)\n", ratio_stats$bullish_ratio * 100))
    cat(sprintf("   Bearish Periods: %.1f%% (Ratio < 1)\n", ratio_stats$bearish_ratio * 100))
    cat(sprintf("   Market Bias: %s\n", 
                if (ratio_stats$mean_ratio > 1.05) "Bullish" 
                else if (ratio_stats$mean_ratio < 0.95) "Bearish" 
                else "Neutral"))
  }
  
  return(list(spread_stats = spread_stats, ratio_stats = if (exists("ratio_stats")) ratio_stats else NULL))
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ TRADES DESCRIPTIVE STATISTICS                                                                        │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
analyze_trades_data <- function(trades_data) {
  if (is.null(trades_data) || nrow(trades_data) == 0) {
    cat("⚠️ No trades data available for analysis\n")
    return(NULL)
  }
  
  cat("\n🔄 TRADES DATA ANALYSIS\n")
  cat(strrep("=", 40), "\n")
  
  cat(sprintf("Total trade observations: %d\n", nrow(trades_data)))
  cat(sprintf("Time range: %s to %s\n", 
              min(trades_data$timestamp), max(trades_data$timestamp)))
  
  # Basic Trade Statistics
  trade_stats <- list(
    total_trades = trades_data$total_trades[1],
    total_volume = trades_data$total_volume[1],
    avg_price = trades_data$avg_price[1],
    volume_weighted_price = trades_data$volume_weighted_price[1],
    price_range = trades_data$price_range[1]
  )
  
  cat("\n📊 TRADE STATISTICS:\n")
  cat(sprintf("   Total Trades: %d\n", trade_stats$total_trades))
  cat(sprintf("   Total Volume: %.2f\n", trade_stats$total_volume))
  cat(sprintf("   Average Price: %.4f USDT\n", trade_stats$avg_price))
  cat(sprintf("   VWAP: %.4f USDT\n", trade_stats$volume_weighted_price))
  cat(sprintf("   Price Range: %.4f USDT\n", trade_stats$price_range))
  
  # Buy vs Sell Analysis
  if ("buy_sell_ratio" %in% names(trades_data)) {
    buysell_stats <- list(
      buy_volume = trades_data$buy_volume[1],
      sell_volume = trades_data$sell_volume[1],
      buy_sell_ratio = trades_data$buy_sell_ratio[1],
      buy_percentage = (trades_data$buy_volume[1] / trades_data$total_volume[1]) * 100,
      sell_percentage = (trades_data$sell_volume[1] / trades_data$total_volume[1]) * 100
    )
    
    cat("\n📈 BUY vs SELL ANALYSIS:\n")
    cat(sprintf("   Buy Volume: %.2f (%.1f%%)\n", 
                buysell_stats$buy_volume, buysell_stats$buy_percentage))
    cat(sprintf("   Sell Volume: %.2f (%.1f%%)\n", 
                buysell_stats$sell_volume, buysell_stats$sell_percentage))
    cat(sprintf("   Buy/Sell Ratio: %.3f\n", buysell_stats$buy_sell_ratio))
    cat(sprintf("   Market Sentiment: %s\n", 
                if (buysell_stats$buy_sell_ratio > 1.2) "Strong Buying Pressure" 
                else if (buysell_stats$buy_sell_ratio > 1.05) "Buying Pressure" 
                else if (buysell_stats$buy_sell_ratio < 0.8) "Strong Selling Pressure"
                else if (buysell_stats$buy_sell_ratio < 0.95) "Selling Pressure"
                else "Balanced"))
    
    # Trade count analysis if available
    if ("buy_trades_count" %in% names(trades_data)) {
      count_stats <- list(
        buy_trades = trades_data$buy_trades_count[1],
        sell_trades = trades_data$sell_trades_count[1],
        avg_buy_size = trades_data$buy_volume[1] / trades_data$buy_trades_count[1],
        avg_sell_size = trades_data$sell_volume[1] / trades_data$sell_trades_count[1]
      )
      
      cat("\n🔢 TRADE COUNT ANALYSIS:\n")
      cat(sprintf("   Buy Trades: %d (Avg size: %.2f)\n", 
                  count_stats$buy_trades, count_stats$avg_buy_size))
      cat(sprintf("   Sell Trades: %d (Avg size: %.2f)\n", 
                  count_stats$sell_trades, count_stats$avg_sell_size))
      cat(sprintf("   Size Ratio: %.3f %s\n", 
                  count_stats$avg_buy_size / count_stats$avg_sell_size,
                  if (count_stats$avg_buy_size > count_stats$avg_sell_size) "(Larger buys)" else "(Larger sells)"))
    }
  }
  
  return(list(
    trade_stats = trade_stats, 
    buysell_stats = if (exists("buysell_stats")) buysell_stats else NULL,
    count_stats = if (exists("count_stats")) count_stats else NULL
  ))
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ HISTORICAL CANDLES DESCRIPTIVE STATISTICS                                                            │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
analyze_historical_candles <- function(candles_data) {
  if (is.null(candles_data) || length(candles_data) == 0) {
    cat("⚠️ No historical candle data available for analysis\n")
    return(NULL)
  }
  
  cat("\n📈 HISTORICAL CANDLES ANALYSIS\n")
  cat(strrep("=", 40), "\n")
  
  results <- list()
  
  for (timeframe in names(candles_data)) {
    candles <- candles_data[[timeframe]]
    
    if (is.null(candles) || nrow(candles) == 0) next
    
    cat(sprintf("\n🕐 TIMEFRAME: %s\n", toupper(timeframe)))
    cat(sprintf("   Candles: %d\n", nrow(candles)))
    cat(sprintf("   Period: %s to %s\n", min(candles$timestamp), max(candles$timestamp)))
    
    # Price Statistics
    price_stats <- list(
      current_price = tail(candles$close, 1),
      period_high = max(candles$high),
      period_low = min(candles$low),
      period_range = max(candles$high) - min(candles$low),
      total_return = ((tail(candles$close, 1) - head(candles$close, 1)) / head(candles$close, 1)) * 100,
      
      # Daily statistics
      daily_returns = c(NA, diff(log(candles$close))) * 100,
      volatility = sd(c(NA, diff(log(candles$close))), na.rm = TRUE) * 100,
      
      # OHLC Analysis
      green_candles = sum(candles$close > candles$open),
      red_candles = sum(candles$close < candles$open),
      doji_candles = sum(abs(candles$close - candles$open) < 0.001)
    )
    
    cat(sprintf("   Current Price: %.4f USDT\n", price_stats$current_price))
    cat(sprintf("   Period High: %.4f USDT\n", price_stats$period_high))
    cat(sprintf("   Period Low: %.4f USDT\n", price_stats$period_low))
    cat(sprintf("   Total Return: %.2f%%\n", price_stats$total_return))
    cat(sprintf("   Volatility: %.2f%%\n", price_stats$volatility))
    cat(sprintf("   Green Candles: %d (%.1f%%)\n", 
                price_stats$green_candles, 
                (price_stats$green_candles / nrow(candles)) * 100))
    cat(sprintf("   Red Candles: %d (%.1f%%)\n", 
                price_stats$red_candles, 
                (price_stats$red_candles / nrow(candles)) * 100))
    
    # Volume Statistics
    volume_stats <- list(
      total_volume = sum(candles$volume),
      avg_volume = mean(candles$volume),
      max_volume = max(candles$volume),
      min_volume = min(candles$volume),
      volume_trend = if (nrow(candles) > 2) {
        tryCatch({
          cor(1:nrow(candles), candles$volume, use = "complete.obs")
        }, error = function(e) NA)
      } else NA
    )
    
    cat(sprintf("   Avg Volume: %.2f\n", volume_stats$avg_volume))
    cat(sprintf("   Max Volume: %.2f\n", volume_stats$max_volume))
    
    # Safe volume trend display with NA handling
    if (!is.na(volume_stats$volume_trend)) {
      trend_label <- if (volume_stats$volume_trend > 0.1) "(Increasing)" 
                     else if (volume_stats$volume_trend < -0.1) "(Decreasing)" 
                     else "(Stable)"
      cat(sprintf("   Volume Trend: %.3f %s\n", volume_stats$volume_trend, trend_label))
    } else {
      cat("   Volume Trend: N/A (insufficient data)\n")
    }
    
    # Technical Patterns (Simple)
    if (nrow(candles) >= 20) {
      # Simple moving averages
      candles$sma_10 <- filter(candles$close, rep(1/10, 10), sides = 1)
      candles$sma_20 <- filter(candles$close, rep(1/20, 20), sides = 1)
      
      current_sma10 <- tail(na.omit(candles$sma_10), 1)
      current_sma20 <- tail(na.omit(candles$sma_20), 1)
      
      cat(sprintf("   SMA(10): %.4f %s\n", current_sma10,
                  if (price_stats$current_price > current_sma10) "(Above)" else "(Below)"))
      cat(sprintf("   SMA(20): %.4f %s\n", current_sma20,
                  if (price_stats$current_price > current_sma20) "(Above)" else "(Below)"))
      cat(sprintf("   Trend: %s\n", 
                  if (current_sma10 > current_sma20 && price_stats$current_price > current_sma10) "Bullish"
                  else if (current_sma10 < current_sma20 && price_stats$current_price < current_sma10) "Bearish"
                  else "Neutral"))
    }
    
    results[[timeframe]] <- list(
      price_stats = price_stats,
      volume_stats = volume_stats
    )
  }
  
  return(results)
}

# ==========================================================================================================
# 📊 COMPREHENSIVE ANALYSIS FUNCTION
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ COMPLETE DATA EXPLORATION - Führt alle Analysen durch                                               │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
complete_data_exploration <- function(symbol = "ADAUSDT_UMCBL", save_report = TRUE) {
  cat("🚀 COMPLETE BITGET DATA EXPLORATION\n")
  cat(strrep("=", 60), "\n")
  cat(sprintf("Symbol: %s\n", symbol))
  cat(sprintf("Analysis time: %s\n", Sys.time()))
  cat("\n")
  
  # Load all data
  all_data <- load_all_bitget_data(symbol)
  
  if (length(all_data$metadata$data_types) == 0) {
    cat("❌ No data found for analysis. Please run data collection first.\n")
    return(NULL)
  }
  
  # Run all analyses
  analysis_results <- list()
  
  # Ticker Analysis
  if ("ticker" %in% names(all_data)) {
    analysis_results$ticker <- analyze_ticker_data(all_data$ticker)
  }
  
  # Orderbook Analysis
  if ("orderbook" %in% names(all_data)) {
    analysis_results$orderbook <- analyze_orderbook_data(all_data$orderbook)
  }
  
  # Trades Analysis
  if ("trades" %in% names(all_data)) {
    analysis_results$trades <- analyze_trades_data(all_data$trades)
  }
  
  # Historical Candles Analysis
  if ("candles" %in% names(all_data)) {
    analysis_results$candles <- analyze_historical_candles(all_data$candles)
  }
  
  # Sentiment Analysis Summary
  if ("sentiment" %in% names(all_data)) {
    cat("\n🧮 SENTIMENT ANALYSIS SUMMARY\n")
    cat(strrep("=", 40), "\n")
    
    sentiment_summary <- table(all_data$sentiment$overall_sentiment)
    for (sentiment in names(sentiment_summary)) {
      cat(sprintf("   %s: %d observations\n", sentiment, sentiment_summary[sentiment]))
    }
    
    if (nrow(all_data$sentiment) > 0) {
      avg_sentiment <- mean(all_data$sentiment$sentiment_percentage, na.rm = TRUE)
      cat(sprintf("   Average Sentiment Score: %.1f%%\n", avg_sentiment))
    }
  }
  
  # Summary
  cat("\n📋 EXPLORATION SUMMARY\n")
  cat(strrep("=", 40), "\n")
  cat(sprintf("Data types analyzed: %d\n", length(analysis_results)))
  cat(sprintf("Analysis completed: %s\n", Sys.time()))
  
  # Save report if requested
  if (save_report) {
    save_exploration_report(analysis_results, all_data, symbol)
  }
  
  return(list(
    raw_data = all_data,
    analysis_results = analysis_results,
    metadata = list(
      symbol = symbol,
      analysis_time = Sys.time(),
      data_types = names(analysis_results)
    )
  ))
}

# ==========================================================================================================
# 💾 REPORT GENERATION
# ==========================================================================================================

save_exploration_report <- function(analysis_results, raw_data, symbol) {
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    report_file <- paste0(REPORTS_PATH, symbol, "_exploration_report_", timestamp, ".txt")
    
    # Create text report
    report_content <- c(
      paste("BITGET DATA EXPLORATION REPORT"),
      paste("Symbol:", symbol),
      paste("Generated:", Sys.time()),
      paste(strrep("=", 60)),
      "",
      paste("DATA SUMMARY:"),
      paste("- Data types analyzed:", length(analysis_results)),
      paste("- Total files loaded:", raw_data$metadata$total_files_loaded),
      ""
    )
    
    # Add key findings
    if ("ticker" %in% names(analysis_results) && !is.null(analysis_results$ticker)) {
      ticker <- analysis_results$ticker
      report_content <- c(report_content,
        paste("PRICE ANALYSIS:"),
        paste("- Current Price:", sprintf("%.4f USDT", ticker$price_stats$current_price)),
        paste("- 24h Change:", sprintf("%.2f%%", ticker$price_stats$change_24h_pct)),
        paste("- Position in 24h Range:", sprintf("%.1f%%", ticker$price_stats$price_position * 100)),
        ""
      )
    }
    
    writeLines(report_content, report_file)
    cat(sprintf("📄 Exploration report saved: %s\n", basename(report_file)))
    
  }, error = function(e) {
    cat("⚠️ Could not save exploration report:", e$message, "\n")
  })
}

# ==========================================================================================================
# 🔧 QUICK FIX FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ FIX CSV FILES - Repariert defekte CSV-Dateien                                                       │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
fix_csv_files <- function(symbol = "ADAUSDT_UMCBL") {
  cat("🔧 Fixing CSV files...\n")
  
  candle_files <- list.files(paste0(RAW_DATA_PATH, "candles/"), 
                             pattern = paste0(symbol, "_.*\\.csv$"), 
                             full.names = TRUE)
  
  fixed_count <- 0
  
  for (file in candle_files) {
    tryCatch({
      # Read file content
      lines <- readLines(file, warn = FALSE)
      
      # Check if last line is incomplete (no newline)
      if (length(lines) > 0) {
        # Add newline if missing
        if (!endsWith(paste(lines, collapse = "\n"), "\n")) {
          writeLines(lines, file)
          fixed_count <- fixed_count + 1
        }
      }
    }, error = function(e) {
      cat("⚠️ Could not fix file:", basename(file), "\n")
    })
  }
  
  cat(sprintf("✅ Fixed %d CSV files\n", fixed_count))
  return(fixed_count)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ BASIC ML ANALYSIS - Ersatz für working_ml_analysis                                                  │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
basic_ml_analysis <- function(symbol = "ADAUSDT_UMCBL", timeframe = "1h") {
  cat("🤖 BASIC ML ANALYSIS\n")
  cat(strrep("=", 30), "\n")
  
  # Load data
  all_data <- load_all_bitget_data(symbol)
  
  if (!("candles" %in% names(all_data)) || 
      !(timeframe %in% names(all_data$candles))) {
    cat("❌ No candle data available for ML analysis\n")
    return(NULL)
  }
  
  candles <- all_data$candles[[timeframe]]
  
  if (nrow(candles) < 20) {
    cat("❌ Insufficient data for ML analysis\n")
    return(NULL)
  }
  
  # Basic technical indicators
  ml_features <- candles
  
  # Simple Moving Averages
  if (nrow(candles) >= 10) {
    ml_features$sma_10 <- filter(candles$close, rep(1/10, 10), sides = 1)
  }
  if (nrow(candles) >= 20) {
    ml_features$sma_20 <- filter(candles$close, rep(1/20, 20), sides = 1)
  }
  
  # Price changes
  ml_features$price_change <- c(NA, diff(candles$close))
  ml_features$price_change_pct <- c(NA, diff(log(candles$close))) * 100
  
  # Volume indicators
  if (nrow(candles) >= 10) {
    ml_features$volume_sma <- filter(candles$volume, rep(1/10, 10), sides = 1)
    ml_features$volume_ratio <- candles$volume / ml_features$volume_sma
  }
  
  # Simple signals
  latest <- tail(ml_features, 1)
  
  signals <- list(
    current_price = latest$close,
    trend = if (!is.na(latest$sma_10) && !is.na(latest$sma_20)) {
      if (latest$close > latest$sma_10 && latest$sma_10 > latest$sma_20) "BULLISH"
      else if (latest$close < latest$sma_10 && latest$sma_10 < latest$sma_20) "BEARISH"
      else "NEUTRAL"
    } else "UNKNOWN",
    volatility = sd(tail(ml_features$price_change_pct, 20), na.rm = TRUE),
    volume_trend = if (!is.na(latest$volume_ratio)) {
      if (latest$volume_ratio > 1.5) "HIGH_VOLUME"
      else if (latest$volume_ratio < 0.5) "LOW_VOLUME" 
      else "NORMAL_VOLUME"
    } else "UNKNOWN"
  )
  
  cat(sprintf("Current Price: %.4f USDT\n", signals$current_price))
  cat(sprintf("Trend: %s\n", signals$trend))
  cat(sprintf("Volatility: %.2f%%\n", signals$volatility))
  cat(sprintf("Volume: %s\n", signals$volume_trend))
  
  return(list(
    ml_features = ml_features,
    signals = signals,
    metadata = list(
      symbol = symbol,
      timeframe = timeframe,
      candles_count = nrow(candles),
      analysis_time = Sys.time()
    )
  ))
}

# ==========================================================================================================
# ✅ ENHANCED QUICK START FUNCTIONS
# ==========================================================================================================

# Quick exploration for immediate insights
quick_exploration <- function(symbol = "ADAUSDT_UMCBL") {
  cat("⚡ QUICK DATA EXPLORATION\n")
  cat(strrep("=", 30), "\n")
  
  # Load latest data only
  all_data <- load_all_bitget_data(symbol)
  
  # Quick ticker summary
  if ("ticker" %in% names(all_data)) {
    ticker <- all_data$ticker
    cat(sprintf("💰 Current Price: %.4f USDT (%.2f%% 24h)\n", 
                ticker$last_price[1], ticker$change_24h_pct[1]))
    cat(sprintf("📊 24h Volume: %.1fM USDT\n", ticker$volume_24h_usdt[1] / 1000000))
  }
  
  # Quick sentiment
  if ("sentiment" %in% names(all_data) && nrow(all_data$sentiment) > 0) {
    latest_sentiment <- tail(all_data$sentiment, 1)
    cat(sprintf("🧮 Latest Sentiment: %s (%.0f%%)\n", 
                latest_sentiment$overall_sentiment, latest_sentiment$sentiment_percentage))
  }
  
  # Quick trades summary
  if ("trades" %in% names(all_data) && nrow(all_data$trades) > 0) {
    latest_trades <- tail(all_data$trades, 1)
    cat(sprintf("🔄 Buy/Sell Ratio: %.2f\n", latest_trades$buy_sell_ratio))
  }
  
  return(all_data)
}

# ==========================================================================================================
# ✅ SYSTEM STATUS & USAGE
# ==========================================================================================================

cat("✅ BITGET DATA EXPLORER LOADED!\n")
cat(strrep("=", 50), "\n")
cat("📊 READY FOR COMPREHENSIVE DATA ANALYSIS\n")
cat("\n🚀 IMMEDIATE USE:\n")
cat("   # Complete exploration (recommended):\n")
cat("   results <- complete_data_exploration('ADAUSDT_UMCBL')\n")
cat("\n   # Quick overview:\n")
cat("   quick <- quick_exploration('ADAUSDT_UMCBL')\n")
cat("\n   # Load data manually:\n")
cat("   data <- load_all_bitget_data('ADAUSDT_UMCBL')\n")
cat("\n📊 ANALYSIS FEATURES:\n")
cat("   ✅ Comprehensive descriptive statistics\n")
cat("   ✅ Price, volume, spread analysis\n")
cat("   ✅ Buy/sell pressure analysis\n")
cat("   ✅ Multi-timeframe candle analysis\n")
cat("   ✅ Sentiment tracking over time\n")
cat("   ✅ Automated report generation\n")
cat(sprintf("\n📄 Reports saved to: %s\n", REPORTS_PATH))
cat(strrep("=", 50), "\n")

# ==========================================================================================================
# 🎯 END OF BITGET DATA EXPLORER
# ==========================================================================================================