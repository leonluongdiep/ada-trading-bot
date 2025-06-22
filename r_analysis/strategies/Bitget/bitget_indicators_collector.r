# =====================================================
# 📊 BITGET ALL INDICATORS DATA COLLECTOR
# =====================================================
# Sammelt alle verfügbaren Marktdaten und Indikatoren für Musteranalyse

# Load your API connection (assuming your connection script is sourced)
#source("BITGET API CONNECTION.r")

# Additional libraries for data analysis
if (!require(dplyr, quietly = TRUE)) install.packages("dplyr")
if (!require(tidyr, quietly = TRUE)) install.packages("tidyr")
if (!require(lubridate, quietly = TRUE)) install.packages("lubridate")

library(dplyr)
library(tidyr)
library(lubridate)

# =====================================================
# 📈 1. KERZENDATEN (OHLCV) - Verschiedene Timeframes
# =====================================================

get_candle_data <- function(symbol = "ADAUSDT_UMCBL", timeframe = "1m", limit = 1000) {
  cat("📊 Fetching", timeframe, "candle data for", symbol, "...\n")
  
  # Timeframe mapping für Bitget API
  timeframe_map <- list(
    "1m" = "1m", "5m" = "5m", "15m" = "15m", "30m" = "30m",
    "1h" = "1H", "4h" = "4H", "1d" = "1D", "1w" = "1W"
  )
  
  interval <- timeframe_map[[timeframe]]
  if (is.null(interval)) {
    cat("❌ Invalid timeframe:", timeframe, "\n")
    return(NULL)
  }
  
  params <- list(
    symbol = symbol,
    granularity = interval,
    limit = as.character(limit)
  )
  
  result <- bitget_request("/api/mix/v1/market/candles", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    # Konvertiere zu strukturiertem DataFrame
    candles_df <- data.frame(
      timestamp = as.POSIXct(as.numeric(result$data[,1])/1000, origin="1970-01-01"),
      open = as.numeric(result$data[,2]),
      high = as.numeric(result$data[,3]),
      low = as.numeric(result$data[,4]),
      close = as.numeric(result$data[,5]),
      volume = as.numeric(result$data[,6]),
      timeframe = timeframe,
      symbol = symbol,
      stringsAsFactors = FALSE
    )
    
    cat("✅ Retrieved", nrow(candles_df), "candles for", timeframe, "\n")
    return(candles_df)
  } else {
    cat("❌ Failed to fetch candle data\n")
    return(NULL)
  }
}

# =====================================================
# 📊 2. TICKER DATA - 24h Statistiken
# =====================================================

get_ticker_data <- function(symbol = "ADAUSDT_UMCBL") {
  cat("📈 Fetching ticker data for", symbol, "...\n")
  
  params <- list(symbol = symbol)
  result <- bitget_request("/api/mix/v1/market/ticker", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    ticker_df <- data.frame(
      symbol = result$data$symbol,
      last_price = as.numeric(result$data$last),
      mark_price = as.numeric(result$data$markPrice),
      index_price = as.numeric(result$data$indexPrice),
      high_24h = as.numeric(result$data$high24h),
      low_24h = as.numeric(result$data$low24h),
      volume_24h = as.numeric(result$data$baseVolume),
      volume_24h_usdt = as.numeric(result$data$quoteVolume),
      change_24h = as.numeric(result$data$chg),
      change_24h_pct = as.numeric(result$data$changeUtc),
      bid_price = as.numeric(result$data$bidPr),
      ask_price = as.numeric(result$data$askPr),
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    cat("✅ Ticker data retrieved\n")
    return(ticker_df)
  } else {
    cat("❌ Failed to fetch ticker data\n")
    return(NULL)
  }
}

# =====================================================
# 📚 3. ORDERBUCH-TIEFE
# =====================================================

get_orderbook_depth <- function(symbol = "ADAUSDT_UMCBL", limit = 50) {
  cat("📚 Fetching orderbook depth for", symbol, "...\n")
  
  params <- list(symbol = symbol, limit = as.character(limit))
  result <- bitget_request("/api/mix/v1/market/depth", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    # Bids (Kaufaufträge)
    bids_df <- data.frame(
      price = as.numeric(result$data$bids[,1]),
      size = as.numeric(result$data$bids[,2]),
      side = "bid",
      level = 1:nrow(result$data$bids),
      symbol = symbol,
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    # Asks (Verkaufsaufträge)  
    asks_df <- data.frame(
      price = as.numeric(result$data$asks[,1]),
      size = as.numeric(result$data$asks[,2]),
      side = "ask",
      level = 1:nrow(result$data$asks),
      symbol = symbol,
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    orderbook_df <- rbind(bids_df, asks_df)
    
    # Zusätzliche Metriken berechnen
    spread <- min(asks_df$price) - max(bids_df$price)
    mid_price <- (min(asks_df$price) + max(bids_df$price)) / 2
    
    orderbook_summary <- data.frame(
      symbol = symbol,
      best_bid = max(bids_df$price),
      best_ask = min(asks_df$price),
      spread = spread,
      spread_pct = (spread / mid_price) * 100,
      mid_price = mid_price,
      bid_volume_10 = sum(head(bids_df$size, 10)),
      ask_volume_10 = sum(head(asks_df$size, 10)),
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    cat("✅ Orderbook depth retrieved:", nrow(orderbook_df), "levels\n")
    
    return(list(
      orderbook = orderbook_df,
      summary = orderbook_summary
    ))
  } else {
    cat("❌ Failed to fetch orderbook data\n")
    return(NULL)
  }
}

# =====================================================
# 🔄 4. RECENT TRADES
# =====================================================

get_recent_trades <- function(symbol = "ADAUSDT_UMCBL", limit = 100) {
  cat("🔄 Fetching recent trades for", symbol, "...\n")
  
  params <- list(symbol = symbol, limit = as.character(limit))
  result <- bitget_request("/api/mix/v1/market/fills", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    trades_df <- data.frame(
      trade_id = result$data$tradeId,
      price = as.numeric(result$data$price),
      size = as.numeric(result$data$size),
      side = result$data$side,
      timestamp = as.POSIXct(as.numeric(result$data$ts)/1000, origin="1970-01-01"),
      symbol = symbol,
      stringsAsFactors = FALSE
    )
    
    # Trade-Analyse
    trades_summary <- trades_df %>%
      summarise(
        symbol = first(symbol),
        total_trades = n(),
        avg_price = mean(price),
        volume_weighted_price = sum(price * size) / sum(size),
        total_volume = sum(size),
        buy_volume = sum(size[side == "buy"]),
        sell_volume = sum(size[side == "sell"]),
        buy_sell_ratio = buy_volume / sell_volume,
        price_range = max(price) - min(price),
        timestamp = Sys.time(),
        .groups = "drop"
      )
    
    cat("✅ Recent trades retrieved:", nrow(trades_df), "trades\n")
    
    return(list(
      trades = trades_df,
      summary = trades_summary
    ))
  } else {
    cat("❌ Failed to fetch trades data\n")
    return(NULL)
  }
}

# =====================================================
# 💰 5. FUNDING RATE (Futures-spezifisch)
# =====================================================

get_funding_rate <- function(symbol = "ADAUSDT_UMCBL") {
  cat("💰 Fetching funding rate for", symbol, "...\n")
  
  params <- list(symbol = symbol)
  result <- bitget_request("/api/mix/v1/market/current-fundingRate", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    funding_df <- data.frame(
      symbol = result$data$symbol,
      funding_rate = as.numeric(result$data$fundingRate),
      funding_time = as.POSIXct(as.numeric(result$data$fundingTime)/1000, origin="1970-01-01"),
      next_funding_time = as.POSIXct(as.numeric(result$data$nextFundingTime)/1000, origin="1970-01-01"),
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    cat("✅ Funding rate retrieved:", funding_df$funding_rate, "\n")
    return(funding_df)
  } else {
    cat("❌ Failed to fetch funding rate\n")
    return(NULL)
  }
}

# =====================================================
# 🔢 6. OPEN INTEREST
# =====================================================

get_open_interest <- function(symbol = "ADAUSDT_UMCBL") {
  cat("🔢 Fetching open interest for", symbol, "...\n")
  
  params <- list(symbol = symbol)
  result <- bitget_request("/api/mix/v1/market/open-interest", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    oi_df <- data.frame(
      symbol = result$data$symbol,
      open_interest = as.numeric(result$data$amount),
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    cat("✅ Open Interest retrieved:", oi_df$open_interest, "\n")
    return(oi_df)
  } else {
    cat("❌ Failed to fetch open interest\n")
    return(NULL)
  }
}

# =====================================================
# ⚖️ 7. LONG/SHORT RATIO
# =====================================================

get_long_short_ratio <- function(symbol = "ADAUSDT_UMCBL") {
  cat("⚖️ Fetching long/short ratio for", symbol, "...\n")
  
  params <- list(symbol = symbol)
  result <- bitget_request("/api/mix/v1/market/account-ratio", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    ratio_df <- data.frame(
      symbol = symbol,
      long_account_ratio = as.numeric(result$data$longAccountRatio),
      short_account_ratio = as.numeric(result$data$shortAccountRatio),
      long_position_ratio = as.numeric(result$data$longPositionRatio),
      short_position_ratio = as.numeric(result$data$shortPositionRatio),
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    cat("✅ Long/Short ratio retrieved\n")
    return(ratio_df)
  } else {
    cat("❌ Failed to fetch long/short ratio\n")
    return(NULL)
  }
}

# =====================================================
# 🧮 8. TECHNISCHE INDIKATOREN BERECHNUNG
# =====================================================

calculate_technical_indicators <- function(candle_data) {
  cat("🧮 Calculating technical indicators...\n")
  
  if (is.null(candle_data) || nrow(candle_data) < 50) {
    cat("❌ Insufficient candle data for indicators\n")
    return(NULL)
  }
  
  # Lade TTR für technische Indikatoren
  if (!require(TTR, quietly = TRUE)) {
    install.packages("TTR")
    library(TTR)
  }
  
  prices <- candle_data$close
  highs <- candle_data$high
  lows <- candle_data$low
  volumes <- candle_data$volume
  
  indicators_df <- data.frame(
    timestamp = candle_data$timestamp,
    symbol = candle_data$symbol,
    timeframe = candle_data$timeframe,
    close = prices,
    
    # Trend-Indikatoren
    sma_20 = SMA(prices, n = 20),
    sma_50 = SMA(prices, n = 50),
    ema_12 = EMA(prices, n = 12),
    ema_26 = EMA(prices, n = 26),
    
    # Momentum-Indikatoren
    rsi_14 = RSI(prices, n = 14),
    roc_10 = ROC(prices, n = 10),
    
    # Volatilität
    atr_14 = ATR(cbind(highs, lows, prices), n = 14)[,2],
    
    # Volumen-Indikatoren
    volume = volumes,
    volume_sma_20 = SMA(volumes, n = 20),
    
    stringsAsFactors = FALSE
  )
  
  # MACD berechnen
  macd_data <- MACD(prices, nFast = 12, nSlow = 26, nSig = 9)
  indicators_df$macd = macd_data[,1]
  indicators_df$macd_signal = macd_data[,2]
  indicators_df$macd_histogram = macd_data[,1] - macd_data[,2]
  
  # Bollinger Bands
  bb_data <- BBands(prices, n = 20, sd = 2)
  indicators_df$bb_upper = bb_data[,1]
  indicators_df$bb_middle = bb_data[,2] 
  indicators_df$bb_lower = bb_data[,3]
  indicators_df$bb_pct = bb_data[,4]
  
  # Stochastic
  stoch_data <- stoch(cbind(highs, lows, prices), nFastK = 14, nFastD = 3, nSlowD = 3)
  indicators_df$stoch_k = stoch_data[,1]
  indicators_df$stoch_d = stoch_data[,2]
  
  cat("✅ Technical indicators calculated\n")
  return(indicators_df)
}

# =====================================================
# 🎯 9. MASTER DATA COLLECTOR - Alle Daten sammeln
# =====================================================

collect_all_market_data <- function(symbol = "ADAUSDT_UMCBL", timeframes = c("1m", "5m", "15m", "1h", "4h", "1d")) {
  cat("\n🎯 COLLECTING ALL MARKET DATA FOR", symbol, "\n")
  cat("================================================\n")
  
  market_data <- list()
  
  # 1. Ticker Data
  market_data$ticker <- get_ticker_data(symbol)
  
  # 2. Funding Rate
  market_data$funding <- get_funding_rate(symbol)
  
  # 3. Open Interest  
  market_data$open_interest <- get_open_interest(symbol)
  
  # 4. Long/Short Ratio
  market_data$long_short_ratio <- get_long_short_ratio(symbol)
  
  # 5. Orderbook
  market_data$orderbook <- get_orderbook_depth(symbol, limit = 50)
  
  # 6. Recent Trades
  market_data$trades <- get_recent_trades(symbol, limit = 100)
  
  # 7. Candle Data für alle Timeframes
  market_data$candles <- list()
  market_data$indicators <- list()
  
  for (tf in timeframes) {
    cat("\n📊 Processing timeframe:", tf, "\n")
    
    # Hole Candle Data
    candles <- get_candle_data(symbol, tf, limit = 1000)
    market_data$candles[[tf]] <- candles
    
    # Berechne technische Indikatoren
    if (!is.null(candles)) {
      indicators <- calculate_technical_indicators(candles)
      market_data$indicators[[tf]] <- indicators
    }
    
    Sys.sleep(0.1) # Kurze Pause zwischen API Calls
  }
  
  # 8. Zusammenfassung erstellen
  market_data$summary <- create_market_summary(market_data)
  
  cat("\n✅ ALL MARKET DATA COLLECTED SUCCESSFULLY!\n")
  cat("================================================\n")
  
  return(market_data)
}

# =====================================================
# 📋 10. MARKET SUMMARY
# =====================================================

create_market_summary <- function(market_data) {
  cat("📋 Creating market summary...\n")
  
  summary_list <- list(
    timestamp = Sys.time(),
    symbol = "ADAUSDT_UMCBL"
  )
  
  # Ticker Summary
  if (!is.null(market_data$ticker)) {
    summary_list$price_info <- list(
      last_price = market_data$ticker$last_price,
      change_24h_pct = market_data$ticker$change_24h_pct,
      volume_24h = market_data$ticker$volume_24h,
      high_24h = market_data$ticker$high_24h,
      low_24h = market_data$ticker$low_24h
    )
  }
  
  # Funding & OI
  if (!is.null(market_data$funding)) {
    summary_list$funding_rate = market_data$funding$funding_rate
  }
  
  if (!is.null(market_data$open_interest)) {
    summary_list$open_interest = market_data$open_interest$open_interest
  }
  
  # Long/Short Sentiment
  if (!is.null(market_data$long_short_ratio)) {
    summary_list$sentiment <- list(
      long_ratio = market_data$long_short_ratio$long_account_ratio,
      short_ratio = market_data$long_short_ratio$short_account_ratio
    )
  }
  
  # Orderbook Summary
  if (!is.null(market_data$orderbook) && !is.null(market_data$orderbook$summary)) {
    summary_list$orderbook <- list(
      spread_pct = market_data$orderbook$summary$spread_pct,
      bid_volume = market_data$orderbook$summary$bid_volume_10,
      ask_volume = market_data$orderbook$summary$ask_volume_10
    )
  }
  
  # Latest Indicators (from 5m timeframe)
  if (!is.null(market_data$indicators$`5m`)) {
    latest_ind <- tail(market_data$indicators$`5m`, 1)
    summary_list$indicators_5m <- list(
      rsi = latest_ind$rsi_14,
      macd = latest_ind$macd,
      bb_pct = latest_ind$bb_pct,
      volume_ratio = latest_ind$volume / latest_ind$volume_sma_20
    )
  }
  
  cat("✅ Market summary created\n")
  return(summary_list)
}

# =====================================================
# 🚀 QUICK START FUNCTION
# =====================================================

quick_analysis <- function(symbol = "ADAUSDT_UMCBL") {
  cat("🚀 QUICK MARKET ANALYSIS FOR", symbol, "\n")
  cat("==========================================\n")
  
  # Sammle die wichtigsten Daten
  data <- collect_all_market_data(symbol, timeframes = c("5m", "1h", "4h"))
  
  # Zeige Summary
  print(data$summary)
  
  # Speichere Daten für weitere Analyse
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("market_data_", symbol, "_", timestamp, ".RData")
  save(data, file = filename)
  
  cat("\n💾 Data saved to:", filename, "\n")
  cat("🎯 Ready for pattern analysis!\n")
  
  return(data)
}

# =====================================================
# 🎯 BEISPIEL VERWENDUNG
# =====================================================

cat("\n🎯 BITGET INDICATORS COLLECTOR READY!\n")
cat("=====================================\n")
cat("Usage examples:\n")
cat("1. Quick analysis:     data <- quick_analysis('ADAUSDT_UMCBL')\n")
cat("2. Full data collect:  data <- collect_all_market_data('ADAUSDT_UMCBL')\n")
cat("3. Single indicator:   ticker <- get_ticker_data('ADAUSDT_UMCBL')\n")
cat("4. Technical analysis: indicators <- calculate_technical_indicators(candles)\n")
cat("\n🚀 Ready to start pattern analysis!\n")



# 1. Lade die korrigierte Ticker-Funktion
get_ticker_data <- function(symbol = "ADAUSDT_UMCBL") {
  cat("📈 Fetching ticker data for", symbol, "...\n")
  
  params <- list(symbol = symbol)
  result <- bitget_request("/api/mix/v1/market/ticker", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    # Die Daten sind eine Liste, nicht ein data.frame!
    data_list <- result$data
    
    ticker_df <- data.frame(
      symbol = data_list$symbol,
      last_price = as.numeric(data_list$last),
      mark_price = as.numeric(data_list$indexPrice),
      best_bid = as.numeric(data_list$bestBid),
      best_ask = as.numeric(data_list$bestAsk),
      high_24h = as.numeric(data_list$high24h),
      low_24h = as.numeric(data_list$low24h),
      volume_24h = as.numeric(data_list$baseVolume),
      volume_24h_usdt = as.numeric(data_list$quoteVolume),
      change_24h_pct = as.numeric(data_list$chgUtc),
      funding_rate = as.numeric(data_list$fundingRate),
      open_interest = as.numeric(data_list$holdingAmount),
      timestamp = as.POSIXct(as.numeric(data_list$timestamp)/1000, origin="1970-01-01"),
      stringsAsFactors = FALSE
    )
    
    cat("✅ Ticker data retrieved - Price:", ticker_df$last_price, "USDT\n")
    return(ticker_df)
  } else {
    cat("❌ Failed to fetch ticker data\n")
    return(NULL)
  }
}



# 2. Teste den neuen Ticker
ticker <- get_ticker_data('ADAUSDT_UMCBL')
print(ticker)


# 4. Oder schaue dir sofort deine ADA-Marktdaten an:
ticker <- get_ticker_data('ADAUSDT_UMCBL')

cat("🎯 ADA LIVE MARKET DATA\n")
cat("=======================\n")
cat("💰 Price:", ticker$last_price, "USDT\n")
cat("📈 24h Change:", ticker$change_24h_pct, "%\n") 
cat("📊 24h High:", ticker$high_24h, "USDT\n")
cat("📉 24h Low:", ticker$low_24h, "USDT\n")
cat("💸 24h Volume:", round(ticker$volume_24h_usdt/1000000, 2), "M USDT\n")
cat("💰 Funding Rate:", ticker$funding_rate, "\n")
cat("🔢 Open Interest:", round(ticker$open_interest/1000000, 2), "M ADA\n")


# 3. Jetzt probiere die schnelle Analyse wieder
data <- quick_analysis('ADAUSDT_UMCBL')
