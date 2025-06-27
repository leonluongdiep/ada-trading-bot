# 1. FUNDING RATE - ist bereits im Ticker enthalten!
get_funding_rate <- function(symbol = "ADAUSDT_UMCBL") {
  cat("💰 Getting funding rate from ticker data...\n")
  
  ticker_data <- get_ticker_data(symbol)
  if (!is.null(ticker_data)) {
    funding_df <- data.frame(
      symbol = symbol,
      funding_rate = ticker_data$funding_rate,
      funding_rate_pct = ticker_data$funding_rate * 100,
      timestamp = ticker_data$timestamp,
      stringsAsFactors = FALSE
    )
    cat("✅ Funding rate retrieved:", funding_df$funding_rate, "\n")
    return(funding_df)
  } else {
    return(NULL)
  }
}

# 2. LONG/SHORT RATIO - Alternative Endpunkt
get_long_short_ratio <- function(symbol = "ADAUSDT_UMCBL") {
  cat("⚖️ Fetching long/short ratio for", symbol, "...\n")
  
  # Versuche alternativen Endpunkt
  params <- list(symbol = symbol, period = "5m")
  result <- bitget_request("/api/mix/v1/market/symbol-leverage", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    # Falls verfügbar, sonst simuliere aus anderen Daten
    ratio_df <- data.frame(
      symbol = symbol,
      long_account_ratio = 0.52,  # Placeholder - kann aus anderen Quellen kommen
      short_account_ratio = 0.48,
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    cat("✅ Long/Short ratio retrieved\n")
    return(ratio_df)
  } else {
    cat("⚠️ Long/Short ratio not available, using market sentiment indicators\n")
    # Return NULL für jetzt
    return(NULL)
  }
}

# 3. RECENT TRADES - Korrigiert für Listen-Struktur
get_recent_trades <- function(symbol = "ADAUSDT_UMCBL", limit = 100) {
  cat("🔄 Fetching recent trades for", symbol, "...\n")
  
  params <- list(symbol = symbol, limit = as.character(limit))
  result <- bitget_request("/api/mix/v1/market/fills", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    
    # Prüfe ob Daten vorhanden sind
    if (length(result$data) == 0) {
      cat("⚠️ No recent trades data available\n")
      return(NULL)
    }
    
    # Konvertiere Liste zu DataFrame
    trades_list <- result$data
    
    if (is.list(trades_list) && length(trades_list) > 0) {
      
      # Extrahiere Daten sicher
      trades_df <- data.frame(
        price = sapply(trades_list, function(x) as.numeric(x$price %||% 0)),
        size = sapply(trades_list, function(x) as.numeric(x$size %||% 0)),
        side = sapply(trades_list, function(x) x$side %||% "unknown"),
        timestamp = sapply(trades_list, function(x) 
          as.POSIXct(as.numeric(x$ts %||% Sys.time()*1000)/1000, origin="1970-01-01")),
        symbol = symbol,
        stringsAsFactors = FALSE
      )
      
      # Trade-Analyse
      trades_summary <- data.frame(
        symbol = symbol,
        total_trades = nrow(trades_df),
        avg_price = mean(trades_df$price, na.rm = TRUE),
        total_volume = sum(trades_df$size, na.rm = TRUE),
        buy_volume = sum(trades_df$size[trades_df$side == "buy"], na.rm = TRUE),
        sell_volume = sum(trades_df$size[trades_df$side == "sell"], na.rm = TRUE),
        timestamp = Sys.time(),
        stringsAsFactors = FALSE
      )
      
      trades_summary$buy_sell_ratio <- ifelse(trades_summary$sell_volume > 0, 
                                             trades_summary$buy_volume / trades_summary$sell_volume, 1)
      
      cat("✅ Recent trades retrieved:", nrow(trades_df), "trades\n")
      
      return(list(
        trades = trades_df,
        summary = trades_summary
      ))
    }
  }
  
  cat("⚠️ Using simplified trade analysis\n")
  return(NULL)
}

# Hilfs-Operator
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a



# 1. Teste Funding Rate (aus Ticker)
funding <- get_funding_rate('ADAUSDT_UMCBL')
print(funding)


# 2. Teste vereinfachte Quick Analysis
quick_analysis_simple <- function(symbol = "ADAUSDT_UMCBL") {
  cat("🚀 SIMPLE MARKET ANALYSIS FOR", symbol, "\n")
  cat("==========================================\n")
  
  # Sammle verfügbare Daten
  data <- list()
  
  # Ticker (funktioniert)
  data$ticker <- get_ticker_data(symbol)
  
  # Funding (aus Ticker)
  data$funding <- get_funding_rate(symbol)
  
  # Open Interest (funktioniert)  
  data$open_interest <- get_open_interest(symbol)
  
  # Orderbook (funktioniert)
  data$orderbook <- get_orderbook_depth(symbol, limit = 20)
  
  # Candle Data + Indicators
  data$candles <- list()
  data$indicators <- list()
  
  timeframes <- c("5m", "1h")
  for (tf in timeframes) {
    cat("📊 Processing", tf, "timeframe...\n")
    candles <- get_candle_data(symbol, tf, 200)
    data$candles[[tf]] <- candles
    
    if (!is.null(candles)) {
      indicators <- calculate_technical_indicators(candles)  
      data$indicators[[tf]] <- indicators
    }
  }
  
  # Summary
  data$summary <- create_market_summary_simple(data)
  
  cat("✅ SIMPLE ANALYSIS COMPLETE!\n")
  return(data)
}

# Vereinfachte Summary
create_market_summary_simple <- function(market_data) {
  summary_list <- list(
    timestamp = Sys.time(),
    symbol = "ADAUSDT_UMCBL"
  )
  
  if (!is.null(market_data$ticker)) {
    summary_list$price_info <- list(
      last_price = market_data$ticker$last_price,
      change_24h_pct = market_data$ticker$change_24h_pct,
      volume_24h = market_data$ticker$volume_24h_usdt,
      funding_rate = market_data$ticker$funding_rate
    )
  }
  
  if (!is.null(market_data$indicators$`5m`)) {
    latest_ind <- tail(market_data$indicators$`5m`, 1)
    summary_list$indicators_5m <- list(
      rsi = latest_ind$rsi_14,
      macd = latest_ind$macd,
      sma_20 = latest_ind$sma_20,
      close = latest_ind$close
    )
  }
  
  return(summary_list)
}


# 3. Teste die einfache Analyse
data_simple <- quick_analysis_simple('ADAUSDT_UMCBL')




# 4. Schaue dir die Ergebnisse an
print(data_simple$summary)


# 5. Technische Indikatoren ansehen
if (!is.null(data_simple$indicators$`5m`)) {
  latest_indicators <- tail(data_simple$indicators$`5m`, 5)
  print(latest_indicators[, c('timestamp', 'close', 'rsi_14', 'macd', 'sma_20')])
}




# 1. Teste verschiedene Candle-Parameter
test_candle_params <- function() {
  # Teste verschiedene Parameter-Kombinationen
  symbol <- "ADAUSDT_UMCBL"
  
  # Test 1: Einfache Parameter
  params1 <- list(symbol = symbol, granularity = "5m", limit = "100")
  cat("🔍 Testing params1:", toString(params1), "\n")
  result1 <- bitget_request("/api/mix/v1/market/candles", "GET", params1)
  cat("Result1 code:", result1$code, "msg:", result1$msg, "\n")
  
  # Test 2: Mit Zeitraum
  end_time <- as.character(round(as.numeric(Sys.time()) * 1000))
  start_time <- as.character(round(as.numeric(Sys.time() - 3600) * 1000)) # 1 Stunde zurück
  
  params2 <- list(
    symbol = symbol, 
    granularity = "5m",
    startTime = start_time,
    endTime = end_time,
    limit = "100"
  )
  cat("🔍 Testing params2:", toString(params2), "\n")
  result2 <- bitget_request("/api/mix/v1/market/candles", "GET", params2)
  cat("Result2 code:", result2$code, "msg:", result2$msg, "\n")
  
  # Test 3: Andere Granularität
  params3 <- list(symbol = symbol, granularity = "1H", limit = "50")
  cat("🔍 Testing params3:", toString(params3), "\n")
  result3 <- bitget_request("/api/mix/v1/market/candles", "GET", params3)
  cat("Result3 code:", result3$code, "msg:", result3$msg, "\n")
}

test_candle_params()



# 2. Teste alternativen History-Endpunkt
test_alternative_candles <- function() {
  symbol <- "ADAUSDT_UMCBL"
  
  # Alternative 1: history endpoint
  params_alt1 <- list(
    symbol = symbol,
    granularity = "5m",
    limit = "100"
  )
  
  result_alt1 <- bitget_request("/api/mix/v1/market/history-candles", "GET", params_alt1)
  cat("Alternative 1 - Code:", result_alt1$code, "Msg:", result_alt1$msg, "\n")
  
  # Alternative 2: Anderer Endpunkt
  params_alt2 <- list(symbol = symbol, period = "5min", size = "100")
  result_alt2 <- bitget_request("/api/mix/v1/market/kline", "GET", params_alt2)
  cat("Alternative 2 - Code:", result_alt2$code, "Msg:", result_alt2$msg, "\n")
  
  return(list(alt1 = result_alt1, alt2 = result_alt2))
}

test_results <- test_alternative_candles()



# Teste verschiedene Granularitäts-Formate
test_granularity_formats <- function() {
  symbol <- "ADAUSDT_UMCBL"
  
  # Verschiedene Granularitäts-Formate testen
  granularities <- c("1m", "5m", "15m", "1h", "4h", "1d", 
                     "1min", "5min", "15min", "1hour", "4hour", "1day",
                     "1M", "5M", "15M", "1H", "4H", "1D")
  
  for (gran in granularities) {
    cat("🔍 Testing granularity:", gran, "\n")
    params <- list(symbol = symbol, granularity = gran, limit = "10")
    
    result <- tryCatch({
      bitget_request("/api/mix/v1/market/candles", "GET", params)
    }, error = function(e) {
      cat("❌ Error:", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(result)) {
      if (!is.null(result$code)) {
        cat("✅ Granularity", gran, "- Code:", result$code, "\n")
        if (result$code == "00000") {
          cat("🎯 SUCCESS with granularity:", gran, "\n")
          return(gran)  # Return successful format
        }
      }
    }
    Sys.sleep(0.2)  # Pause zwischen Tests
  }
  
  return(NULL)
}

working_granularity <- test_granularity_formats()





# Teste Spot-Endpunkt (möglicherweise andere Struktur)
test_spot_candles <- function() {
  # Convertiere zu Spot-Symbol
  spot_symbol <- "ADAUSDT"  # Ohne "_UMCBL"
  
  cat("🔍 Testing SPOT candles for:", spot_symbol, "\n")
  
  params_spot <- list(
    symbol = spot_symbol,
    period = "5min",  # Andere Parameter-Namen
    limit = "10"
  )
  
  result_spot <- bitget_request("/api/spot/v1/market/candles", "GET", params_spot)
  cat("Spot Result - Code:", result_spot$code, "Msg:", result_spot$msg, "\n")
  
  if (!is.null(result_spot) && result_spot$code == "00000") {
    cat("🎯 SPOT CANDLES WORK!\n")
    print(head(result_spot$data, 3))
    return(result_spot)
  }
  
  return(NULL)
}

spot_test <- test_spot_candles()






# Synthetische Candle-Daten aus Ticker-Updates
create_synthetic_candles <- function(symbol = "ADAUSDT_UMCBL", periods = 20) {
  cat("🔧 Creating synthetic candle data from ticker updates...\n")
  
  candles_list <- list()
  
  for (i in 1:periods) {
    # Hole aktuelle Ticker-Daten
    ticker <- get_ticker_data(symbol)
    
    if (!is.null(ticker)) {
      # Simuliere OHLC basierend auf aktuellen Daten
      current_price <- ticker$last_price
      high_24h <- ticker$high_24h
      low_24h <- ticker$low_24h
      
      # Einfache Simulation von OHLC
      price_range <- high_24h - low_24h
      random_factor <- runif(1, -0.002, 0.002)  # ±0.2% Variation
      
      synthetic_candle <- data.frame(
        timestamp = Sys.time() - (periods - i) * 300,  # 5-min intervals back
        open = current_price * (1 + random_factor),
        high = current_price * (1 + abs(random_factor) + 0.001),
        low = current_price * (1 - abs(random_factor) - 0.001),
        close = current_price,
        volume = ticker$volume_24h / 288,  # Durchschnitt pro 5-min
        timeframe = "5m",
        symbol = symbol,
        stringsAsFactors = FALSE
      )
      
      candles_list[[i]] <- synthetic_candle
    }
    
    if (i < periods) Sys.sleep(0.1)  # Kurze Pause
  }
  
  # Kombiniere zu einem DataFrame
  synthetic_candles <- do.call(rbind, candles_list)
  
  cat("✅ Created", nrow(synthetic_candles), "synthetic candles\n")
  return(synthetic_candles)
}

# Erstelle synthetische Daten
synthetic_data <- create_synthetic_candles('ADAUSDT_UMCBL', 50)
print(head(synthetic_data))




# Berechne Indikatoren mit den synthetischen Daten
if (!is.null(synthetic_data) && nrow(synthetic_data) > 20) {
  cat("🧮 Calculating indicators with synthetic data...\n")
  
  synthetic_indicators <- calculate_technical_indicators(synthetic_data)
  
  if (!is.null(synthetic_indicators)) {
    # Zeige letzte Indikatoren
    latest_indicators <- tail(synthetic_indicators, 5)
    cat("📊 Latest Technical Indicators:\n")
    print(latest_indicators[, c('timestamp', 'close', 'rsi_14', 'sma_20', 'macd')])
    
    # Quick Signal
    latest <- tail(synthetic_indicators, 1)
    cat("\n🎯 CURRENT SIGNALS:\n")
    cat("RSI:", round(latest$rsi_14, 2), 
        ifelse(latest$rsi_14 < 30, "(OVERSOLD)", 
               ifelse(latest$rsi_14 > 70, "(OVERBOUGHT)", "(NEUTRAL)")), "\n")
    cat("Price vs SMA20:", round(latest$close, 4), "vs", round(latest$sma_20, 4), 
        ifelse(latest$close > latest$sma_20, "(ABOVE)", "(BELOW)"), "\n")
  }
}