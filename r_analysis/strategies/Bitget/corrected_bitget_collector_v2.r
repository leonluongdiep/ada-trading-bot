# =====================================================
# 🔧 KORRIGIERTE BITGET INDIKATOREN INTEGRATION
# =====================================================
# Füge diese Funktionen zu Ihrem complete_trading_analysis_v3.r hinzu

# =====================================================
# 📊 KORRIGIERTE TICKER FUNKTION (funktioniert bereits)
# =====================================================

get_enhanced_ticker_data <- function(symbol = DEFAULT_SYMBOL) {
  cat("📈 Fetching enhanced ticker data for", symbol, "...\n")
  
  params <- list(symbol = symbol)
  result <- bitget_request("/api/mix/v1/market/ticker", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
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
    
    cat("✅ Enhanced ticker data retrieved - Price:", ticker_df$last_price, "USDT\n")
    return(ticker_df)
  } else {
    cat("❌ Failed to fetch enhanced ticker data\n")
    return(NULL)
  }
}

# =====================================================
# 📚 KORRIGIERTE ORDERBOOK FUNKTION
# =====================================================

get_enhanced_orderbook <- function(symbol = DEFAULT_SYMBOL, limit = 20) {
  cat("📚 Fetching enhanced orderbook for", symbol, "...\n")
  
  params <- list(symbol = symbol, limit = as.character(limit))
  result <- bitget_request("/api/mix/v1/market/depth", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    tryCatch({
      # Sicherheitscheck für Bids und Asks
      if (is.null(result$data$bids) || is.null(result$data$asks) || 
          length(result$data$bids) == 0 || length(result$data$asks) == 0) {
        cat("⚠️ Empty orderbook data\n")
        return(NULL)
      }
      
      # Bids verarbeiten
      bids_matrix <- result$data$bids
      if (is.matrix(bids_matrix) && ncol(bids_matrix) >= 2) {
        best_bid <- as.numeric(bids_matrix[1,1])
        bid_volume_total <- sum(as.numeric(bids_matrix[,2]))
      } else {
        best_bid <- NA
        bid_volume_total <- 0
      }
      
      # Asks verarbeiten  
      asks_matrix <- result$data$asks
      if (is.matrix(asks_matrix) && ncol(asks_matrix) >= 2) {
        best_ask <- as.numeric(asks_matrix[1,1])
        ask_volume_total <- sum(as.numeric(asks_matrix[,2]))
      } else {
        best_ask <- NA
        ask_volume_total <- 0
      }
      
      # Orderbook Summary
      if (!is.na(best_bid) && !is.na(best_ask)) {
        spread <- best_ask - best_bid
        mid_price <- (best_ask + best_bid) / 2
        spread_pct <- (spread / mid_price) * 100
      } else {
        spread <- NA
        mid_price <- NA
        spread_pct <- NA
      }
      
      orderbook_summary <- data.frame(
        symbol = symbol,
        best_bid = best_bid,
        best_ask = best_ask,
        spread = spread,
        spread_pct = spread_pct,
        mid_price = mid_price,
        bid_volume_total = bid_volume_total,
        ask_volume_total = ask_volume_total,
        bid_ask_ratio = if(!is.na(ask_volume_total) && ask_volume_total > 0) bid_volume_total / ask_volume_total else NA,
        timestamp = Sys.time(),
        stringsAsFactors = FALSE
      )
      
      cat("✅ Enhanced orderbook retrieved - Spread:", round(spread_pct, 4), "%\n")
      return(orderbook_summary)
      
    }, error = function(e) {
      cat("❌ Error processing orderbook data:", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("❌ Failed to fetch orderbook data\n")
    return(NULL)
  }
}

# =====================================================
# 🔄 KORRIGIERTE RECENT TRADES FUNKTION
# =====================================================

get_enhanced_trades <- function(symbol = DEFAULT_SYMBOL, limit = 50) {
  cat("🔄 Fetching enhanced trades for", symbol, "...\n")
  
  params <- list(symbol = symbol, limit = as.character(limit))
  result <- bitget_request("/api/mix/v1/market/fills", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    tryCatch({
      # Detaillierte Struktur-Analyse für Debugging
      if (is.null(result$data)) {
        cat("⚠️ No trade data available (NULL)\n")
        return(create_empty_trades_summary(symbol))
      }
      
      # Prüfe Datentyp und Struktur
      if (is.atomic(result$data) || length(result$data) == 0) {
        cat("⚠️ Trade data is atomic or empty, length:", length(result$data), "\n")
        return(create_empty_trades_summary(symbol))
      }
      
      # Wenn data eine Matrix ist (typisch für manche Bitget endpoints)
      if (is.matrix(result$data)) {
        if (ncol(result$data) >= 4) {
          trades_data <- data.frame(
            price = as.numeric(result$data[,2]),  # Preis meist in Spalte 2
            size = as.numeric(result$data[,3]),   # Größe meist in Spalte 3
            side = ifelse(as.character(result$data[,4]) == "sell", "sell", "buy"),
            timestamp = as.POSIXct(as.numeric(result$data[,1])/1000, origin="1970-01-01"),
            stringsAsFactors = FALSE
          )
        } else {
          cat("⚠️ Trade matrix has insufficient columns:", ncol(result$data), "\n")
          return(create_empty_trades_summary(symbol))
        }
      }
      # Wenn data eine Liste von Listen ist
      else if (is.list(result$data) && length(result$data) > 0 && is.list(result$data[[1]])) {
        trades_data <- do.call(rbind, lapply(result$data, function(trade) {
          data.frame(
            price = as.numeric(trade$price %||% 0),
            size = as.numeric(trade$size %||% 0),
            side = as.character(trade$side %||% "unknown"),
            timestamp = as.POSIXct(as.numeric(trade$ts %||% Sys.time()*1000)/1000, origin="1970-01-01"),
            stringsAsFactors = FALSE
          )
        }))
      }
      # Fallback: Erstelle leeres Summary
      else {
        cat("⚠️ Unhandled trade data structure, type:", class(result$data), "\n")
        return(create_empty_trades_summary(symbol))
      }
      
      # Trade-Analyse erstellen
      if (!is.null(trades_data) && nrow(trades_data) > 0) {
        trades_summary <- data.frame(
          symbol = symbol,
          total_trades = nrow(trades_data),
          avg_price = mean(trades_data$price, na.rm = TRUE),
          volume_weighted_price = sum(trades_data$price * trades_data$size, na.rm = TRUE) / sum(trades_data$size, na.rm = TRUE),
          total_volume = sum(trades_data$size, na.rm = TRUE),
          buy_volume = sum(trades_data$size[trades_data$side == "buy"], na.rm = TRUE),
          sell_volume = sum(trades_data$size[trades_data$side == "sell"], na.rm = TRUE),
          buy_sell_ratio = sum(trades_data$size[trades_data$side == "buy"], na.rm = TRUE) / 
                          max(sum(trades_data$size[trades_data$side == "sell"], na.rm = TRUE), 1),
          price_range = max(trades_data$price, na.rm = TRUE) - min(trades_data$price, na.rm = TRUE),
          timestamp = Sys.time(),
          stringsAsFactors = FALSE
        )
      } else {
        trades_summary <- create_empty_trades_summary(symbol)
      }
      
      cat("✅ Enhanced trades retrieved:", trades_summary$total_trades, "trades\n")
      return(trades_summary)
      
    }, error = function(e) {
      cat("❌ Error processing trades data:", e$message, "\n")
      cat("📊 Returning empty trades summary instead\n")
      return(create_empty_trades_summary(symbol))
    })
  } else {
    cat("❌ Failed to fetch trades data\n")
    return(create_empty_trades_summary(symbol))
  }
}

# =====================================================
# 💰 ALTERNATIVE FUNDING RATE (von Ticker)
# =====================================================

get_funding_from_ticker <- function(symbol = DEFAULT_SYMBOL) {
  cat("💰 Getting funding rate from ticker for", symbol, "...\n")
  
  ticker <- get_enhanced_ticker_data(symbol)
  
  if (!is.null(ticker) && !is.na(ticker$funding_rate)) {
    funding_df <- data.frame(
      symbol = symbol,
      funding_rate = ticker$funding_rate,
      funding_rate_pct = ticker$funding_rate * 100,
      timestamp = ticker$timestamp,
      stringsAsFactors = FALSE
    )
    
    cat("✅ Funding rate from ticker:", funding_df$funding_rate_pct, "%\n")
    return(funding_df)
  } else {
    cat("❌ No funding rate available in ticker\n")
    return(NULL)
  }
}

# =====================================================
# 🧮 MARKET SENTIMENT CALCULATOR
# =====================================================

calculate_market_sentiment <- function(ticker_data, orderbook_data, trades_data) {
  cat("🧮 Calculating market sentiment...\n")
  
  sentiment_score <- 0
  sentiment_factors <- list()
  
  # 1. Price Change Sentiment
  if (!is.null(ticker_data) && !is.na(ticker_data$change_24h_pct)) {
    price_sentiment <- ifelse(ticker_data$change_24h_pct > 0, 1, -1)
    sentiment_score <- sentiment_score + price_sentiment
    sentiment_factors$price_change <- price_sentiment
  }
  
  # 2. Volume Analysis  
  if (!is.null(ticker_data) && !is.na(ticker_data$volume_24h_usdt)) {
    # Hohe Volume = mehr Interesse (positiv)
    volume_millions <- ticker_data$volume_24h_usdt / 1000000
    volume_sentiment <- ifelse(volume_millions > 50, 1, 0)  # > 50M USDT = bullish
    sentiment_score <- sentiment_score + volume_sentiment
    sentiment_factors$volume <- volume_sentiment
  }
  
  # 3. Orderbook Sentiment (Bid/Ask Ratio)
  if (!is.null(orderbook_data) && !is.na(orderbook_data$bid_ask_ratio)) {
    orderbook_sentiment <- ifelse(orderbook_data$bid_ask_ratio > 1, 1, -1)
    sentiment_score <- sentiment_score + orderbook_sentiment  
    sentiment_factors$orderbook <- orderbook_sentiment
  }
  
  # 4. Recent Trades Sentiment (Buy/Sell Ratio)
  if (!is.null(trades_data) && !is.na(trades_data$buy_sell_ratio)) {
    trades_sentiment <- ifelse(trades_data$buy_sell_ratio > 1, 1, -1)
    sentiment_score <- sentiment_score + trades_sentiment
    sentiment_factors$trades <- trades_sentiment
  }
  
  # 5. Funding Rate Sentiment
  if (!is.null(ticker_data) && !is.na(ticker_data$funding_rate)) {
    # Positive Funding = Longs zahlen Shorts = bullish pressure
    funding_sentiment <- ifelse(ticker_data$funding_rate > 0, 1, -1)
    sentiment_score <- sentiment_score + funding_sentiment
    sentiment_factors$funding <- funding_sentiment
  }
  
  # Gesamtbewertung
  max_factors <- length(sentiment_factors)
  if (max_factors > 0) {
    sentiment_percentage <- (sentiment_score / max_factors) * 100
    
    overall_sentiment <- if (sentiment_percentage > 40) {
      "BULLISH"
    } else if (sentiment_percentage < -40) {
      "BEARISH" 
    } else {
      "NEUTRAL"
    }
  } else {
    sentiment_percentage <- 0
    overall_sentiment <- "UNKNOWN"
  }
  
  sentiment_result <- list(
    overall_sentiment = overall_sentiment,
    sentiment_score = sentiment_score,
    sentiment_percentage = sentiment_percentage,
    factors = sentiment_factors,
    max_factors = max_factors,
    timestamp = Sys.time()
  )
  
  cat("✅ Market sentiment calculated:", overall_sentiment, "(", round(sentiment_percentage), "%)\n")
  return(sentiment_result)
}

# =====================================================
# 🚀 ENHANCED MARKET ANALYSIS - Integration in Ihr Script
# =====================================================

get_enhanced_market_data <- function(symbol = DEFAULT_SYMBOL) {
  cat("🚀 ENHANCED MARKET DATA COLLECTION\n")
  cat("===================================\n")
  
  market_data <- list()
  
  # 1. Enhanced Ticker (funktioniert bereits)
  market_data$ticker <- get_enhanced_ticker_data(symbol)
  
  # 2. Enhanced Orderbook
  market_data$orderbook <- get_enhanced_orderbook(symbol, limit = 20)
  
  # 3. Enhanced Trades
  market_data$trades <- get_enhanced_trades(symbol, limit = 50)
  
  # 4. Funding Rate (von Ticker)
  market_data$funding <- get_funding_from_ticker(symbol)
  
  # 5. Market Sentiment Berechnung
  market_data$sentiment <- calculate_market_sentiment(
    market_data$ticker,
    market_data$orderbook, 
    market_data$trades
  )
  
  # 6. Summary erstellen
  market_data$summary <- create_enhanced_summary(market_data)
  
  cat("✅ Enhanced market data collection completed!\n")
  return(market_data)
}

# =====================================================
# 📋 ENHANCED SUMMARY
# =====================================================

create_enhanced_summary <- function(market_data) {
  summary_list <- list(
    timestamp = Sys.time(),
    symbol = DEFAULT_SYMBOL
  )
  
  # Basis-Marktdaten
  if (!is.null(market_data$ticker)) {
    summary_list$price_info <- list(
      current_price = market_data$ticker$last_price,
      change_24h_pct = market_data$ticker$change_24h_pct,
      volume_24h_usdt = market_data$ticker$volume_24h_usdt,
      high_24h = market_data$ticker$high_24h,
      low_24h = market_data$ticker$low_24h,
      funding_rate_pct = market_data$ticker$funding_rate * 100,
      open_interest = market_data$ticker$open_interest
    )
  }
  
  # Orderbook Info
  if (!is.null(market_data$orderbook)) {
    summary_list$orderbook_info <- list(
      spread_pct = market_data$orderbook$spread_pct,
      bid_ask_ratio = market_data$orderbook$bid_ask_ratio
    )
  }
  
  # Trades Info
  if (!is.null(market_data$trades)) {
    summary_list$trades_info <- list(
      total_trades = market_data$trades$total_trades,
      buy_sell_ratio = market_data$trades$buy_sell_ratio
    )
  }
  
  # Sentiment
  if (!is.null(market_data$sentiment)) {
    summary_list$sentiment_info <- list(
      overall = market_data$sentiment$overall_sentiment,
      score_pct = market_data$sentiment$sentiment_percentage,
      factors_count = market_data$sentiment$max_factors
    )
  }
  
  return(summary_list)
}

# =====================================================
# 🔄 NULL-SAFE OPERATOR & HELPER FUNCTIONS
# =====================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# Hilfsfunktion für leeres Trades Summary
create_empty_trades_summary <- function(symbol) {
  return(data.frame(
    symbol = symbol,
    total_trades = 0,
    avg_price = NA,
    volume_weighted_price = NA,
    total_volume = 0,
    buy_volume = 0,
    sell_volume = 0,
    buy_sell_ratio = NA,
    price_range = 0,
    timestamp = Sys.time(),
    stringsAsFactors = FALSE
  ))
}

# =====================================================
# 🎯 INTEGRATION IN IHR COMPLETE_TRADING_ANALYSIS
# =====================================================

# Fügen Sie diese Funktion zu Ihrer complete_trading_analysis hinzu:
complete_trading_analysis_enhanced <- function(symbol = DEFAULT_SYMBOL) {
  cat("🚀 COMPLETE ENHANCED TRADING ANALYSIS\n")
  cat("=====================================\n")
  
  # 1. Ihre ursprüngliche technische Analyse (nur symbol Parameter)
  base_analysis <- complete_trading_analysis(symbol)
  
  if (is.null(base_analysis)) {
    cat("❌ Base analysis failed\n")
    return(NULL)
  }
  
  # 2. Enhanced Market Data hinzufügen
  enhanced_data <- get_enhanced_market_data(symbol)
  
  # 3. Kombinierte Analyse
  enhanced_analysis <- base_analysis
  enhanced_analysis$enhanced_market_data <- enhanced_data
  
  # 4. Enhanced Summary anzeigen
  if (!is.null(enhanced_data$summary)) {
    cat("\n🔥 ENHANCED MARKET SUMMARY:\n")
    
    if (!is.null(enhanced_data$summary$price_info)) {
      price_info <- enhanced_data$summary$price_info
      cat("   💰 Price:", price_info$current_price, "USDT (", 
          round(price_info$change_24h_pct, 2), "% 24h)\n")
      cat("   📊 Volume:", round(price_info$volume_24h_usdt/1000000, 1), "M USDT\n")
      cat("   💸 Funding:", round(price_info$funding_rate_pct, 4), "%\n")
    }
    
    if (!is.null(enhanced_data$summary$sentiment_info)) {
      sentiment <- enhanced_data$summary$sentiment_info
      cat("   🎯 Sentiment:", sentiment$overall, "(", round(sentiment$score_pct), "%)\n")
    }
    
    if (!is.null(enhanced_data$summary$orderbook_info)) {
      orderbook <- enhanced_data$summary$orderbook_info
      cat("   📚 Spread:", round(orderbook$spread_pct, 4), "% | Bid/Ask Ratio:", 
          round(orderbook$bid_ask_ratio, 2), "\n")
    }
  }
  
  return(enhanced_analysis)
}

cat("\n🔥 ENHANCED BITGET INTEGRATION READY!\n")
cat("=====================================\n")
cat("📋 NEW ENHANCED FUNCTIONS:\n")
cat("   - get_enhanced_market_data()          # Alle Marktdaten\n") 
cat("   - complete_trading_analysis_enhanced() # Ihre Analyse + Enhanced Data\n")
cat("   - calculate_market_sentiment()         # Market Sentiment Analysis\n")
cat("\n🚀 Usage: analysis <- complete_trading_analysis_enhanced('ADAUSDT_UMCBL')\n")