# ==========================================================================================================
# 🚀 BITGET API & MARKET DATA ENGINE V3
# ==========================================================================================================
# Pfad: C:\freeding\tbot202506\r_analysis\riskassetm\core\api_engine.r
# Konsolidiert aus: bitget_core_engine.r + bitget_api_fixes.r + enhanced functions
# ==========================================================================================================

cat("🚀 Loading Bitget API & Market Data Engine V3...\n")

# Load required libraries
required_libs <- c("httr", "jsonlite", "openssl", "TTR", "dplyr")
for (lib in required_libs) {
  if (!require(lib, quietly = TRUE, character.only = TRUE)) {
    install.packages(lib, quiet = TRUE)
    library(lib, character.only = TRUE)
  }
}

# ==========================================================================================================
# 📡 CORE API REQUEST FUNCTION
# ==========================================================================================================

#' Enhanced Bitget API request with retry logic
bitget_request <- function(endpoint, method = "GET", params = NULL, retry_count = 0) {
  
  if (is.null(API_CREDENTIALS)) {
    cat("❌ API credentials not available\n")
    return(NULL)
  }
  
  tryCatch({
    # Generate timestamp
    timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
    
    # Build query string for GET
    query_string <- ""
    if (!is.null(params) && toupper(method) == "GET") {
      query_params <- paste(names(params), params, sep = "=", collapse = "&")
      query_string <- paste0("?", query_params)
    }
    
    # Build body for POST
    body_json <- ""
    if (toupper(method) == "POST" && !is.null(params)) {
      body_json <- toJSON(params, auto_unbox = TRUE)
    }
    
    # Create signature
    prehash <- paste0(timestamp, toupper(method), endpoint, query_string)
    signature_raw <- openssl::sha256(
      charToRaw(paste0(prehash, body_json)), 
      key = charToRaw(API_CREDENTIALS$api_secret)
    )
    signature <- openssl::base64_encode(signature_raw)
    
    # Headers
    headers <- c(
      "ACCESS-KEY" = API_CREDENTIALS$api_key,
      "ACCESS-SIGN" = signature,
      "ACCESS-TIMESTAMP" = timestamp,
      "ACCESS-PASSPHRASE" = API_CREDENTIALS$passphrase,
      "Content-Type" = "application/json"
    )
    
    # Execute request
    url <- paste0(API_CONFIG$base_url, endpoint)
    
    if (toupper(method) == "GET") {
      response <- GET(url, add_headers(.headers = headers), 
                      query = params, timeout(API_CONFIG$timeout_seconds))
    } else {
      response <- VERB(method, url, add_headers(.headers = headers),
                       body = body_json, encode = "json", 
                       timeout(API_CONFIG$timeout_seconds))
    }
    
    # Handle errors with retry
    if (http_error(response)) {
      error_msg <- sprintf("HTTP %s: %s", status_code(response), content(response, "text"))
      
      if (status_code(response) %in% c(429, 500, 502, 503) && retry_count < API_CONFIG$max_retries) {
        cat("⚠️ API error", status_code(response), "- Retrying...\n")
        Sys.sleep(API_CONFIG$rate_limit_delay)
        return(bitget_request(endpoint, method, params, retry_count + 1))
      }
      
      stop(error_msg)
    }
    
    # Parse response
    result <- fromJSON(content(response, "text"), flatten = TRUE)
    return(result)
    
  }, error = function(e) {
    if (DISPLAY_CONFIG$show_api_calls) {
      cat("❌ API Request Error:", e$message, "\n")
    }
    return(NULL)
  })
}

# ==========================================================================================================
# 📊 MARKET DATA FUNCTIONS
# ==========================================================================================================

#' Get enhanced ticker data with all fixes applied
get_enhanced_ticker_data <- function(symbol) {
  
  if (DISPLAY_CONFIG$show_debug) {
    cat("📈 Fetching ticker for", symbol, "...\n")
  }
  
  tryCatch({
    # Use v2 API endpoint
    result <- bitget_request(
      endpoint = API_CONFIG$endpoints$ticker,
      params = list(
        symbol = symbol,
        productType = "USDT-FUTURES"
      )
    )
    
    if (is.null(result) || result$code != "00000") {
      return(create_fallback_ticker(symbol))
    }
    
    data <- result$data
    
    # Parse with correct field names (from fixes)
    ticker_data <- list(
      symbol = symbol,
      asset_name = get_asset_config(symbol)$name,
      
      # Price data - handle multiple possible field names
      last_price = as.numeric(data$lastPr %||% data$last %||% data$markPrice %||% 0),
      mark_price = as.numeric(data$markPrice %||% data$indexPrice %||% 0),
      
      # 24h data
      high_24h = as.numeric(data$high24h %||% 0),
      low_24h = as.numeric(data$low24h %||% 0),
      change_24h_pct = as.numeric(data$change24h %||% data$chgUtc %||% 0) * 100,
      
      # Volume
      volume_24h = as.numeric(data$baseVolume %||% data$volume %||% 0),
      volume_24h_usdt = as.numeric(data$quoteVolume %||% data$volCcy24h %||% 0),
      
      # Orderbook
      best_bid = as.numeric(data$bidPr %||% data$bestBid %||% 0),
      best_ask = as.numeric(data$askPr %||% data$bestAsk %||% 0),
      
      # Futures specific
      funding_rate = as.numeric(data$fundingRate %||% 0),
      open_interest = as.numeric(data$holdingAmount %||% data$openInterest %||% 0),
      
      # Metadata
      timestamp = Sys.time(),
      data_source = "live"
    )
    
    # Calculate spread
    if (ticker_data$best_bid > 0 && ticker_data$best_ask > 0) {
      ticker_data$spread = ticker_data$best_ask - ticker_data$best_bid
      ticker_data$spread_pct = (ticker_data$spread / ticker_data$best_bid) * 100
    } else {
      ticker_data$spread = 0
      ticker_data$spread_pct = 0
    }
    
    return(ticker_data)
    
  }, error = function(e) {
    cat("❌ Error fetching ticker:", e$message, "\n")
    return(create_fallback_ticker(symbol))
  })
}

#' Create fallback ticker data
create_fallback_ticker <- function(symbol) {
  config <- get_asset_config(symbol)
  
  # Realistic fallback prices
  fallback_prices <- list(
    "ADAUSDT_UMCBL" = 0.8500,
    "ALGOUSDT_UMCBL" = 0.2800,
    "ICPUSDT_UMCBL" = 7.500,
    "ETCUSDT_UMCBL" = 28.00,
    "VETUSDT_UMCBL" = 0.0350
  )
  
  base_price <- fallback_prices[[symbol]] %||% 1.0
  
  return(list(
    symbol = symbol,
    asset_name = config$name,
    last_price = base_price,
    mark_price = base_price,
    best_bid = base_price * 0.9995,
    best_ask = base_price * 1.0005,
    high_24h = base_price * 1.02,
    low_24h = base_price * 0.98,
    change_24h_pct = runif(1, -2, 2),
    volume_24h = 1000000,
    volume_24h_usdt = base_price * 1000000,
    spread = base_price * 0.001,
    spread_pct = 0.1,
    funding_rate = 0.0001,
    open_interest = 5000000,
    timestamp = Sys.time(),
    data_source = "fallback"
  ))
}

#' Get enhanced orderbook data
get_enhanced_orderbook <- function(symbol, depth = 20) {
  
  tryCatch({
    result <- bitget_request(
      endpoint = API_CONFIG$endpoints$orderbook,
      params = list(
        symbol = symbol,
        limit = as.character(depth)
      )
    )
    
    if (is.null(result) || result$code != "00000") {
      return(NULL)
    }
    
    # Parse orderbook
    bids <- result$data$bids
    asks <- result$data$asks
    
    if (is.null(bids) || is.null(asks)) {
      return(NULL)
    }
    
    # Calculate metrics
    best_bid <- as.numeric(bids[1,1])
    best_ask <- as.numeric(asks[1,1])
    bid_volume <- sum(as.numeric(bids[,2]))
    ask_volume <- sum(as.numeric(asks[,2]))
    
    orderbook <- list(
      symbol = symbol,
      best_bid = best_bid,
      best_ask = best_ask,
      spread = best_ask - best_bid,
      spread_pct = ((best_ask - best_bid) / best_bid) * 100,
      mid_price = (best_ask + best_bid) / 2,
      bid_volume_total = bid_volume,
      ask_volume_total = ask_volume,
      bid_ask_ratio = bid_volume / max(ask_volume, 1),
      depth = depth,
      timestamp = Sys.time()
    )
    
    # Add depth analysis
    orderbook$bid_depth <- analyze_order_depth(bids)
    orderbook$ask_depth <- analyze_order_depth(asks)
    
    return(orderbook)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Analyze order depth
analyze_order_depth <- function(orders) {
  if (is.null(orders) || nrow(orders) < 2) return(list())
  
  prices <- as.numeric(orders[,1])
  volumes <- as.numeric(orders[,2])
  
  list(
    total_volume = sum(volumes),
    avg_price = weighted.mean(prices, volumes),
    price_range = max(prices) - min(prices),
    volume_concentration = sum(volumes[1:min(5, length(volumes))]) / sum(volumes)
  )
}

#' Get recent trades
get_recent_trades <- function(symbol, limit = 50) {
  
  tryCatch({
    result <- bitget_request(
      endpoint = API_CONFIG$endpoints$trades,
      params = list(
        symbol = symbol,
        limit = as.character(limit)
      )
    )
    
    if (is.null(result) || result$code != "00000") {
      return(NULL)
    }
    
    trades <- result$data
    if (is.null(trades) || length(trades) == 0) {
      return(NULL)
    }
    
    # Parse trades
    trades_df <- do.call(rbind, lapply(trades, function(trade) {
      data.frame(
        price = as.numeric(trade$price %||% 0),
        size = as.numeric(trade$size %||% 0),
        side = trade$side %||% "unknown",
        timestamp = as.POSIXct(as.numeric(trade$ts %||% 0) / 1000, origin = "1970-01-01"),
        stringsAsFactors = FALSE
      )
    }))
    
    # Calculate summary
    buy_volume <- sum(trades_df$size[trades_df$side == "buy"])
    sell_volume <- sum(trades_df$size[trades_df$side == "sell"])
    
    trades_summary <- list(
      symbol = symbol,
      total_trades = nrow(trades_df),
      buy_volume = buy_volume,
      sell_volume = sell_volume,
      buy_sell_ratio = buy_volume / max(sell_volume, 1),
      avg_price = mean(trades_df$price),
      vwap = sum(trades_df$price * trades_df$size) / sum(trades_df$size),
      trades_data = trades_df
    )
    
    return(trades_summary)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Get kline/candle data
get_kline_data <- function(symbol, granularity = "5m", limit = 100) {
  
  tryCatch({
    result <- bitget_request(
      endpoint = API_CONFIG$endpoints$candles,
      params = list(
        symbol = symbol,
        granularity = granularity,
        limit = as.character(limit)
      )
    )
    
    if (is.null(result) || result$code != "00000") {
      return(NULL)
    }
    
    klines <- result$data
    if (is.null(klines) || length(klines) == 0) {
      return(NULL)
    }
    
    # Parse kline data
    klines_df <- do.call(rbind, lapply(klines, function(k) {
      data.frame(
        timestamp = as.POSIXct(as.numeric(k[[1]]) / 1000, origin = "1970-01-01"),
        open = as.numeric(k[[2]]),
        high = as.numeric(k[[3]]),
        low = as.numeric(k[[4]]),
        close = as.numeric(k[[5]]),
        volume = as.numeric(k[[6]]),
        stringsAsFactors = FALSE
      )
    }))
    
    # Sort by timestamp
    klines_df <- klines_df[order(klines_df$timestamp), ]
    
    return(klines_df)
    
  }, error = function(e) {
    return(NULL)
  })
}

# ==========================================================================================================
# 📊 POSITION & ORDER FUNCTIONS (WITH FIXES)
# ==========================================================================================================

#' Get current positions with all fixes applied
get_current_positions <- function() {
  
  tryCatch({
    response <- bitget_request(
      endpoint = API_CONFIG$endpoints$positions,
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(response) || is.null(response$data)) {
      return(data.frame())
    }
    
    data <- response$data
    
    # Handle both data.frame and list responses
    if (is.data.frame(data)) {
      # Direct data.frame response
      positions_df <- data
    } else if (is.list(data)) {
      # List of positions
      if (length(data) == 0) return(data.frame())
      
      positions_df <- do.call(rbind, lapply(data, function(pos) {
        data.frame(
          symbol = pos$symbol %||% "UNKNOWN",
          side = pos$holdSide %||% pos$side %||% "unknown",
          size = as.numeric(pos$total %||% pos$size %||% 0),
          available = as.numeric(pos$available %||% 0),
          avg_price = as.numeric(pos$openPriceAvg %||% pos$averageOpenPrice %||% 0),
          mark_price = as.numeric(pos$markPrice %||% 0),
          unrealized_pnl = as.numeric(pos$unrealizedPL %||% 0),
          leverage = as.numeric(pos$leverage %||% 1),
          margin = as.numeric(pos$marginSize %||% pos$im %||% 0),
          stringsAsFactors = FALSE
        )
      }))
    } else {
      return(data.frame())
    }
    
    # Filter active positions (size > 0)
    active_positions <- positions_df[positions_df$size > 0, ]
    
    # Add calculated fields
    if (nrow(active_positions) > 0) {
      active_positions$pnl_ratio <- ifelse(
        active_positions$size > 0,
        active_positions$unrealized_pnl / (active_positions$size * active_positions$avg_price),
        0
      )
      
      # Risk score calculation
      active_positions$risk_score <- calculate_position_risk_score(active_positions)
    }
    
    return(active_positions)
    
  }, error = function(e) {
    cat("❌ Error fetching positions:", e$message, "\n")
    return(data.frame())
  })
}

#' Calculate position risk score
calculate_position_risk_score <- function(positions) {
  sapply(1:nrow(positions), function(i) {
    pos <- positions[i, ]
    
    # Risk factors
    pnl_risk <- abs(pos$pnl_ratio)
    leverage_risk <- pos$leverage / 50
    size_risk <- (pos$size * pos$mark_price) / 10000  # Normalize by 10k USDT
    
    # Combined risk score
    risk_score <- (pnl_risk * 0.4 + leverage_risk * 0.3 + size_risk * 0.3)
    
    return(min(max(risk_score, 0), 1))
  })
}

#' Get open orders
get_open_orders <- function() {
  
  tryCatch({
    response <- bitget_request(
      endpoint = API_CONFIG$endpoints$orders_pending,
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(response) || is.null(response$data)) {
      return(data.frame())
    }
    
    orders <- response$data
    if (length(orders) == 0) return(data.frame())
    
    # Parse orders
    orders_df <- do.call(rbind, lapply(orders, function(order) {
      data.frame(
        order_id = order$orderId %||% "unknown",
        symbol = order$symbol %||% "unknown",
        side = order$side %||% "unknown",
        size = as.numeric(order$size %||% 0),
        filled_size = as.numeric(order$filledSize %||% 0),
        price = as.numeric(order$price %||% 0),
        order_type = order$orderType %||% "unknown",
        status = order$status %||% "unknown",
        created_time = as.POSIXct(as.numeric(order$cTime %||% 0) / 1000, origin = "1970-01-01"),
        stringsAsFactors = FALSE
      )
    }))
    
    return(orders_df)
    
  }, error = function(e) {
    cat("❌ Error fetching orders:", e$message, "\n")
    return(data.frame())
  })
}

#' Get account balance
get_account_balance <- function() {
  
  tryCatch({
    response <- bitget_request(
      endpoint = API_CONFIG$endpoints$accounts,
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(response) || is.null(response$data)) {
      return(NULL)
    }
    
    # Find USDT account
    for (account in response$data) {
      if (account$marginCoin == "USDT") {
        return(list(
          available = as.numeric(account$available %||% 0),
          equity = as.numeric(account$equity %||% 0),
          locked = as.numeric(account$locked %||% 0),
          unrealized_pnl = as.numeric(account$unrealizedPL %||% 0),
          margin_ratio = as.numeric(account$marginRatio %||% 0)
        ))
      }
    }
    
    return(NULL)
    
  }, error = function(e) {
    cat("❌ Error fetching balance:", e$message, "\n")
    return(NULL)
  })
}

# ==========================================================================================================
# 🔧 UTILITY FUNCTIONS
# ==========================================================================================================

#' Test API connectivity
test_api_connection <- function() {
  cat("\n🔍 Testing API connection...\n")
  
  # Test public endpoint
  test_public <- tryCatch({
    response <- GET(paste0(API_CONFIG$base_url, "/api/v2/public/time"))
    if (status_code(response) == 200) {
      cat("✅ Public API: Connected\n")
      TRUE
    } else {
      cat("❌ Public API: Failed\n")
      FALSE
    }
  }, error = function(e) {
    cat("❌ Public API: Error -", e$message, "\n")
    FALSE
  })
  
  # Test authenticated endpoint
  if (!is.null(API_CREDENTIALS)) {
    test_auth <- tryCatch({
      balance <- get_account_balance()
      if (!is.null(balance)) {
        cat("✅ Authenticated API: Connected\n")
        cat("💰 Available Balance:", round(balance$available, 2), "USDT\n")
        TRUE
      } else {
        cat("❌ Authenticated API: Failed\n")
        FALSE
      }
    }, error = function(e) {
      cat("❌ Authenticated API: Error -", e$message, "\n")
      FALSE
    })
  } else {
    cat("⚠️ API credentials not loaded\n")
    test_auth <- FALSE
  }
  
  return(test_public && test_auth)
}

#' Multi-symbol data fetcher
fetch_multi_symbol_data <- function(symbols = PORTFOLIO_ASSETS, data_type = "ticker") {
  
  results <- list()
  
  for (symbol in symbols) {
    cat("📡 Fetching", data_type, "for", symbol, "...")
    
    result <- switch(data_type,
                     "ticker" = get_enhanced_ticker_data(symbol),
                     "orderbook" = get_enhanced_orderbook(symbol),
                     "trades" = get_recent_trades(symbol),
                     NULL
    )
    
    if (!is.null(result)) {
      results[[symbol]] <- result
      cat(" ✅\n")
    } else {
      cat(" ❌\n")
    }
    
    Sys.sleep(API_CONFIG$rate_limit_delay)
  }
  
  return(results)
}

cat("✅ API_ENGINE.R loaded successfully!\n")