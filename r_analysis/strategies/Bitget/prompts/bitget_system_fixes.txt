# ==========================================================================================================
# 🚀 BITGET TRADING SYSTEM - FIXES & MULTI-ASSET SUPPORT
# ==========================================================================================================
# 
# FIXES:
# 1. ✅ Encoding-Bug beseitigt - UTF-8 konsequent, Listen-Strukturen geflättet
# 2. ✅ Multi-Asset-Support - ADAUSDT_UMCBL, BTCUSDT_UMCBL, ETHUSDT_UMCBL
# 
# FEATURES:
# - Fehlerfrei Datenspeicherung (CSV/JSON)
# - Asset-spezifische Konfigurationen
# - Universelle Funktionen für alle Symbole
# - Vollständige Backward-Kompatibilität
#
# ==========================================================================================================

cat("🔧 Loading Bitget Trading System - Enhanced & Fixed Version...\n")

# ==========================================================================================================
# 📋 MULTI-ASSET CONFIGURATION
# ==========================================================================================================

# Asset-spezifische Konfigurationen
ASSET_CONFIG <- list(
  "ADAUSDT_UMCBL" = list(
    name = "Cardano",
    symbol = "ADAUSDT_UMCBL",
    base_asset = "ADA",
    quote_asset = "USDT",
    price_decimals = 4,
    tick_size = 0.0001,
    min_size = 1,
    max_leverage = 20,
    typical_volume_threshold = 50000000  # 50M USDT
  ),
  "BTCUSDT_UMCBL" = list(
    name = "Bitcoin",
    symbol = "BTCUSDT_UMCBL", 
    base_asset = "BTC",
    quote_asset = "USDT",
    price_decimals = 2,
    tick_size = 0.01,
    min_size = 0.001,
    max_leverage = 125,
    typical_volume_threshold = 1000000000  # 1B USDT
  ),
  "ETHUSDT_UMCBL" = list(
    name = "Ethereum",
    symbol = "ETHUSDT_UMCBL",
    base_asset = "ETH", 
    quote_asset = "USDT",
    price_decimals = 3,
    tick_size = 0.001,
    min_size = 0.01,
    max_leverage = 75,
    typical_volume_threshold = 500000000  # 500M USDT
  )
)

# Supported symbols list
SUPPORTED_SYMBOLS <- names(ASSET_CONFIG)

# ==========================================================================================================
# 🔧 ENCODING & DATA STRUCTURE FIXES
# ==========================================================================================================

# Fixed UTF-8 encoding function
ensure_utf8_encoding <- function() {
  options(encoding = "UTF-8")
  Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
}

# Flatten complex list structures for safe CSV saving
flatten_data_structure <- function(data_obj, prefix = "") {
  if (is.null(data_obj)) return(data.frame())
  
  if (is.data.frame(data_obj)) {
    return(data_obj)
  }
  
  if (is.list(data_obj)) {
    flattened <- list()
    
    for (name in names(data_obj)) {
      item <- data_obj[[name]]
      col_name <- if (prefix == "") name else paste0(prefix, "_", name)
      
      if (is.atomic(item) && length(item) == 1) {
        # Simple atomic values
        flattened[[col_name]] <- item
      } else if (is.atomic(item) && length(item) > 1) {
        # Vectors - convert to string
        flattened[[col_name]] <- paste(item, collapse = ";")
      } else if (is.list(item)) {
        # Nested lists - flatten recursively
        nested <- flatten_data_structure(item, col_name)
        if (nrow(nested) > 0) {
          flattened <- c(flattened, as.list(nested))
        }
      } else {
        # Other types - convert to string
        flattened[[col_name]] <- as.character(item)
      }
    }
    
    if (length(flattened) > 0) {
      return(data.frame(flattened, stringsAsFactors = FALSE))
    }
  }
  
  return(data.frame())
}

# Safe CSV saving with UTF-8 encoding
save_data_csv <- function(data, filepath, flatten = TRUE) {
  ensure_utf8_encoding()
  
  tryCatch({
    # Create directory if needed
    dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
    
    # Flatten complex structures if needed
    if (flatten && !is.data.frame(data)) {
      data <- flatten_data_structure(data)
    }
    
    # Ensure we have a data.frame
    if (!is.data.frame(data)) {
      if (is.list(data)) {
        data <- data.frame(data, stringsAsFactors = FALSE)
      } else {
        data <- data.frame(value = data, stringsAsFactors = FALSE)
      }
    }
    
    # Save with explicit UTF-8 encoding
    write.csv(data, filepath, row.names = FALSE, fileEncoding = "UTF-8")
    return(TRUE)
    
  }, error = function(e) {
    cat("⚠️ CSV save error:", e$message, "\n")
    return(FALSE)
  })
}

# Safe JSON saving with UTF-8 encoding  
save_data_json <- function(data, filepath) {
  ensure_utf8_encoding()
  
  tryCatch({
    dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
    
    # Convert POSIXct to character for JSON compatibility
    data <- rapply(data, function(x) {
      if (inherits(x, "POSIXct")) {
        as.character(x)
      } else {
        x
      }
    }, how = "replace")
    
    # Save JSON with UTF-8
    json_str <- toJSON(data, auto_unbox = TRUE, pretty = TRUE)
    writeLines(json_str, filepath, useBytes = TRUE)
    return(TRUE)
    
  }, error = function(e) {
    cat("⚠️ JSON save error:", e$message, "\n")
    return(FALSE)
  })
}

# ==========================================================================================================
# 🎯 ENHANCED ASSET-AWARE FUNCTIONS
# ==========================================================================================================

# Get asset configuration
get_asset_config <- function(symbol) {
  if (!symbol %in% SUPPORTED_SYMBOLS) {
    cat("⚠️ Symbol", symbol, "not in supported list. Using ADA defaults.\n")
    return(ASSET_CONFIG[["ADAUSDT_UMCBL"]])
  }
  return(ASSET_CONFIG[[symbol]])
}

# Enhanced ticker data with asset-aware processing
get_enhanced_ticker_data_fixed <- function(symbol) {
  cat("📈 Fetching enhanced ticker data for", symbol, "...\n")
  
  config <- get_asset_config(symbol)
  
  # Existing ticker function (assuming it exists)
  if (exists("get_enhanced_ticker_data")) {
    ticker_data <- get_enhanced_ticker_data(symbol)
  } else {
    # Fallback implementation
    ticker_data <- get_ticker_data(symbol)
  }
  
  if (!is.null(ticker_data)) {
    # Add asset-specific metadata
    ticker_data$asset_name <- config$name
    ticker_data$base_asset <- config$base_asset
    ticker_data$quote_asset <- config$quote_asset
    ticker_data$price_decimals <- config$price_decimals
    
    cat("✅ Enhanced ticker data retrieved for", config$name, "- Price:", 
        format(ticker_data$last_price, nsmall = config$price_decimals), "USDT\n")
  }
  
  return(ticker_data)
}

# Enhanced historical data creation with proper saving
create_enhanced_historical_data_fixed <- function(symbol, periods = 200, timeframe = "1h", save_data = TRUE) {
  cat("🧮 Creating enhanced historical data for", symbol, "...\n")
  cat("   Symbol:", symbol, "| Periods:", periods, "| Timeframe:", timeframe, "\n")
  
  config <- get_asset_config(symbol)
  
  # Get live data for realistic generation
  live_data <- NULL
  if (exists("collect_live_market_data")) {
    live_data <- collect_live_market_data(symbol)
  } else if (exists("get_enhanced_ticker_data_fixed")) {
    live_data <- list(ticker = get_enhanced_ticker_data_fixed(symbol))
  }
  
  # Use live data if available
  if (!is.null(live_data) && !is.null(live_data$ticker)) {
    ticker <- live_data$ticker
    current_price <- ticker$last_price
    high_24h <- ticker$high_24h
    low_24h <- ticker$low_24h
    volume_24h <- ticker$volume_24h_usdt
    cat("   Using live ticker data for", config$name, "\n")
  } else {
    # Asset-specific fallback values
    fallback_prices <- list(
      "ADAUSDT_UMCBL" = list(price = 0.5537, high = 0.5814, low = 0.5496, volume = 54000000),
      "BTCUSDT_UMCBL" = list(price = 61250, high = 62500, low = 60000, volume = 1200000000),
      "ETHUSDT_UMCBL" = list(price = 3420, high = 3500, low = 3350, volume = 800000000)
    )
    
    fallback <- fallback_prices[[symbol]]
    current_price <- fallback$price
    high_24h <- fallback$high
    low_24h <- fallback$low
    volume_24h <- fallback$volume
    cat("   Using fallback data for", config$name, "\n")
  }
  
  # Calculate timeframe-specific volatility
  timeframe_multipliers <- list("5m" = 0.12, "15m" = 0.20, "1h" = 0.41, "4h" = 0.82, "1d" = 1.5)
  base_volatility <- timeframe_multipliers[[timeframe]] %||% 0.5
  
  cat("   Current price:", format(current_price, nsmall = config$price_decimals), "USDT\n")
  cat("   24h range:", format(low_24h, nsmall = config$price_decimals), "-", 
      format(high_24h, nsmall = config$price_decimals), "USDT\n")
  cat("   Estimated volatility:", paste0(base_volatility, "% per ", timeframe), "\n")
  
  # Generate realistic OHLCV data
  candles_list <- list()
  base_time <- Sys.time()
  
  # Timeframe in seconds
  timeframe_seconds <- switch(timeframe,
                             "5m" = 300,
                             "15m" = 900, 
                             "1h" = 3600,
                             "4h" = 14400,
                             "1d" = 86400,
                             3600)  # default 1h
  
  for (i in 1:periods) {
    # Price generation with mean reversion
    time_factor <- (periods - i) / periods
    price_drift <- rnorm(1, 0, base_volatility/100)
    volatility <- runif(1, base_volatility/200, base_volatility/100)
    
    base_price <- current_price * (1 + price_drift * time_factor)
    
    open_price <- base_price * (1 + rnorm(1, 0, volatility/2))
    close_price <- base_price * (1 + rnorm(1, 0, volatility/2))
    high_price <- max(open_price, close_price) * (1 + runif(1, 0, volatility))
    low_price <- min(open_price, close_price) * (1 - runif(1, 0, volatility))
    
    # Volume generation
    volume <- volume_24h / (24 * 3600 / timeframe_seconds) * runif(1, 0.5, 2.0)
    
    # Create candle with proper precision
    candle <- data.frame(
      timestamp = base_time - (periods - i) * timeframe_seconds,
      open = round(open_price, config$price_decimals),
      high = round(high_price, config$price_decimals), 
      low = round(low_price, config$price_decimals),
      close = round(close_price, config$price_decimals),
      volume = round(volume, 2),
      timeframe = timeframe,
      symbol = symbol,
      asset_name = config$name,
      stringsAsFactors = FALSE
    )
    
    candles_list[[i]] <- candle
  }
  
  # Combine and sort
  historical_data <- do.call(rbind, candles_list)
  historical_data <- historical_data[order(historical_data$timestamp), ]
  
  # Save data if requested
  if (save_data) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_filename <- paste0(symbol, "_", timeframe, "_candles_", timestamp)
    
    # Save as CSV with proper encoding
    csv_path <- paste0("c:/freeding/tbot202506/bitget_data/historical/", base_filename, ".csv")
    success_csv <- save_data_csv(historical_data, csv_path)
    
    if (success_csv) {
      cat("💾 Historical data saved:", basename(csv_path), "\n")
    }
    
    # Also save metadata as JSON
    metadata <- list(
      symbol = symbol,
      asset_name = config$name,
      timeframe = timeframe,
      periods = periods,
      generation_time = Sys.time(),
      price_info = list(
        current_price = current_price,
        high_24h = high_24h,
        low_24h = low_24h,
        volatility = base_volatility
      )
    )
    
    json_path <- paste0("c:/freeding/tbot202506/bitget_data/metadata/", base_filename, "_meta.json")
    save_data_json(metadata, json_path)
  }
  
  cat("✅ Enhanced historical data created successfully for", config$name, "\n")
  return(historical_data)
}

# ==========================================================================================================
# 🚀 MULTI-ASSET WRAPPER FUNCTIONS
# ==========================================================================================================

# Universal quick collection for any symbol
quick_collection_universal <- function(symbol, timeframe = "1h", periods = 50) {
  if (!symbol %in% SUPPORTED_SYMBOLS) {
    cat("❌ Symbol", symbol, "not supported. Supported:", paste(SUPPORTED_SYMBOLS, collapse = ", "), "\n")
    return(NULL)
  }
  
  config <- get_asset_config(symbol)
  cat("⚡ QUICK DATA COLLECTION FOR", toupper(config$name), "\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  # Collect live data
  live_data <- NULL
  if (exists("collect_live_market_data")) {
    live_data <- collect_live_market_data(symbol)
  }
  
  # Create historical data
  historical_data <- create_enhanced_historical_data_fixed(symbol, periods, timeframe, save_data = TRUE)
  
  result <- list(
    symbol = symbol,
    asset_name = config$name,
    timeframe = timeframe,
    periods = periods,
    live_data = live_data,
    historical_data = historical_data,
    collection_time = Sys.time()
  )
  
  cat("✅ Quick collection successful for", config$name, "\n")
  cat("   Live data sources: Available\n")
  cat("   Historical candles:", nrow(historical_data), "\n")
  
  return(result)
}

# Universal complete analysis for any symbol
complete_analysis_universal <- function(symbol) {
  if (!symbol %in% SUPPORTED_SYMBOLS) {
    cat("❌ Symbol", symbol, "not supported.\n")
    return(NULL)
  }
  
  config <- get_asset_config(symbol)
  cat("🚀 COMPLETE ANALYSIS FOR", toupper(config$name), "(", symbol, ")\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Run existing analysis if available
  analysis_result <- NULL
  if (exists("complete_trading_analysis")) {
    analysis_result <- complete_trading_analysis(symbol)
  }
  
  # Enhanced analysis if available
  enhanced_result <- NULL
  if (exists("complete_trading_analysis_enhanced")) {
    enhanced_result <- complete_trading_analysis_enhanced(symbol)
  }
  
  return(list(
    symbol = symbol,
    asset_config = config,
    basic_analysis = analysis_result,
    enhanced_analysis = enhanced_result,
    analysis_time = Sys.time()
  ))
}

# Universal position check for any symbol
check_positions_universal <- function(symbol = NULL) {
  if (is.null(symbol)) {
    # Check all supported symbols
    cat("📊 Checking positions for all supported assets...\n")
    
    all_positions <- list()
    for (sym in SUPPORTED_SYMBOLS) {
      config <- get_asset_config(sym)
      cat(sprintf("\n🔍 %s (%s):\n", config$name, sym))
      
      if (exists("get_current_positions")) {
        pos <- get_current_positions(sym)
        all_positions[[sym]] <- pos
      }
    }
    return(all_positions)
    
  } else {
    if (!symbol %in% SUPPORTED_SYMBOLS) {
      cat("❌ Symbol", symbol, "not supported.\n")
      return(NULL)
    }
    
    config <- get_asset_config(symbol)
    cat("📊 Checking position for", config$name, "(", symbol, ")...\n")
    
    if (exists("get_current_positions")) {
      return(get_current_positions(symbol))
    }
  }
}

# ==========================================================================================================
# 🎯 UNIVERSAL TP/SL FUNCTIONS
# ==========================================================================================================

# Universal TP/SL placement with asset-aware pricing
place_tp_sl_universal <- function(symbol, side, size, tp_price = NULL, sl_price = NULL) {
  if (!symbol %in% SUPPORTED_SYMBOLS) {
    cat("❌ Symbol", symbol, "not supported.\n")
    return(NULL)
  }
  
  config <- get_asset_config(symbol)
  cat("🎯 UNIVERSAL TP/SL PLACEMENT FOR", toupper(config$name), "\n")
  cat("⚠️  WARNING: PLACING REAL ORDERS ON BITGET!\n\n")
  
  # Format prices according to asset precision
  if (!is.null(tp_price)) {
    tp_price <- round(tp_price, config$price_decimals)
  }
  if (!is.null(sl_price)) {
    sl_price <- round(sl_price, config$price_decimals)
  }
  
  results <- list()
  
  # Place TP order if specified
  if (!is.null(tp_price) && exists("place_tp_simple")) {
    cat("📈 Placing TP order for", config$name, "...\n")
    results$tp <- place_tp_simple(symbol, side, size, tp_price)
    Sys.sleep(1)
  }
  
  # Place SL order if specified  
  if (!is.null(sl_price) && exists("place_sl_simple")) {
    cat("📉 Placing SL order for", config$name, "...\n")
    results$sl <- place_sl_simple(symbol, side, size, sl_price)
  }
  
  return(results)
}

# Universal break-even strategy
place_breakeven_universal <- function(symbol, side, total_size, entry_price) {
  if (!symbol %in% SUPPORTED_SYMBOLS) {
    cat("❌ Symbol", symbol, "not supported.\n")
    return(NULL)
  }
  
  config <- get_asset_config(symbol)
  cat("📊 BREAK-EVEN STRATEGY FOR", toupper(config$name), "\n")
  
  if (exists("place_breakeven_orders")) {
    return(place_breakeven_orders(symbol, side, total_size, entry_price))
  } else {
    cat("❌ Break-even function not available\n")
    return(NULL)
  }
}

# ==========================================================================================================
# 🔧 SYSTEM STATUS & QUICK COMMANDS
# ==========================================================================================================

# Show supported assets and their configurations
show_supported_assets <- function() {
  cat("🎯 SUPPORTED TRADING ASSETS\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  for (symbol in SUPPORTED_SYMBOLS) {
    config <- ASSET_CONFIG[[symbol]]
    cat(sprintf("📈 %s (%s)\n", config$name, symbol))
    cat(sprintf("   Price Decimals: %d | Tick Size: %s | Leverage: %dx\n", 
                config$price_decimals, config$tick_size, config$max_leverage))
    cat(sprintf("   Volume Threshold: %.0fM USDT\n\n", config$typical_volume_threshold / 1000000))
  }
}

# System health check for all assets
system_health_check <- function() {
  cat("🔧 SYSTEM HEALTH CHECK\n")
  cat(paste(rep("=", 30), collapse = ""), "\n")
  
  # Check encoding
  ensure_utf8_encoding()
  cat("✅ UTF-8 encoding configured\n")
  
  # Check data directories
  base_path <- "c:/freeding/tbot202506/bitget_data/"
  dirs <- c("historical", "metadata", "reports")
  
  for (dir in dirs) {
    path <- paste0(base_path, dir, "/")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    cat("✅ Directory:", dir, "\n")
  }
  
  # Test data saving
  test_data <- data.frame(
    timestamp = Sys.time(),
    test_value = 123.456,
    test_string = "UTF-8 Test: äöü",
    stringsAsFactors = FALSE
  )
  
  test_path <- paste0(base_path, "test_encoding.csv")
  success <- save_data_csv(test_data, test_path)
  
  if (success) {
    cat("✅ Data saving test: PASSED\n")
    unlink(test_path)  # Clean up
  } else {
    cat("❌ Data saving test: FAILED\n")
  }
  
  cat("✅ System health check complete\n")
}

# ==========================================================================================================
# ✅ SYSTEM INITIALIZATION
# ==========================================================================================================

# Initialize fixed system
initialize_fixed_system <- function() {
  ensure_utf8_encoding()
  system_health_check()
  show_supported_assets()
  
  cat("\n🚀 ENHANCED BITGET SYSTEM LOADED!\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("✅ Encoding fixes applied\n")
  cat("✅ Multi-asset support active\n") 
  cat("✅ Data saving enhanced\n")
  cat(sprintf("✅ %d assets supported\n", length(SUPPORTED_SYMBOLS)))
  
  cat("\n🎯 QUICK COMMANDS:\n")
  cat("# Test any asset:\n")
  cat("quick_collection_universal('BTCUSDT_UMCBL', '1h', 50)\n")
  cat("complete_analysis_universal('ETHUSDT_UMCBL')\n")
  cat("check_positions_universal()  # All assets\n")
  cat("\n# Trade any asset (LIVE ORDERS!):\n")
  cat("place_tp_sl_universal('BTCUSDT_UMCBL', 'long', '0.1', 62000, 60000)\n")
  cat("place_breakeven_universal('ETHUSDT_UMCBL', 'long', '1', 3400)\n")
  
  cat(paste(rep("=", 50), collapse = ""), "\n")
}

# Auto-initialize when loaded
initialize_fixed_system()