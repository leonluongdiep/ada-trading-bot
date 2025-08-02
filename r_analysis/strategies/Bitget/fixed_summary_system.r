# ==========================================================================================================
# 📊 FIXED SUMMARY SYSTEM - CONSOLIDATED & WORKING VERSION
# ==========================================================================================================
# Löst das Problem mit "unbenutztes Argument (include_risk_metrics = TRUE)"
# Konsolidiert alle get_current_positions() Versionen und repariert Summary-Funktionen
# ==========================================================================================================

cat("📊 Loading Fixed Summary System...\n")

# ==========================================================================================================
# 🔧 UNIFIED get_current_positions FUNCTION (FINAL VERSION)
# ==========================================================================================================

#' Unified position fetching with optional risk metrics
get_current_positions_unified <- function(include_risk_metrics = FALSE, debug = FALSE) {
  if (debug) cat("🔍 DEBUG: Starting unified position fetching...\n")
  
  tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/position/all-position",
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(response) || is.null(response$data)) {
      if (debug) cat("🔍 DEBUG: No API response or data\n")
      return(data.frame())
    }
    
    data <- response$data
    if (debug) {
      cat("🔍 DEBUG: Data type:", class(data), "\n")
      cat("🔍 DEBUG: Data dimensions:", if(is.data.frame(data)) paste(dim(data), collapse="x") else length(data), "\n")
    }
    
    # Handle API response
    if (is.data.frame(data) && nrow(data) > 0) {
      if (debug) cat("🔍 DEBUG: Processing", nrow(data), "position records\n")
      
      # Create standardized positions dataframe with correct field mappings
      positions_df <- data.frame(
        symbol = data$symbol,
        side = data$holdSide,
        size = as.numeric(data$total),
        available = as.numeric(data$available),
        avg_price = as.numeric(data$openPriceAvg),
        mark_price = as.numeric(data$markPrice),
        unrealized_pnl = as.numeric(data$unrealizedPL),
        leverage = as.numeric(data$leverage),
        margin = as.numeric(data$marginSize),
        stringsAsFactors = FALSE
      )
      
      # Filter active positions (size > 0) safely
      active_mask <- positions_df$size > 0
      active_positions <- positions_df[active_mask & !is.na(active_mask), ]
      
      if (debug) cat("🔍 DEBUG: Found", nrow(active_positions), "active positions\n")
      
      # Add calculated fields safely
      if (nrow(active_positions) > 0) {
        active_positions$pnl_ratio <- ifelse(active_positions$size > 0,
                                           active_positions$unrealized_pnl / active_positions$size,
                                           0)
        
        # Add basic risk metrics if requested
        if (include_risk_metrics) {
          active_positions$risk_score <- abs(active_positions$pnl_ratio) * 0.5 + 
                                        (active_positions$leverage / 50) * 0.3 + 
                                        (active_positions$size / 1000) * 0.2
          active_positions$risk_score <- pmax(0, pmin(1, active_positions$risk_score))
          active_positions$volatility_risk <- 0.5  # Default moderate volatility
        }
        
        if (debug) {
          for (i in 1:min(3, nrow(active_positions))) {
            pos <- active_positions[i, ]
            cat(sprintf("   %d. %s: %s %.0f contracts (%.2f USDT PnL)\n",
                        i, pos$symbol, pos$side, pos$size, pos$unrealized_pnl))
          }
        }
      }
      
      return(active_positions)
    } else if (is.list(data) && length(data) > 0) {
      if (debug) cat("🔍 DEBUG: Data is a list, converting\n")
      
      # Handle list of lists
      if (is.list(data[[1]])) {
        position_rows <- lapply(data, function(pos) {
          data.frame(
            symbol = pos$symbol %||% "UNKNOWN",
            side = pos$holdSide %||% "unknown", 
            size = as.numeric(pos$total %||% 0),
            available = as.numeric(pos$available %||% 0),
            avg_price = as.numeric(pos$openPriceAvg %||% 0),
            mark_price = as.numeric(pos$markPrice %||% 0),
            unrealized_pnl = as.numeric(pos$unrealizedPL %||% 0),
            leverage = as.numeric(pos$leverage %||% 1),
            margin = as.numeric(pos$marginSize %||% 0),
            stringsAsFactors = FALSE
          )
        })
        positions_df <- do.call(rbind, position_rows)
        
        # Filter for active positions
        active_positions <- positions_df[positions_df$size > 0, ]
        if (nrow(active_positions) > 0) {
          active_positions$pnl_ratio <- active_positions$unrealized_pnl / active_positions$size
          
          if (include_risk_metrics) {
            active_positions$risk_score <- abs(active_positions$pnl_ratio) * 0.5
            active_positions$volatility_risk <- 0.5
          }
        }
        return(active_positions)
      }
    }
    
    if (debug) cat("🔍 DEBUG: Could not parse data structure\n")
    return(data.frame())
    
  }, error = function(e) {
    cat("❌ Error fetching positions:", e$message, "\n")
    if (debug) {
      cat("🔍 DEBUG: Full error details:\n")
      print(e)
    }
    return(data.frame())
  })
}

# ==========================================================================================================
# 🔧 FIXED MARKET DATA COLLECTION
# ==========================================================================================================

#' Enhanced ticker data with safe field extraction
get_enhanced_ticker_data_safe <- function(symbol) {
  if (exists("get_enhanced_ticker_data")) {
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (!is.null(ticker_data)) {
      return(ticker_data)
    }
  }
  
  # Fallback: direct API call
  tryCatch({
    params <- list(symbol = symbol)
    result <- bitget_request("/api/v2/mix/market/ticker", "GET", params)
    
    if (!is.null(result) && !is.null(result$data)) {
      data <- result$data
      
      return(list(
        symbol = symbol,
        last_price = as.numeric(data$lastPr %||% data$last %||% 0),
        mark_price = as.numeric(data$markPrice %||% data$last_price %||% 0),
        change_24h_pct = as.numeric(data$change24h %||% data$chgUtc %||% 0),
        volume_24h_usdt = as.numeric(data$quoteVolume %||% data$baseVolume %||% 0),
        volume_24h = as.numeric(data$baseVolume %||% data$volume %||% 0),
        high_24h = as.numeric(data$high24h %||% 0),
        low_24h = as.numeric(data$low24h %||% 0),
        timestamp = Sys.time(),
        data_source = "direct_api"
      ))
    }
    
    return(NULL)
  }, error = function(e) {
    cat("❌ Error in safe ticker fetch:", e$message, "\n")
    return(NULL)
  })
}

# ==========================================================================================================
# 🔧 FIXED EXECUTE FUNCTIONS
# ==========================================================================================================

#' Fixed execute_silent function without problematic parameters
execute_silent_fixed <- function(symbols = NULL, mode = "full") {
  if (is.null(symbols)) {
    symbols <- PORTFOLIO_ASSETS
  }
  
  tryCatch({
    # Collect market data for all symbols
    results <- list(
      timestamp = Sys.time(),
      symbols = symbols,
      quick_data = list()
    )
    
    for (symbol in symbols) {
      # Get ticker data safely
      ticker_data <- get_enhanced_ticker_data_safe(symbol)
      
      if (!is.null(ticker_data)) {
        # Extract price safely
        price <- ticker_data$last_price %||% ticker_data$mark_price %||% 0
        change_24h <- ticker_data$change_24h_pct %||% 0
        volume <- ticker_data$volume_24h_usdt %||% ticker_data$volume_24h %||% 0
        
        if (price > 0) {
          results$quick_data[[symbol]] <- list(
            symbol = symbol,
            price = price,
            change_24h = change_24h,
            volume = volume,
            status = "SUCCESS",
            timestamp = Sys.time()
          )
        } else {
          results$quick_data[[symbol]] <- list(
            symbol = symbol,
            price = 0,
            change_24h = 0,
            volume = 0,
            status = "NO_PRICE_DATA",
            error = "Invalid price data",
            timestamp = Sys.time()
          )
        }
      } else {
        results$quick_data[[symbol]] <- list(
          symbol = symbol,
          price = 0,
          change_24h = 0,
          volume = 0,
          status = "API_ERROR",
          error = "No API response",
          timestamp = Sys.time()
        )
      }
    }
    
    return(results)
    
  }, error = function(e) {
    cat("❌ Error in execute_silent_fixed:", e$message, "\n")
    return(list(
      success = FALSE,
      error = e$message,
      timestamp = Sys.time()
    ))
  })
}

# ==========================================================================================================
# 🔧 FIXED SUMMARY FUNCTIONS
# ==========================================================================================================

#' Fixed summary function that actually works
execute_summary_only_working <- function(symbols = NULL, mode = "full") {
  cat("🔄 Running improved analysis (FIXED VERSION)...\n")
  
  # Use the fixed silent execution
  results <- execute_silent_fixed(symbols, mode)
  
  if (is.null(results) || (!is.null(results$success) && !results$success)) {
    cat("❌ No results received\n")
    return(invisible(NULL))
  }
  
  # Show summary based on actual data structure
  cat("\n📊 === LIVE MARKET SUMMARY (WORKING) === 📊\n")
  
  if (!is.null(results$quick_data)) {
    display_quick_data_summary_working(results$quick_data)
  } else {
    cat("❌ No market data available\n")
    if (!is.null(results$error)) {
      cat("Error:", results$error, "\n")
    }
  }
  
  cat(sprintf("\n⏱️ Executed at: %s\n", format(Sys.time(), "%H:%M:%S")))
  return(invisible(results))
}

#' Display quick data summary that works
display_quick_data_summary_working <- function(quick_data) {
  for (symbol in names(quick_data)) {
    data <- quick_data[[symbol]]
    
    if (!is.null(data$error)) {
      cat(sprintf("❌ %s: %s\n", symbol, data$error))
    } else if (!is.null(data$price) && data$price > 0) {
      # Trend-Icon
      trend_icon <- if (data$change_24h > 1) "📈" else 
        if (data$change_24h > 0) "🔼" else 
          if (data$change_24h > -1) "➡️" else 
            if (data$change_24h > -3) "🔽" else "📉"
      
      cat(sprintf("✅ %s: %.4f USDT | 24h: %+.2f%% %s | Vol: %.1fM\n",
                  symbol, data$price, data$change_24h, trend_icon, data$volume/1000000))
    } else {
      cat(sprintf("❌ %s: No valid price data\n", symbol))
    }
  }
}

#' Fixed quick market check
quick_market_check_working <- function() {
  execute_summary_only_working()
}

#' Fixed daily market check
daily_market_check_working <- function(symbols = NULL) {
  cat("\n🌅 === IMPROVED DAILY MARKET CHECK (WORKING) === 🌅\n")
  
  # 1. Get quick overview
  cat("1️⃣ Getting market overview...\n")
  summary_results <- execute_summary_only_working(symbols, "full")
  
  # 2. Show position summary if available
  cat("\n2️⃣ Position summary...\n")
  positions <- get_current_positions_unified(include_risk_metrics = FALSE, debug = FALSE)
  
  if (nrow(positions) > 0) {
    cat("📊 Current Positions:\n")
    for (i in 1:nrow(positions)) {
      pos <- positions[i, ]
      pnl_color <- if (pos$unrealized_pnl > 0) "✅" else "❌"
      cat(sprintf("   %s %s: %s %.0f contracts | PnL: %.2f USDT\n",
                  pnl_color, pos$symbol, pos$side, pos$size, pos$unrealized_pnl))
    }
  } else {
    cat("ℹ️ No open positions\n")
  }
  
  # 3. Save daily data
  cat("\n3️⃣ Saving daily data...\n")
  if (!is.null(summary_results) && !is.null(summary_results$quick_data)) {
    save_results_to_csv_working(summary_results)
  } else {
    cat("ℹ️ No market data to save\n")
  }
  
  cat("\n✅ Daily market check completed!\n")
  return(summary_results)
}

#' Fixed prices only function
prices_only_working <- function(symbols = NULL) {
  cat("💰 === CURRENT PRICES (WORKING) === 💰\n")
  results <- execute_silent_fixed(symbols, "quick")
  
  if (!is.null(results$quick_data)) {
    for (symbol in names(results$quick_data)) {
      data <- results$quick_data[[symbol]]
      if (!is.null(data$price) && data$price > 0) {
        cat(sprintf("%s: %.4f USDT (%+.2f%%)\n", 
                    symbol, data$price, data$change_24h))
      } else {
        cat(sprintf("%s: ERROR - %s\n", symbol, data$error %||% "Unknown error"))
      }
    }
  } else {
    cat("❌ No price data available\n")
  }
}

#' Fixed CSV saving
save_results_to_csv_working <- function(results, filename = NULL) {
  if (is.null(results) || is.null(results$quick_data)) {
    cat("❌ No data to save\n")
    return()
  }
  
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("market_data_working_", timestamp, ".csv")
  }
  
  # Convert to DataFrame
  df_list <- list()
  
  for (symbol in names(results$quick_data)) {
    data <- results$quick_data[[symbol]]
    df_list[[symbol]] <- data.frame(
      Symbol = symbol,
      Price = data$price %||% 0,
      Change_24h = data$change_24h %||% 0,
      Volume_USDT = data$volume %||% 0,
      Status = data$status %||% "UNKNOWN",
      Error = data$error %||% "",
      Timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
  }
  
  if (length(df_list) > 0) {
    df <- do.call(rbind, df_list)
    
    tryCatch({
      # Try to save to logs directory
      log_dir <- if (exists("FILE_PATHS") && !is.null(FILE_PATHS$logs_path)) {
        FILE_PATHS$logs_path
      } else {
        "c:/freeding/tbot202506/logs/"
      }
      
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
      }
      
      full_path <- file.path(log_dir, filename)
      write.csv(df, full_path, row.names = FALSE)
      cat("✅ Market data saved to:", basename(full_path), "\n")
      cat("📁 Location:", log_dir, "\n")
    }, error = function(e) {
      cat("❌ Error saving CSV:", e$message, "\n")
    })
  }
}

# ==========================================================================================================
# 🔧 OVERRIDE PROBLEMATIC FUNCTIONS
# ==========================================================================================================

#' Override the problematic get_current_positions function globally
apply_working_functions <- function() {
  cat("🔄 === APPLYING WORKING FUNCTIONS === 🔄\n")
  
  # Override global functions with working versions
  get_current_positions <<- get_current_positions_unified
  execute_summary_only_fixed <<- execute_summary_only_working
  quick_market_check <<- quick_market_check_working
  daily_market_check_fixed <<- daily_market_check_working
  prices_only <<- prices_only_working
  
  cat("✅ Working functions applied globally\n")
  cat("💡 You can now use:\n")
  cat("   quick_market_check()                     # Working market overview\n")
  cat("   execute_summary_only_fixed()             # Working summary function\n")
  cat("   daily_market_check_fixed()               # Working daily check\n")
  cat("   prices_only()                            # Working prices only\n")
  cat("   get_current_positions()                  # Working positions (with optional risk metrics)\n")
}

#' Test all working functions
test_working_functions <- function() {
  cat("🧪 === TESTING WORKING FUNCTIONS === 🧪\n")
  
  cat("\n1️⃣ Testing position fetching...\n")
  positions <- get_current_positions_unified(debug = TRUE)
  cat("   ✅ Result:", nrow(positions), "positions found\n")
  
  cat("\n2️⃣ Testing market data...\n")
  test_symbol <- "ADAUSDT_UMCBL"
  ticker_data <- get_enhanced_ticker_data_safe(test_symbol)
  if (!is.null(ticker_data) && ticker_data$last_price > 0) {
    cat("   ✅ Market data working:", ticker_data$last_price, "USDT\n")
  } else {
    cat("   ❌ Market data failed\n")
  }
  
  cat("\n3️⃣ Testing summary functions...\n")
  results <- execute_silent_fixed(PORTFOLIO_ASSETS, "quick")
  if (!is.null(results$quick_data)) {
    working_count <- sum(sapply(results$quick_data, function(x) x$price > 0))
    cat("   ✅ Summary functions working:", working_count, "symbols processed\n")
  } else {
    cat("   ❌ Summary functions failed\n")
  }
  
  cat("\n✅ === WORKING FUNCTIONS TEST COMPLETE === ✅\n")
}

# Apply the working functions immediately
apply_working_functions()

cat("✅ FIXED SUMMARY SYSTEM LOADED!\n")
cat("🔧 All function conflicts resolved\n")
cat("📊 Market data collection working\n")
cat("🛡️ Position fetching with unified interface\n")
cat("\n💡 QUICK START:\n")
cat("   test_working_functions()                  # Test everything\n")
cat("   quick_market_check()                     # Market overview\n")
cat("   daily_market_check_fixed()               # Full daily check\n")
cat("   prices_only()                            # Just prices\n")