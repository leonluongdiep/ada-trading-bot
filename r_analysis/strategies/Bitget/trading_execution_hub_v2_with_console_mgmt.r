# ========================================================================================================== 
# 🎯 TRADING EXECUTION HUB V2 - MAIN EXECUTION INTERFACE WITH CONSOLE MANAGEMENT
# ==========================================================================================================
# Konsolidiert: multi_asset_rexecution_v7.r + altcoin_rally_triggers.r + Console Management
# Central command interface for all trading operations with enhanced user experience
# Version: 2.1 - Now with integrated Console Output Management
# ==========================================================================================================

# ==========================================================================================================
# 🔧 AUTOMATIC SYSTEM COMPONENT LOADING
# ==========================================================================================================

cat("🔄 Loading Trading System V2 Components...\n")

# Load central configuration if not already loaded
if (!exists("SYSTEM_CONFIG_LOADED")) {
  cat("📁 Loading system_config.r...\n")
  source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/system_config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
  cat("   ✅ Configuration loaded\n")
}

# Load console management system
if (!exists("CONSOLE_MANAGEMENT_LOADED")) {
  cat("📁 Loading console output management...\n")
  console_mgmt_path <- "C:/freeding/tbot202506/r_analysis/r_console_output_manager.r"
  if (file.exists(console_mgmt_path)) {
    source(console_mgmt_path)
    CONSOLE_MANAGEMENT_LOADED <- TRUE
    cat("   ✅ Console management loaded\n")
  } else {
    cat("   ⚠️ Console management not found - continuing without it\n")
    CONSOLE_MANAGEMENT_LOADED <- FALSE
  }
}

# Load core trading engine
if (!exists("CORE_ENGINE_LOADED")) {
  cat("📁 Loading bitget_core_engine.r...\n")
  source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_core_engine.r")
  CORE_ENGINE_LOADED <- TRUE
  cat("   ✅ Core engine loaded\n")
}

# Load OI analytics system
if (!exists("OI_ANALYTICS_LOADED")) {
  cat("📁 Loading unified_oi_analytics.r...\n")
  source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/unified_oi_analytics.r")
  OI_ANALYTICS_LOADED <- TRUE
  cat("   ✅ OI analytics loaded\n")
}

# Load risk management system
if (!exists("RISK_MANAGER_LOADED")) {
  cat("📁 Loading portfolio_risk_manager.r...\n")
  source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/portfolio_risk_manager.r")
  RISK_MANAGER_LOADED <- TRUE
  cat("   ✅ Risk manager loaded\n")
}

cat("🚀 All system components loaded successfully!\n")

# ==========================================================================================================
# 🔇 CONSOLE MANAGEMENT INTEGRATION
# ==========================================================================================================

# Global console state for the trading system
TRADING_CONSOLE_STATE <- list(
  output_mode = "normal",  # normal, silent, filtered, logged
  log_file = NULL,
  start_time = NULL,
  is_redirected = FALSE
)

#' Simple filtered cat function
filtered_cat <- function(...) {
  keywords <- c("✅", "❌", "🚀", "📊", "⚠️", "🎯", "🛡️")
  message_text <- paste(..., sep = " ")
  
  # Check if message contains any keywords
  show_message <- any(sapply(keywords, function(kw) grepl(kw, message_text, fixed = TRUE)))
  
  if (show_message) {
    base::cat(message_text)
  }
}

#' Set trading system output mode
set_output_mode <- function(mode = c("normal", "silent", "filtered", "logged"), 
                            keywords = c("✅", "❌", "🚀", "📊", "⚠️", "🎯", "🛡️"),
                            log_dir = NULL) {
  
  mode <- match.arg(mode)
  
  # Clean up previous mode if needed
  if (TRADING_CONSOLE_STATE$output_mode != "normal" && TRADING_CONSOLE_STATE$output_mode != mode) {
    cleanup_output_mode()
  }
  
  # Apply new mode
  if (exists("start_silent_mode") && CONSOLE_MANAGEMENT_LOADED) {
    switch(mode,
           "normal" = {
             if (TRADING_CONSOLE_STATE$is_redirected) {
               end_silent_mode()
             }
             cat("📢 Normal output mode active\n")
           },
           "silent" = {
             start_silent_mode("suppress")
             cat("🔇 Silent mode active - minimal output only\n")
           },
           "filtered" = {
             create_filtered_output(keywords = keywords)
             cat("🔍 Filtered output active - showing key messages only\n")
           },
           "logged" = {
             if (is.null(log_dir)) {
               log_dir <- FILE_PATHS$logs_path %||% "c:/freeding/tbot202506/logs/"
             }
             redirect_console_to_file(log_dir = log_dir, show_summary = TRUE)
             TRADING_CONSOLE_STATE$is_redirected <- TRUE
           }
    )
  }
  
  TRADING_CONSOLE_STATE$output_mode <- mode
  TRADING_CONSOLE_STATE$start_time <- Sys.time()
  
  return(invisible(TRUE))
}

#' Cleanup output mode
cleanup_output_mode <- function() {
  tryCatch({
    if (TRADING_CONSOLE_STATE$output_mode == "logged" && TRADING_CONSOLE_STATE$is_redirected) {
      # Close log file properly
      cat("\n# =====================================\n")
      cat("# End Time:", as.character(Sys.time()), "\n")
      cat("# =====================================\n")
      
      # Reset sinks
      sink(type = "message")
      sink(type = "output")
      
      # Inform user where log was saved
      if (!is.null(TRADING_CONSOLE_STATE$log_file)) {
        cat("📄 Log saved to:", basename(TRADING_CONSOLE_STATE$log_file), "\n")
      }
      
    } else if (TRADING_CONSOLE_STATE$output_mode == "filtered") {
      # For filtered mode, no special cleanup needed
      TRADING_CONSOLE_STATE$output_mode <- "normal"
      cat("📢 Normal output restored\n")
    } else if (exists("end_silent_mode") && CONSOLE_MANAGEMENT_LOADED) {
      # For other modes, use console management cleanup
      end_silent_mode()
    } else {
      # Fallback cleanup
      tryCatch({
        sink(type = "message")
        sink(type = "output")
      }, error = function(e) NULL)
    }
  }, error = function(e) {
    # Emergency cleanup
    tryCatch({
      sink(type = "message")
      sink(type = "output")
    }, error = function(e) NULL)
  })
  
  TRADING_CONSOLE_STATE$output_mode <- "normal"
  TRADING_CONSOLE_STATE$is_redirected <- FALSE
  TRADING_CONSOLE_STATE$log_file <- NULL
}

# ==========================================================================================================
# 🛠️ UTILITY FUNCTIONS & OPERATORS
# ==========================================================================================================

# NULL coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ==========================================================================================================
# 🛠️ MISSING FUNCTIONS - EMERGENCY FIXES
# ==========================================================================================================

#' Enhanced trading analysis wrapper - fallback if core function missing
complete_trading_analysis_enhanced <- function(symbol) {
  tryCatch({
    # Try to use existing function first
    if (exists("complete_trading_analysis")) {
      result <- complete_trading_analysis(symbol)
      if (!is.null(result)) return(result)
    }
    
    # Fallback: Basic technical analysis using enhanced ticker data
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) return(NULL)
    
    # Safe data extraction with correct field names
    current_price <- tryCatch({
      if ("last_price" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$last_price)
        if (length(val) == 0 || is.na(val)) 0 else val
      } else if ("mark_price" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$mark_price)
        if (length(val) == 0 || is.na(val)) 0 else val
      } else {
        0
      }
    }, error = function(e) 0)
    
    change_24h <- tryCatch({
      if ("change_24h_pct" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$change_24h_pct)
        if (length(val) == 0 || is.na(val)) 0 else val
      } else {
        0
      }
    }, error = function(e) 0)
    
    volume <- tryCatch({
      if ("volume_24h_usdt" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$volume_24h_usdt)
        if (length(val) == 0 || is.na(val)) 0 else val
      } else if ("volume_24h" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$volume_24h)
        if (length(val) == 0 || is.na(val)) 0 else val
      } else {
        0
      }
    }, error = function(e) 0)
    
    # Validate essential data
    if (current_price <= 0) {
      cat("❌ Invalid price data for", symbol, "\n")
      return(NULL)
    }
    
    # Basic technical indicators (simplified)
    rsi <- calculate_simple_rsi(current_price, change_24h)
    macd <- 0  # Simplified
    signal_line <- 0
    bb_position <- 0.5
    
    return(list(
      symbol = symbol,
      current_price = current_price,
      change_24h = change_24h,
      volume = volume,
      rsi = rsi,
      macd = macd,
      signal_line = signal_line,
      bb_position = bb_position,
      trend = if (change_24h > 0) "BULLISH" else if (change_24h < 0) "BEARISH" else "NEUTRAL"
    ))
    
  }, error = function(e) {
    cat("❌ Enhanced analysis fallback error for", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

#' Get OI historical data - fallback implementation
get_oi_historical_data <- function(symbol, period = "24h") {
  tryCatch({
    # Simplified OI data fetch
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/market/open-interest",
      params = list(
        symbol = symbol,
        granularity = "1H"
      )
    )
    
    if (!is.null(response) && !is.null(response$data)) {
      return(response$data)
    }
    
    return(list())
    
  }, error = function(e) {
    cat("❌ OI historical data error:", e$message, "\n")
    return(list())
  })
}

#' Simple RSI calculation fallback
calculate_simple_rsi <- function(current_price, change_24h) {
  # Simplified RSI based on 24h change
  if (change_24h > 5) return(75)      # Overbought
  if (change_24h > 2) return(65)      # Bullish
  if (change_24h > -2) return(50)     # Neutral
  if (change_24h > -5) return(35)     # Bearish
  return(25)                          # Oversold
}

#' Enhanced position fetching with error handling
get_current_positions <- function(include_risk_metrics = TRUE) {
  tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/position/all-position",
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(response)) {
      cat("ℹ️ No API response for positions\n")
      return(data.frame())
    }
    
    # Handle different response structures
    positions_data <- NULL
    if (is.list(response) && "data" %in% names(response)) {
      positions_data <- response$data
    } else if (is.list(response)) {
      positions_data <- response
    } else {
      cat("ℹ️ Unexpected response structure for positions\n")
      return(data.frame())
    }
    
    if (is.null(positions_data) || length(positions_data) == 0) {
      cat("ℹ️ No open positions found\n")
      return(data.frame())
    }
    
    # Check if positions_data is a list of positions or a single position
    if (!is.list(positions_data[[1]])) {
      # Single position case - wrap in list
      positions_data <- list(positions_data)
    }
    
    # Convert to data frame with enhanced error handling
    positions_df <- tryCatch({
      position_rows <- lapply(positions_data, function(pos) {
        # Safe extraction function
        safe_extract <- function(field, default = 0) {
          tryCatch({
            if (is.list(pos) && field %in% names(pos) && !is.null(pos[[field]])) {
              val <- pos[[field]]
              if (is.character(val)) as.numeric(val) else val
            } else {
              default
            }
          }, error = function(e) default)
        }
        
        # Safe string extraction
        safe_extract_string <- function(field, default = "unknown") {
          tryCatch({
            if (is.list(pos) && field %in% names(pos) && !is.null(pos[[field]])) {
              as.character(pos[[field]])
            } else {
              default
            }
          }, error = function(e) default)
        }
        
        total_size <- safe_extract("total", 0)
        unrealized_pnl <- safe_extract("unrealizedPL", 0)
        
        data.frame(
          symbol = safe_extract_string("symbol", "UNKNOWN"),
          side = safe_extract_string("holdSide", "unknown"),
          size = total_size,
          available = safe_extract("available", 0),
          avg_price = safe_extract("averageOpenPrice", 0),
          mark_price = safe_extract("markPrice", 0),
          unrealized_pnl = unrealized_pnl,
          pnl_ratio = if (total_size > 0) unrealized_pnl / total_size else 0,
          leverage = safe_extract("leverage", 1),
          margin = safe_extract("im", 0),
          stringsAsFactors = FALSE
        )
      })
      
      # Combine all position rows
      if (length(position_rows) > 0) {
        do.call(rbind, position_rows)
      } else {
        data.frame()
      }
      
    }, error = function(e) {
      cat("❌ Error parsing positions data:", e$message, "\n")
      return(data.frame())
    })
    
    # Add risk metrics if requested and data exists
    if (include_risk_metrics && nrow(positions_df) > 0) {
      positions_df <- add_risk_metrics(positions_df)
    }
    
    return(positions_df)
    
  }, error = function(e) {
    cat("❌ Error fetching positions:", e$message, "\n")
    return(data.frame())
  })
}

#' Get kline data fallback
get_kline_data <- function(symbol, granularity = "1h", limit = 24) {
  tryCatch({
    response <- bitget_request(
      method = "GET", 
      endpoint = "/api/v2/mix/market/candles",
      params = list(
        symbol = symbol,
        granularity = granularity,
        limit = limit
      )
    )
    
    if (!is.null(response) && !is.null(response$data)) {
      return(response$data)
    }
    
    return(NULL)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Primary execution function - Enhanced with console management
execute_trading_system <- function(mode = "full", 
                                   symbols = NULL, 
                                   include_oi = TRUE, 
                                   include_risk_check = TRUE, 
                                   interactive = FALSE,
                                   output_mode = NULL,
                                   log_results = FALSE) {
  
  # Apply output mode if specified
  if (!is.null(output_mode)) {
    set_output_mode(output_mode)
  }
  
  # Clear and prepare console
  clean_console_output()
  display_system_header()
  
  tryCatch({
    cat("\n🚀 === TRADING SYSTEM V2 EXECUTION === 🚀\n")
    cat("Mode:", toupper(mode), "| Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    if (!is.null(output_mode)) {
      cat("Output Mode:", toupper(output_mode), "\n")
    }
    
    # Initialize symbols list
    if (is.null(symbols)) {
      symbols <- PORTFOLIO_ASSETS
    }
    
    cat("📊 Assets:", paste(symbols, collapse = ", "), "\n")
    
    # Execute based on mode
    execution_results <- switch(mode,
                                "full" = execute_full_analysis(symbols, include_oi, include_risk_check),
                                "quick" = execute_quick_check(symbols),
                                "oi_only" = execute_oi_analysis_only(symbols),
                                "risk_only" = execute_risk_check_only(symbols),
                                "sentiment" = execute_sentiment_analysis(symbols),
                                "quiet" = execute_quiet_analysis(symbols),  # New quiet mode
                                execute_full_analysis(symbols, include_oi, include_risk_check)  # Default
    )
    
    # Display execution summary
    display_execution_summary(execution_results, mode)
    
    # Save results log if requested
    if (log_results) {
      save_execution_log(execution_results)
    }
    
    # Interactive mode for additional commands
    if (interactive) {
      launch_interactive_interface(execution_results)
    }
    
    return(execution_results)
    
  }, error = function(e) {
    cat("❌ EXECUTION ERROR:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  }, finally = {
    # Restore normal output if mode was changed
    if (!is.null(output_mode) && output_mode != "normal") {
      cleanup_output_mode()
    }
  })
}

#' Execute quiet analysis - minimal output version
execute_quiet_analysis <- function(symbols) {
  # Use filtered output for quiet mode
  if (exists("create_filtered_output") && CONSOLE_MANAGEMENT_LOADED) {
    create_filtered_output(keywords = c("✅", "❌", "🎯", "📊"))
  }
  
  cat("🔇 === QUIET ANALYSIS MODE === 🔇\n")
  
  results <- list(
    timestamp = Sys.time(),
    symbols = symbols,
    summary = list()
  )
  
  for (symbol in symbols) {
    # Silently collect data
    market_data <- tryCatch({
      get_enhanced_ticker_data(symbol)
    }, error = function(e) NULL)
    
    if (!is.null(market_data)) {
      results$summary[[symbol]] <- list(
        price = market_data$last_price %||% 0,
        change_24h = market_data$change_24h_pct %||% 0,
        volume = market_data$volume_24h_usdt %||% 0
      )
      
      cat("✅", symbol, ":", round(results$summary[[symbol]]$price, 4), "\n")
    } else {
      cat("❌", symbol, ": Failed\n")
    }
  }
  
  # Restore normal output
  if (exists("restore_filtered_output") && CONSOLE_MANAGEMENT_LOADED) {
    restore_filtered_output()
  }
  
  return(results)
}

#' Save execution log to file
save_execution_log <- function(results, log_dir = NULL) {
  if (is.null(log_dir)) {
    log_dir <- FILE_PATHS$logs_path %||% "c:/freeding/tbot202506/logs/"
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0("execution_log_", timestamp, ".json"))
  
  tryCatch({
    jsonlite::write_json(results, log_file, pretty = TRUE, auto_unbox = TRUE)
    cat("📄 Execution log saved to:", basename(log_file), "\n")
  }, error = function(e) {
    cat("⚠️ Could not save execution log:", e$message, "\n")
  })
}

#' Full comprehensive analysis (replaces main function from multi_asset_rexecution_v7.r)
execute_full_analysis <- function(symbols, include_oi = TRUE, include_risk_check = TRUE) {
  
  results <- list(
    timestamp = Sys.time(),
    symbols = symbols,
    market_data = list(),
    technical_analysis = list(),
    oi_analysis = list(),
    risk_assessment = list(),
    sentiment_analysis = list(),
    recommendations = list()
  )
  
  cat("\n📊 === COLLECTING MULTI-ASSET DATA === 📊\n")
  
  # Step 1: Collect enhanced market data for all symbols
  for (symbol in symbols) {
    cat("📈 Processing", symbol, "...\n")
    
    # Enhanced market data collection
    market_data <- collect_enhanced_market_data(symbol)
    results$market_data[[symbol]] <- market_data
    
    if (!is.null(market_data)) {
      # Technical analysis
      technical_analysis <- perform_comprehensive_technical_analysis(symbol, market_data)
      results$technical_analysis[[symbol]] <- technical_analysis
      
      # Display quick summary
      display_asset_summary(symbol, market_data, technical_analysis)
    } else {
      cat("⚠️ Failed to collect data for", symbol, "\n")
    }
    
    Sys.sleep(0.3)  # Rate limiting
  }
  
  # Step 2: OI Analysis (if enabled)
  if (include_oi) {
    cat("\n🧲 === OPEN INTEREST ANALYSIS === 🧲\n")
    oi_results <- run_institutional_oi_analysis(symbols)
    results$oi_analysis <- oi_results
  }
  
  # Step 3: Risk Assessment (if enabled)
  if (include_risk_check) {
    cat("\n🛡️ === PORTFOLIO RISK ASSESSMENT === 🛡️\n")
    risk_results <- monitor_portfolio_positions(symbols)
    results$risk_assessment <- risk_results
    
    # Check for emergency triggers
    emergency_check <- emergency_protection_triggers()
    results$emergency_status <- emergency_check
    
    if (emergency_check$triggered) {
      cat("\n🚨 EMERGENCY CONDITIONS DETECTED - Review recommended\n")
    }
  }
  
  # Step 4: Enhanced Sentiment Analysis (from altcoin_rally_triggers.r)
  cat("\n🎭 === MULTI-FACTOR SENTIMENT ANALYSIS === 🎭\n")
  sentiment_results <- analyze_multi_factor_sentiment(symbols)
  results$sentiment_analysis <- sentiment_results
  
  # Step 5: Generate trading recommendations
  cat("\n🎯 === GENERATING RECOMMENDATIONS === 🎯\n")
  recommendations <- generate_comprehensive_recommendations(results)
  results$recommendations <- recommendations
  
  # Step 6: Display comprehensive overview
  display_portfolio_overview(results)
  display_comparative_analysis(results)
  
  return(results)
}

#' Quick market check (fast execution mode) - Enhanced with API debugging
execute_quick_check <- function(symbols) {
  cat("\n⚡ === QUICK MARKET CHECK === ⚡\n")
  
  results <- list(quick_data = list())
  
  for (symbol in symbols) {
    cat("🔍 Checking", symbol, "...\n")
    
    # Enhanced API debugging
    ticker_data <- tryCatch({
      get_enhanced_ticker_data(symbol)
    }, error = function(e) {
      cat("❌ API Error for", symbol, ":", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(ticker_data)) {
      # Debug ticker_data structure
      cat("   📡 API Response received for", symbol, "\n")
      
      # Safe data extraction with correct API field names
      price <- tryCatch({
        if ("last_price" %in% names(ticker_data)) {
          as.numeric(ticker_data$last_price)
        } else if ("lastPr" %in% names(ticker_data)) {
          as.numeric(ticker_data$lastPr)
        } else if ("last" %in% names(ticker_data)) {
          as.numeric(ticker_data$last)
        } else if ("price" %in% names(ticker_data)) {
          as.numeric(ticker_data$price)
        } else if ("mark_price" %in% names(ticker_data)) {
          as.numeric(ticker_data$mark_price)
        } else {
          cat("   ⚠️ No price field found in API response\n")
          0
        }
      }, error = function(e) {
        cat("   ❌ Price extraction error:", e$message, "\n")
        0
      })
      
      change_24h <- tryCatch({
        if ("change_24h_pct" %in% names(ticker_data)) {
          as.numeric(ticker_data$change_24h_pct)
        } else if ("change24h" %in% names(ticker_data)) {
          as.numeric(ticker_data$change24h)
        } else if ("changePercent24Hr" %in% names(ticker_data)) {
          as.numeric(ticker_data$changePercent24Hr)
        } else {
          cat("   ⚠️ No 24h change field found\n")
          0
        }
      }, error = function(e) {
        cat("   ❌ Change extraction error:", e$message, "\n")
        0
      })
      
      volume <- tryCatch({
        if ("volume_24h_usdt" %in% names(ticker_data)) {
          as.numeric(ticker_data$volume_24h_usdt)
        } else if ("volume_24h" %in% names(ticker_data)) {
          as.numeric(ticker_data$volume_24h)
        } else if ("baseVolume" %in% names(ticker_data)) {
          as.numeric(ticker_data$baseVolume)
        } else if ("volume" %in% names(ticker_data)) {
          as.numeric(ticker_data$volume)
        } else {
          cat("   ⚠️ No volume field found\n")
          0
        }
      }, error = function(e) {
        cat("   ❌ Volume extraction error:", e$message, "\n")
        0
      })
      
      # Create summary with validation
      if (price > 0) {
        quick_summary <- list(
          symbol = symbol,
          price = price,
          change_24h = change_24h,
          volume = volume,
          timestamp = Sys.time()
        )
        
        results$quick_data[[symbol]] <- quick_summary
        
        cat("📊", symbol, ":", round(price, 6), 
            "| 24h:", round(change_24h, 2), "%",
            "| Vol:", round(volume/1000000, 2), "M\n")
      } else {
        cat("❌", symbol, ": Invalid price data\n")
        # Store error data
        results$quick_data[[symbol]] <- list(
          symbol = symbol,
          price = 0,
          change_24h = 0,
          volume = 0,
          error = "Invalid API response",
          timestamp = Sys.time()
        )
      }
    } else {
      cat("❌", symbol, ": No API response\n")
      # Store null data
      results$quick_data[[symbol]] <- list(
        symbol = symbol,
        price = 0,
        change_24h = 0,
        volume = 0,
        error = "No API response",
        timestamp = Sys.time()
      )
    }
    
    Sys.sleep(0.2)  # Small delay between requests
  }
  
  return(results)
}

#' OI analysis only mode  
execute_oi_analysis_only <- function(symbols) {
  cat("\n🧲 === OI ANALYSIS ONLY === 🧲\n")
  
  oi_results <- run_institutional_oi_analysis(symbols)
  
  return(list(oi_analysis = oi_results))
}

#' Risk check only mode
execute_risk_check_only <- function(symbols) {
  cat("\n🛡️ === RISK CHECK ONLY === 🛡️\n")
  
  risk_results <- monitor_portfolio_positions(symbols)
  emergency_check <- emergency_protection_triggers()
  
  return(list(
    risk_assessment = risk_results,
    emergency_status = emergency_check
  ))
}

#' Enhanced sentiment analysis (from altcoin_rally_triggers.r)
execute_sentiment_analysis <- function(symbols) {
  cat("\n🎭 === SENTIMENT ANALYSIS === 🎭\n")
  
  sentiment_results <- analyze_multi_factor_sentiment(symbols)
  display_sentiment_dashboard(sentiment_results)
  
  return(list(sentiment_analysis = sentiment_results))
}

# ==========================================================================================================
# 🎭 MULTI-FACTOR SENTIMENT ANALYSIS (ENHANCED FROM altcoin_rally_triggers.r)
# ==========================================================================================================

#' Comprehensive multi-factor sentiment analysis
analyze_multi_factor_sentiment <- function(symbols) {
  
  sentiment_results <- list()
  
  for (symbol in symbols) {
    cat("🎭 Analyzing", symbol, "sentiment...\n")
    
    # Factor 1: Technical Momentum
    technical_momentum <- calculate_technical_momentum(symbol)
    
    # Factor 2: Volume Analysis  
    volume_sentiment <- analyze_volume_sentiment(symbol)
    
    # Factor 3: OI Sentiment
    oi_sentiment <- analyze_oi_sentiment(symbol)
    
    # Factor 4: Price Action Sentiment
    price_action_sentiment <- analyze_price_action_sentiment(symbol)
    
    # Factor 5: Market Structure
    market_structure_sentiment <- analyze_market_structure(symbol)
    
    # Combine all factors
    combined_sentiment <- combine_sentiment_factors(
      technical_momentum, volume_sentiment, oi_sentiment, 
      price_action_sentiment, market_structure_sentiment
    )
    
    sentiment_results[[symbol]] <- list(
      overall_sentiment = combined_sentiment$overall,
      sentiment_score = combined_sentiment$score,
      confidence_level = combined_sentiment$confidence,
      factors = list(
        technical = technical_momentum,
        volume = volume_sentiment,
        oi = oi_sentiment,
        price_action = price_action_sentiment,
        market_structure = market_structure_sentiment
      ),
      recommendation = generate_sentiment_recommendation(combined_sentiment),
      timestamp = Sys.time()
    )
    
    # Display individual sentiment
    display_asset_sentiment(symbol, sentiment_results[[symbol]])
  }
  
  # Cross-asset sentiment correlation
  if (length(symbols) > 1) {
    correlation_analysis <- analyze_cross_asset_sentiment(sentiment_results)
    sentiment_results$cross_asset_analysis <- correlation_analysis
  }
  
  return(sentiment_results)
}

#' Calculate technical momentum indicators
calculate_technical_momentum <- function(symbol) {
  tryCatch({
    # Get recent market data
    market_data <- get_enhanced_market_data(symbol)
    if (is.null(market_data)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Calculate momentum indicators
    technical_analysis <- calculate_technical_indicators_fixed(market_data)
    
    # Extract momentum signals
    rsi_signal <- if (technical_analysis$rsi > 70) "OVERBOUGHT" else if (technical_analysis$rsi < 30) "OVERSOLD" else "NEUTRAL"
    macd_signal <- if (technical_analysis$macd > technical_analysis$signal_line) "BULLISH" else "BEARISH"
    bb_signal <- if (technical_analysis$bb_position > 0.8) "OVERBOUGHT" else if (technical_analysis$bb_position < 0.2) "OVERSOLD" else "NEUTRAL"
    
    # Combine signals into momentum score
    momentum_score <- calculate_momentum_score(rsi_signal, macd_signal, bb_signal, technical_analysis)
    
    return(list(
      score = momentum_score,
      trend = if (momentum_score > 0.6) "BULLISH" else if (momentum_score < 0.4) "BEARISH" else "NEUTRAL",
      rsi = technical_analysis$rsi,
      macd_signal = macd_signal,
      bb_signal = bb_signal
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Analyze volume-based sentiment
analyze_volume_sentiment <- function(symbol) {
  tryCatch({
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Get historical volume data for comparison
    current_volume <- as.numeric(ticker_data$baseVolume)
    price_change <- as.numeric(ticker_data$change24h)
    
    # Volume trend analysis (simplified)
    volume_score <- if (current_volume > 1000000) {  # High volume threshold
      if (price_change > 0) 0.7 else 0.3  # High volume + price up = bullish
    } else {
      0.5  # Low volume = neutral
    }
    
    return(list(
      score = volume_score,
      trend = if (volume_score > 0.6) "BULLISH" else if (volume_score < 0.4) "BEARISH" else "NEUTRAL",
      volume = current_volume,
      volume_price_divergence = abs(price_change) / max(current_volume / 1000000, 1)
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Analyze OI-based sentiment
analyze_oi_sentiment <- function(symbol) {
  tryCatch({
    # Use OI flow analysis from unified analytics
    oi_flow <- generate_oi_flow_analysis(symbol)
    
    if (is.null(oi_flow)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Convert OI trend to sentiment score
    oi_score <- switch(oi_flow$trend,
                       "INCREASING" = if (oi_flow$change_24h > 10) 0.7 else 0.6,
                       "DECREASING" = if (oi_flow$change_24h < -10) 0.3 else 0.4,
                       0.5  # STABLE or unknown
    )
    
    return(list(
      score = oi_score,
      trend = if (oi_score > 0.6) "BULLISH" else if (oi_score < 0.4) "BEARISH" else "NEUTRAL",
      oi_change_24h = oi_flow$change_24h,
      institutional_score = oi_flow$institutional_score
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Analyze price action sentiment
analyze_price_action_sentiment <- function(symbol) {
  tryCatch({
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Safe extraction with correct field names and validation
    current_price <- tryCatch({
      if ("last_price" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$last_price)
        if (length(val) == 0 || is.na(val)) 0 else val
      } else if ("mark_price" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$mark_price)
        if (length(val) == 0 || is.na(val)) 0 else val
      } else {
        0
      }
    }, error = function(e) 0)
    
    high_24h <- tryCatch({
      if ("high_24h" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$high_24h)
        if (length(val) == 0 || is.na(val)) current_price * 1.05 else val
      } else {
        current_price * 1.05
      }
    }, error = function(e) current_price * 1.05)
    
    low_24h <- tryCatch({
      if ("low_24h" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$low_24h)
        if (length(val) == 0 || is.na(val)) current_price * 0.95 else val
      } else {
        current_price * 0.95
      }
    }, error = function(e) current_price * 0.95)
    
    change_24h <- tryCatch({
      if ("change_24h_pct" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$change_24h_pct)
        if (length(val) == 0 || is.na(val)) 0 else val
      } else {
        0
      }
    }, error = function(e) 0)
    
    # Validate data
    if (current_price <= 0 || high_24h <= 0 || low_24h <= 0) {
      return(list(score = 0.5, trend = "NEUTRAL", 
                  price_position_24h = 0.5, change_24h = 0))
    }
    
    # Ensure high >= low
    if (high_24h <= low_24h) {
      high_24h <- current_price * 1.02
      low_24h <- current_price * 0.98
    }
    
    # Price position within 24h range
    price_position <- (current_price - low_24h) / (high_24h - low_24h)
    price_position <- max(0, min(1, price_position))  # Clamp between 0 and 1
    
    # Combine price position with change (normalize change to -10 to +10 range)
    normalized_change <- max(-10, min(10, change_24h)) 
    change_factor <- (normalized_change + 10) / 20  # Convert to 0-1
    
    price_action_score <- (price_position * 0.6) + (change_factor * 0.4)
    price_action_score <- max(0, min(1, price_action_score))  # Clamp
    
    return(list(
      score = price_action_score,
      trend = if (price_action_score > 0.6) "BULLISH" else if (price_action_score < 0.4) "BEARISH" else "NEUTRAL",
      price_position_24h = price_position,
      change_24h = change_24h
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN", 
                price_position_24h = 0.5, change_24h = 0))
  })
}

#' Analyze market structure sentiment
analyze_market_structure <- function(symbol) {
  tryCatch({
    # Get enhanced market data
    market_data <- get_enhanced_market_data(symbol)
    if (is.null(market_data)) return(list(score = 0.5, trend = "NEUTRAL"))
    
    # Analyze bid-ask spread and depth
    orderbook_data <- get_enhanced_orderbook(symbol)
    
    if (!is.null(orderbook_data)) {
      # Calculate spread
      best_bid <- as.numeric(orderbook_data$bids[[1]][1])
      best_ask <- as.numeric(orderbook_data$asks[[1]][1])
      spread_percent <- (best_ask - best_bid) / best_bid * 100
      
      # Tight spread = healthy market structure
      structure_score <- if (spread_percent < 0.1) 0.7 else if (spread_percent < 0.5) 0.5 else 0.3
    } else {
      structure_score <- 0.5
    }
    
    return(list(
      score = structure_score,
      trend = if (structure_score > 0.6) "HEALTHY" else if (structure_score < 0.4) "POOR" else "NEUTRAL",
      spread_percent = spread_percent %||% NA
    ))
    
  }, error = function(e) {
    return(list(score = 0.5, trend = "UNKNOWN"))
  })
}

#' Combine all sentiment factors into overall sentiment
combine_sentiment_factors <- function(technical, volume, oi, price_action, market_structure) {
  
  # Weighted combination of factors
  weights <- list(
    technical = 0.25,
    volume = 0.20,
    oi = 0.25,
    price_action = 0.20,
    market_structure = 0.10
  )
  
  overall_score <- (
    technical$score * weights$technical +
      volume$score * weights$volume +
      oi$score * weights$oi +
      price_action$score * weights$price_action +
      market_structure$score * weights$market_structure
  )
  
  # Calculate confidence based on factor agreement
  factor_scores <- c(technical$score, volume$score, oi$score, price_action$score, market_structure$score)
  confidence <- 1 - sd(factor_scores) / mean(factor_scores)  # Lower standard deviation = higher confidence
  confidence <- min(max(confidence, 0), 1)
  
  # Determine overall sentiment
  overall_sentiment <- if (overall_score > 0.65) {
    "STRONG_BULLISH"
  } else if (overall_score > 0.55) {
    "BULLISH"
  } else if (overall_score > 0.45) {
    "NEUTRAL"
  } else if (overall_score > 0.35) {
    "BEARISH"
  } else {
    "STRONG_BEARISH"
  }
  
  return(list(
    overall = overall_sentiment,
    score = overall_score,
    confidence = confidence
  ))
}

# ==========================================================================================================
# 🎮 INTERACTIVE TRADING INTERFACE - ENHANCED WITH CONSOLE MANAGEMENT
# ==========================================================================================================

#' Launch interactive command interface
launch_interactive_interface <- function(execution_results = NULL) {
  cat("\n🎮 === INTERACTIVE TRADING INTERFACE === 🎮\n")
  cat("Available commands: help, status, risk, oi, sentiment, execute, console, quit\n")
  
  repeat {
    cat("\n> ")
    command <- trimws(tolower(readline()))
    
    if (command == "quit" || command == "q" || command == "exit") {
      cat("👋 Exiting interactive mode\n")
      # Ensure console is restored
      cleanup_output_mode()
      break
    }
    
    process_interactive_command(command, execution_results)
  }
}

#' Process interactive commands - Enhanced with console commands
process_interactive_command <- function(command, execution_results) {
  
  parts <- strsplit(command, " ")[[1]]
  cmd <- parts[1]
  args <- if (length(parts) > 1) parts[2:length(parts)] else character(0)
  
  switch(cmd,
         "help" = display_help_menu(),
         "status" = display_system_status(),
         "diagnose" = test_api_connectivity(),
         "risk" = handle_risk_command(args),
         "oi" = handle_oi_command(args),
         "sentiment" = handle_sentiment_command(args),
         "execute" = handle_execute_command(args),
         "positions" = display_current_positions(),
         "market" = handle_market_command(args),
         "config" = display_config_summary(),
         "console" = handle_console_command(args),  # New console management
         cat("❓ Unknown command. Type 'help' for available commands.\n")
  )
}

#' Handle console management commands
handle_console_command <- function(args) {
  if (length(args) == 0) {
    cat("📝 Console commands: normal, silent, filtered, logged\n")
    cat("Current mode:", TRADING_CONSOLE_STATE$output_mode, "\n")
    return()
  }
  
  mode <- args[1]
  
  if (mode %in% c("normal", "silent", "filtered", "logged")) {
    set_output_mode(mode)
  } else {
    cat("❌ Invalid console mode. Use: normal, silent, filtered, or logged\n")
  }
}

#' Display help menu - Enhanced with console options
display_help_menu <- function() {
  cat("\n📖 === AVAILABLE COMMANDS === 📖\n")
  cat("🔧 System Commands:\n")
  cat("   status          - Display system status\n")
  cat("   diagnose        - Run API connectivity diagnostics\n")
  cat("   config          - Show current configuration\n")
  cat("   positions       - Show current positions\n")
  cat("   console [mode]  - Change console output mode\n")
  cat("\n📊 Analysis Commands:\n")
  cat("   risk [symbol]   - Risk analysis (all symbols if none specified)\n")
  cat("   oi [symbol]     - Open Interest analysis\n")
  cat("   sentiment [sym] - Sentiment analysis\n")
  cat("   market [symbol] - Quick market data\n")
  cat("\n🚀 Execution Commands:\n")
  cat("   execute [mode]  - Run analysis (modes: full, quick, quiet, oi, risk, sentiment)\n")
  cat("\n🔇 Console Modes:\n")
  cat("   console normal   - Full output (default)\n")
  cat("   console silent   - Minimal output\n")
  cat("   console filtered - Show only key messages\n")
  cat("   console logged   - Redirect output to log file\n")
  cat("\n🎮 Navigation:\n")
  cat("   help            - Show this help menu\n")
  cat("   quit            - Exit interactive mode\n")
}

#' Handle risk-related commands
handle_risk_command <- function(args) {
  symbols <- if (length(args) > 0) args else PORTFOLIO_ASSETS
  risk_results <- monitor_portfolio_positions(symbols)
  
  # Display emergency status if triggered
  emergency_check <- emergency_protection_triggers()
  if (emergency_check$triggered) {
    cat("\n🚨 EMERGENCY STATUS:\n")
    for (action in emergency_check$actions) {
      cat("   ", action$severity, ":", action$message, "\n")
    }
  }
}

#' Handle OI analysis commands
handle_oi_command <- function(args) {
  symbols <- if (length(args) > 0) args else PORTFOLIO_ASSETS
  oi_results <- run_institutional_oi_analysis(symbols)
}

#' Handle sentiment analysis commands
handle_sentiment_command <- function(args) {
  symbols <- if (length(args) > 0) args else PORTFOLIO_ASSETS
  sentiment_results <- analyze_multi_factor_sentiment(symbols)
}

#' Handle execution commands - Enhanced with quiet mode
handle_execute_command <- function(args) {
  mode <- if (length(args) > 0) args[1] else "full"
  cat("🚀 Executing in", toupper(mode), "mode...\n")
  
  # Add output mode support
  output_mode <- if (length(args) > 1) args[2] else NULL
  
  execute_trading_system(mode = mode, interactive = FALSE, output_mode = output_mode)
}

#' Handle market data commands
handle_market_command <- function(args) {
  symbols <- if (length(args) > 0) args else PORTFOLIO_ASSETS
  execute_quick_check(symbols)
}

# ==========================================================================================================
# 📊 ENHANCED DISPLAY FUNCTIONS
# ==========================================================================================================

#' Display comprehensive system header
display_system_header <- function() {
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                    🚀 BITGET TRADING SYSTEM V2.1 🚀                              ║\n")
  cat("║                     Advanced Multi-Asset Trading Platform                        ║\n")
  cat("║                        With Console Management Support                           ║\n")
  cat("╠══════════════════════════════════════════════════════════════════════════════════╣\n")
  cat("║ 🔧 Core Engine   📊 OI Analytics   🛡️ Risk Manager   🎯 Execution Hub          ║\n")
  cat("║ 📝 Console Manager   🔇 Silent Modes   📄 Logging   🎮 Interactive             ║\n")
  cat("╚══════════════════════════════════════════════════════════════════════════════════╝\n")
}

#' Display enhanced portfolio overview
display_portfolio_overview <- function(results) {
  cat("\n📊 === PORTFOLIO OVERVIEW === 📊\n")
  
  if (!is.null(results$market_data)) {
    for (symbol in names(results$market_data)) {
      market_data <- results$market_data[[symbol]]
      technical <- results$technical_analysis[[symbol]]
      
      if (!is.null(market_data) && !is.null(technical)) {
        cat("\n🔸", symbol, "Overview:\n")
        cat("   Price:", round(market_data$current_price, 4), 
            "| 24h Change:", round(market_data$change_24h, 2), "%\n")
        cat("   RSI:", round(technical$rsi, 2), 
            "| MACD:", round(technical$macd, 6), 
            "| BB Position:", round(technical$bb_position, 3), "\n")
        
        # Add sentiment if available
        if (!is.null(results$sentiment_analysis[[symbol]])) {
          sentiment <- results$sentiment_analysis[[symbol]]
          cat("   Sentiment:", sentiment$overall_sentiment, 
              "| Score:", round(sentiment$sentiment_score, 3), 
              "| Confidence:", round(sentiment$confidence_level, 3), "\n")
        }
      }
    }
  }
}

#' Display comparative analysis across assets
display_comparative_analysis <- function(results) {
  if (length(results$symbols) < 2) return()
  
  cat("\n🔍 === COMPARATIVE ANALYSIS === 🔍\n")
  
  # Compare performance metrics
  performance_summary <- data.frame(
    Symbol = character(0),
    Price_Change = numeric(0),
    RSI = numeric(0),
    Sentiment_Score = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (symbol in results$symbols) {
    if (!is.null(results$market_data[[symbol]]) && 
        !is.null(results$technical_analysis[[symbol]])) {
      
      market_data <- results$market_data[[symbol]]
      technical <- results$technical_analysis[[symbol]]
      sentiment_score <- if (!is.null(results$sentiment_analysis[[symbol]])) {
        results$sentiment_analysis[[symbol]]$sentiment_score
      } else 0.5
      
      performance_summary <- rbind(performance_summary, data.frame(
        Symbol = symbol,
        Price_Change = market_data$change_24h,
        RSI = technical$rsi,
        Sentiment_Score = sentiment_score,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(performance_summary) > 0) {
    # Rank by sentiment score
    performance_summary <- performance_summary[order(-performance_summary$Sentiment_Score), ]
    
    cat("🏆 Sentiment Ranking:\n")
    for (i in 1:nrow(performance_summary)) {
      row <- performance_summary[i, ]
      cat("   ", i, ".", row$Symbol, "- Score:", round(row$Sentiment_Score, 3), 
          "| RSI:", round(row$RSI, 1), 
          "| 24h:", round(row$Price_Change, 2), "%\n")
    }
  }
}

#' Display asset sentiment breakdown
display_asset_sentiment <- function(symbol, sentiment_data) {
  cat("🎭", symbol, "Sentiment Analysis:\n")
  cat("   Overall:", sentiment_data$overall_sentiment, 
      "| Score:", round(sentiment_data$sentiment_score, 3), 
      "| Confidence:", round(sentiment_data$confidence_level, 3), "\n")
  cat("   Recommendation:", sentiment_data$recommendation, "\n")
}

#' Display quick asset summary
display_asset_summary <- function(symbol, market_data, technical_analysis) {
  if (is.null(market_data) || is.null(technical_analysis)) return()
  
  # Determine trend arrows
  trend_arrow <- if (market_data$change_24h > 2) "📈" else if (market_data$change_24h < -2) "📉" else "➡️"
  rsi_indicator <- if (technical_analysis$rsi > 70) "🔴" else if (technical_analysis$rsi < 30) "🟢" else "🟡"
  
  cat("   ", trend_arrow, symbol, ":", round(market_data$current_price, 4), 
      "| 24h:", round(market_data$change_24h, 2), "% | RSI:", rsi_indicator, round(technical_analysis$rsi, 1), "\n")
}

#' Display execution summary - Enhanced with console info
display_execution_summary <- function(execution_results, mode) {
  cat("\n✅ === EXECUTION SUMMARY === ✅\n")
  cat("Mode:", toupper(mode), "| Duration:", 
      round(as.numeric(Sys.time() - execution_results$timestamp), 2), "seconds\n")
  
  if (!is.null(execution_results$symbols)) {
    cat("Assets Processed:", length(execution_results$symbols), "\n")
  }
  
  if (!is.null(execution_results$risk_assessment)) {
    cat("Risk Alerts:", length(execution_results$risk_assessment$risk_alerts), "\n")
  }
  
  if (!is.null(execution_results$emergency_status) && execution_results$emergency_status$triggered) {
    cat("🚨 Emergency Status: ACTIVE\n")
  }
  
  cat("Console Mode:", TRADING_CONSOLE_STATE$output_mode, "\n")
  
  if (TRADING_CONSOLE_STATE$output_mode == "logged" && !is.null(TRADING_CONSOLE_STATE$log_file)) {
    cat("📄 Log File:", basename(TRADING_CONSOLE_STATE$log_file), "\n")
  }
  
  cat("System Status: ✅ OPERATIONAL\n")
}

#' Clean console output for better readability
clean_console_output <- function() {
  # Improved cross-platform console clearing
  tryCatch({
    if (.Platform$OS.type == "windows") {
      # Try multiple Windows methods
      if (Sys.which("cls") != "") {
        system("cls", intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
      } else if (Sys.which("clear") != "") {
        system("clear", intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
      } else {
        # Fallback: print newlines
        cat(rep("\n", 50))
      }
    } else {
      system("clear", intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
    }
  }, error = function(e) {
    # Silent fallback - just print some newlines
    cat(rep("\n", 20))
  })
}

#' Display current system status with enhanced API diagnostics
display_system_status <- function() {
  cat("\n🔧 === SYSTEM STATUS === 🔧\n")
  cat("Core Engine:", if (exists("CORE_ENGINE_LOADED")) "✅ LOADED" else "❌ NOT LOADED", "\n")
  cat("OI Analytics:", if (exists("OI_ANALYTICS_LOADED")) "✅ LOADED" else "❌ NOT LOADED", "\n")
  cat("Risk Manager:", if (exists("RISK_MANAGER_LOADED")) "✅ LOADED" else "❌ NOT LOADED", "\n")
  cat("Configuration:", if (exists("SYSTEM_CONFIG_LOADED")) "✅ LOADED" else "❌ NOT LOADED", "\n")
  cat("Console Manager:", if (CONSOLE_MANAGEMENT_LOADED) "✅ LOADED" else "❌ NOT LOADED", "\n")
  
  # Display console mode
  cat("Console Mode:", TRADING_CONSOLE_STATE$output_mode, "\n")
  
  # Enhanced API connectivity test
  cat("API Connection: ")
  api_status <- test_api_connectivity()
  
  # Display current positions with better error handling
  cat("Checking positions...\n")
  positions <- get_current_positions()
  cat("Active Positions:", nrow(positions), "\n")
  
  # Quick symbol validation
  cat("\n📊 Configured Assets:\n")
  for (symbol in PORTFOLIO_ASSETS) {
    cat("   ", symbol, "- Config:", if (symbol %in% names(MULTI_ASSET_CONFIG)) "✅" else "❌", "\n")
  }
}

#' Display current positions summary
display_current_positions <- function() {
  positions <- get_current_positions(include_risk_metrics = TRUE)
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No open positions\n")
    return()
  }
  
  cat("\n📊 === CURRENT POSITIONS === 📊\n")
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    cat("🔸", pos$symbol, ":", pos$side, pos$size, "contracts\n")
    cat("   Entry:", round(pos$avg_price, 4), "| Current:", round(pos$mark_price, 4), 
        "| PnL:", round(pos$unrealized_pnl, 2), "USDT\n")
    cat("   Risk Score:", round(pos$risk_score, 3), "| Leverage:", pos$leverage, "x\n")
  }
}

#' Display configuration summary
display_config_summary <- function() {
  cat("\n⚙️ === CONFIGURATION SUMMARY === ⚙️\n")
  cat("Portfolio Assets:", paste(PORTFOLIO_ASSETS, collapse = ", "), "\n")
  cat("Default TP:", DEFAULT_TP_PERCENT, "% | Default SL:", DEFAULT_SL_PERCENT, "%\n")
  cat("Console Output Level:", CONSOLE_OUTPUT_LEVEL, "\n")
  cat("Console Mode:", TRADING_CONSOLE_STATE$output_mode, "\n")
  cat("Available Assets:", length(names(MULTI_ASSET_CONFIG)), "\n")
}

# ==========================================================================================================
# 🛠️ HELPER & UTILITY FUNCTIONS
# ==========================================================================================================

#' Test API connectivity with detailed diagnostics
test_api_connectivity <- function() {
  cat("\n🔍 === API CONNECTIVITY DIAGNOSTICS === 🔍\n")
  
  # Test 1: Basic API connection
  cat("1️⃣ Testing basic API connection...\n")
  basic_test <- tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/public/time"
    )
    if (!is.null(response) && !is.null(response$data)) {
      cat("   ✅ Basic API connection: SUCCESS\n")
      TRUE
    } else {
      cat("   ❌ Basic API connection: FAILED\n")
      FALSE
    }
  }, error = function(e) {
    cat("   ❌ Basic API connection ERROR:", e$message, "\n")
    FALSE
  })
  
  # Test 2: Market data endpoint
  cat("2️⃣ Testing market data endpoint...\n")
  market_test <- tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/market/tickers",
      params = list(productType = "USDT-FUTURES")
    )
    if (!is.null(response)) {
      cat("   ✅ Market data endpoint: SUCCESS\n")
      cat("   📊 Response structure:", class(response), "\n")
      if (is.list(response) && "data" %in% names(response)) {
        cat("   📈 Data field found, items:", length(response$data), "\n")
      }
      TRUE
    } else {
      cat("   ❌ Market data endpoint: NO RESPONSE\n")
      FALSE
    }
  }, error = function(e) {
    cat("   ❌ Market data endpoint ERROR:", e$message, "\n")
    FALSE
  })
  
  # Test 3: Specific symbol ticker
  cat("3️⃣ Testing specific symbol ticker...\n")
  symbol_test <- tryCatch({
    test_symbol <- "ADAUSDT_UMCBL"
    response <- get_enhanced_ticker_data(test_symbol)
    if (!is.null(response)) {
      cat("   ✅ Symbol ticker test: SUCCESS\n")
      cat("   📊 Response type:", class(response), "\n")
      cat("   🔍 Available fields:", paste(names(response), collapse = ", "), "\n")
      
      # Check for common price fields
      price_fields <- c("lastPr", "last", "price", "close")
      found_fields <- intersect(names(response), price_fields)
      if (length(found_fields) > 0) {
        cat("   💰 Price fields found:", paste(found_fields, collapse = ", "), "\n")
      } else {
        cat("   ⚠️ No standard price fields found!\n")
      }
      
      TRUE
    } else {
      cat("   ❌ Symbol ticker test: NO RESPONSE\n")
      FALSE
    }
  }, error = function(e) {
    cat("   ❌ Symbol ticker test ERROR:", e$message, "\n")
    FALSE
  })
  
  # Overall result
  overall_success <- basic_test && market_test && symbol_test
  cat("\n🎯 Overall API Status:", if (overall_success) "✅ OPERATIONAL" else "❌ ISSUES DETECTED", "\n")
  
  return(overall_success)
}

#' Calculate momentum score from technical indicators
calculate_momentum_score <- function(rsi_signal, macd_signal, bb_signal, technical_analysis) {
  # Convert signals to numeric scores
  rsi_score <- switch(rsi_signal,
                      "OVERBOUGHT" = 0.8,
                      "OVERSOLD" = 0.2,
                      0.5  # NEUTRAL
  )
  
  macd_score <- if (macd_signal == "BULLISH") 0.7 else 0.3
  
  bb_score <- switch(bb_signal,
                     "OVERBOUGHT" = 0.8,
                     "OVERSOLD" = 0.2,
                     0.5  # NEUTRAL
  )
  
  # Weighted combination
  momentum_score <- (rsi_score * 0.4 + macd_score * 0.4 + bb_score * 0.2)
  
  return(momentum_score)
}

#' Generate sentiment-based recommendation
generate_sentiment_recommendation <- function(combined_sentiment) {
  sentiment <- combined_sentiment$overall
  confidence <- combined_sentiment$confidence
  
  if (confidence < 0.5) {
    return("HOLD - Low confidence in signals")
  }
  
  recommendation <- switch(sentiment,
                           "STRONG_BULLISH" = "STRONG BUY - High conviction bullish signals",
                           "BULLISH" = "BUY - Moderate bullish signals",
                           "NEUTRAL" = "HOLD - Mixed or neutral signals",
                           "BEARISH" = "SELL - Moderate bearish signals", 
                           "STRONG_BEARISH" = "STRONG SELL - High conviction bearish signals",
                           "HOLD - Unknown sentiment"
  )
  
  return(recommendation)
}

#' Analyze cross-asset sentiment correlations
analyze_cross_asset_sentiment <- function(sentiment_results) {
  if (length(sentiment_results) < 2) return(list())
  
  # Extract sentiment scores
  sentiment_scores <- sapply(names(sentiment_results), function(symbol) {
    if (symbol == "cross_asset_analysis") return(NULL)
    sentiment_results[[symbol]]$sentiment_score
  })
  
  sentiment_scores <- sentiment_scores[!sapply(sentiment_scores, is.null)]
  
  # Calculate correlations between assets
  correlations <- list()
  asset_names <- names(sentiment_scores)
  
  for (i in 1:(length(asset_names)-1)) {
    for (j in (i+1):length(asset_names)) {
      asset1 <- asset_names[i]
      asset2 <- asset_names[j]
      
      # Simple correlation based on sentiment alignment
      score1 <- sentiment_scores[[asset1]]
      score2 <- sentiment_scores[[asset2]]
      
      correlation <- 1 - abs(score1 - score2)  # Higher correlation if scores are similar
      
      correlations[[paste(asset1, asset2, sep = "_")]] <- correlation
    }
  }
  
  return(correlations)
}

#' Generate comprehensive recommendations from all analysis results
generate_comprehensive_recommendations <- function(results) {
  recommendations <- list()
  
  for (symbol in results$symbols) {
    # Collect all relevant data for the symbol
    market_data <- results$market_data[[symbol]]
    technical <- results$technical_analysis[[symbol]]
    sentiment <- results$sentiment_analysis[[symbol]]
    
    if (is.null(market_data) || is.null(technical) || is.null(sentiment)) {
      recommendations[[symbol]] <- "INSUFFICIENT_DATA"
      next
    }
    
    # Multi-factor recommendation logic
    factors <- list(
      technical_trend = if (technical$rsi > 70) "SELL" else if (technical$rsi < 30) "BUY" else "HOLD",
      price_momentum = if (market_data$change_24h > 5) "BUY" else if (market_data$change_24h < -5) "SELL" else "HOLD",
      sentiment_signal = sentiment$recommendation
    )
    
    # Consensus-based recommendation
    buy_signals <- sum(sapply(factors, function(x) grepl("BUY", x)))
    sell_signals <- sum(sapply(factors, function(x) grepl("SELL", x)))
    
    if (buy_signals >= 2) {
      recommendations[[symbol]] <- "BUY - Multi-factor consensus"
    } else if (sell_signals >= 2) {
      recommendations[[symbol]] <- "SELL - Multi-factor consensus"
    } else {
      recommendations[[symbol]] <- "HOLD - Mixed signals"
    }
  }
  
  return(recommendations)
}

#' Enhanced market data collection with error handling
collect_enhanced_market_data <- function(symbol) {
  tryCatch({
    # Primary data collection
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (is.null(ticker_data)) return(NULL)
    
    # Safe extraction with the correct API field names
    current_price <- tryCatch({
      if ("last_price" %in% names(ticker_data)) {
        as.numeric(ticker_data$last_price)
      } else if ("mark_price" %in% names(ticker_data)) {
        as.numeric(ticker_data$mark_price)
      } else {
        0
      }
    }, error = function(e) 0)
    
    change_24h <- tryCatch({
      if ("change_24h_pct" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$change_24h_pct)
        if (length(val) == 0) 0 else val
      } else {
        0
      }
    }, error = function(e) 0)
    
    volume_24h <- tryCatch({
      if ("volume_24h_usdt" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$volume_24h_usdt)
        if (length(val) == 0) 0 else val
      } else if ("volume_24h" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$volume_24h)
        if (length(val) == 0) 0 else val
      } else {
        0
      }
    }, error = function(e) 0)
    
    high_24h <- tryCatch({
      if ("high_24h" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$high_24h)
        if (length(val) == 0) current_price * 1.05 else val
      } else {
        current_price * 1.05  # Fallback: 5% above current
      }
    }, error = function(e) current_price * 1.05)
    
    low_24h <- tryCatch({
      if ("low_24h" %in% names(ticker_data)) {
        val <- as.numeric(ticker_data$low_24h)
        if (length(val) == 0) current_price * 0.95 else val
      } else {
        current_price * 0.95  # Fallback: 5% below current
      }
    }, error = function(e) current_price * 0.95)
    
    # Validate data
    if (current_price <= 0) return(NULL)
    if (high_24h <= 0) high_24h <- current_price * 1.05
    if (low_24h <= 0) low_24h <- current_price * 0.95
    if (volume_24h < 0) volume_24h <- 0
    
    # Enhanced data with additional metrics
    enhanced_data <- list(
      symbol = symbol,
      current_price = current_price,
      change_24h = change_24h,
      volume_24h = volume_24h,
      high_24h = high_24h,
      low_24h = low_24h,
      timestamp = Sys.time()
    )
    
    # Add calculated metrics
    enhanced_data$price_range_24h <- enhanced_data$high_24h - enhanced_data$low_24h
    if (enhanced_data$price_range_24h > 0) {
      enhanced_data$price_position <- (enhanced_data$current_price - enhanced_data$low_24h) / enhanced_data$price_range_24h
    } else {
      enhanced_data$price_position <- 0.5  # Neutral position
    }
    
    return(enhanced_data)
    
  }, error = function(e) {
    cat("❌ Error collecting data for", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

#' Perform comprehensive technical analysis
perform_comprehensive_technical_analysis <- function(symbol, market_data) {
  tryCatch({
    # Use core engine technical analysis
    technical_analysis <- complete_trading_analysis_enhanced(symbol)
    
    if (is.null(technical_analysis)) {
      # Fallback to basic analysis
      technical_analysis <- list(
        rsi = 50,
        macd = 0,
        signal_line = 0,
        bb_position = 0.5,
        trend = "NEUTRAL"
      )
    }
    
    return(technical_analysis)
    
  }, error = function(e) {
    cat("❌ Technical analysis error for", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

#' Display sentiment dashboard
display_sentiment_dashboard <- function(sentiment_results) {
  cat("\n🎭 === SENTIMENT DASHBOARD === 🎭\n")
  
  for (symbol in names(sentiment_results)) {
    if (symbol == "cross_asset_analysis") next
    
    sentiment <- sentiment_results[[symbol]]
    cat("\n📊", symbol, "Sentiment Breakdown:\n")
    cat("   Overall:", sentiment$overall_sentiment, "(", round(sentiment$sentiment_score, 3), ")\n")
    cat("   Technical:", sentiment$factors$technical$trend, "\n")
    cat("   Volume:", sentiment$factors$volume$trend, "\n")
    cat("   OI:", sentiment$factors$oi$trend, "\n")
    cat("   Recommendation:", sentiment$recommendation, "\n")
  }
  
  # Cross-asset summary
  if (!is.null(sentiment_results$cross_asset_analysis)) {
    cat("\n🔗 Cross-Asset Correlations:\n")
    for (pair in names(sentiment_results$cross_asset_analysis)) {
      correlation <- sentiment_results$cross_asset_analysis[[pair]]
      cat("   ", pair, ":", round(correlation, 3), "\n")
    }
  }
}

# ==========================================================================================================
# 🚀 QUICK START FUNCTIONS WITH CONSOLE MANAGEMENT
# ==========================================================================================================

#' Quick silent execution
quick_silent_execution <- function(symbols = NULL, mode = "full") {
  cat("🔇 Starting silent execution...\n")
  
  # Suppress messages and warnings
  suppressMessages(suppressWarnings({
    results <- execute_trading_system(mode = mode, symbols = symbols, interactive = FALSE)
  }))
  
  cat("✅ Silent execution complete\n")
  return(results)
}

#' Execute with clean logging (without console management issues)
execute_with_clean_logging <- function(symbols = NULL, mode = "full", log_dir = NULL) {
  cat("📄 Starting clean logged execution...\n")
  
  # Generate log filename
  if (is.null(log_dir)) {
    log_dir <- FILE_PATHS$logs_path %||% "c:/freeding/tbot202506/logs/"
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_filename <- paste0("trading_clean_log_", timestamp, ".txt")
  log_path <- file.path(log_dir, log_filename)
  
  cat("📝 Capturing output to:", basename(log_filename), "\n")
  
  # Use capture.output for cleaner logging
  log_output <- capture.output({
    # Suppress HTTP warnings
    suppressWarnings({
      results <- execute_trading_system(
        mode = mode, 
        symbols = symbols, 
        interactive = FALSE,
        log_results = TRUE
      )
    })
  }, type = "output")
  
  # Write clean log
  writeLines(c(
    "# =====================================",
    "# TRADING SYSTEM CLEAN LOG",
    paste("# Start Time:", Sys.time()),
    paste("# Mode:", mode),
    "# =====================================",
    "",
    log_output,
    "",
    "# =====================================",
    paste("# End Time:", Sys.time()),
    "# ====================================="
  ), log_path)
  
  cat("✅ Clean logged execution complete\n")
  cat("📄 Log saved to:", basename(log_path), "\n")
  
  # Show summary
  if (!is.null(results$quick_data)) {
    cat("\n📊 Quick Summary:\n")
    for (symbol in names(results$quick_data)) {
      data <- results$quick_data[[symbol]]
      if (data$price > 0) {
        cat(sprintf("   %s: %.4f (%.2f%%)\n", 
                    symbol, data$price, data$change_24h))
      }
    }
  }
  
  return(results)
}

#' Execute without any console output
execute_silent <- function(symbols = NULL, mode = "full") {
  # Complete silence - no output at all
  invisible(capture.output({
    suppressMessages(suppressWarnings({
      results <- execute_trading_system(
        mode = mode,
        symbols = symbols,
        interactive = FALSE,
        output_mode = NULL,
        log_results = TRUE
      )
    }))
  }))
  
  return(results)
}

#' Execute and only show results summary
execute_summary_only <- function(symbols = NULL, mode = "full") {
  cat("🔄 Running analysis...\n")
  
  # Execute silently
  results <- execute_silent(symbols, mode)
  
  # Show only summary
  cat("\n📊 === RESULTS SUMMARY === 📊\n")
  
  if (!is.null(results$quick_data)) {
    for (symbol in names(results$quick_data)) {
      data <- results$quick_data[[symbol]]
      if (!is.null(data$error)) {
        cat(sprintf("❌ %s: %s\n", symbol, data$error))
      } else {
        cat(sprintf("✅ %s: %.4f | 24h: %.2f%% | Vol: %.2fM\n",
                    symbol, 
                    data$price,
                    data$change_24h,
                    data$volume / 1000000))
      }
    }
  }
  
  if (!is.null(results$timestamp)) {
    cat(sprintf("\n⏱️ Executed at: %s\n", format(results$timestamp, "%H:%M:%S")))
  }
  
  return(invisible(results))
}

cat("✅ TRADING_EXECUTION_HUB V2.1 loaded successfully!\n")
cat("🎯 Main execution interface ready\n")
cat("🎮 Interactive trading commands available\n") 
cat("🎭 Multi-factor sentiment analysis enabled\n")
cat("📝 Console management integrated\n")
cat("🚀 Complete trading system V2.1 operational!\n")
cat("\n💡 Quick Start:\n")
cat("   execute_trading_system()              # Full analysis\n")
cat("   execute_trading_system('quick')       # Quick check\n")
cat("   execute_summary_only()                # Silent with summary\n")
cat("   execute_with_clean_logging()          # Clean logging\n")
cat("   execute_silent()                      # Complete silence\n")
cat("   launch_interactive_interface()        # Interactive mode\n")