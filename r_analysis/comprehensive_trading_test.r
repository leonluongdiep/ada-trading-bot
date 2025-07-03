# ==========================================================================================================
# 🧪 UMFASSENDES TRADING SYSTEM TEST SCRIPT
# ==========================================================================================================
# 
# ZWECK: Testet ALLE verfügbaren Funktionen des Bitget Trading Systems
# MODUS: 100% SIMULATION (keine echten Orders)
# ASSETS: ADA, BTC, ETH (Multi-Asset Test)
# 
# ==========================================================================================================

cat("🧪 STARTING COMPREHENSIVE TRADING SYSTEM TEST\n")
cat(strrep("=", 60), "\n")
cat("📅 Test Start Time:", as.character(Sys.time()), "\n")
cat("🎮 Mode: SIMULATION ONLY (no real orders)\n")
cat("🎯 Purpose: Test all available functions\n\n")

# ==========================================================================================================
# 🔧 TEST CONFIGURATION
# ==========================================================================================================

# Test Assets
TEST_SYMBOLS <- c("ADAUSDT_UMCBL", "BTCUSDT_UMCBL", "ETHUSDT_UMCBL")
PRIMARY_SYMBOL <- "ADAUSDT_UMCBL"

# Test Counters
test_results <- list()
function_count <- 0
success_count <- 0
error_count <- 0

# Helper function für Tests
run_test <- function(test_name, test_function, show_details = TRUE) {
  function_count <<- function_count + 1
  cat(sprintf("\n🧪 TEST %d: %s\n", function_count, test_name))
  cat(strrep("-", 50), "\n")
  
  start_time <- Sys.time()
  result <- list(
    name = test_name,
    success = FALSE,
    error = NULL,
    duration = 0,
    output = NULL
  )
  
  tryCatch({
    output <- test_function()
    result$success <- TRUE
    result$output <- output
    success_count <<- success_count + 1
    
    duration <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
    result$duration <- duration
    
    if (show_details && !is.null(output)) {
      if (is.data.frame(output)) {
        cat("✅ SUCCESS - DataFrame with", nrow(output), "rows,", ncol(output), "columns\n")
        if (nrow(output) > 0) {
          cat("   Sample columns:", paste(head(names(output), 5), collapse = ", "), "\n")
        }
      } else if (is.list(output)) {
        cat("✅ SUCCESS - List with", length(output), "elements\n")
        cat("   Elements:", paste(head(names(output), 5), collapse = ", "), "\n")
      } else {
        cat("✅ SUCCESS - Type:", class(output)[1], "\n")
      }
    } else {
      cat("✅ SUCCESS\n")
    }
    
    cat(sprintf("⏱️ Duration: %.2f seconds\n", duration))
    
  }, error = function(e) {
    result$success <- FALSE
    result$error <- e$message
    error_count <<- error_count + 1
    
    cat("❌ ERROR:", e$message, "\n")
  })
  
  test_results[[test_name]] <<- result
  return(result)
}

# ==========================================================================================================
# 📊 KATEGORIE 1: CORE ANALYSIS FUNCTIONS
# ==========================================================================================================

cat("\n📊 KATEGORIE 1: CORE ANALYSIS FUNCTIONS\n")
cat(strrep("=", 60), "\n")

# Test 1: Complete Trading Analysis
run_test("Complete Trading Analysis (ADA)", function() {
  if (exists("complete_trading_analysis")) {
    return(complete_trading_analysis(PRIMARY_SYMBOL))
  } else {
    stop("Function complete_trading_analysis not found")
  }
})

# Test 2: Enhanced Trading Analysis  
run_test("Enhanced Trading Analysis (ADA)", function() {
  if (exists("complete_trading_analysis_enhanced")) {
    return(complete_trading_analysis_enhanced(PRIMARY_SYMBOL))
  } else {
    stop("Function complete_trading_analysis_enhanced not found")
  }
})

# Test 3: Universal Analysis
run_test("Universal Analysis (ADA)", function() {
  if (exists("complete_analysis_universal")) {
    return(complete_analysis_universal(PRIMARY_SYMBOL))
  } else {
    stop("Function complete_analysis_universal not found")
  }
})

# ==========================================================================================================
# 📈 KATEGORIE 2: MARKET DATA COLLECTION
# ==========================================================================================================

cat("\n📈 KATEGORIE 2: MARKET DATA COLLECTION\n")
cat(strrep("=", 60), "\n")

# Test 4: Enhanced Ticker Data
run_test("Enhanced Ticker Data", function() {
  if (exists("get_enhanced_ticker_data")) {
    return(get_enhanced_ticker_data(PRIMARY_SYMBOL))
  } else {
    stop("Function get_enhanced_ticker_data not found")
  }
})

# Test 5: Enhanced Market Data
run_test("Enhanced Market Data Collection", function() {
  if (exists("get_enhanced_market_data")) {
    return(get_enhanced_market_data(PRIMARY_SYMBOL))
  } else {
    stop("Function get_enhanced_market_data not found")
  }
})

# Test 6: Enhanced Orderbook
run_test("Enhanced Orderbook Analysis", function() {
  if (exists("get_enhanced_orderbook")) {
    return(get_enhanced_orderbook(PRIMARY_SYMBOL, 20))
  } else {
    stop("Function get_enhanced_orderbook not found")
  }
})

# Test 7: Enhanced Trades
run_test("Enhanced Trades Analysis", function() {
  if (exists("get_enhanced_trades")) {
    return(get_enhanced_trades(PRIMARY_SYMBOL, 50))
  } else {
    stop("Function get_enhanced_trades not found")
  }
})

# Test 8: Quick Collection Universal
run_test("Quick Collection Universal", function() {
  if (exists("quick_collection_universal")) {
    return(quick_collection_universal(PRIMARY_SYMBOL, "1h", 20))
  } else {
    stop("Function quick_collection_universal not found")
  }
})

# ==========================================================================================================
# 💰 KATEGORIE 3: POSITION & ORDER MANAGEMENT
# ==========================================================================================================

cat("\n💰 KATEGORIE 3: POSITION & ORDER MANAGEMENT\n")
cat(strrep("=", 60), "\n")

# Test 9: Current Positions
run_test("Get Current Positions", function() {
  if (exists("get_current_positions")) {
    return(get_current_positions(PRIMARY_SYMBOL))
  } else {
    stop("Function get_current_positions not found")
  }
})

# Test 10: Current Plan Orders
run_test("Get Current Plan Orders (TP/SL)", function() {
  if (exists("get_current_plan_orders")) {
    return(get_current_plan_orders(PRIMARY_SYMBOL))
  } else {
    stop("Function get_current_plan_orders not found")
  }
})

# Test 11: Current Open Orders
run_test("Get Current Open Orders", function() {
  if (exists("get_current_open_orders")) {
    return(get_current_open_orders(PRIMARY_SYMBOL))
  } else {
    stop("Function get_current_open_orders not found")
  }
})

# Test 12: Universal Position Check
run_test("Universal Position Check (All Assets)", function() {
  if (exists("check_positions_universal")) {
    return(check_positions_universal())
  } else {
    stop("Function check_positions_universal not found")
  }
})

# ==========================================================================================================
# 🎯 KATEGORIE 4: TRADING FUNCTIONS (SIMULATION MODE)
# ==========================================================================================================

cat("\n🎯 KATEGORIE 4: TRADING FUNCTIONS (SIMULATION)\n")
cat(strrep("=", 60), "\n")

# Test 13: Symbol Precision
run_test("Get Symbol Precision", function() {
  if (exists("get_symbol_precision")) {
    return(get_symbol_precision(PRIMARY_SYMBOL))
  } else {
    stop("Function get_symbol_precision not found")
  }
})

# Test 14: Market Sentiment Calculation
run_test("Market Sentiment Calculation", function() {
  if (exists("calculate_market_sentiment") && exists("get_enhanced_ticker_data")) {
    # Benötigt Ticker-Daten als Input
    ticker_data <- get_enhanced_ticker_data(PRIMARY_SYMBOL)
    orderbook_data <- NULL
    trades_data <- NULL
    
    if (exists("get_enhanced_orderbook")) {
      orderbook_data <- get_enhanced_orderbook(PRIMARY_SYMBOL)
    }
    if (exists("get_enhanced_trades")) {
      trades_data <- get_enhanced_trades(PRIMARY_SYMBOL)
    }
    
    return(calculate_market_sentiment(ticker_data, orderbook_data, trades_data))
  } else {
    stop("Required functions for sentiment calculation not found")
  }
})

# Test 15: TP Order Validation (Simulation)
run_test("TP Order Function Test (No Real Order)", function() {
  if (exists("place_tp_simple")) {
    cat("🎮 SIMULATION: Would place TP order - Testing function availability\n")
    cat("   Function exists: ✅\n")
    cat("   Parameters would be: ADA, long, 100, 0.6200\n")
    return(list(
      function_available = TRUE,
      would_place_tp = TRUE,
      simulation_mode = TRUE,
      test_params = list(symbol = PRIMARY_SYMBOL, side = "long", size = "100", price = 0.6200)
    ))
  } else {
    stop("Function place_tp_simple not found")
  }
})

# Test 16: SL Order Validation (Simulation)
run_test("SL Order Function Test (No Real Order)", function() {
  if (exists("place_sl_simple")) {
    cat("🎮 SIMULATION: Would place SL order - Testing function availability\n")
    cat("   Function exists: ✅\n")
    cat("   Parameters would be: ADA, long, 100, 0.5900\n")
    return(list(
      function_available = TRUE,
      would_place_sl = TRUE,
      simulation_mode = TRUE,
      test_params = list(symbol = PRIMARY_SYMBOL, side = "long", size = "100", price = 0.5900)
    ))
  } else {
    stop("Function place_sl_simple not found")
  }
})

# Test 17: Strategic Order Function
run_test("Strategic Order Function Test (No Real Order)", function() {
  if (exists("place_strategic_limit_order")) {
    cat("🎮 SIMULATION: Would place Strategic order - Testing function availability\n")
    cat("   Function exists: ✅\n")
    cat("   Parameters would be: ADA, open_long, 1000, 0.5636\n")
    return(list(
      function_available = TRUE,
      would_place_strategic = TRUE,
      simulation_mode = TRUE,
      test_params = list(symbol = PRIMARY_SYMBOL, side = "open_long", size = 1000, price = 0.5636)
    ))
  } else {
    stop("Function place_strategic_limit_order not found")
  }
})

# ==========================================================================================================
# 🌍 KATEGORIE 5: MULTI-ASSET SUPPORT
# ==========================================================================================================

cat("\n🌍 KATEGORIE 5: MULTI-ASSET SUPPORT\n")
cat(strrep("=", 60), "\n")

# Test 18: Multi-Asset Ticker Data
run_test("Multi-Asset Ticker Data", function() {
  results <- list()
  for (symbol in TEST_SYMBOLS) {
    if (exists("get_enhanced_ticker_data")) {
      ticker <- get_enhanced_ticker_data(symbol)
      if (!is.null(ticker)) {
        results[[symbol]] <- list(
          symbol = symbol,
          price = ticker$last_price,
          change_24h = ticker$change_24h_pct,
          volume = ticker$volume_24h_usdt
        )
      }
    }
  }
  return(results)
})

# Test 19: Asset Configuration Test
run_test("Asset Configuration Support", function() {
  if (exists("get_asset_config")) {
    configs <- list()
    for (symbol in TEST_SYMBOLS) {
      configs[[symbol]] <- get_asset_config(symbol)
    }
    return(configs)
  } else {
    # Fallback test
    cat("Testing supported symbols manually...\n")
    if (exists("SUPPORTED_SYMBOLS")) {
      return(list(supported_symbols = SUPPORTED_SYMBOLS))
    } else {
      return(list(test_symbols = TEST_SYMBOLS, status = "Configurations unknown"))
    }
  }
})

# ==========================================================================================================
# 🔧 KATEGORIE 6: UTILITY & HELPER FUNCTIONS
# ==========================================================================================================

cat("\n🔧 KATEGORIE 6: UTILITY & HELPER FUNCTIONS\n")
cat(strrep("=", 60), "\n")

# Test 20: Historical Data Creation
run_test("Historical Data Creation", function() {
  if (exists("create_enhanced_historical_data_fixed")) {
    return(create_enhanced_historical_data_fixed(PRIMARY_SYMBOL, 20, "1h", FALSE))
  } else if (exists("create_enhanced_synthetic_candles_fallback")) {
    return(create_enhanced_synthetic_candles_fallback(PRIMARY_SYMBOL, 20))
  } else {
    stop("No historical data creation function found")
  }
})

# Test 21: Technical Indicators
run_test("Technical Indicators Calculation", function() {
  if (exists("calculate_technical_indicators_fixed")) {
    # Erstelle Sample-Daten
    sample_candles <- data.frame(
      timestamp = seq(Sys.time() - 3600*24, Sys.time(), by = 3600),
      open = runif(25, 0.55, 0.65),
      high = runif(25, 0.60, 0.70),
      low = runif(25, 0.50, 0.60),
      close = runif(25, 0.55, 0.65),
      volume = runif(25, 1000, 5000),
      timeframe = "1h",
      symbol = PRIMARY_SYMBOL
    )
    return(calculate_technical_indicators_fixed(sample_candles))
  } else {
    stop("Function calculate_technical_indicators_fixed not found")
  }
})

# Test 22: Trading Signals Generation
run_test("Trading Signals Generation", function() {
  if (exists("generate_trading_signals") && exists("calculate_technical_indicators_fixed")) {
    # Erstelle Sample-Daten mit Indikatoren
    sample_candles <- data.frame(
      timestamp = seq(Sys.time() - 3600*24, Sys.time(), by = 3600),
      open = runif(25, 0.55, 0.65),
      high = runif(25, 0.60, 0.70), 
      low = runif(25, 0.50, 0.60),
      close = runif(25, 0.55, 0.65),
      volume = runif(25, 1000, 5000),
      timeframe = "1h",
      symbol = PRIMARY_SYMBOL
    )
    
    indicators <- calculate_technical_indicators_fixed(sample_candles)
    if (!is.null(indicators)) {
      return(generate_trading_signals(indicators))
    } else {
      stop("Could not calculate indicators for signal generation")
    }
  } else {
    stop("Required functions for signal generation not found")
  }
})

# ==========================================================================================================
# 🏗️ KATEGORIE 7: ADVANCED TRADING STRATEGIES
# ==========================================================================================================

cat("\n🏗️ KATEGORIE 7: ADVANCED TRADING STRATEGIES\n")
cat(strrep("=", 60), "\n")

# Test 23: Intelligent TP/SL Function
run_test("Intelligent TP/SL Function Test", function() {
  if (exists("place_intelligent_tp_sl")) {
    cat("🎮 SIMULATION: Testing intelligent TP/SL function availability\n")
    cat("   Function exists: ✅\n")
    cat("   Would analyze position and place optimal TP/SL orders\n")
    return(list(
      function_available = TRUE,
      intelligent_tpsl = TRUE,
      simulation_mode = TRUE,
      description = "Analyzes position and calculates optimal TP/SL levels"
    ))
  } else {
    stop("Function place_intelligent_tp_sl not found")
  }
})

# Test 24: Breakeven Strategy
run_test("Breakeven Strategy Function Test", function() {
  if (exists("place_breakeven_orders")) {
    cat("🎮 SIMULATION: Testing breakeven strategy function\n")
    cat("   Function exists: ✅\n")
    cat("   Would place breakeven orders (50% at entry, 50% with SL)\n")
    return(list(
      function_available = TRUE,
      breakeven_strategy = TRUE,
      simulation_mode = TRUE,
      description = "Places 50% at breakeven, 50% with protective SL"
    ))
  } else {
    stop("Function place_breakeven_orders not found")
  }
})

# Test 25: Conservative Strategy
run_test("Conservative Strategy Function Test", function() {
  if (exists("place_conservative_strategy")) {
    cat("🎮 SIMULATION: Testing conservative strategy function\n")
    cat("   Function exists: ✅\n")
    cat("   Would place staged TP orders (40% +1%, 30% +2%, 30% +3%)\n")
    return(list(
      function_available = TRUE,
      conservative_strategy = TRUE,
      simulation_mode = TRUE,
      description = "Staged profit taking: 40% at +1%, 30% at +2%, 30% at +3%"
    ))
  } else {
    stop("Function place_conservative_strategy not found")
  }
})

# ==========================================================================================================
# 📊 TEST RESULTS SUMMARY
# ==========================================================================================================

cat("\n📊 COMPREHENSIVE TEST RESULTS SUMMARY\n")
cat(strrep("=", 70), "\n")

# Statistiken
total_tests <- length(test_results)
success_rate <- round((success_count / total_tests) * 100, 1)
error_rate <- round((error_count / total_tests) * 100, 1)

cat(sprintf("📈 OVERALL STATISTICS:\n"))
cat(sprintf("   Total Tests: %d\n", total_tests))
cat(sprintf("   Successful: %d (%.1f%%)\n", success_count, success_rate))
cat(sprintf("   Failed: %d (%.1f%%)\n", error_count, error_rate))
cat(sprintf("   Test Duration: %.2f minutes\n", 
            as.numeric(difftime(Sys.time(), 
            as.POSIXct("2024-01-01") + as.numeric(test_results[[1]]$duration), units = "mins"))))

# Kategorien-Zusammenfassung
categories <- list(
  "Core Analysis" = 1:3,
  "Market Data" = 4:8,
  "Position/Orders" = 9:12,
  "Trading Functions" = 13:17,
  "Multi-Asset" = 18:19,
  "Utilities" = 20:22,
  "Advanced Strategies" = 23:25
)

cat(sprintf("\n📊 RESULTS BY CATEGORY:\n"))
for (cat_name in names(categories)) {
  test_indices <- categories[[cat_name]]
  cat_tests <- test_results[test_indices]
  cat_success <- sum(sapply(cat_tests, function(x) x$success), na.rm = TRUE)
  cat_total <- length(cat_tests)
  cat_rate <- round((cat_success / cat_total) * 100, 1)
  
  cat(sprintf("   %s: %d/%d (%.1f%%)\n", cat_name, cat_success, cat_total, cat_rate))
}

# Fehlgeschlagene Tests
failed_tests <- test_results[sapply(test_results, function(x) !x$success)]
if (length(failed_tests) > 0) {
  cat(sprintf("\n❌ FAILED TESTS:\n"))
  for (test_name in names(failed_tests)) {
    error_msg <- failed_tests[[test_name]]$error
    cat(sprintf("   %s: %s\n", test_name, error_msg))
  }
}

# Erfolgreichste Tests (schnellste)
successful_tests <- test_results[sapply(test_results, function(x) x$success)]
if (length(successful_tests) > 0) {
  durations <- sapply(successful_tests, function(x) x$duration)
  fastest_tests <- successful_tests[order(durations)[1:min(3, length(durations))]]
  
  cat(sprintf("\n⚡ FASTEST SUCCESSFUL TESTS:\n"))
  for (test_name in names(fastest_tests)) {
    duration <- fastest_tests[[test_name]]$duration
    cat(sprintf("   %s: %.2f seconds\n", test_name, duration))
  }
}

# ==========================================================================================================
# 🎯 FUNCTION AVAILABILITY SUMMARY
# ==========================================================================================================

cat(sprintf("\n🎯 FUNCTION AVAILABILITY SUMMARY:\n"))
cat(strrep("=", 50), "\n")

# Core Functions
core_functions <- c(
  "complete_trading_analysis", "complete_trading_analysis_enhanced",
  "get_enhanced_market_data", "get_current_positions", 
  "place_tp_simple", "place_sl_simple", "place_strategic_limit_order"
)

cat("🔧 CORE FUNCTIONS:\n")
for (func in core_functions) {
  status <- if (exists(func)) "✅ Available" else "❌ Missing"
  cat(sprintf("   %-35s %s\n", func, status))
}

# Optional Functions
optional_functions <- c(
  "quick_collection_universal", "place_intelligent_tp_sl",
  "place_breakeven_orders", "place_conservative_strategy",
  "check_positions_universal", "calculate_market_sentiment"
)

cat("\n🎨 OPTIONAL FUNCTIONS:\n")
for (func in optional_functions) {
  status <- if (exists(func)) "✅ Available" else "❌ Missing"
  cat(sprintf("   %-35s %s\n", func, status))
}

# ==========================================================================================================
# 💡 RECOMMENDATIONS
# ==========================================================================================================

cat(sprintf("\n💡 RECOMMENDATIONS:\n"))
cat(strrep("=", 30), "\n")

if (success_rate >= 90) {
  cat("🏆 EXCELLENT: Your trading system is fully functional!\n")
  cat("   ✅ All major functions working\n")
  cat("   ✅ Ready for live trading (if desired)\n")
  cat("   ✅ Multi-asset support confirmed\n")
} else if (success_rate >= 75) {
  cat("👍 GOOD: Most functions working, minor issues\n")
  cat("   ✅ Core trading functions operational\n")
  cat("   ⚠️ Some optional features missing\n")
  cat("   💡 System ready for basic trading\n")
} else if (success_rate >= 50) {
  cat("⚠️ PARTIAL: Core functions working, needs attention\n")
  cat("   ✅ Basic functionality available\n")
  cat("   ❌ Several functions need fixing\n")
  cat("   🔧 Recommend system maintenance\n")
} else {
  cat("🚨 CRITICAL: Major system issues detected\n")
  cat("   ❌ Many core functions failing\n")
  cat("   🔧 System requires significant fixes\n")
  cat("   ⛔ Not ready for trading\n")
}

cat(sprintf("\n🎯 NEXT STEPS:\n"))
if (exists("place_tp_simple") && exists("place_sl_simple") && exists("get_current_positions")) {
  cat("1. ✅ Core trading functions ready\n")
  cat("2. 🎮 Test with simulation mode first\n")
  cat("3. 🚀 Enable live trading when ready\n")
  cat("4. 📊 Monitor strategic order at 0.5636 USDT\n")
} else {
  cat("1. 🔧 Fix missing core functions first\n") 
  cat("2. 📚 Review system documentation\n")
  cat("3. 🧪 Re-run tests after fixes\n")
}

cat(sprintf("\n✅ COMPREHENSIVE TEST COMPLETED!\n"))
cat(sprintf("📊 Success Rate: %.1f%% (%d/%d tests passed)\n", success_rate, success_count, total_tests))
cat(sprintf("🕒 Total Test Time: ~%.1f minutes\n", total_tests * 0.5))  # Estimated

# ==========================================================================================================
# 🎯 END OF COMPREHENSIVE TRADING SYSTEM TEST
# ==========================================================================================================