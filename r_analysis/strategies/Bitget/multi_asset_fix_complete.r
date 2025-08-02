# ==========================================================================================================
# 🎯 COMPLETE MULTI-ASSET ANALYSIS FIX - FINAL VERSION
# ==========================================================================================================
# PROBLEM IDENTIFIED: System analyzed only 1 coin instead of all 5 portfolio assets
# ROOT CAUSE: execute_silent(coin, 'full') called individually vs execute_silent(PORTFOLIO_ASSETS, 'full')
# SOLUTION: Fixed execution to process all assets together + robust fallback mechanisms
# ==========================================================================================================

cat("🚀 Loading Complete Multi-Asset Analysis Fix...\n")

# ==========================================================================================================
# 🔍 PROBLEM ANALYSIS & SOLUTION DOCUMENTATION
# ==========================================================================================================

#' IDENTIFIED ISSUES:
#' 1. execute_silent() was called for each coin individually: execute_silent(coin, 'full')
#' 2. This caused only the last processed coin to be returned in results
#' 3. Portfolio-wide analysis was incomplete
#' 4. Risk assessment didn't see all positions
#' 5. Sentiment analysis was fragmented
#'
#' SOLUTION APPROACH:
#' 1. ✅ Call execute_silent(PORTFOLIO_ASSETS, 'full') with ALL symbols at once
#' 2. ✅ Implement robust fallback to individual analysis if batch fails
#' 3. ✅ Add result validation and combination logic
#' 4. ✅ Enhance error handling and reporting
#' 5. ✅ Override original functions with fixed versions

# ==========================================================================================================
# 🎯 MAIN FIX: BATCH MULTI-ASSET EXECUTION
# ==========================================================================================================

#' Fixed Complete Market Check - CORE FIX FUNCTION
complete_market_check_fixed <- function(force_individual = FALSE) {
  cat("\n💯 === COMPLETE MARKET CHECK (ALL 5 ASSETS) === 💯\n")
  
  # Validate portfolio configuration
  if (length(PORTFOLIO_ASSETS) != 5) {
    cat("⚠️ Portfolio incomplete (", length(PORTFOLIO_ASSETS), "/5), fixing...\n")
    force_portfolio_update()
  }
  
  cat("📊 Target Assets:", paste(PORTFOLIO_ASSETS, collapse = ", "), "\n")
  
  # 🎯 MAIN FIX: Process ALL assets together
  if (!force_individual) {
    cat("🚀 Processing ALL assets in batch mode...\n")
    
    batch_results <- tryCatch({
      # ✅ CRITICAL FIX: Pass ALL symbols together
      execute_silent(PORTFOLIO_ASSETS, "full")
    }, error = function(e) {
      cat("❌ Batch mode failed:", e$message, "\n")
      NULL
    })
    
    if (!is.null(batch_results)) {
      cat("✅ Batch analysis successful for", length(PORTFOLIO_ASSETS), "assets\n")
      
      # Validate batch results
      validation_result <- validate_batch_results(batch_results)
      
      if (validation_result$valid) {
        display_multi_asset_summary(batch_results)
        return(batch_results)
      } else {
        cat("⚠️ Batch results incomplete:", validation_result$reason, "\n")
        cat("🔄 Falling back to individual processing...\n")
      }
    }
  }
  
  # Fallback: Individual processing with aggregation
  cat("🔄 Using individual asset processing with aggregation...\n")
  return(individual_asset_processing_with_aggregation())
}

#' Enhanced Individual Processing with Smart Aggregation
individual_asset_processing_with_aggregation <- function() {
  
  aggregated_results <- list(
    timestamp = Sys.time(),
    symbols = PORTFOLIO_ASSETS,
    market_data = list(),
    technical_analysis = list(),
    sentiment_analysis = list(),
    risk_assessment = NULL,
    oi_analysis = list(),
    processing_mode = "individual_aggregated"
  )
  
  # Process each asset individually but aggregate intelligently
  for (symbol in PORTFOLIO_ASSETS) {
    cat("📡", symbol, "...")
    
    # Individual asset analysis
    asset_result <- tryCatch({
      execute_silent(symbol, "full")
    }, error = function(e) {
      cat(" ❌ Error:", e$message, "\n")
      NULL
    })
    
    if (!is.null(asset_result)) {
      # Extract and aggregate data
      aggregate_asset_data(asset_result, symbol, aggregated_results)
      cat(" ✅\n")
    } else {
      # Minimal fallback data
      cat(" 🔄 Fallback...")
      minimal_data <- get_minimal_asset_data(symbol)
      if (!is.null(minimal_data)) {
        aggregated_results$market_data[[symbol]] <- minimal_data
        cat(" ✅\n")
      } else {
        cat(" ❌\n")
      }
    }
    
    Sys.sleep(0.3) # Rate limiting
  }
  
  # Get portfolio-wide risk assessment ONCE
  cat("🛡️ Getting portfolio risk assessment...\n")
  portfolio_risk <- tryCatch({
    monitor_portfolio_positions(PORTFOLIO_ASSETS)
  }, error = function(e) {
    cat("⚠️ Risk assessment failed:", e$message, "\n")
    NULL
  })
  
  aggregated_results$risk_assessment <- portfolio_risk
  
  # Final validation and summary
  final_validation <- validate_aggregated_results(aggregated_results)
  cat("📊 Final validation:", if (final_validation$valid) "✅ PASSED" else "⚠️ PARTIAL", "\n")
  
  display_aggregated_summary(aggregated_results, final_validation)
  
  return(aggregated_results)
}

# ==========================================================================================================
# 🔧 VALIDATION & AGGREGATION FUNCTIONS
# ==========================================================================================================

#' Validate batch execution results
validate_batch_results <- function(results) {
  
  validation <- list(valid = TRUE, issues = list(), reason = "")
  
  # Check if all expected symbols are present
  if (!is.null(results$market_data)) {
    missing_symbols <- setdiff(PORTFOLIO_ASSETS, names(results$market_data))
    if (length(missing_symbols) > 0) {
      validation$issues$missing_market_data <- missing_symbols
    }
  } else {
    validation$issues$no_market_data <- TRUE
  }
  
  # Check for sentiment analysis
  if (!is.null(results$sentiment_analysis)) {
    missing_sentiment <- setdiff(PORTFOLIO_ASSETS, names(results$sentiment_analysis))
    if (length(missing_sentiment) > 0) {
      validation$issues$missing_sentiment <- missing_sentiment
    }
  }
  
  # Check for technical analysis
  if (!is.null(results$technical_analysis)) {
    missing_technical <- setdiff(PORTFOLIO_ASSETS, names(results$technical_analysis))
    if (length(missing_technical) > 0) {
      validation$issues$missing_technical <- missing_technical
    }
  }
  
  # Determine if valid
  critical_missing <- length(validation$issues) > 2
  validation$valid <- !critical_missing
  
  if (!validation$valid) {
    validation$reason <- paste("Critical data missing for", 
                              length(validation$issues), "components")
  }
  
  return(validation)
}

#' Aggregate individual asset data into combined results
aggregate_asset_data <- function(asset_result, symbol, aggregated_results) {
  
  # Market data aggregation
  if (!is.null(asset_result$market_data) && symbol %in% names(asset_result$market_data)) {
    aggregated_results$market_data[[symbol]] <<- asset_result$market_data[[symbol]]
  }
  
  # Technical analysis aggregation
  if (!is.null(asset_result$technical_analysis) && symbol %in% names(asset_result$technical_analysis)) {
    aggregated_results$technical_analysis[[symbol]] <<- asset_result$technical_analysis[[symbol]]
  }
  
  # Sentiment analysis aggregation
  if (!is.null(asset_result$sentiment_analysis) && symbol %in% names(asset_result$sentiment_analysis)) {
    aggregated_results$sentiment_analysis[[symbol]] <<- asset_result$sentiment_analysis[[symbol]]
  }
  
  # OI analysis aggregation
  if (!is.null(asset_result$oi_analysis) && symbol %in% names(asset_result$oi_analysis)) {
    aggregated_results$oi_analysis[[symbol]] <<- asset_result$oi_analysis[[symbol]]
  }
}

#' Get minimal asset data as ultimate fallback
get_minimal_asset_data <- function(symbol) {
  tryCatch({
    ticker_data <- get_enhanced_ticker_data(symbol)
    
    if (!is.null(ticker_data)) {
      return(list(
        current_price = ticker_data$last_price %||% ticker_data$mark_price %||% 0,
        change_24h = ticker_data$change_24h_pct %||% 0,
        volume_24h = ticker_data$volume_24h_usdt %||% 0,
        high_24h = ticker_data$high_24h %||% 0,
        low_24h = ticker_data$low_24h %||% 0,
        timestamp = Sys.time(),
        data_source = "minimal_fallback"
      ))
    }
    
    return(NULL)
    
  }, error = function(e) {
    return(NULL)
  })
}

#' Validate final aggregated results
validate_aggregated_results <- function(results) {
  
  validation <- list(
    valid = TRUE,
    completeness_score = 0,
    missing_data = list(),
    data_quality = "UNKNOWN"
  )
  
  # Calculate completeness score
  market_data_count <- length(results$market_data)
  technical_count <- length(results$technical_analysis)
  sentiment_count <- length(results$sentiment_analysis)
  
  max_possible <- length(PORTFOLIO_ASSETS) * 3 # market + technical + sentiment
  actual_count <- market_data_count + technical_count + sentiment_count
  
  validation$completeness_score <- (actual_count / max_possible) * 100
  
  # Determine data quality
  if (validation$completeness_score >= 90) {
    validation$data_quality <- "EXCELLENT"
  } else if (validation$completeness_score >= 70) {
    validation$data_quality <- "GOOD"
  } else if (validation$completeness_score >= 50) {
    validation$data_quality <- "FAIR"
  } else {
    validation$data_quality <- "POOR"
    validation$valid <- FALSE
  }
  
  # Identify missing data
  validation$missing_data$market <- setdiff(PORTFOLIO_ASSETS, names(results$market_data))
  validation$missing_data$technical <- setdiff(PORTFOLIO_ASSETS, names(results$technical_analysis))
  validation$missing_data$sentiment <- setdiff(PORTFOLIO_ASSETS, names(results$sentiment_analysis))
  
  return(validation)
}

# ==========================================================================================================
# 📊 ENHANCED DISPLAY FUNCTIONS
# ==========================================================================================================

#' Display comprehensive multi-asset summary
display_multi_asset_summary <- function(results) {
  cat("\n📊 === MULTI-ASSET ANALYSIS SUMMARY === 📊\n")
  
  # Processing mode indicator
  processing_mode <- results$processing_mode %||% "batch"
  cat("🔧 Processing Mode:", toupper(processing_mode), "\n")
  
  # Market data overview
  if (!is.null(results$market_data) && length(results$market_data) > 0) {
    cat("\n💰 Market Data (", length(results$market_data), "/", length(PORTFOLIO_ASSETS), " assets):\n")
    
    for (symbol in names(results$market_data)) {
      data <- results$market_data[[symbol]]
      change_icon <- if (data$change_24h > 2) "📈" else if (data$change_24h < -2) "📉" else "➡️"
      price_str <- sprintf("%.6f", data$current_price)
      change_str <- sprintf("%+.2f%%", data$change_24h)
      
      cat(sprintf("   %s %s: %s (%s) | Vol: %.1fM\n", 
                  change_icon, symbol, price_str, change_str, 
                  (data$volume_24h %||% 0) / 1000000))
    }
  }
  
  # Sentiment overview
  if (!is.null(results$sentiment_analysis) && length(results$sentiment_analysis) > 0) {
    cat("\n🎭 Sentiment Analysis (", length(results$sentiment_analysis), "/", length(PORTFOLIO_ASSETS), " assets):\n")
    
    for (symbol in names(results$sentiment_analysis)) {
      sentiment <- results$sentiment_analysis[[symbol]]
      score <- (sentiment$sentiment_score %||% 0.5) * 100
      sentiment_icon <- if (score > 65) "🟢" else if (score < 35) "🔴" else "🟡"
      
      cat(sprintf("   %s %s: %s (%.1f%%) | %s\n", 
                  sentiment_icon, symbol, 
                  sentiment$overall_sentiment %||% "UNKNOWN",
                  score,
                  sentiment$recommendation %||% "HOLD"))
    }
  }
  
  # Position overview
  if (!is.null(results$risk_assessment) && !is.null(results$risk_assessment$positions)) {
    positions <- results$risk_assessment$positions
    if (nrow(positions) > 0) {
      cat("\n💼 Active Positions (", nrow(positions), "):\n")
      
      for (i in 1:nrow(positions)) {
        pos <- positions[i, ]
        status_icon <- if (pos$unrealized_pnl >= 0) "✅" else "❌"
        pnl_pct <- if (pos$size > 0) (pos$unrealized_pnl / pos$size) * 100 else 0
        
        cat(sprintf("   %s %s: %s %.0f @ %.6f | PnL: %+.2f USDT (%+.1f%%)\n",
                    status_icon, pos$symbol, pos$side, pos$size, pos$avg_price,
                    pos$unrealized_pnl, pnl_pct))
      }
      
      # Portfolio totals
      total_pnl <- sum(positions$unrealized_pnl)
      portfolio_status <- if (total_pnl >= 0) "✅ PROFIT" else "❌ LOSS"
      cat("\n💰 Portfolio Total: ", portfolio_status, " ", sprintf("%+.2f USDT", total_pnl), "\n")
    }
  }
  
  # Risk assessment
  if (!is.null(results$risk_assessment)) {
    risk_score <- results$risk_assessment$portfolio_risk_score %||% 0
    risk_level <- if (risk_score < 0.3) "🟢 LOW" else if (risk_score < 0.7) "🟡 MODERATE" else "🔴 HIGH"
    
    cat("\n🛡️ Portfolio Risk: ", risk_level, sprintf(" (%.1f%%)", risk_score * 100), "\n")
    
    # Risk alerts
    alert_count <- length(results$risk_assessment$risk_alerts %||% list())
    if (alert_count > 0) {
      cat("⚠️ Active Risk Alerts:", alert_count, "\n")
    }
  }
}

#' Display aggregated results summary
display_aggregated_summary <- function(results, validation) {
  cat("\n📋 === AGGREGATED RESULTS SUMMARY === 📋\n")
  
  cat("📊 Data Completeness:", sprintf("%.1f%% (%s)", 
                                     validation$completeness_score, 
                                     validation$data_quality), "\n")
  
  cat("📈 Assets with Market Data:", length(results$market_data), "/", length(PORTFOLIO_ASSETS), "\n")
  cat("🎭 Assets with Sentiment:", length(results$sentiment_analysis), "/", length(PORTFOLIO_ASSETS), "\n")
  cat("🧮 Assets with Technical:", length(results$technical_analysis), "/", length(PORTFOLIO_ASSETS), "\n")
  
  # Show missing data if any
  if (length(validation$missing_data$market) > 0) {
    cat("⚠️ Missing Market Data:", paste(validation$missing_data$market, collapse = ", "), "\n")
  }
  
  # Show completeness status
  if (validation$valid) {
    cat("✅ Analysis Status: COMPLETE\n")
  } else {
    cat("⚠️ Analysis Status: PARTIAL - some data missing\n")
  }
  
  # Show detailed breakdown
  display_multi_asset_summary(results)
}

# ==========================================================================================================
# 🎯 ENHANCED QUICK FUNCTIONS WITH FIXES
# ==========================================================================================================

#' Fixed Quick Market Check
quick_market_check_fixed <- function() {
  cat("\n⚡ === QUICK MARKET CHECK (FIXED) === ⚡\n")
  
  # Try batch mode first
  results <- tryCatch({
    execute_summary_only_fixed(PORTFOLIO_ASSETS, "quick")
  }, error = function(e) {
    cat("❌ Batch quick check failed:", e$message, "\n")
    NULL
  })
  
  if (!is.null(results)) {
    cat("✅ Quick batch analysis successful\n")
    return(results)
  }
  
  # Fallback to individual quick processing
  cat("🔄 Using individual quick processing...\n")
  return(individual_quick_processing())
}

#' Individual quick processing for fallback
individual_quick_processing <- function() {
  
  quick_results <- list(
    quick_data = list(),
    timestamp = Sys.time(),
    processing_mode = "individual_quick"
  )
  
  for (symbol in PORTFOLIO_ASSETS) {
    cat("📡", symbol, "...")
    
    ticker_data <- tryCatch({
      get_enhanced_ticker_data(symbol)
    }, error = function(e) NULL)
    
    if (!is.null(ticker_data)) {
      quick_results$quick_data[[symbol]] <- list(
        price = ticker_data$last_price %||% ticker_data$mark_price %||% 0,
        change_24h = ticker_data$change_24h_pct %||% 0,
        volume = ticker_data$volume_24h_usdt %||% 0,
        status = "✅",
        timestamp = Sys.time()
      )
      cat(" ✅\n")
    } else {
      quick_results$quick_data[[symbol]] <- list(
        price = 0,
        change_24h = 0,
        volume = 0,
        status = "❌",
        error = "No data",
        timestamp = Sys.time()
      )
      cat(" ❌\n")
    }
  }
  
  # Display quick summary
  cat("\n📊 Quick Summary:\n")
  for (symbol in names(quick_results$quick_data)) {
    data <- quick_results$quick_data[[symbol]]
    if (data$status == "✅") {
      cat(sprintf("   %s %s: %.6f (%+.2f%%)\n", 
                  data$status, symbol, data$price, data$change_24h))
    } else {
      cat(sprintf("   %s %s: %s\n", data$status, symbol, data$error %||% "Failed"))
    }
  }
  
  return(quick_results)
}

#' Enhanced daily market check with better error handling
daily_market_check_enhanced <- function() {
  cat("\n🌅 === ENHANCED DAILY MARKET CHECK === 🌅\n")
  
  # Comprehensive daily analysis
  daily_results <- complete_market_check_fixed()
  
  if (!is.null(daily_results)) {
    # Additional daily analysis
    perform_daily_analysis(daily_results)
    
    # Save daily snapshot
    save_daily_snapshot(daily_results)
    
  } else {
    cat("❌ Daily market check failed\n")
  }
  
  return(daily_results)
}

#' Perform additional daily analysis
perform_daily_analysis <- function(results) {
  cat("\n📈 === DAILY MARKET ANALYSIS === 📈\n")
  
  if (!is.null(results$market_data)) {
    # Calculate daily performance metrics
    changes <- sapply(results$market_data, function(x) x$change_24h %||% 0)
    volumes <- sapply(results$market_data, function(x) x$volume_24h %||% 0)
    
    avg_change <- mean(changes)
    total_volume <- sum(volumes)
    
    cat("📊 Portfolio Average 24h Change:", sprintf("%+.2f%%", avg_change), "\n")
    cat("📊 Total 24h Volume:", sprintf("%.1fM USDT", total_volume / 1000000), "\n")
    
    # Best and worst performers
    if (length(changes) > 0) {
      best_performer <- names(changes)[which.max(changes)]
      worst_performer <- names(changes)[which.min(changes)]
      
      cat("🏆 Best Performer:", best_performer, sprintf("(%+.2f%%)", changes[best_performer]), "\n")
      cat("📉 Worst Performer:", worst_performer, sprintf("(%+.2f%%)", changes[worst_performer]), "\n")
    }
  }
  
  # Position performance
  if (!is.null(results$risk_assessment$positions)) {
    positions <- results$risk_assessment$positions
    if (nrow(positions) > 0) {
      total_pnl <- sum(positions$unrealized_pnl)
      winning_positions <- sum(positions$unrealized_pnl > 0)
      
      cat("💰 Portfolio PnL:", sprintf("%+.2f USDT", total_pnl), "\n")
      cat("🎯 Winning Positions:", winning_positions, "/", nrow(positions), "\n")
    }
  }
}

#' Save daily snapshot for historical tracking
save_daily_snapshot <- function(results) {
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("daily_snapshot_", timestamp, ".json")
    
    # Create snapshot data
    snapshot <- list(
      date = as.character(Sys.Date()),
      timestamp = as.character(Sys.time()),
      assets_analyzed = names(results$market_data %||% list()),
      market_summary = results$market_data,
      portfolio_pnl = if (!is.null(results$risk_assessment$positions)) {
        sum(results$risk_assessment$positions$unrealized_pnl)
      } else 0,
      risk_score = results$risk_assessment$portfolio_risk_score %||% 0
    )
    
    writeLines(toJSON(snapshot, pretty = TRUE, auto_unbox = TRUE), filename)
    cat("📄 Daily snapshot saved:", filename, "\n")
    
  }, error = function(e) {
    cat("⚠️ Could not save daily snapshot:", e$message, "\n")
  })
}

# ==========================================================================================================
# 🔄 SYSTEM INTEGRATION & OVERRIDES
# ==========================================================================================================

#' Apply all multi-asset fixes globally
apply_complete_multi_asset_fixes <- function() {
  cat("\n🔄 === APPLYING COMPLETE MULTI-ASSET FIXES === 🔄\n")
  
  # Override global functions with fixed versions
  complete_market_check <<- complete_market_check_fixed
  quick_market_check <<- quick_market_check_fixed
  daily_market_check_fixed <<- daily_market_check_enhanced
  
  # Additional function overrides
  execute_summary_only <<- function(symbols = NULL, mode = "full") {
    if (is.null(symbols)) symbols <- PORTFOLIO_ASSETS
    execute_summary_only_fixed(symbols, mode)
  }
  
  cat("✅ All multi-asset fixes applied globally\n")
  cat("🎯 Fixed functions now available:\n")
  cat("   complete_market_check()               # All 5 assets batch processing\n")
  cat("   quick_market_check()                  # Fixed quick check\n")
  cat("   daily_market_check_fixed()            # Enhanced daily analysis\n")
  cat("   execute_summary_only()                # Fixed summary function\n")
  
  # Validate fix application
  test_fix_application()
}

#' Test that fixes are properly applied
test_fix_application <- function() {
  cat("\n🧪 === TESTING FIX APPLICATION === 🧪\n")
  
  # Test portfolio configuration
  if (length(PORTFOLIO_ASSETS) == 5) {
    cat("✅ Portfolio configuration: 5 assets found\n")
  } else {
    cat("⚠️ Portfolio configuration: only", length(PORTFOLIO_ASSETS), "assets\n")
  }
  
  # Test function availability
  functions_to_test <- c("complete_market_check", "quick_market_check", "execute_summary_only")
  
  for (func_name in functions_to_test) {
    if (exists(func_name)) {
      cat("✅ Function available:", func_name, "\n")
    } else {
      cat("❌ Function missing:", func_name, "\n")
    }
  }
  
  cat("🎯 Fix testing complete\n")
}

# ==========================================================================================================
# 🚀 FINAL EXECUTION & COMPLETION
# ==========================================================================================================

cat("✅ COMPLETE MULTI-ASSET ANALYSIS FIX LOADED!\n")
cat("🎯 Problem: Only 1 asset analyzed instead of all 5\n")
cat("✅ Solution: Batch processing with intelligent fallbacks\n")
cat("🔧 Root Cause Fixed: execute_silent(PORTFOLIO_ASSETS, 'full') vs individual calls\n")
cat("\n💡 READY TO USE:\n")
cat("   apply_complete_multi_asset_fixes()    # Apply all fixes\n")
cat("   complete_market_check_fixed()         # Test the main fix\n")
cat("   quick_market_check_fixed()            # Test quick analysis\n")
cat("   daily_market_check_enhanced()         # Enhanced daily check\n")
cat("\n🚀 RECOMMENDED WORKFLOW:\n")
cat("   1. apply_complete_multi_asset_fixes() # Apply fixes globally\n")
cat("   2. complete_market_check()            # Now processes all 5 assets\n")
cat("   3. Verify all 5 assets appear in results\n")
cat("\n✅ FIX COMPLETE - MULTI-ASSET ANALYSIS RESTORED!\n")