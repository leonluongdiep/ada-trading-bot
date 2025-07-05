# ==========================================================================================================
# 🚀 COMPREHENSIVE TRADING ANALYSIS MASTER SCRIPT
# ==========================================================================================================
# 
# ZWECK: Vollständige Analyse aller verfügbaren Trading-Daten
# INTEGRIERT: Alle bestehenden R-Scripts aus tb202506
# OUTPUT: Strukturierte, umfassende Analyse-Ausgabe
# KOMPATIBEL: Mit allen vorhandenen Funktionen
# 
# ==========================================================================================================

cat("🚀 COMPREHENSIVE TRADING ANALYSIS MASTER\n")
cat(strrep("=", 60), "\n")
cat("📅 Analysis Start:", as.character(Sys.time()), "\n")
cat("🎯 Target: Complete market and position analysis\n\n")

# ==========================================================================================================
# 📋 CONFIGURATION
# ==========================================================================================================

# Analysis Configuration
ANALYSIS_CONFIG <- list(
  symbol = "ADAUSDT_UMCBL",
  timeframes = c("5m", "15m", "1h", "4h", "1d"),
  analysis_days = c(7, 14, 30, 60, 90),
  include_multi_asset = TRUE,
  multi_assets = c("ADAUSDT_UMCBL", "BTCUSDT_UMCBL", "ETHUSDT_UMCBL"),
  save_results = TRUE,
  detailed_output = TRUE
)

# Output Configuration
OUTPUT_CONFIG <- list(
  show_raw_data = FALSE,
  show_charts = TRUE,
  show_recommendations = TRUE,
  export_json = TRUE,
  export_csv = TRUE
)

cat("🔧 Configuration loaded:\n")
cat(sprintf("   Primary Symbol: %s\n", ANALYSIS_CONFIG$symbol))
cat(sprintf("   Timeframes: %s\n", paste(ANALYSIS_CONFIG$timeframes, collapse = ", ")))
cat(sprintf("   Analysis Periods: %s days\n", paste(ANALYSIS_CONFIG$analysis_days, collapse = ", ")))
cat(sprintf("   Multi-Asset: %s\n", if(ANALYSIS_CONFIG$include_multi_asset) "YES" else "NO"))

# ==========================================================================================================
# 🔧 MASTER ANALYSIS FUNCTION
# ==========================================================================================================

run_comprehensive_analysis <- function(config = ANALYSIS_CONFIG) {
  
  cat("\n🎯 STARTING COMPREHENSIVE ANALYSIS\n")
  cat(strrep("=", 50), "\n")
  
  # Initialize results structure
  master_results <- list(
    timestamp = Sys.time(),
    config = config,
    analysis_results = list(),
    summary = list(),
    recommendations = list()
  )
  
  # ==========================================================================================================
  # PHASE 1: CURRENT MARKET STATUS
  # ==========================================================================================================
  
  cat("📊 PHASE 1: Current Market Status Analysis\n")
  cat(strrep("-", 40), "\n")
  
  # Current Positions
  cat("🔍 Analyzing current positions...\n")
  current_positions <- NULL
  if (exists("get_current_positions")) {
    tryCatch({
      current_positions <- get_current_positions(config$symbol)
      if (!is.null(current_positions)) {
        cat("✅ Active position found\n")
        master_results$analysis_results$current_position <- current_positions
      } else {
        cat("📭 No active positions\n")
      }
    }, error = function(e) {
      cat("⚠️ Position check failed:", e$message, "\n")
    })
  }
  
  # Current Orders
  cat("🔍 Analyzing current orders...\n")
  current_orders <- NULL
  if (exists("get_current_plan_orders")) {
    tryCatch({
      current_orders <- get_current_plan_orders(config$symbol)
      master_results$analysis_results$current_orders <- current_orders
      if (!is.null(current_orders) && nrow(current_orders) > 0) {
        cat(sprintf("✅ Found %d active orders\n", nrow(current_orders)))
      } else {
        cat("📭 No active orders\n")
      }
    }, error = function(e) {
      cat("⚠️ Orders check failed:", e$message, "\n")
    })
  }
  
  # Enhanced Market Data
  cat("🔍 Collecting enhanced market data...\n")
  market_data <- NULL
  if (exists("get_enhanced_market_data")) {
    tryCatch({
      market_data <- get_enhanced_market_data(config$symbol)
      master_results$analysis_results$market_data <- market_data
      cat("✅ Enhanced market data collected\n")
    }, error = function(e) {
      cat("⚠️ Market data collection failed:", e$message, "\n")
    })
  }
  
  # ==========================================================================================================
  # PHASE 2: TECHNICAL ANALYSIS
  # ==========================================================================================================
  
  cat("\n📈 PHASE 2: Technical Analysis\n")
  cat(strrep("-", 40), "\n")
  
  # Complete Trading Analysis
  cat("🔍 Running complete trading analysis...\n")
  technical_analysis <- NULL
  if (exists("complete_trading_analysis")) {
    tryCatch({
      technical_analysis <- complete_trading_analysis(config$symbol)
      master_results$analysis_results$technical_analysis <- technical_analysis
      cat("✅ Technical analysis completed\n")
    }, error = function(e) {
      cat("⚠️ Technical analysis failed:", e$message, "\n")
    })
  }
  
  # Enhanced Trading Analysis
  cat("🔍 Running enhanced trading analysis...\n")
  enhanced_analysis <- NULL
  if (exists("complete_trading_analysis_enhanced")) {
    tryCatch({
      enhanced_analysis <- complete_trading_analysis_enhanced(config$symbol)
      master_results$analysis_results$enhanced_analysis <- enhanced_analysis
      cat("✅ Enhanced analysis completed\n")
    }, error = function(e) {
      cat("⚠️ Enhanced analysis failed:", e$message, "\n")
    })
  }
  
  # ==========================================================================================================
  # PHASE 3: MULTI-TIMEFRAME ANALYSIS
  # ==========================================================================================================
  
  cat("\n⏰ PHASE 3: Multi-Timeframe Analysis\n")
  cat(strrep("-", 40), "\n")
  
  timeframe_results <- list()
  
  for (period in config$analysis_days) {
    cat(sprintf("🔍 Analyzing %d-day period...\n", period))
    
    if (exists("quick_efficiency_check_fixed")) {
      tryCatch({
        efficiency_check <- quick_efficiency_check_fixed(config$symbol, period, TRUE)
        timeframe_results[[paste0("days_", period)]] <- efficiency_check
        cat(sprintf("✅ %d-day analysis completed\n", period))
      }, error = function(e) {
        cat(sprintf("⚠️ %d-day analysis failed: %s\n", period, e$message))
      })
    }
  }
  
  master_results$analysis_results$timeframe_analysis <- timeframe_results
  
  # ==========================================================================================================
  # PHASE 4: TRADING EFFICIENCY ANALYSIS
  # ==========================================================================================================
  
  cat("\n📊 PHASE 4: Trading Efficiency Analysis\n")
  cat(strrep("-", 40), "\n")
  
  efficiency_results <- NULL
  if (exists("generate_comprehensive_efficiency_report_fixed")) {
    tryCatch({
      cat("🔍 Generating comprehensive efficiency report...\n")
      efficiency_results <- generate_comprehensive_efficiency_report_fixed(config$symbol, 30, TRUE)
      master_results$analysis_results$efficiency_analysis <- efficiency_results
      cat("✅ Efficiency analysis completed\n")
    }, error = function(e) {
      cat("⚠️ Efficiency analysis failed:", e$message, "\n")
    })
  }
  
  # ==========================================================================================================
  # PHASE 5: MULTI-ASSET COMPARISON
  # ==========================================================================================================
  
  if (config$include_multi_asset) {
    cat("\n🌍 PHASE 5: Multi-Asset Analysis\n")
    cat(strrep("-", 40), "\n")
    
    multi_asset_results <- list()
    
    for (asset in config$multi_assets) {
      cat(sprintf("🔍 Analyzing %s...\n", asset))
      
      if (exists("quick_efficiency_check_fixed")) {
        tryCatch({
          asset_analysis <- quick_efficiency_check_fixed(asset, 30, TRUE)
          multi_asset_results[[asset]] <- asset_analysis
          cat(sprintf("✅ %s analysis completed\n", asset))
        }, error = function(e) {
          cat(sprintf("⚠️ %s analysis failed: %s\n", asset, e$message))
        })
      }
    }
    
    master_results$analysis_results$multi_asset_analysis <- multi_asset_results
  }
  
  # ==========================================================================================================
  # PHASE 6: SUMMARY GENERATION
  # ==========================================================================================================
  
  cat("\n📋 PHASE 6: Summary Generation\n")
  cat(strrep("-", 40), "\n")
  
  # Generate comprehensive summary
  summary <- generate_analysis_summary(master_results)
  master_results$summary <- summary
  
  # Generate recommendations
  recommendations <- generate_trading_recommendations(master_results)
  master_results$recommendations <- recommendations
  
  cat("✅ Summary and recommendations generated\n")
  
  return(master_results)
}

# ==========================================================================================================
# 📊 SUMMARY GENERATION FUNCTIONS
# ==========================================================================================================

generate_analysis_summary <- function(results) {
  summary <- list(
    timestamp = Sys.time(),
    total_data_points = 0,
    position_status = "Unknown",
    market_sentiment = "Unknown",
    technical_signal = "Unknown",
    efficiency_score = 0,
    primary_recommendation = "HOLD"
  )
  
  # Position Summary
  if (!is.null(results$analysis_results$current_position)) {
    pos <- results$analysis_results$current_position
    if (nrow(pos) > 0) {
      summary$position_status <- sprintf("%s %s contracts @ %.4f USDT", 
                                        pos$holdSide[1], pos$total[1], 
                                        as.numeric(pos$averageOpenPrice[1]))
      summary$current_pnl <- as.numeric(pos$unrealizedPL[1])
    }
  }
  
  # Market Sentiment from Enhanced Analysis
  if (!is.null(results$analysis_results$enhanced_analysis)) {
    enhanced <- results$analysis_results$enhanced_analysis
    if (!is.null(enhanced$enhanced_market_data$sentiment)) {
      sentiment <- enhanced$enhanced_market_data$sentiment
      summary$market_sentiment <- sentiment$overall_sentiment
      summary$sentiment_score <- sentiment$sentiment_percentage
    }
  }
  
  # Technical Signal from Base Analysis
  if (!is.null(results$analysis_results$technical_analysis)) {
    tech <- results$analysis_results$technical_analysis
    if (!is.null(tech$signals)) {
      summary$technical_signal <- tech$signals$overall_signal
    }
  }
  
  # Efficiency Score
  if (!is.null(results$analysis_results$efficiency_analysis)) {
    eff <- results$analysis_results$efficiency_analysis
    if (!is.null(eff$summary$trading_score)) {
      summary$efficiency_score <- eff$summary$trading_score
    }
  }
  
  return(summary)
}

generate_trading_recommendations <- function(results) {
  recommendations <- list(
    immediate_actions = list(),
    position_management = list(),
    risk_management = list(),
    strategic_considerations = list()
  )
  
  # Immediate Actions based on current position
  if (!is.null(results$analysis_results$current_position)) {
    pos <- results$analysis_results$current_position
    if (nrow(pos) > 0) {
      current_pnl <- as.numeric(pos$unrealizedPL[1])
      
      if (current_pnl > 0) {
        recommendations$immediate_actions <- append(recommendations$immediate_actions, 
          "Consider partial profit taking - position currently profitable")
      } else {
        recommendations$immediate_actions <- append(recommendations$immediate_actions, 
          "Monitor position closely - currently underwater")
      }
    }
  }
  
  # Technical Signal Recommendations
  if (!is.null(results$summary$technical_signal)) {
    signal <- results$summary$technical_signal
    
    if (signal == "BUY") {
      recommendations$position_management <- append(recommendations$position_management,
        "Technical signals suggest buying opportunity")
    } else if (signal == "SELL") {
      recommendations$position_management <- append(recommendations$position_management,
        "Technical signals suggest selling pressure")
    }
  }
  
  # Risk Management based on efficiency
  if (!is.null(results$summary$efficiency_score)) {
    score <- results$summary$efficiency_score
    
    if (score < 50) {
      recommendations$risk_management <- append(recommendations$risk_management,
        "Trading efficiency below average - consider tighter risk controls")
    } else if (score > 80) {
      recommendations$risk_management <- append(recommendations$risk_management,
        "Excellent trading efficiency - maintain current strategy")
    }
  }
  
  return(recommendations)
}

# ==========================================================================================================
# 📋 COMPREHENSIVE OUTPUT DISPLAY
# ==========================================================================================================

display_comprehensive_results <- function(results) {
  cat("\n🚀 COMPREHENSIVE TRADING ANALYSIS RESULTS\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf("📅 Analysis Time: %s\n", results$timestamp))
  cat(sprintf("🎯 Primary Symbol: %s\n", results$config$symbol))
  
  # === EXECUTIVE SUMMARY ===
  cat("\n📊 EXECUTIVE SUMMARY\n")
  cat(strrep("=", 25), "\n")
  
  summary <- results$summary
  cat(sprintf("🔍 Position Status: %s\n", summary$position_status))
  if (!is.null(summary$current_pnl)) {
    cat(sprintf("💰 Current P&L: %.2f USDT (%s)\n", 
                summary$current_pnl, 
                if(summary$current_pnl > 0) "🟢 PROFIT" else "🔴 LOSS"))
  }
  cat(sprintf("🎭 Market Sentiment: %s\n", summary$market_sentiment))
  cat(sprintf("📈 Technical Signal: %s\n", summary$technical_signal))
  cat(sprintf("⭐ Efficiency Score: %d/100\n", summary$efficiency_score))
  
  # === CURRENT MARKET DATA ===
  if (!is.null(results$analysis_results$market_data)) {
    market <- results$analysis_results$market_data
    
    cat("\n📈 CURRENT MARKET DATA\n")
    cat(strrep("=", 25), "\n")
    
    if (!is.null(market$ticker)) {
      ticker <- market$ticker
      cat(sprintf("💰 Current Price: %.4f USDT\n", ticker$last_price))
      cat(sprintf("📊 24h Change: %.2f%%\n", ticker$change_24h_pct))
      cat(sprintf("📈 24h High: %.4f USDT\n", ticker$high_24h))
      cat(sprintf("📉 24h Low: %.4f USDT\n", ticker$low_24h))
      cat(sprintf("💸 24h Volume: %.0f USDT\n", ticker$volume_24h_usdt))
    }
    
    if (!is.null(market$orderbook)) {
      orderbook <- market$orderbook
      cat(sprintf("📚 Bid-Ask Spread: %.4f%% \n", orderbook$spread_pct))
    }
    
    if (!is.null(market$sentiment)) {
      sentiment <- market$sentiment
      cat(sprintf("🎯 Market Sentiment: %s (%.0f%%)\n", 
                  sentiment$overall_sentiment, sentiment$sentiment_percentage))
    }
  }
  
  # === TECHNICAL INDICATORS ===
  if (!is.null(results$analysis_results$technical_analysis)) {
    tech <- results$analysis_results$technical_analysis
    
    cat("\n🧮 TECHNICAL INDICATORS\n")
    cat(strrep("=", 25), "\n")
    
    if (!is.null(tech$indicators)) {
      indicators <- tail(tech$indicators, 1)
      cat(sprintf("📊 RSI(14): %.2f\n", indicators$rsi_14))
      cat(sprintf("📈 SMA(10): %.4f USDT\n", indicators$sma_10))
      cat(sprintf("📉 SMA(20): %.4f USDT\n", indicators$sma_20))
      if (!is.na(indicators$macd)) {
        cat(sprintf("🌊 MACD: %.6f\n", indicators$macd))
      }
    }
    
    if (!is.null(tech$signals)) {
      signals <- tech$signals
      cat(sprintf("🎯 Overall Signal: %s\n", signals$overall_signal))
      cat(sprintf("📊 RSI Signal: %s\n", signals$rsi_signal))
      cat(sprintf("📈 Trend Signal: %s\n", signals$sma_signal))
    }
  }
  
  # === MULTI-TIMEFRAME PERFORMANCE ===
  if (!is.null(results$analysis_results$timeframe_analysis)) {
    timeframes <- results$analysis_results$timeframe_analysis
    
    cat("\n⏰ MULTI-TIMEFRAME PERFORMANCE\n")
    cat(strrep("=", 35), "\n")
    
    for (period_name in names(timeframes)) {
      period_data <- timeframes[[period_name]]
      days <- gsub("days_", "", period_name)
      
      if (!is.null(period_data$metrics)) {
        metrics <- period_data$metrics
        if (!is.null(metrics$win_rate) && !is.null(metrics$profit_loss)) {
          cat(sprintf("📅 %s Days: %.1f%% Win Rate, %.2f USDT P&L\n", 
                      days, metrics$win_rate$win_rate_pct, metrics$profit_loss$total_pnl))
        }
      }
    }
  }
  
  # === ACTIVE ORDERS STATUS ===
  if (!is.null(results$analysis_results$current_orders)) {
    orders <- results$analysis_results$current_orders
    
    cat("\n📋 ACTIVE ORDERS\n")
    cat(strrep("=", 15), "\n")
    
    if (nrow(orders) > 0) {
      for (i in 1:nrow(orders)) {
        order <- orders[i, ]
        cat(sprintf("📄 Order %d: %s at %.4f USDT (Size: %s)\n", 
                    i, order$planType, as.numeric(order$triggerPrice), order$size))
      }
    } else {
      cat("📭 No active orders\n")
    }
  }
  
  # === RECOMMENDATIONS ===
  cat("\n💡 TRADING RECOMMENDATIONS\n")
  cat(strrep("=", 30), "\n")
  
  recommendations <- results$recommendations
  
  if (length(recommendations$immediate_actions) > 0) {
    cat("🚨 IMMEDIATE ACTIONS:\n")
    for (action in recommendations$immediate_actions) {
      cat(sprintf("   • %s\n", action))
    }
  }
  
  if (length(recommendations$position_management) > 0) {
    cat("\n📊 POSITION MANAGEMENT:\n")
    for (action in recommendations$position_management) {
      cat(sprintf("   • %s\n", action))
    }
  }
  
  if (length(recommendations$risk_management) > 0) {
    cat("\n🛡️ RISK MANAGEMENT:\n")
    for (action in recommendations$risk_management) {
      cat(sprintf("   • %s\n", action))
    }
  }
  
  cat("\n✅ COMPREHENSIVE ANALYSIS COMPLETE!\n")
  
  return(invisible(results))
}

# ==========================================================================================================
# 🎯 MAIN EXECUTION FUNCTION
# ==========================================================================================================

execute_master_analysis <- function(symbol = "ADAUSDT_UMCBL", 
                                   include_multi_asset = TRUE,
                                   save_results = TRUE) {
  
  # Update configuration
  ANALYSIS_CONFIG$symbol <- symbol
  ANALYSIS_CONFIG$include_multi_asset <- include_multi_asset
  ANALYSIS_CONFIG$save_results <- save_results
  
  # Run comprehensive analysis
  results <- run_comprehensive_analysis(ANALYSIS_CONFIG)
  
  # Display results
  display_comprehensive_results(results)
  
  # Save results if requested
  if (save_results) {
    tryCatch({
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      
      # Save as RDS for R
      rds_path <- sprintf("c:/freeding/tbot202506/analysis_results/comprehensive_analysis_%s_%s.rds", 
                          symbol, timestamp)
      dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(results, rds_path)
      
      # Save summary as JSON for other tools
      if (OUTPUT_CONFIG$export_json) {
        json_path <- sprintf("c:/freeding/tbot202506/analysis_results/analysis_summary_%s_%s.json", 
                            symbol, timestamp)
        write(jsonlite::toJSON(results$summary, auto_unbox = TRUE, pretty = TRUE), json_path)
      }
      
      cat(sprintf("\n💾 Results saved:\n"))
      cat(sprintf("   📄 Full analysis: %s\n", basename(rds_path)))
      if (OUTPUT_CONFIG$export_json) {
        cat(sprintf("   📄 Summary JSON: %s\n", basename(json_path)))
      }
      
    }, error = function(e) {
      cat("⚠️ Could not save results:", e$message, "\n")
    })
  }
  
  return(results)
}

# ==========================================================================================================
# 🚀 READY TO EXECUTE
# ==========================================================================================================

cat("\n🚀 COMPREHENSIVE TRADING ANALYSIS MASTER READY!\n")
cat(strrep("=", 60), "\n")

cat("\n💡 USAGE EXAMPLES:\n")
cat("=================\n")

cat("\n# 1. COMPLETE ADA ANALYSIS:\n")
cat("ada_master_analysis <- execute_master_analysis('ADAUSDT_UMCBL')\n")

cat("\n# 2. SINGLE ASSET ANALYSIS:\n")
cat("btc_analysis <- execute_master_analysis('BTCUSDT_UMCBL', include_multi_asset = FALSE)\n")

cat("\n# 3. QUICK ANALYSIS WITHOUT SAVING:\n")
cat("quick_analysis <- execute_master_analysis('ADAUSDT_UMCBL', save_results = FALSE)\n")

cat("\n# 4. ACCESS SPECIFIC RESULTS:\n")
cat("print(ada_master_analysis$summary)\n")
cat("print(ada_master_analysis$recommendations)\n")
cat("View(ada_master_analysis$analysis_results$technical_analysis)\n")

cat("\n🎯 FEATURES:\n")
cat("============\n")
cat("✅ Complete market data collection\n")
cat("✅ Technical analysis integration\n")
cat("✅ Multi-timeframe performance\n")
cat("✅ Trading efficiency analysis\n")
cat("✅ Multi-asset comparison\n")
cat("✅ Position and order status\n")
cat("✅ Automated recommendations\n")
cat("✅ Results saving (RDS + JSON)\n")
cat("✅ Comprehensive output display\n")

cat("\n⚡ EXECUTE NOW:\n")
cat("ada_master_analysis <- execute_master_analysis('ADAUSDT_UMCBL')\n")

cat(strrep("=", 60), "\n")
cat("🎯 Master Analysis System Ready!\n")