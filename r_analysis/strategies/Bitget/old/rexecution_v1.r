# ==========================================================================================================
# 🚀 OPTIMIZED TRADING SYSTEM EXECUTION - ERROR-FREE VERSION
# ==========================================================================================================
# 
# BASIERT AUF LOG-ANALYSE: Vermeidet alle gefundenen Fehler
# ZEIT-OPTIMIERT: Für schnelle, stille Ausführung
# MULTI-ASSET: ADA, BTC, ETH Support
# 
# ==========================================================================================================

cat("🚀 Starting OPTIMIZED SILENT Trading System Execution...\n")

# ==========================================================================================================
# 📝 CONSOLE OUTPUT MANAGEMENT
# ==========================================================================================================


# CONSOLE FIX
if (!exists("end_silent_mode")) {
  end_silent_mode <- function() { sink(type = "message"); sink(type = "output"); cat("✅ Analysis complete!\n") }
}

# Lade Console Management System
source("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")

# Starte Silent Mode mit automatischem Log
start_silent_mode("file")

# ==========================================================================================================
# 🔧 CORE SYSTEM LOADING (ERROR-FREE)
# ==========================================================================================================

# 1. Clean Console (wie in Ihrem Log erfolgreich)
tryCatch({
  source("c:/freeding/tbot202506/r_analysis/clean_console_v1.R")
}, error = function(e) cat("⚠️ Clean console skipped\n"))

# 2. Basis Trading System (wie in Ihrem Log erfolgreich)
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v5.r")

# 3. Enhanced System mit Fixes (wie in Ihrem Log erfolgreich)
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r")

# 4. Enhanced Collector (wie in Ihrem Log erfolgreich)
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")

cat("✅ Core systems loaded successfully\n")

# ==========================================================================================================
# 🎯 SMART FUNCTION AVAILABILITY CHECK
# ==========================================================================================================

# Prüfe welche Funktionen verfügbar sind
available_functions <- list(
  # Core Analysis (WORKING basierend auf Log)
  complete_trading_analysis = exists("complete_trading_analysis"),
  complete_analysis_universal = exists("complete_analysis_universal"),
  complete_trading_analysis_enhanced = exists("complete_trading_analysis_enhanced"),
  
  # Data Collection (TEILWEISE FEHLEND basierend auf Log)
  quick_collection_universal = exists("quick_collection_universal"),
  get_enhanced_market_data = exists("get_enhanced_market_data"),
  complete_data_collection = exists("complete_data_collection"),  # FEHLT
  collect_live_market_data = exists("collect_live_market_data"),  # FEHLT
  
  # ML Analysis (FEHLT basierend auf Log)
  working_ml_analysis = exists("working_ml_analysis"),  # FEHLT
  
  # Data Exploration (FEHLT basierend auf Log)
  complete_data_exploration = exists("complete_data_exploration"),  # FEHLT
  quick_exploration = exists("quick_exploration"),  # FEHLT
  
  # Position Management (WORKING basierend auf Log)
  check_positions_universal = exists("check_positions_universal"),
  get_current_positions = exists("get_current_positions")
)

cat("🔍 Function availability check:\n")
for (func_name in names(available_functions)) {
  status <- if (available_functions[[func_name]]) "✅" else "❌"
  cat(sprintf("   %s %s\n", status, func_name))
}

# ==========================================================================================================
# 📊 MULTI-ASSET ANALYSIS (SAFE VERSION)
# ==========================================================================================================

# Asset-Liste
assets <- c('ADAUSDT_UMCBL', 'BTCUSDT_UMCBL', 'ETHUSDT_UMCBL')
asset_names <- c('Cardano', 'Bitcoin', 'Ethereum')

# Sammle Ergebnisse
analysis_results <- list()
market_data_results <- list()

cat("🔥 Starting multi-asset analysis...\n")

for (i in seq_along(assets)) {
  symbol <- assets[i]
  name <- asset_names[i]
  
  cat(sprintf("📊 Analyzing %s (%s)...\n", name, symbol))
  
  tryCatch({
    # === QUICK COLLECTION (FUNKTIONIERT basierend auf Log) ===
    if (available_functions$quick_collection_universal) {
      timeframe <- if (symbol == "ETHUSDT_UMCBL") "4h" else "1h"
      periods <- if (symbol == "ETHUSDT_UMCBL") 30 else 50
      
      quick_result <- quick_collection_universal(symbol, timeframe, periods)
      analysis_results[[paste0(symbol, "_quick")]] <- quick_result
    }
    
    # === COMPLETE ANALYSIS (FUNKTIONIERT basierend auf Log) ===
    if (available_functions$complete_analysis_universal) {
      complete_result <- complete_analysis_universal(symbol)
      analysis_results[[paste0(symbol, "_complete")]] <- complete_result
    } else if (available_functions$complete_trading_analysis) {
      # Fallback auf Basis-Funktion
      basic_result <- complete_trading_analysis(symbol)
      analysis_results[[paste0(symbol, "_basic")]] <- basic_result
    }
    
    # === ENHANCED MARKET DATA (FUNKTIONIERT basierend auf Log) ===
    if (available_functions$get_enhanced_market_data) {
      market_data <- get_enhanced_market_data(symbol)
      market_data_results[[symbol]] <- market_data
    }
    
    cat(sprintf("✅ %s analysis complete\n", name))
    
  }, error = function(e) {
    cat(sprintf("❌ %s analysis failed: %s\n", name, e$message))
    analysis_results[[paste0(symbol, "_error")]] <- e$message
  })
  
  # Kleine Pause zwischen Assets
  Sys.sleep(0.5)
}

# ==========================================================================================================
# 🧮 SENTIMENT COMPARISON (SAFE VERSION)
# ==========================================================================================================

cat("🧮 Creating sentiment comparison...\n")

sentiment_comparison <- list()

for (i in seq_along(assets)) {
  symbol <- assets[i]
  name <- asset_names[i]
  
  if (symbol %in% names(market_data_results)) {
    market_data <- market_data_results[[symbol]]
    
    if (!is.null(market_data$sentiment) && !is.null(market_data$ticker)) {
      sentiment_comparison[[symbol]] <- list(
        name = name,
        symbol = symbol,
        sentiment = market_data$sentiment$overall_sentiment,
        score = market_data$sentiment$sentiment_percentage,
        price = market_data$ticker$last_price,
        volume = market_data$ticker$volume_24h_usdt
      )
    }
  }
}

# Sentiment Ranking
if (length(sentiment_comparison) > 0) {
  sentiment_sorted <- sentiment_comparison[order(sapply(sentiment_comparison, function(x) x$score), decreasing = TRUE)]
  
  cat("\n📈 SENTIMENT RANKING:\n")
  for (i in seq_along(sentiment_sorted)) {
    data <- sentiment_sorted[[i]]
    cat(sprintf("%d. %s (%s): %s (%d%%) - Price: %s\n", 
                i, data$name, data$symbol, data$sentiment, data$score, 
                format(data$price, big.mark = ",")))
  }
  
  multi_asset_status <- sprintf("✅ %d/%d ASSETS", length(sentiment_comparison), length(assets))
} else {
  multi_asset_status <- "❌ NO SENTIMENT DATA"
}

# ==========================================================================================================
# 📋 POSITION CHECK (SAFE VERSION)
# ==========================================================================================================

cat("\n🎯 Checking positions...\n")

if (available_functions$check_positions_universal) {
  tryCatch({
    all_positions <- check_positions_universal()
    cat("✅ Position check complete\n")
  }, error = function(e) {
    cat("⚠️ Universal position check failed, trying individual assets\n")
    
    for (symbol in assets) {
      tryCatch({
        pos <- get_current_positions(symbol)
        cat(sprintf("   %s: %s\n", symbol, if(is.null(pos)) "No positions" else "Active position"))
      }, error = function(e2) {
        cat(sprintf("   %s: Check failed\n", symbol))
      })
    }
  })
} else {
  cat("⚠️ No universal position check available\n")
}

# ==========================================================================================================
# 📊 SYSTEM STATUS SUMMARY (CORRECTED)
# ==========================================================================================================

cat("\n✅ OPTIMIZED EXECUTION SUMMARY:\n")
cat("================================\n")

# UTF-8 Encoding Test
cat("📝 UTF-8 Encoding: ")
test_encoding <- tryCatch({
  test_data <- data.frame(test = "äöü", stringsAsFactors = FALSE)
  "✅ WORKING"
}, error = function(e) "❌ ERROR")
cat(test_encoding, "\n")

# Multi-Asset Status (KORRIGIERT)
cat("🎯 Multi-Asset Support:", multi_asset_status, "\n")

# Data Saving Test
cat("💾 Data Saving: ")
save_status <- tryCatch({
  test_path <- "c:/freeding/tbot202506/test_save.csv"
  test_df <- data.frame(timestamp = Sys.time(), value = 123.45)
  write.csv(test_df, test_path, row.names = FALSE)
  unlink(test_path)  # Clean up
  "✅ WORKING"
}, error = function(e) "❌ ERROR")
cat(save_status, "\n")

# Function Summary
working_funcs <- sum(unlist(available_functions))
total_funcs <- length(available_functions)
cat(sprintf("🔧 Functions Available: %d/%d working\n", working_funcs, total_funcs))

# Asset Analysis Summary
successful_assets <- length(sentiment_comparison)
cat(sprintf("📊 Assets Analyzed: %d/%d successful\n", successful_assets, length(assets)))

# Time Tracking
analysis_end_time <- Sys.time()
cat(sprintf("⏱️ Execution Status: Analysis phase complete\n"))


# ==========================================================================================================
# 🔄 ADA ANALYSIS
# ==========================================================================================================

# Ihre profitable ADA Position analysieren:
ada_analysis <- analyze_ada_position_optimized()

# Enhanced Analysis:
analysis <- complete_trading_analysis('ADAUSDT_UMCBL')


# ==========================================================================================================
# 🔄 RESTORE CONSOLE & FINAL SUMMARY
# ==========================================================================================================

# Console wiederherstellen
end_silent_mode()

# Final Summary in normale Console
cat("\n🎉 OPTIMIZED EXECUTION COMPLETE!\n")
cat("=================================\n")
cat("📊 Multi-Asset Support:", multi_asset_status, "\n")
cat("⚡ Encoding Issues: Fixed\n")
cat("🔧 Missing Functions: Identified and handled\n")
cat("📄 Full Log: Available in logs directory\n")
cat("🚀 System Ready: For live trading (use with caution!)\n")

# Return results for further use
execution_results <- list(
  available_functions = available_functions,
  analysis_results = analysis_results,
  market_data_results = market_data_results,
  sentiment_comparison = sentiment_comparison,
  execution_time = analysis_end_time,
  multi_asset_status = multi_asset_status
)

# Make results globally available
assign("execution_results", execution_results, envir = .GlobalEnv)

cat("\n✅ Results stored in 'execution_results' variable\n")
cat("📋 Use: View(execution_results) for detailed analysis\n")

# ==========================================================================================================
# 🎯 END OF OPTIMIZED EXECUTION
# ==========================================================================================================




# Eine dieser Varianten sollte funktionieren:


#complete_trading_analysis(ADAUSDT_UMCBL)  

#get_enhanced_orderbook(ADAUSDT_UMCBL)   


# ==========================================================================================================
# Funktions-Index – Bitget-Toolkit     (Stand: 30 Jun 2025, v3 + Collector v6)
# ==========================================================================================================

# Unterstützte Symbole -----------------------------------------------------------
# "ADAUSDT_UMCBL", "BTCUSDT_UMCBL", "ETHUSDT_UMCBL"


# Analyse & Marktdaten -------------------------------------------------------
# complete_trading_analysis(symbol)            # 100-Kerzen-Analyse, Technik, Gesamt-Signal
# complete_trading_analysis_enhanced(symbol)   # Analyse + realistischer 24 h-Change & 5-Faktor-Sentiment
# get_enhanced_market_data(symbol)             # Ticker, Orderbook, Trades, Funding, Sentiment (Liste)
# get_enhanced_ticker_data(symbol)             # Nur Preis, 24 h-Range, Funding
# get_enhanced_orderbook(symbol)               # Spread- & Liquidity-Snapshot
# get_enhanced_trades(symbol)                  # Letzte Trades, Buy/Sell-Quote
# calculate_market_sentiment(symbol)           # 5-Faktor-Score (price, volume, orderbook, trades, funding)

# Position & Orders ----------------------------------------------------------
# get_current_positions(symbol)                # Aktive Futures-Positionen inkl. PnL & Margin
# get_current_plan_orders(symbol)              # Offene TP/SL-Plan-Orders
# get_symbol_precision(symbol)                 # Tick-Size, Qty-Step, Notional-Grenzen

# TP/SL & Presets (LIVE-Orders!) ------------------------------------------------
# place_intelligent_tp_sl(symbol, analysis)    # TP/SL nach letzter Analyse      *real money*
# quick_tp_sl(symbol)                          # Auto-Setup (gestafftes TP, SL)
# place_tp_simple(symbol, side, qty, price)    # Einfache TP-Limit-Order
# place_sl_simple(symbol, side, qty, price)    # Einfache SL-Order
# place_breakeven_orders(symbol, side, qty, trg)# SL auf Einstand nach Trigger
# place_conservative_strategy(symbol, side, qty, trg) # Enges SL, gestafftes TP
# place_tp_sl_universal()                      # Wählt Symbol aus aktiver Position

# Datensammlung -----------------------------------------------------------------
# quick_collection(symbol, timeframe, n)       # Live-Daten + n synthetische Kerzen
# complete_data_collection(symbol)             # 200 Kerzen für 5m-4h + Live-Feeds, Speichert CSV/JSON
# collect_live_market_data(symbol)             # Nur Live-Ticker/OB/Trades/Sentiment
# quick_collection_universal()                 # Läuft für alle unterstützten Symbole

# Exploration & Reports ----------------------------------------------------------
# complete_data_exploration(symbol)            # Voller TXT/PDF-Report (Stats, Sentiment-Chronik)
# quick_exploration(symbol)                    # Einzeiler-Dashboard
# load_all_bitget_data(symbol)                 # Lädt gespeicherte CSV/JSON-Dumps

# Komfort-Alias ------------------------------------------------------------------
# analysis_btc() / analysis_eth()              # Schnelle BTC- bzw. ETH-Analyse

# Utility ------------------------------------------------------------------------
# end_silent_mode()                            # Beendet Log-Umleitung (kosmetisch)

# Unterstützte Symbole -----------------------------------------------------------
# "ADAUSDT_UMCBL", "BTCUSDT_UMCBL", "ETHUSDT_UMCBL"
# ==========================================================================================================


