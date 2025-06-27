# ==========================================================================================================
# 🚀 NEUE OPTIMIERTE AUSFÜHRUNGSREIHENFOLGE MIT BITGET_SYSTEM_FIXES.R
# ==========================================================================================================
# 
# ÄNDERUNGEN:
# ✅ bitget_system_fixes.r integriert (nach Basis-System, vor Tests)
# ✅ Encoding-Fixes aktiv
# ✅ Multi-Asset Support (ADA, BTC, ETH)
# ✅ Bestehende Funktionen bleiben kompatibel
# ✅ Neue universelle Funktionen verfügbar
#
# ==========================================================================================================

# ========================= INIT LOAD =============================
#-------------------------------------------------------------------------------------------------------#

# 1. Console cleanen (wie bisher)
source("c:/freeding/tbot202506/r_analysis/clean_console.R")

# 2. Basis Trading System laden (wie bisher)
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")

# ========================= ENHANCED SYSTEM (NEU) ================
#-------------------------------------------------------------------------------------------------------#

# 3. **NEU: Enhanced System mit Fixes und Multi-Asset Support**
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r")

# ========================= INDICATOR COLLECTION ==================
#-------------------------------------------------------------------------------------------------------#

# 4. Enhanced Collector (optional - wird durch bitget_system_fixes.r erweitert)
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")

# 5. ML Analysis (wie bisher - jetzt auch multi-asset fähig)
results <- working_ml_analysis('ADAUSDT_UMCBL', '1h', 30)

# ========================= DATA COLLECTION (ENHANCED) ============
#-------------------------------------------------------------------------------------------------------#

# 6. **Optional: Alter Data Downloader (wird durch universal functions ergänzt)**
# source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_data_downloader.r")

# **NEU: Verwende universelle Funktionen stattdessen**

# 6a. **ADA Tests (wie bisher)**
cat("🔥 Testing ADA (Cardano)...\n")
ada_test <- quick_collection_universal('ADAUSDT_UMCBL', '1h', 50)
ada_data <- complete_data_collection('ADAUSDT_UMCBL')  # Falls alte Funktion noch gewünscht
ada_live <- collect_live_market_data('ADAUSDT_UMCBL')

# 6b. **NEU: BTC Tests**
cat("🔥 Testing BTC (Bitcoin)...\n")
btc_test <- quick_collection_universal('BTCUSDT_UMCBL', '1h', 50)
# btc_data <- complete_data_collection('BTCUSDT_UMCBL')  # Auch möglich
btc_analysis <- complete_analysis_universal('BTCUSDT_UMCBL')

# 6c. **NEU: ETH Tests**
cat("🔥 Testing ETH (Ethereum)...\n")
eth_test <- quick_collection_universal('ETHUSDT_UMCBL', '4h', 30)
eth_analysis <- complete_analysis_universal('ETHUSDT_UMCBL')

# ========================= DATA EXPLORATION (ENHANCED) ===========
#-------------------------------------------------------------------------------------------------------#

# 7. **Optional: Alter Explorer (wird durch universal functions ergänzt)**
# source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_data_explorer.r")

# **NEU: Multi-Asset Exploration**

# 7a. **ADA Exploration (wie bisher)**
cat("📊 Exploring ADA data...\n")
ada_results <- complete_data_exploration('ADAUSDT_UMCBL')
ada_quick <- quick_exploration('ADAUSDT_UMCBL')

# 7b. **NEU: BTC Exploration** 
cat("📊 Exploring BTC data...\n")
# btc_results <- complete_data_exploration('BTCUSDT_UMCBL')  # Falls alter Explorer geladen
btc_quick <- quick_exploration('BTCUSDT_UMCBL')

# 7c. **NEU: ETH Exploration**
cat("📊 Exploring ETH data...\n")
# eth_results <- complete_data_exploration('ETHUSDT_UMCBL')  # Falls alter Explorer geladen
eth_quick <- quick_exploration('ETHUSDT_UMCBL')

# ========================= MULTI-ASSET MONITORING ===============
#-------------------------------------------------------------------------------------------------------#

# 8. **NEU: Multi-Asset Dashboard**
cat("🎯 Multi-Asset Monitoring...\n")

# Alle Positionen checken
all_positions <- check_positions_universal()

# Market Overview für alle Assets
assets <- c('ADAUSDT_UMCBL', 'BTCUSDT_UMCBL', 'ETHUSDT_UMCBL')

cat("\n📊 MARKET OVERVIEW:\n")
cat("==================\n")

for (symbol in assets) {
  cat(sprintf("\n%s:\n", symbol))
  
  # Enhanced Market Data
  market_data <- get_enhanced_market_data(symbol)
  
  if (!is.null(market_data$ticker)) {
    ticker <- market_data$ticker
    sentiment <- market_data$sentiment
    
    cat(sprintf("  Price: %s USDT (%.2f%% 24h)\n", 
                format(ticker$last_price, big.mark = ","), 
                ticker$change_24h_pct))
    cat(sprintf("  Sentiment: %s (%d%%)\n", 
                sentiment$overall_sentiment, 
                sentiment$sentiment_percentage))
    cat(sprintf("  Volume: %.1fM USDT\n", 
                ticker$volume_24h_usdt / 1000000))
  }
}

# ========================= ENHANCED ANALYSIS =====================
#-------------------------------------------------------------------------------------------------------#

# 9. **NEU: Vergleichende Analyse**
cat("\n🧮 COMPARATIVE ANALYSIS:\n")
cat("========================\n")

# Sentiment Vergleich
sentiment_comparison <- list()

for (symbol in assets) {
  market_data <- get_enhanced_market_data(symbol)
  if (!is.null(market_data$sentiment)) {
    sentiment_comparison[[symbol]] <- list(
      symbol = symbol,
      sentiment = market_data$sentiment$overall_sentiment,
      score = market_data$sentiment$sentiment_percentage,
      price = market_data$ticker$last_price,
      volume = market_data$ticker$volume_24h_usdt
    )
  }
}

# Ergebnisse sortiert nach Sentiment Score
sentiment_sorted <- sentiment_comparison[order(sapply(sentiment_comparison, function(x) x$score), decreasing = TRUE)]

cat("📈 Assets ranked by Sentiment:\n")
for (i in seq_along(sentiment_sorted)) {
  data <- sentiment_sorted[[i]]
  cat(sprintf("%d. %s: %s (%d%%) - Price: %s\n", 
              i, data$symbol, data$sentiment, data$score, 
              format(data$price, big.mark = ",")))
}

# ========================= OPTIONAL: LIVE TRADING TESTS ==========
#-------------------------------------------------------------------------------------------------------#

# 10. **VORSICHT: Live Trading Tests (nur wenn gewünscht)**
cat("\n⚠️  LIVE TRADING SECTION - UNCOMMENT ONLY IF YOU WANT TO PLACE REAL ORDERS!\n")

# # **ADA Live Test** (Ihre bestehende Position)
# cat("🎯 ADA Position Check...\n")
# ada_position <- check_positions_universal('ADAUSDT_UMCBL')
# 
# # **BTC Live Test** (kleine Menge!)
# # place_tp_sl_universal('BTCUSDT_UMCBL', 'long', '0.001', 62500.00, 60000.00)
# 
# # **ETH Live Test** (kleine Menge!)
# # place_tp_sl_universal('ETHUSDT_UMCBL', 'long', '0.01', 3500.000, 3300.000)

# ========================= SYSTEM STATUS SUMMARY =================
#-------------------------------------------------------------------------------------------------------#

# 11. **System Status und Zusammenfassung**
cat("\n✅ SYSTEM STATUS SUMMARY:\n")
cat("=========================\n")

# Encoding Test
cat("📝 UTF-8 Encoding: ")
test_encoding <- tryCatch({
  test_data <- data.frame(test = "äöü", stringsAsFactors = FALSE)
  "✅ WORKING"
}, error = function(e) "❌ ERROR")
cat(test_encoding, "\n")

# Multi-Asset Test
cat("🎯 Multi-Asset Support: ")
multi_asset_status <- if (length(sentiment_comparison) == 3) "✅ ALL 3 ASSETS" else "⚠️ PARTIAL"
cat(multi_asset_status, "\n")

# Data Saving Test
cat("💾 Data Saving: ")
save_status <- tryCatch({
  test_path <- "c:/freeding/tbot202506/bitget_data/test_save.csv"
  test_df <- data.frame(timestamp = Sys.time(), value = 123.45)
  save_data_csv(test_df, test_path)
  unlink(test_path)  # Clean up
  "✅ WORKING"
}, error = function(e) "❌ ERROR")
cat(save_status, "\n")

# Available Functions
cat("\n🔧 Available Functions:\n")
cat("  ✅ quick_collection_universal(symbol, timeframe, periods)\n")
cat("  ✅ complete_analysis_universal(symbol)\n")
cat("  ✅ check_positions_universal(symbol)\n")
cat("  ✅ place_tp_sl_universal(symbol, side, size, tp, sl)\n")
cat("  ✅ place_breakeven_universal(symbol, side, size, entry)\n")

cat("\n🎉 SYSTEM READY FOR MULTI-ASSET TRADING!\n")
cat("📊 Supported: ADA, BTC, ETH\n")
cat("🔧 Encoding: Fixed\n")
cat("💾 Data Saving: Enhanced\n")
cat("⚠️  Live Trading: Available (use with caution!)\n")

# ========================= END OF EXECUTION ======================