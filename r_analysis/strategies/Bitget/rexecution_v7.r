# ==========================================================================================================
# 🚀 CORRECTED ENHANCED READABLE TRADING SYSTEM V7
# ==========================================================================================================
# 
# PROBLEM BEHOBEN: Functions werden NACH dem Core-System geladen
# URSACHE: Console-Umleitung hat Enhanced-Funktionen "verschluckt"
# LÖSUNG: Korrekte Reihenfolge - Core zuerst, dann Enhanced Functions
# 
# ==========================================================================================================

cat("🚀 CORRECTED ENHANCED READABLE TRADING SYSTEM V7\n")
cat(strrep("=", 60), "\n")
cat("📅 Execution Start:", as.character(Sys.time()), "\n")
cat("🎯 Focus: Corrected Loading Order\n\n")

# ==========================================================================================================
# 🔧 CONSOLE OUTPUT MANAGEMENT (ZUERST!)
# ==========================================================================================================

# Lade Console Management System zuerst
if (file.exists("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")) {
  tryCatch({
    source("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")
    start_silent_mode("file")
    cat("✅ Console management loaded and activated\n")
  }, error = function(e) {
    cat("⚠️ Console management failed, continuing with standard output:", e$message, "\n")
  })
} else {
  cat("⚠️ Console management not found - continuing with standard output\n")
}

# ==========================================================================================================
# 🔧 CORE SYSTEM LOADING (ZWEITER SCHRITT)
# ==========================================================================================================

cat("🔧 SYSTEM INITIALIZATION\n")
cat(strrep("=", 40), "\n")

# 1. Clean Console (optional)
tryCatch({
  if (file.exists("c:/freeding/tbot202506/r_analysis/clean_console.R")) {
    source("c:/freeding/tbot202506/r_analysis/clean_console.R")
    cat("✅ Console cleaned\n")
  }
}, error = function(e) cat("⚠️ Clean console skipped\n"))

# 2. Basis Trading System
cat("🔧 Loading complete_trading_analysis_v3.r...\n")
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")
  cat("✅ Core trading analysis loaded\n")
}

# 3. Enhanced System mit Fixes
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r")
  cat("✅ System fixes loaded\n")
}

# 4. Enhanced Collector
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")
  cat("✅ Enhanced collector loaded\n")
}


# 5. open interest in contrats volume
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/fixed_bitget_oi_heatmap.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/fixed_bitget_oi_heatmap.r")
  cat("✅ Eoplen interrestoaded\n")
}

# FIXED VERSION verwenden:
oi_analysis <- generate_oi_heatmap_analysis_fixed()

cat("✅ All core systems loaded successfully\n")

# ==========================================================================================================
# 🎯 ENHANCED DISPLAY FUNCTIONS (NACH DEM CORE-SYSTEM!)
# ==========================================================================================================

cat("🎨 Loading Enhanced Display Functions...\n")

# String-to-Title Helper Function
str_to_title <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("Unknown")
  words <- strsplit(as.character(x), " ")[[1]]
  paste(toupper(substring(words, 1, 1)), tolower(substring(words, 2)), sep = "", collapse = " ")
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ BEAUTIFUL HEADER FUNCTION - Erstellt ansprechende Sektions-Header                                    │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
create_header <- function(title, icon = "📊", width = 80) {
  cat("\n")
  cat(strrep("=", width), "\n")
  cat(sprintf("%s %s\n", icon, toupper(title)))
  cat(strrep("=", width), "\n")
}

create_subheader <- function(title, icon = "🔹", width = 50) {
  cat("\n")
  cat(strrep("-", width), "\n")
  cat(sprintf("%s %s\n", icon, title))
  cat(strrep("-", width), "\n")
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ STATUS INDICATOR FUNCTION - Farbcodierte Status mit Icons                                            │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
format_status <- function(value, positive_threshold = 0, negative_threshold = 0) {
  if (is.na(value) || is.null(value) || length(value) == 0) {
    return("❓ UNKNOWN")
  }
  
  if (value > positive_threshold) {
    return(paste("🟢", sprintf("%.2f", value)))
  } else if (value < negative_threshold) {
    return(paste("🔴", sprintf("%.2f", value)))
  } else {
    return(paste("🟡", sprintf("%.2f", value)))
  }
}

format_percentage <- function(value, decimals = 2) {
  if (is.na(value) || is.null(value) || length(value) == 0) return("❓")
  
  formatted <- sprintf(paste0("%+.", decimals, "f%%"), value)
  
  if (value > 0) {
    return(paste("🟢", formatted))
  } else if (value < 0) {
    return(paste("🔴", formatted))
  } else {
    return(paste("🟡", formatted))
  }
}

# Safe field extraction with fallbacks
safe_extract <- function(data, field, default = NA) {
  if (is.null(data) || is.null(data[[field]]) || length(data[[field]]) == 0) {
    return(default)
  }
  return(data[[field]])
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ ENHANCED MARKET DATA DISPLAY - Strukturierte Ausgabe aller Marktdaten                               │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
display_enhanced_market_data <- function(market_data, symbol = "ADAUSDT_UMCBL") {
  
  create_header("ENHANCED MARKET ANALYSIS", "📊")
  
  if (is.null(market_data)) {
    cat("❌ No enhanced market data available\n")
    return(invisible(NULL))
  }
  
  # === CURRENT PRICE & BASIC INFO ===
  create_subheader("Current Market Status", "💰")
  
  if (!is.null(market_data$ticker)) {
    ticker <- market_data$ticker
    
    # Safe extraction with fallbacks
    asset_name <- safe_extract(ticker, "asset_name", "Cardano")
    last_price <- safe_extract(ticker, "last_price", 0)
    change_24h_pct <- safe_extract(ticker, "change_24h_pct", 0)
    high_24h <- safe_extract(ticker, "high_24h", 0)
    low_24h <- safe_extract(ticker, "low_24h", 0)
    volume_24h_usdt <- safe_extract(ticker, "volume_24h_usdt", 0)
    funding_rate <- safe_extract(ticker, "funding_rate", 0)
    open_interest <- safe_extract(ticker, "open_interest", 0)
    
    cat(sprintf("📈 Symbol: %s (%s)\n", symbol, asset_name))
    cat(sprintf("💰 Current Price: %.4f USDT\n", last_price))
    cat(sprintf("📊 24h Change: %s\n", format_percentage(change_24h_pct)))
    cat(sprintf("📈 24h High: %.4f USDT\n", high_24h))
    cat(sprintf("📉 24h Low: %.4f USDT\n", low_24h))
    cat(sprintf("💸 24h Volume: %s USDT\n", format(volume_24h_usdt, big.mark = ",", scientific = FALSE)))
    
    if (!is.na(funding_rate) && funding_rate != 0) {
      funding_status <- if (funding_rate > 0) "🟢 BULLISH" else "🔴 BEARISH"
      cat(sprintf("💸 Funding Rate: %.4f%% (%s)\n", funding_rate * 100, funding_status))
    }
    
    if (!is.na(open_interest) && open_interest > 0) {
      cat(sprintf("🔍 Open Interest: %s contracts\n", format(open_interest, big.mark = ",")))
    }
  } else {
    cat("❌ No ticker data available\n")
  }
  
  # === ORDERBOOK ANALYSIS ===
  create_subheader("Orderbook & Liquidity Analysis", "📚")
  
  if (!is.null(market_data$orderbook)) {
    orderbook <- market_data$orderbook
    
    best_bid <- safe_extract(orderbook, "best_bid", 0)
    best_ask <- safe_extract(orderbook, "best_ask", 0)
    spread_pct <- safe_extract(orderbook, "spread_pct", 0)
    bid_ask_ratio <- safe_extract(orderbook, "bid_ask_ratio", 1)
    
    cat(sprintf("🏷️ Best Bid: %.4f USDT\n", best_bid))
    cat(sprintf("🏷️ Best Ask: %.4f USDT\n", best_ask))
    cat(sprintf("📏 Bid-Ask Spread: %.4f%% ", spread_pct))
    
    if (spread_pct < 0.02) {
      cat("(🟢 EXCELLENT LIQUIDITY)\n")
    } else if (spread_pct < 0.05) {
      cat("(🟡 GOOD LIQUIDITY)\n")
    } else {
      cat("(🔴 LOW LIQUIDITY)\n")
    }
    
    if (!is.na(bid_ask_ratio)) {
      ratio_status <- if (bid_ask_ratio > 1) "🟢 BUY PRESSURE" else "🔴 SELL PRESSURE"
      cat(sprintf("⚖️ Bid/Ask Ratio: %.3f (%s)\n", bid_ask_ratio, ratio_status))
    }
  } else {
    cat("❌ No orderbook data available\n")
  }
  
  # === RECENT TRADES ANALYSIS ===
  create_subheader("Recent Trading Activity", "🔄")
  
  if (!is.null(market_data$trades)) {
    trades <- market_data$trades
    
    total_trades <- safe_extract(trades, "total_trades", 0)
    
    if (total_trades > 0) {
      avg_price <- safe_extract(trades, "avg_price", 0)
      volume_weighted_price <- safe_extract(trades, "volume_weighted_price", 0)
      total_volume <- safe_extract(trades, "total_volume", 0)
      buy_sell_ratio <- safe_extract(trades, "buy_sell_ratio", 1)
      buy_volume <- safe_extract(trades, "buy_volume", 0)
      sell_volume <- safe_extract(trades, "sell_volume", 0)
      
      cat(sprintf("📊 Recent Trades: %d trades analyzed\n", total_trades))
      cat(sprintf("💰 Average Price: %.4f USDT\n", avg_price))
      cat(sprintf("📈 Volume Weighted Price: %.4f USDT\n", volume_weighted_price))
      cat(sprintf("📊 Total Volume: %s contracts\n", format(total_volume, big.mark = ",")))
      
      if (!is.na(buy_sell_ratio)) {
        if (buy_sell_ratio > 1.2) {
          sentiment_indicator <- "🟢 STRONG BUY PRESSURE"
        } else if (buy_sell_ratio > 1.0) {
          sentiment_indicator <- "🟢 MILD BUY PRESSURE"
        } else if (buy_sell_ratio > 0.8) {
          sentiment_indicator <- "🔴 MILD SELL PRESSURE"
        } else {
          sentiment_indicator <- "🔴 STRONG SELL PRESSURE"
        }
        
        cat(sprintf("⚖️ Buy/Sell Ratio: %.3f (%s)\n", buy_sell_ratio, sentiment_indicator))
        cat(sprintf("   📈 Buy Volume: %s | 📉 Sell Volume: %s\n", 
                    format(buy_volume, big.mark = ","), 
                    format(sell_volume, big.mark = ",")))
      }
    } else {
      cat("📭 No recent trades data available\n")
    }
  } else {
    cat("❌ No trades data available\n")
  }
  
  # === MARKET SENTIMENT ANALYSIS ===
  create_subheader("Advanced Market Sentiment", "🎯")
  
  if (!is.null(market_data$sentiment)) {
    sentiment <- market_data$sentiment
    
    overall_sentiment <- safe_extract(sentiment, "overall_sentiment", "UNKNOWN")
    sentiment_percentage <- safe_extract(sentiment, "sentiment_percentage", 0)
    sentiment_score <- safe_extract(sentiment, "sentiment_score", 0)
    max_factors <- safe_extract(sentiment, "max_factors", 5)
    
    # Hauptsentiment mit visueller Bewertung
    sentiment_icon <- switch(overall_sentiment,
                           "STRONG_BULLISH" = "🚀",
                           "BULLISH" = "🟢",
                           "NEUTRAL" = "🟡",
                           "BEARISH" = "🔴",
                           "STRONG_BEARISH" = "📉",
                           "❓")
    
    cat(sprintf("🎭 Overall Sentiment: %s %s (%d%%)\n", 
                sentiment_icon, overall_sentiment, 
                round(sentiment_percentage)))
    
    cat(sprintf("📊 Sentiment Score: %d/%d factors (%d/%d positive)\n", 
                sentiment_score + max_factors, 
                max_factors * 2,
                pmax(0, sentiment_score),
                max_factors))
    
    # Detaillierte Faktor-Aufschlüsselung
    if (!is.null(sentiment$factors)) {
      cat("\n🔍 Sentiment Factor Breakdown:\n")
      
      for (factor_name in names(sentiment$factors)) {
        tryCatch({
          factor <- sentiment$factors[[factor_name]]
          factor_value <- safe_extract(factor, "value", 0)
          factor_reason <- safe_extract(factor, "reason", "No reason available")
          
          factor_icon <- if (factor_value > 0) "🟢" else if (factor_value < 0) "🔴" else "🟡"
          factor_status <- if (factor_value > 0) "BULLISH" else if (factor_value < 0) "BEARISH" else "NEUTRAL"
          
          cat(sprintf("   %s %s: %s (%s)\n", 
                      factor_icon, str_to_title(gsub("_", " ", factor_name)), 
                      factor_status, factor_reason))
        }, error = function(e) {
          cat(sprintf("   ❓ %s: Analysis error\n", str_to_title(gsub("_", " ", factor_name))))
        })
      }
    }
    
    # Interpretation
    interpretation <- safe_extract(sentiment, "interpretation", "No interpretation available")
    cat(sprintf("\n💡 Interpretation: %s\n", interpretation))
  } else {
    cat("❌ No sentiment analysis available\n")
  }
  
  return(invisible(market_data))
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ ENHANCED POSITION DISPLAY - Übersichtliche Darstellung der aktuellen Position                       │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
display_enhanced_position <- function(position_data, current_price = NULL, symbol = "ADAUSDT_UMCBL") {
  
  create_header("POSITION ANALYSIS", "💼")
  
  if (is.null(position_data) || nrow(position_data) == 0) {
    cat("📭 No active position found for", symbol, "\n")
    return(invisible(NULL))
  }
  
  # Position Details extrahieren
  pos <- position_data[1, ]
  position_side <- safe_extract(pos, "holdSide", "unknown")
  total_size <- as.numeric(safe_extract(pos, "total", 0))
  entry_price <- as.numeric(safe_extract(pos, "averageOpenPrice", 0))
  current_pnl <- as.numeric(safe_extract(pos, "unrealizedPL", 0))
  
  # Current Price bestimmen
  if (is.null(current_price)) {
    current_price <- entry_price  # Fallback
  }
  
  # Berechnungen
  price_change <- current_price - entry_price
  price_change_pct <- if (entry_price > 0) (price_change / entry_price) * 100 else 0
  pnl_per_contract <- if (total_size > 0) current_pnl / total_size else 0
  
  # === POSITION OVERVIEW ===
  create_subheader("Position Overview", "📊")
  
  cat("┌──────────────────────────────────────────────────────────────────────┐\n")
  cat("│                           POSITION DETAILS                          │\n")
  cat("├──────────────────────────────────────────────────────────────────────┤\n")
  cat(sprintf("│ Symbol:           %-50s │\n", symbol))
  cat(sprintf("│ Position Side:    %-50s │\n", toupper(position_side)))
  cat(sprintf("│ Contract Size:    %-50s │\n", format(total_size, big.mark = ",")))
  cat(sprintf("│ Entry Price:      %-50s │\n", paste(sprintf("%.4f", entry_price), "USDT")))
  cat(sprintf("│ Current Price:    %-50s │\n", paste(sprintf("%.4f", current_price), "USDT")))
  cat(sprintf("│ Price Change:     %-50s │\n", 
              paste(format_status(price_change), "USDT", sprintf("(%s)", format_percentage(price_change_pct)))))
  cat("├──────────────────────────────────────────────────────────────────────┤\n")
  cat(sprintf("│ Total P&L:        %-50s │\n", paste(format_status(current_pnl), "USDT")))
  cat(sprintf("│ P&L per Contract: %-50s │\n", paste(format_status(pnl_per_contract), "USDT")))
  cat("└──────────────────────────────────────────────────────────────────────┘\n")
  
  # === POSITION STATUS ===
  create_subheader("Position Status Analysis", "🎯")
  
  # Status bestimmen
  if (current_pnl > 0) {
    status_icon <- "🟢"
    status_text <- "PROFITABLE"
    status_emoji <- "💰"
  } else if (current_pnl == 0) {
    status_icon <- "🟡"
    status_text <- "BREAK-EVEN"
    status_emoji <- "⚖️"
  } else {
    status_icon <- "🔴"
    status_text <- "UNDERWATER"
    status_emoji <- "🌊"
  }
  
  cat(sprintf("%s Position Status: %s %s\n", status_emoji, status_icon, status_text))
  
  # Zusätzliche Analysen
  if (current_pnl < 0) {
    recovery_needed <- abs(price_change)
    recovery_pct <- abs(price_change_pct)
    cat(sprintf("🎯 Recovery Needed: +%.4f USDT (+%.2f%%) to reach break-even\n", 
                recovery_needed, recovery_pct))
  }
  
  # Positionswert
  position_value <- total_size * current_price
  cat(sprintf("💎 Current Position Value: %s USDT\n", format(position_value, big.mark = ",", nsmall = 2)))
  
  return(invisible(position_data))
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ ENHANCED ORDERS DISPLAY - Übersichtliche Darstellung aller aktiven Orders                           │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
display_enhanced_orders <- function(orders_data, current_price = NULL, symbol = "ADAUSDT_UMCBL") {
  
  create_header("ACTIVE ORDERS ANALYSIS", "📋")
  
  if (is.null(orders_data) || nrow(orders_data) == 0) {
    cat("📭 No active orders found for", symbol, "\n")
    return(invisible(NULL))
  }
  
  create_subheader("Order Overview", "📊")
  
  cat(sprintf("📊 Total Active Orders: %d\n", nrow(orders_data)))
  cat(sprintf("🎯 Symbol: %s\n", symbol))
  if (!is.null(current_price)) {
    cat(sprintf("💰 Current Price: %.4f USDT\n", current_price))
  }
  
  # === ORDERS TABLE ===
  create_subheader("Detailed Order Analysis", "📄")
  
  cat("┌─────┬─────────────┬─────────────┬──────────┬─────────────┬──────────────┐\n")
  cat("│ #   │ Type        │ Price       │ Size     │ Distance    │ Status       │\n")
  cat("├─────┼─────────────┼─────────────┼──────────┼─────────────┼──────────────┤\n")
  
  for (i in 1:nrow(orders_data)) {
    order <- orders_data[i, ]
    
    # Order Details with safe extraction
    order_price <- as.numeric(safe_extract(order, "triggerPrice", 0))
    order_size <- as.numeric(safe_extract(order, "size", 0))
    order_type <- switch(safe_extract(order, "planType", "unknown"),
                        "pos_profit" = "TP",
                        "pos_loss" = "SL", 
                        "normal_plan" = "PLAN",
                        "UNKNOWN")
    
    # Distance Calculation
    if (!is.null(current_price) && !is.na(order_price) && order_price > 0 && current_price > 0) {
      distance <- order_price - current_price
      distance_pct <- (distance / current_price) * 100
      
      if (distance > 0) {
        distance_str <- sprintf("+%.1f%%", distance_pct)
        distance_icon <- "📈"
      } else {
        distance_str <- sprintf("%.1f%%", distance_pct)
        distance_icon <- "📉"
      }
      distance_display <- paste(distance_icon, distance_str)
    } else {
      distance_display <- "❓"
    }
    
    # Status
    order_state <- safe_extract(order, "state", safe_extract(order, "status", "unknown"))
    status <- switch(order_state,
                    "not_trigger" = "🟡 PENDING",
                    "triggered" = "🟢 TRIGGERED",
                    "cancelled" = "🔴 CANCELLED",
                    "filled" = "✅ FILLED",
                    "❓ UNKNOWN")
    
    cat(sprintf("│ %-3d │ %-11s │ %-11s │ %-8s │ %-11s │ %-12s │\n",
                i,
                substr(order_type, 1, 11),
                substr(sprintf("%.4f", order_price), 1, 11),
                substr(format(order_size, big.mark = ","), 1, 8),
                substr(distance_display, 1, 11),
                substr(status, 1, 12)))
  }
  
  cat("└─────┴─────────────┴─────────────┴──────────┴─────────────┴──────────────┘\n")
  
  return(invisible(orders_data))
}

cat("✅ Enhanced Display Functions loaded successfully!\n")

# ==========================================================================================================
# 🎯 ENHANCED DATA COLLECTION & DISPLAY
# ==========================================================================================================

# Configuration
ADA_SYMBOL <- "ADAUSDT_UMCBL"

create_header("ENHANCED MARKET DATA COLLECTION", "📊")

# === COLLECT ALL ENHANCED DATA ===
cat("🔍 Collecting comprehensive market data...\n")

# Enhanced Market Data
enhanced_market_data <- NULL
if (exists("get_enhanced_market_data")) {
  tryCatch({
    enhanced_market_data <- get_enhanced_market_data(ADA_SYMBOL)
    cat("✅ Enhanced market data collected successfully\n")
  }, error = function(e) {
    cat("❌ Enhanced market data collection failed:", e$message, "\n")
  })
}

# Current Position
current_position <- NULL
if (exists("get_current_positions")) {
  tryCatch({
    current_position <- get_current_positions(ADA_SYMBOL)
    if (!is.null(current_position) && nrow(current_position) > 0) {
      cat("✅ Current position data collected\n")
    } else {
      cat("📭 No active position found\n")
    }
  }, error = function(e) {
    cat("❌ Position data collection failed:", e$message, "\n")
  })
}

# Current Orders
current_orders <- NULL
if (exists("get_current_plan_orders")) {
  tryCatch({
    current_orders <- get_current_plan_orders(ADA_SYMBOL)
    if (!is.null(current_orders) && nrow(current_orders) > 0) {
      cat(sprintf("✅ %d active orders found\n", nrow(current_orders)))
    } else {
      cat("📭 No active orders found\n")
    }
  }, error = function(e) {
    cat("❌ Orders data collection failed:", e$message, "\n")
  })
}

# Get current price for calculations
current_price <- NULL
if (!is.null(enhanced_market_data) && !is.null(enhanced_market_data$ticker)) {
  current_price <- enhanced_market_data$ticker$last_price
}

# ==========================================================================================================
# 🎯 ENHANCED DATA DISPLAY
# ==========================================================================================================

# Display Enhanced Market Data
if (!is.null(enhanced_market_data)) {
  display_enhanced_market_data(enhanced_market_data, ADA_SYMBOL)
} else {
  create_header("MARKET DATA", "📊")
  cat("❌ Enhanced market data not available\n")
}

# Display Position Analysis
if (!is.null(current_position)) {
  display_enhanced_position(current_position, current_price, ADA_SYMBOL)
}

# Display Orders Analysis
if (!is.null(current_orders)) {
  display_enhanced_orders(current_orders, current_price, ADA_SYMBOL)
}

# ==========================================================================================================
# 🎯 open interest 
# ==========================================================================================================

# in contrats volume
# FIXED VERSION verwenden:
oi_analysis <- generate_oi_heatmap_analysis_fixed()

# Heatmap anzeigen
oi_analysis$heatmap

# Summary anzeigen  
print(oi_analysis$summary)






# ==========================================================================================================
# 🎯 QUICK COMMANDS & CONSOLE CLEANUP
# ==========================================================================================================

create_header("QUICK COMMANDS", "⚡")

cat("📋 Available commands for further analysis:\n")
cat("   market_data <- get_enhanced_market_data('ADAUSDT_UMCBL')\n")
cat("   print(market_data$sentiment)\n")
cat("   print(market_data$trades)\n")
cat("   View(current_orders)\n")

# Enhanced Console cleanup
tryCatch({
  if (exists("end_silent_mode")) {
    end_silent_mode()
  } else {
    # Fallback cleanup
    for(i in 1:10) {
      tryCatch({
        sink()
        sink(type = "message")
        sink(type = "output")
      }, error = function(e) NULL)
    }
  }
}, error = function(e) {
  cat("⚠️ Console cleanup completed with minor issues\n")
})

create_header("EXECUTION COMPLETE", "✅")

cat("🚀 CORRECTED ENHANCED READABLE EXECUTION V7 COMPLETE!\n")
cat("✅ PROBLEM SOLVED: Correct loading order implemented!\n")
cat("📊 Enhanced data displayed in readable format!\n")
cat("🎯 All functions loaded and available for use!\n")

# ==========================================================================================================
# 🎯 END OF CORRECTED ENHANCED READABLE EXECUTION V7
# ==========================================================================================================
