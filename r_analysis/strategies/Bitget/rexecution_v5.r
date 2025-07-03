# ==========================================================================================================
# 🚀 ENHANCED TRADING SYSTEM EXECUTION - WITH STRATEGIC ORDER SUPPORT
# ==========================================================================================================
# 
# ENHANCED: Strategischer Order von 5000 Kontrakten bei 0.5636 USDT hinzugefügt
# NEUE POSITION: 1000 ADA @ 0.6014 USDT (+3.43 USDT profitabel)
# STRATEGISCHER ORDER: 5000 ADA @ 0.5636 USDT (Buy Limit Order)
# 
# ==========================================================================================================

cat("🚀 Starting ENHANCED Trading System with Strategic Orders...\n")

# ==========================================================================================================
# 📝 CONSOLE OUTPUT MANAGEMENT
# ==========================================================================================================

# CONSOLE FIX

# CONSOLE FIX
if (!exists("end_silent_mode")) {
  end_silent_mode <- function() { sink(type = "message"); sink(type = "output"); cat("✅ Analysis complete!\n") }
}

# Lade Console Management System
source("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")

# Starte Silent Mode mit automatischem Log
start_silent_mode("file")


# Lade Console Management System
source("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")

# Starte Silent Mode mit automatischem Log
start_silent_mode("file")

# ==========================================================================================================
# 🔧 CORE SYSTEM LOADING (FIXED - MIT TP/SL SUPPORT)
# ==========================================================================================================

# 1. Clean Console (optional)
tryCatch({
  source("c:/freeding/tbot202506/r_analysis/clean_console.R")
}, error = function(e) cat("⚠️ Clean console skipped\n"))

# 2. BASIS TRADING SYSTEM (FIXED - V3 STATT V5!)
cat("🔧 Loading complete_trading_analysis_v3.r (WITH TP/SL functions)...\n")
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")

# 3. Enhanced System mit Fixes
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r")

# 4. Enhanced Collector 
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")

cat("✅ Core systems loaded successfully\n")

# ==========================================================================================================
# 🆕 STRATEGIC ORDER FUNCTIONS - NEUE FUNKTIONALITÄT
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ STRATEGIC LIMIT ORDER - Platziert Buy/Sell Limit Orders für strategische Einstiege                 │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
place_strategic_limit_order <- function(symbol, side, size, limit_price, symbol_info = NULL) {
  cat("📊 Placing Strategic Limit Order...\n")
  cat("   ⚠️  WARNING: PLACING REAL ORDER ON BITGET!\n")
  
  # Symbol-Präzision automatisch abrufen falls nicht vorhanden
  if (is.null(symbol_info) && exists("get_symbol_precision")) {
    symbol_info <- get_symbol_precision(symbol)
  }
  
  # Preis formatieren
  limit_price_formatted <- if (!is.null(symbol_info)) {
    format_price_precise(limit_price, symbol_info)
  } else {
    sprintf("%.4f", round(limit_price, 4))
  }
  
  # ORDER BODY für Standard Limit Order
  body <- list(
    symbol = symbol,                          # Trading Pair
    marginCoin = "USDT",                      # Margin Currency
    side = side,                              # "open_long" oder "open_short"
    orderType = "limit",                      # Limit Order Type
    price = limit_price_formatted,            # Limit Preis
    size = as.character(size),                # Order Größe
    timeInForceValue = "normal"               # Time in Force
  )
  
  # ORDER DETAILS ANZEIGEN
  cat("📋 Strategic Limit Order Details:\n")
  cat("   Symbol:", symbol, "\n")
  cat("   Side:", side, "\n")
  cat("   Type: limit\n")
  cat("   Size:", size, "contracts\n")
  cat("   Limit Price:", limit_price_formatted, "USDT\n")
  cat("   Total Value:", round(as.numeric(size) * limit_price, 2), "USDT\n")
  
  # ⚠️  ECHTE ORDER PLATZIEREN!
  if (exists("bitget_request")) {
    result <- bitget_request("/api/mix/v1/order/placeOrder", "POST", body)
    
    if (!is.null(result) && result$code == "00000") {
      cat("✅ Strategic Limit Order placed successfully!\n")
      cat("   Order ID:", result$data$orderId, "\n")
      return(list(success = TRUE, order_id = result$data$orderId, type = "limit"))
    } else {
      error_msg <- if(!is.null(result)) result$msg else "Unknown error"
      cat("❌ Strategic Limit Order failed:", error_msg, "\n")
      if (!is.null(result)) {
        cat("   Full error:", toJSON(result, auto_unbox = TRUE), "\n")
      }
      return(list(success = FALSE, error = error_msg, type = "limit"))
    }
  } else {
    cat("❌ bitget_request function not available\n")
    return(list(success = FALSE, error = "bitget_request not found", type = "limit"))
  }
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ STRATEGIC ORDERS CHECKER - Prüft aktuelle offene Orders                                             │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
get_current_open_orders <- function(symbol) {
  cat("📋 Checking current open orders for", symbol, "...\n")
  
  if (exists("bitget_request")) {
    params <- list(symbol = symbol, productType = "umcbl")
    result <- bitget_request("/api/mix/v1/order/current", "GET", params)
    
    if (!is.null(result) && result$code == "00000") {
      if (length(result$data) > 0 && nrow(result$data) > 0) {
        cat("📊 Found", nrow(result$data), "open orders:\n")
        
        for (i in 1:nrow(result$data)) {
          order <- result$data[i, ]
          cat(sprintf("   Order %d: %s %s at %.4f USDT (Size: %s)\n",
                      i, order$orderType, order$side, 
                      as.numeric(order$price), order$size))
        }
        return(result$data)
      } else {
        cat("📭 No open orders found\n")
        return(NULL)
      }
    } else {
      cat("❌ Failed to fetch open orders\n")
      return(NULL)
    }
  } else {
    cat("❌ bitget_request function not available\n")
    return(NULL)
  }
}

# ==========================================================================================================
# 🎯 FUNCTION AVAILABILITY CHECK
# ==========================================================================================================

available_functions <- list(
  # Core Analysis
  complete_trading_analysis = exists("complete_trading_analysis"),
  complete_analysis_universal = exists("complete_analysis_universal"),
  complete_trading_analysis_enhanced = exists("complete_trading_analysis_enhanced"),
  
  # Data Collection
  quick_collection_universal = exists("quick_collection_universal"),
  get_enhanced_market_data = exists("get_enhanced_market_data"),
  
  # Position Management
  check_positions_universal = exists("check_positions_universal"),
  get_current_positions = exists("get_current_positions"),
  
  # TP/SL Functions
  place_tp_simple = exists("place_tp_simple"),
  place_sl_simple = exists("place_sl_simple"),
  place_intelligent_tp_sl = exists("place_intelligent_tp_sl"),
  quick_tp_sl = exists("quick_tp_sl"),
  
  # Strategic Order Functions (NEW)
  place_strategic_limit_order = exists("place_strategic_limit_order"),
  get_current_open_orders = exists("get_current_open_orders")
)

cat("🔍 Function availability check:\n")
for (func_name in names(available_functions)) {
  status <- if (available_functions[[func_name]]) "✅" else "❌"
  cat(sprintf("   %s %s\n", status, func_name))
}

# ==========================================================================================================
# 🎯 ADA POSITION CHECK & ANALYSIS
# ==========================================================================================================

cat("\n📊 CHECKING ADA POSITION STATUS...\n")

# Aktuelle Position prüfen
ada_position <- get_current_positions('ADAUSDT_UMCBL')

if (!is.null(ada_position) && nrow(ada_position) > 0) {
  # Position Details
  position_side <- ada_position$holdSide[1]
  total_size <- as.numeric(ada_position$total[1])
  entry_price <- as.numeric(ada_position$averageOpenPrice[1])
  current_pnl <- as.numeric(ada_position$unrealizedPL[1])
  
  cat(sprintf("✅ ADA Position found:\n"))
  cat(sprintf("   Side: %s\n", position_side))
  cat(sprintf("   Size: %.0f contracts\n", total_size))
  cat(sprintf("   Entry: %.4f USDT\n", entry_price))
  cat(sprintf("   PnL: %.2f USDT\n", current_pnl))
  
  # Aktuelle Orders checken
  cat("\n📋 Checking existing orders...\n")
  existing_orders <- get_current_plan_orders('ADAUSDT_UMCBL')
  
  # Marktpreis abrufen
  market_data <- get_enhanced_ticker_data('ADAUSDT_UMCBL')
  current_price <- if(!is.null(market_data)) market_data$last_price else 0.6048
  
  cat(sprintf("   Current Price: %.4f USDT\n", current_price))
  cat(sprintf("   Position Status: %s\n", if(current_pnl > 0) "🟢 PROFITABLE" else "🔴 UNDERWATER"))
  
} else {
  cat("❌ No ADA position found!\n")
  current_price <- 0.6048   # Fallback
  entry_price <- 0.6014     # Fallback 
  total_size <- 1000        # Fallback
  position_side <- "long"   # Fallback
}

# ==========================================================================================================
# 🔧 ENHANCED CONFIGURATION WITH STRATEGIC ORDER
# ==========================================================================================================

cat("\n🔧 ENHANCED CONFIGURATION WITH STRATEGIC ORDER\n")
cat("=============================================\n")

# Automatische Anpassung basierend auf aktueller Position
ADA_SYMBOL <- "ADAUSDT_UMCBL"

# TP Konfiguration (angepasst an 1000 Kontrakte Position)
TP_PRICE <- 0.6200        # Konservatives TP (+3% from current)
TP_SIZE <- 500            # 50% der Position

# SL Konfiguration  
SL_PRICE <- 0.5950        # Konservatives SL (-1% from entry)
SL_SIZE <- 1000           # Gesamte Position

# 🆕 STRATEGISCHER ORDER KONFIGURATION
STRATEGIC_PRICE <- 0.5636   # Ihr gewünschter strategischer Preis
STRATEGIC_SIZE <- 5000      # Ihre gewünschte Menge
STRATEGIC_SIDE <- "open_long"  # "open_long" für Buy, "open_short" für Sell

# Berechne strategische Order Details
strategic_total_value <- STRATEGIC_SIZE * STRATEGIC_PRICE
strategic_discount_pct <- ((current_price - STRATEGIC_PRICE) / current_price) * 100
strategic_risk_per_contract <- if(position_side == "long") STRATEGIC_PRICE - SL_PRICE else 0

# ==========================================================================================================

# ⚡ TRADING MODE CONTROL - EINFACH ZU ÄNDERN!
# ==========================================================================================================


EXECUTE_LIVE_ORDERS <- FALSE      # ← HIER: TRUE für Live Trading, FALSE für Simulation

EXECUTE_TP_ORDERS <- FALSE         # ← TP Orders aktivieren/deaktivieren

EXECUTE_SL_ORDERS <- FALSE         # ← SL Orders aktivieren/deaktivieren

EXECUTE_STRATEGIC_ORDERS <- FALSE  # ← 🆕 Strategische Orders aktivieren/deaktivieren

# ==========================================================================================================

# ==========================================================================================================



cat(sprintf("📊 Current Configuration:\n"))
cat(sprintf("   Position Size: %.0f contracts\n", total_size))
cat(sprintf("   Entry Price: %.4f USDT\n", entry_price))
cat(sprintf("   Current Price: %.4f USDT\n", current_price))
cat(sprintf("   TP Target: %.4f USDT (%.0f contracts)\n", TP_PRICE, TP_SIZE))
cat(sprintf("   SL Target: %.4f USDT (%.0f contracts)\n", SL_PRICE, SL_SIZE))

cat(sprintf("\n🆕 Strategic Order Configuration:\n"))
cat(sprintf("   Strategic Price: %.4f USDT (%.2f%% discount)\n", STRATEGIC_PRICE, strategic_discount_pct))
cat(sprintf("   Strategic Size: %.0f contracts\n", STRATEGIC_SIZE))
cat(sprintf("   Strategic Value: %.2f USDT\n", strategic_total_value))
cat(sprintf("   Strategy: %s\n", if(STRATEGIC_SIDE == "open_long") "🟢 BUY at DIP" else "🔴 SELL at SPIKE"))

# ==========================================================================================================
# 🆕 STRATEGIC ORDER PLACEMENT SECTION
# ==========================================================================================================

if (EXECUTE_STRATEGIC_ORDERS && available_functions$place_strategic_limit_order) {
  cat("\n🎯 PROCESSING STRATEGIC ORDER...\n")
  cat(strrep("-", 50), "\n")
  
  tryCatch({
    # Prüfe aktuelle offene Orders um Duplikate zu vermeiden
    existing_open_orders <- get_current_open_orders(ADA_SYMBOL)
    
    # Prüfe ob bereits ein ähnlicher strategischer Order existiert
    duplicate_order <- FALSE
    if (!is.null(existing_open_orders)) {
      for (i in 1:nrow(existing_open_orders)) {
        order <- existing_open_orders[i, ]
        order_price <- as.numeric(order$price)
        
        # Prüfe ob Order innerhalb 0.1% des strategischen Preises liegt
        if (abs(order_price - STRATEGIC_PRICE) / STRATEGIC_PRICE < 0.001) {
          duplicate_order <- TRUE
          cat(sprintf("⚠️ Similar order already exists at %.4f USDT\n", order_price))
          break
        }
      }
    }
    
    if (!duplicate_order) {
      cat(sprintf("🎯 Strategic Order Details:\n"))
      cat(sprintf("   Symbol: %s\n", ADA_SYMBOL))
      cat(sprintf("   Type: Limit Order (%s)\n", STRATEGIC_SIDE))
      cat(sprintf("   Current Price: %.4f USDT\n", current_price))
      cat(sprintf("   Strategic Price: %.4f USDT\n", STRATEGIC_PRICE))
      cat(sprintf("   Discount: %.2f%%\n", strategic_discount_pct))
      cat(sprintf("   Size: %.0f contracts\n", STRATEGIC_SIZE))
      cat(sprintf("   Total Value: %.2f USDT\n", strategic_total_value))
      cat(sprintf("   🔥 LIVE TRADING: %s\n", if(EXECUTE_LIVE_ORDERS) "ENABLED" else "DISABLED (SIMULATION)"))
      
      # Strategische Order Validierung
      if ((STRATEGIC_SIDE == "open_long" && STRATEGIC_PRICE < current_price) ||
          (STRATEGIC_SIDE == "open_short" && STRATEGIC_PRICE > current_price)) {
        
        if (EXECUTE_LIVE_ORDERS) {
          # LIVE STRATEGIC ORDER
          cat("\n🚀 PLACING LIVE STRATEGIC ORDER IN:\n")
          for (countdown in 3:1) {
            cat(sprintf("   %d seconds...\n", countdown))
            Sys.sleep(1)
          }
          
          strategic_result <- place_strategic_limit_order(ADA_SYMBOL, STRATEGIC_SIDE, 
                                                          STRATEGIC_SIZE, STRATEGIC_PRICE)
          
          if (!is.null(strategic_result) && strategic_result$success) {
            cat("\n✅ STRATEGIC ORDER SUCCESSFULLY PLACED!\n")
            cat(sprintf("   Order ID: %s\n", strategic_result$order_id))
            cat(sprintf("   Side: %s\n", STRATEGIC_SIDE))
            cat(sprintf("   Size: %.0f contracts\n", STRATEGIC_SIZE))
            cat(sprintf("   Limit Price: %.4f USDT\n", STRATEGIC_PRICE))
            cat(sprintf("   Expected Value: %.2f USDT\n", strategic_total_value))
            
            strategic_order_success <- TRUE
            strategic_order_id <- strategic_result$order_id
          } else {
            cat("\n❌ STRATEGIC ORDER FAILED!\n")
            if (!is.null(strategic_result) && !is.null(strategic_result$error)) {
              cat(sprintf("   Error: %s\n", strategic_result$error))
            }
            strategic_order_success <- FALSE
            strategic_order_id <- NULL
          }
        } else {
          # STRATEGIC ORDER SIMULATION
          cat("\n🎮 STRATEGIC ORDER SIMULATION MODE - NO REAL ORDERS PLACED\n")
          cat("   ✅ Strategic Order validation: PASSED\n")
          cat(sprintf("      Price: %.4f USDT | Size: %.0f | Value: %.2f USDT\n", 
                      STRATEGIC_PRICE, STRATEGIC_SIZE, strategic_total_value))
          strategic_order_success <- FALSE
          strategic_order_id <- "STRATEGIC_SIMULATION"
        }
      } else {
        cat("\n❌ STRATEGIC ORDER VALIDATION FAILED!\n")
        cat("   Validation checks:\n")
        if (STRATEGIC_SIDE == "open_long") {
          cat(sprintf("      Strategic < Current: %s (%.4f < %.4f)\n", 
                      if(STRATEGIC_PRICE < current_price) "✅" else "❌", STRATEGIC_PRICE, current_price))
        } else {
          cat(sprintf("      Strategic > Current: %s (%.4f > %.4f)\n", 
                      if(STRATEGIC_PRICE > current_price) "✅" else "❌", STRATEGIC_PRICE, current_price))
        }
        strategic_order_success <- FALSE
        strategic_order_id <- NULL
      }
    } else {
      cat("\n⏭️ STRATEGIC ORDER SKIPPED - DUPLICATE DETECTED\n")
      strategic_order_success <- FALSE
      strategic_order_id <- "DUPLICATE_SKIPPED"
    }
    
  }, error = function(e) {
    cat(sprintf("\n❌ STRATEGIC ORDER ERROR: %s\n", e$message))
    strategic_order_success <- FALSE
    strategic_order_id <- NULL
  })
} else {
  if (!EXECUTE_STRATEGIC_ORDERS) {
    cat("\n⏭️ STRATEGIC ORDERS DISABLED (EXECUTE_STRATEGIC_ORDERS = FALSE)\n")
  } else {
    cat("\n❌ STRATEGIC ORDER FUNCTIONS NOT AVAILABLE\n")
  }
  strategic_order_success <- FALSE
  strategic_order_id <- NULL
}

# ==========================================================================================================
# 🎯 EXISTING TP/SL ORDER PLACEMENT (UNCHANGED)
# ==========================================================================================================

# Erweiterte Funktion-Checks
tp_sl_functions_available <- all(c(
  available_functions$place_tp_simple,
  available_functions$place_sl_simple,
  available_functions$get_current_positions
))

if (tp_sl_functions_available) {
  cat("\n✅ TP/SL functions are available! Proceeding with order placement...\n")
  
  # ==========================================================================================================
  # 📈 TAKE PROFIT ORDER SECTION
  # ==========================================================================================================
  
  if (EXECUTE_TP_ORDERS) {
    cat("\n📈 PROCESSING TAKE PROFIT ORDERS...\n")
    cat(strrep("-", 50), "\n")
    
    tryCatch({
      # TP Order Details berechnen
      if (position_side == "long") {
        tp_profit_per_contract <- TP_PRICE - entry_price
        tp_total_profit <- tp_profit_per_contract * TP_SIZE
        tp_profit_percentage <- ((TP_PRICE / entry_price) - 1) * 100
      }
      
      cat(sprintf("🎯 TP Order Configuration:\n"))
      cat(sprintf("   Current Price: %.4f USDT\n", current_price))
      cat(sprintf("   TP Price: %.4f USDT\n", TP_PRICE))
      cat(sprintf("   TP Size: %.0f contracts (%.0f%% of position)\n", TP_SIZE, (TP_SIZE/total_size)*100))
      cat(sprintf("   Expected Profit: %.2f USDT (+%.2f%%)\n", tp_total_profit, tp_profit_percentage))
      cat(sprintf("   🔥 LIVE TRADING: %s\n", if(EXECUTE_LIVE_ORDERS) "ENABLED" else "DISABLED (SIMULATION)"))
      
      # TP Sicherheitsvalidierung
      if (position_side == "long" && TP_PRICE > entry_price && TP_PRICE > current_price && TP_SIZE <= total_size) {
        
        if (EXECUTE_LIVE_ORDERS) {
          # LIVE TP ORDER
          cat("\n🚀 PLACING LIVE TP ORDER IN:\n")
          for (countdown in 3:1) {
            cat(sprintf("   %d seconds...\n", countdown))
            Sys.sleep(1)
          }
          
          tp_result <- place_tp_simple(ADA_SYMBOL, position_side, as.character(TP_SIZE), TP_PRICE)
          
          if (!is.null(tp_result) && tp_result$success) {
            cat("\n✅ TP ORDER SUCCESSFULLY PLACED!\n")
            cat(sprintf("   Order ID: %s\n", tp_result$order_id))
            
            tp_order_success <- TRUE
            tp_order_id <- tp_result$order_id
          } else {
            cat("\n❌ TP ORDER FAILED!\n")
            tp_order_success <- FALSE
            tp_order_id <- NULL
          }
        } else {
          # TP SIMULATION
          cat("\n🎮 TP SIMULATION MODE - NO REAL ORDERS PLACED\n")
          tp_order_success <- FALSE
          tp_order_id <- "TP_SIMULATION"
        }
      } else {
        cat("\n❌ TP ORDER VALIDATION FAILED!\n")
        tp_order_success <- FALSE
        tp_order_id <- NULL
      }
      
    }, error = function(e) {
      cat(sprintf("\n❌ TP ORDER ERROR: %s\n", e$message))
      tp_order_success <- FALSE
      tp_order_id <- NULL
    })
  } else {
    cat("\n⏭️ TP ORDERS DISABLED (EXECUTE_TP_ORDERS = FALSE)\n")
    tp_order_success <- FALSE
    tp_order_id <- NULL
  }
  
  # ==========================================================================================================
  # 📉 STOP LOSS ORDER SECTION  
  # ==========================================================================================================
  
  if (EXECUTE_SL_ORDERS) {
    cat("\n📉 PROCESSING STOP LOSS ORDERS...\n")
    cat(strrep("-", 50), "\n")
    
    tryCatch({
      # SL Order Details berechnen
      if (position_side == "long") {
        sl_loss_per_contract <- SL_PRICE - entry_price
        sl_total_loss <- sl_loss_per_contract * SL_SIZE
        sl_loss_percentage <- ((SL_PRICE / entry_price) - 1) * 100
      }
      
      cat(sprintf("🛡️ SL Order Configuration:\n"))
      cat(sprintf("   Current Price: %.4f USDT\n", current_price))
      cat(sprintf("   SL Price: %.4f USDT\n", SL_PRICE))
      cat(sprintf("   SL Size: %.0f contracts (%.0f%% of position)\n", SL_SIZE, (SL_SIZE/total_size)*100))
      cat(sprintf("   Max Loss: %.2f USDT (%.2f%%)\n", sl_total_loss, sl_loss_percentage))
      cat(sprintf("   🔥 LIVE TRADING: %s\n", if(EXECUTE_LIVE_ORDERS) "ENABLED" else "DISABLED (SIMULATION)"))
      
      # SL Sicherheitsvalidierung
      if (position_side == "long" && SL_PRICE < entry_price && SL_PRICE < current_price && SL_SIZE <= total_size) {
        
        if (EXECUTE_LIVE_ORDERS) {
          # LIVE SL ORDER
          cat("\n🛡️ PLACING LIVE SL ORDER IN:\n")
          for (countdown in 3:1) {
            cat(sprintf("   %d seconds...\n", countdown))
            Sys.sleep(1)
          }
          
          sl_result <- place_sl_simple(ADA_SYMBOL, position_side, as.character(SL_SIZE), SL_PRICE)
          
          if (!is.null(sl_result) && sl_result$success) {
            cat("\n✅ SL ORDER SUCCESSFULLY PLACED!\n")
            cat(sprintf("   Order ID: %s\n", sl_result$order_id))
            
            sl_order_success <- TRUE
            sl_order_id <- sl_result$order_id
          } else {
            cat("\n❌ SL ORDER FAILED!\n")
            sl_order_success <- FALSE
            sl_order_id <- NULL
          }
        } else {
          # SL SIMULATION
          cat("\n🎮 SL SIMULATION MODE - NO REAL ORDERS PLACED\n")
          sl_order_success <- FALSE
          sl_order_id <- "SL_SIMULATION"
        }
      } else {
        cat("\n❌ SL ORDER VALIDATION FAILED!\n")
        sl_order_success <- FALSE
        sl_order_id <- NULL
      }
      
    }, error = function(e) {
      cat(sprintf("\n❌ SL ORDER ERROR: %s\n", e$message))
      sl_order_success <- FALSE
      sl_order_id <- NULL
    })
  } else {
    cat("\n⏭️ SL ORDERS DISABLED (EXECUTE_SL_ORDERS = FALSE)\n")
    sl_order_success <- FALSE
    sl_order_id <- NULL
  }
  
} else {
  cat("❌ TP/SL functions not available!\n")
  tp_order_success <- FALSE
  tp_order_id <- NULL
  sl_order_success <- FALSE
  sl_order_id <- NULL
}

# ==========================================================================================================
# 🔍 VERIFICATION - CHECK ALL PLACED ORDERS
# ==========================================================================================================

if (EXECUTE_LIVE_ORDERS && (
  (exists("tp_order_success") && tp_order_success) || 
  (exists("sl_order_success") && sl_order_success) ||
  (exists("strategic_order_success") && strategic_order_success)
)) {
  cat("\n🔍 VERIFYING ALL PLACED ORDERS...\n")
  cat(strrep("=", 50), "\n")
  
  tryCatch({
    Sys.sleep(2)  # Kurz warten damit Orders im System sind
    
    # Plan Orders (TP/SL) prüfen
    plan_orders <- get_current_plan_orders(ADA_SYMBOL)
    if (!is.null(plan_orders) && nrow(plan_orders) > 0) {
      cat("✅ Active plan orders (TP/SL):\n")
      for (i in 1:nrow(plan_orders)) {
        order <- plan_orders[i, ]
        order_type_display <- switch(order$planType,
                                     "pos_profit" = "📈 Take Profit",
                                     "pos_loss" = "📉 Stop Loss", 
                                     order$planType)
        
        cat(sprintf("   %s at %.4f USDT (Size: %s)\n", 
                    order_type_display, as.numeric(order$triggerPrice), order$size))
      }
    }
    
    # Open Orders (Strategic) prüfen
    open_orders <- get_current_open_orders(ADA_SYMBOL)
    if (!is.null(open_orders) && nrow(open_orders) > 0) {
      cat("\n✅ Active open orders (Strategic):\n")
      for (i in 1:nrow(open_orders)) {
        order <- open_orders[i, ]
        cat(sprintf("   🎯 %s %s at %.4f USDT (Size: %s)\n", 
                    order$orderType, order$side, as.numeric(order$price), order$size))
      }
    }
    
  }, error = function(e) {
    cat("⚠️ Could not verify orders:", e$message, "\n")
  })
}

# ==========================================================================================================
# 📊 FINAL ENHANCED SUMMARY
# ==========================================================================================================

cat("\n📊 ENHANCED EXECUTION SUMMARY\n")
cat(strrep("=", 60), "\n")

# Position Summary
cat(sprintf("📋 Current Position:\n"))
cat(sprintf("   Symbol: %s\n", ADA_SYMBOL))
cat(sprintf("   Size: %.0f contracts\n", total_size))
cat(sprintf("   Entry: %.4f USDT\n", entry_price))
cat(sprintf("   Current: %.4f USDT\n", current_price))
cat(sprintf("   PnL: %.2f USDT (%s)\n", current_pnl, if(current_pnl > 0) "🟢 PROFIT" else "🔴 LOSS"))

# Trading Mode
trading_mode <- if(EXECUTE_LIVE_ORDERS) "🚀 LIVE TRADING" else "🎮 SIMULATION"
cat(sprintf("\n🎯 Trading Mode: %s\n", trading_mode))

# TP Order Status
if (EXECUTE_TP_ORDERS) {
  tp_status <- if(exists("tp_order_success") && tp_order_success && EXECUTE_LIVE_ORDERS) {
    "✅ PLACED"
  } else if(exists("tp_order_success") && !EXECUTE_LIVE_ORDERS) {
    "🎮 SIMULATED"
  } else {
    "❌ FAILED"
  }
  cat(sprintf("📈 TP Order: %s (%.4f USDT, %.0f contracts)\n", tp_status, TP_PRICE, TP_SIZE))
}

# SL Order Status
if (EXECUTE_SL_ORDERS) {
  sl_status <- if(exists("sl_order_success") && sl_order_success && EXECUTE_LIVE_ORDERS) {
    "✅ PLACED"
  } else if(exists("sl_order_success") && !EXECUTE_LIVE_ORDERS) {
    "🎮 SIMULATED"
  } else {
    "❌ FAILED"
  }
  cat(sprintf("📉 SL Order: %s (%.4f USDT, %.0f contracts)\n", sl_status, SL_PRICE, SL_SIZE))
}

# 🆕 Strategic Order Status
if (EXECUTE_STRATEGIC_ORDERS) {
  strategic_status <- if(exists("strategic_order_success") && strategic_order_success && EXECUTE_LIVE_ORDERS) {
    "✅ PLACED"
  } else if(exists("strategic_order_success") && !EXECUTE_LIVE_ORDERS) {
    "🎮 SIMULATED"
  } else if(exists("strategic_order_id") && strategic_order_id == "DUPLICATE_SKIPPED") {
    "⏭️ SKIPPED (DUPLICATE)"
  } else {
    "❌ FAILED"
  }
  cat(sprintf("🎯 Strategic Order: %s (%.4f USDT, %.0f contracts)\n", 
              strategic_status, STRATEGIC_PRICE, STRATEGIC_SIZE))
  cat(sprintf("   Strategy: %s (%.2f%% discount)\n", 
              if(STRATEGIC_SIDE == "open_long") "Buy at Dip" else "Sell at Spike", 
              strategic_discount_pct))
}

# ==========================================================================================================
# 🔄 RESTORE CONSOLE & END
# ==========================================================================================================

# Console wiederherstellen
end_silent_mode()

cat("\n✅ ENHANCED EXECUTION WITH STRATEGIC ORDER COMPLETE!\n")
cat("===================================================\n")
cat("🆕 New Features:\n")
cat("   ✅ Strategic Order Support (5000 @ 0.5636 USDT)\n") 
cat("   ✅ Duplicate Order Detection\n")
cat("   ✅ Enhanced Order Verification\n")
cat("   ✅ Comprehensive Order Status Tracking\n")

cat("\n💡 To enable live trading:\n")
cat("   Set EXECUTE_LIVE_ORDERS <- TRUE in the configuration section\n")

cat("\n🎯 Strategic Order Details:\n")
cat(sprintf("   Price: %.4f USDT (%.2f%% below current)\n", STRATEGIC_PRICE, strategic_discount_pct))
cat(sprintf("   Size: %.0f contracts\n", STRATEGIC_SIZE))
cat(sprintf("   Value: %.2f USDT\n", strategic_total_value))
cat(sprintf("   Strategy: Wait for dip to %.4f USDT\n", STRATEGIC_PRICE))

# ==========================================================================================================
# 🎯 END OF ENHANCED EXECUTION WITH STRATEGIC ORDERS
# ==========================================================================================================

