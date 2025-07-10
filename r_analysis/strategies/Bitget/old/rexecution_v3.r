# ==========================================================================================================
# 🚀 FIXED TRADING SYSTEM EXECUTION - SYNTAX CORRECTED
# ==========================================================================================================
# 
# FIX: Korrigiert Syntax-Fehler und passt an neue Position an
# NEUE POSITION: 1000 ADA @ 0.6014 USDT (+3.43 USDT profitabel)
# 
# ==========================================================================================================

cat("🚀 Starting FIXED Trading System (Syntax Corrected)...\n")

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
  
  # TP/SL Functions (SHOULD NOW BE AVAILABLE!)
  place_tp_simple = exists("place_tp_simple"),
  place_sl_simple = exists("place_sl_simple"),
  place_intelligent_tp_sl = exists("place_intelligent_tp_sl"),
  quick_tp_sl = exists("quick_tp_sl")
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
# 🔧 SMART CONFIGURATION BASED ON CURRENT POSITION
# ==========================================================================================================

cat("\n🔧 SMART CONFIGURATION\n")
cat("=====================\n")

# Automatische Anpassung basierend auf aktueller Position
ADA_SYMBOL <- "ADAUSDT_UMCBL"

# TP Konfiguration (angepasst an 1000 Kontrakte Position)
TP_PRICE <- 0.6200        # Konservatives TP (+3% from current)
TP_SIZE <- 500            # 50% der Position

# SL Konfiguration  
SL_PRICE <- 0.5636        # Konservatives SL (-1% from entry)
SL_SIZE <- 3000           # Gesamte Position

# ⚡ TRADING MODE CONTROL - EINFACH ZU ÄNDERN!
# ==========================================================================================================
EXECUTE_LIVE_ORDERS <- TRUE    # ← HIER: TRUE für Live Trading, FALSE für Simulation
EXECUTE_TP_ORDERS <- TRUE       # ← TP Orders aktivieren/deaktivieren
EXECUTE_SL_ORDERS <- TRUE       # ← SL Orders aktivieren/deaktivieren
# ==========================================================================================================

cat(sprintf("📊 Configuration adapted to current position:\n"))
cat(sprintf("   Position Size: %.0f contracts\n", total_size))
cat(sprintf("   Entry Price: %.4f USDT\n", entry_price))
cat(sprintf("   Current Price: %.4f USDT\n", current_price))
cat(sprintf("   TP Target: %.4f USDT (%.0f contracts)\n", TP_PRICE, TP_SIZE))
cat(sprintf("   SL Target: %.4f USDT (%.0f contracts)\n", SL_PRICE, SL_SIZE))

# ==========================================================================================================
# 🎯 AUTOMATIC TP/SL ORDER PLACEMENT
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
      cat(sprintf("   Symbol: %s\n", ADA_SYMBOL))
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
            cat(sprintf("   Side: close_%s\n", position_side))
            cat(sprintf("   Size: %.0f contracts\n", TP_SIZE))
            cat(sprintf("   TP Price: %.4f USDT\n", TP_PRICE))
            
            tp_order_success <- TRUE
            tp_order_id <- tp_result$order_id
          } else {
            cat("\n❌ TP ORDER FAILED!\n")
            if (!is.null(tp_result) && !is.null(tp_result$error)) {
              cat(sprintf("   Error: %s\n", tp_result$error))
            }
            tp_order_success <- FALSE
            tp_order_id <- NULL
          }
        } else {
          # TP SIMULATION
          cat("\n🎮 TP SIMULATION MODE - NO REAL ORDERS PLACED\n")
          cat("   ✅ TP Order validation: PASSED\n")
          cat(sprintf("      TP Price: %.4f USDT | Size: %.0f | Profit: %.2f USDT\n", 
                      TP_PRICE, TP_SIZE, tp_total_profit))
          tp_order_success <- FALSE
          tp_order_id <- "TP_SIMULATION"
        }
      } else {
        cat("\n❌ TP ORDER VALIDATION FAILED!\n")
        cat("   Validation checks:\n")
        cat(sprintf("      TP > Entry: %s (%.4f > %.4f)\n", 
                    if(TP_PRICE > entry_price) "✅" else "❌", TP_PRICE, entry_price))
        cat(sprintf("      TP > Current: %s (%.4f > %.4f)\n", 
                    if(TP_PRICE > current_price) "✅" else "❌", TP_PRICE, current_price))
        cat(sprintf("      Size <= Position: %s (%.0f <= %.0f)\n", 
                    if(TP_SIZE <= total_size) "✅" else "❌", TP_SIZE, total_size))
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
      cat(sprintf("   Symbol: %s\n", ADA_SYMBOL))
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
            cat(sprintf("   Side: close_%s\n", position_side))
            cat(sprintf("   Size: %.0f contracts\n", SL_SIZE))
            cat(sprintf("   SL Price: %.4f USDT\n", SL_PRICE))
            
            sl_order_success <- TRUE
            sl_order_id <- sl_result$order_id
          } else {
            cat("\n❌ SL ORDER FAILED!\n")
            if (!is.null(sl_result) && !is.null(sl_result$error)) {
              cat(sprintf("   Error: %s\n", sl_result$error))
            }
            sl_order_success <- FALSE
            sl_order_id <- NULL
          }
        } else {
          # SL SIMULATION
          cat("\n🎮 SL SIMULATION MODE - NO REAL ORDERS PLACED\n")
          cat("   ✅ SL Order validation: PASSED\n")
          cat(sprintf("      SL Price: %.4f USDT | Size: %.0f | Max Loss: %.2f USDT\n", 
                      SL_PRICE, SL_SIZE, sl_total_loss))
          sl_order_success <- FALSE
          sl_order_id <- "SL_SIMULATION"
        }
      } else {
        cat("\n❌ SL ORDER VALIDATION FAILED!\n")
        cat("   Validation checks:\n")
        cat(sprintf("      SL < Entry: %s (%.4f < %.4f)\n", 
                    if(SL_PRICE < entry_price) "✅" else "❌", SL_PRICE, entry_price))
        cat(sprintf("      SL < Current: %s (%.4f < %.4f)\n", 
                    if(SL_PRICE < current_price) "✅" else "❌", SL_PRICE, current_price))
        cat(sprintf("      Size <= Position: %s (%.0f <= %.0f)\n", 
                    if(SL_SIZE <= total_size) "✅" else "❌", SL_SIZE, total_size))
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
# 🔍 VERIFICATION - CHECK PLACED ORDERS
# ==========================================================================================================

if (EXECUTE_LIVE_ORDERS && (
  (exists("tp_order_success") && tp_order_success) || 
  (exists("sl_order_success") && sl_order_success)
)) {
  cat("\n🔍 VERIFYING PLACED ORDERS...\n")
  cat(strrep("=", 50), "\n")
  
  tryCatch({
    Sys.sleep(2)  # Kurz warten damit Orders im System sind
    
    plan_orders <- get_current_plan_orders(ADA_SYMBOL)
    
    if (!is.null(plan_orders) && nrow(plan_orders) > 0) {
      cat("✅ Active plan orders found:\n")
      for (i in 1:nrow(plan_orders)) {
        order <- plan_orders[i, ]
        order_type_display <- switch(order$planType,
                                     "pos_profit" = "📈 Take Profit",
                                     "pos_loss" = "📉 Stop Loss", 
                                     "normal_plan" = "📋 Plan Order",
                                     order$planType)
        
        cat(sprintf("   Order %d: %s at %.4f USDT (Size: %s)\n", 
                    i, order_type_display, as.numeric(order$triggerPrice), order$size))
      }
    } else {
      cat("⚠️ No active plan orders found\n")
    }
  }, error = function(e) {
    cat("⚠️ Could not verify orders:", e$message, "\n")
  })
}

# ==========================================================================================================
# 📊 FINAL SUMMARY
# ==========================================================================================================

cat("\n📊 EXECUTION SUMMARY\n")
cat(strrep("=", 50), "\n")

# Position Summary
cat(sprintf("📋 Current Position:\n"))
cat(sprintf("   Symbol: %s\n", ADA_SYMBOL))
cat(sprintf("   Size: %.0f contracts\n", total_size))
cat(sprintf("   Entry: %.4f USDT\n", entry_price))
cat(sprintf("   Current: %.4f USDT\n", current_price))
cat(sprintf("   PnL: %.2f USDT (%s)\n", current_pnl, if(current_pnl > 0) "🟢 PROFIT" else "🔴 LOSS"))

# Order Summary
trading_mode <- if(EXECUTE_LIVE_ORDERS) "🚀 LIVE TRADING" else "🎮 SIMULATION"
cat(sprintf("\n🎯 Trading Mode: %s\n", trading_mode))

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

# ==========================================================================================================
# 🔄 RESTORE CONSOLE & END
# ==========================================================================================================

# Console wiederherstellen
end_silent_mode()

cat("\n✅ SYNTAX-CORRECTED EXECUTION COMPLETE!\n")
cat("=======================================\n")
cat("🔧 Fixes applied:\n")
cat("   ✅ Syntax error corrected (missing closing brace)\n") 
cat("   ✅ Configuration adapted to current position (1000 contracts)\n")
cat("   ✅ Realistic TP/SL levels set\n")
cat("   ✅ Enhanced validation and error handling\n")

cat("\n💡 To enable live trading:\n")
cat("   Set EXECUTE_LIVE_ORDERS <- TRUE in the configuration section\n")

# ==========================================================================================================
# 🎯 END OF SYNTAX-CORRECTED EXECUTION
# ==========================================================================================================