# ==========================================================================================================
# 🚀 FIXED TRADING SYSTEM EXECUTION - TP ORDER FUNKTIONSFÄHIG
# ==========================================================================================================
# 
# FIX: Lädt correct_trading_analysis_v3.r (MIT TP/SL Funktionen)
# STATT: complete_trading_analysis_v5.r (OHNE TP/SL Funktionen)
# 
# ==========================================================================================================

cat("🚀 Starting FIXED Trading System with TP Order Support...\n")

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
# 🎯 FUNCTION AVAILABILITY CHECK (SHOULD NOW INCLUDE TP/SL)
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

  # Aktuelle Position checken
  get_current_plan_orders('ADAUSDT_UMCBL')  
  
  # Marktpreis abrufen
  market_data <- get_enhanced_ticker_data('ADAUSDT_UMCBL')
  current_price <- if(!is.null(market_data)) market_data$last_price else 0.5664
  
  cat(sprintf("   Current Price: %.4f USDT\n", current_price))
  cat(sprintf("   Position Status: %s\n", if(current_pnl > 0) "🟢 PROFITABLE" else "🔴 UNDERWATER"))
  
} else {
  cat("❌ No ADA position found!\n")
  current_price <- 0.5664  # Fallback
  entry_price <- 0.5609    # Fallback 
  total_size <- 6000       # Fallback
  position_side <- "long"  # Fallback
}

# ==========================================================================================================
# 🎯 AUTOMATIC TP ORDER PLACEMENT (IMPROVED)
# ==========================================================================================================

cat("\n🎯 IMPROVED TP ORDER PLACEMENT\n")
cat("==============================\n")

# TP Konfiguration
ADA_SYMBOL <- "ADAUSDT_UMCBL"
TP_PRICE <- 0.5600      # Ihr gewünschter TP
#TP_PERCENTAGE <- 0.5    # 50% der Position

# Erweiterte Funktion-Checks
tp_functions_available <- all(c(
  available_functions$place_tp_simple,
  available_functions$get_current_positions
))

if (tp_functions_available) {
  cat("✅ TP functions are available! Proceeding with order placement...\n")
  
  tryCatch({
    # TP Order Details berechnen
    tp_size <-5787 #round(total_size * TP_PERCENTAGE)
    
    if (position_side == "long") {
      profit_per_contract <- TP_PRICE - entry_price
      total_profit <- profit_per_contract * tp_size
      profit_percentage <- ((TP_PRICE / entry_price) - 1) * 100
    }
    
    cat(sprintf("\n🎯 TP Order Configuration:\n"))
    cat(sprintf("   Symbol: %s\n", ADA_SYMBOL))
    cat(sprintf("   Current Price: %.4f USDT\n", current_price))
    cat(sprintf("   TP Price: %.4f USDT\n", TP_PRICE))
    cat(sprintf("   TP Size: %.0f contracts (%.0f%%)\n", tp_size, TP_PERCENTAGE * 100))
    cat(sprintf("   Expected Profit: %.2f USDT (+%.2f%%)\n", total_profit, profit_percentage))
    
    # Sicherheitsvalidierung
    if (position_side == "long" && TP_PRICE > entry_price && TP_PRICE > current_price * 0.99) {
      
      # 3-Sekunden Countdown
      cat("\n🚀 PLACING LIVE TP ORDER IN:\n")
      for (countdown in 3:1) {
        cat(sprintf("   %d seconds...\n", countdown))
        Sys.sleep(1)
      }
      
      # TP ORDER PLATZIEREN!
      #tp_result <- place_tp_simple(ADA_SYMBOL, position_side, as.character(tp_size), TP_PRICE)
      
      if (!is.null(tp_result) && tp_result$success) {
        cat("\n✅ TP ORDER SUCCESSFULLY PLACED!\n")
        cat(sprintf("   Order ID: %s\n", tp_result$order_id))
        cat(sprintf("   Symbol: %s\n", ADA_SYMBOL))
        cat(sprintf("   Side: close_%s\n", position_side))
        cat(sprintf("   Size: %.0f contracts\n", tp_size))
        cat(sprintf("   TP Price: %.4f USDT\n", TP_PRICE))
        cat(sprintf("   Expected Profit: %.2f USDT\n", total_profit))
        
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
      cat("\n❌ TP ORDER VALIDATION FAILED!\n")
      cat("   Reason: TP price validation failed\n")
      tp_order_success <- FALSE
      tp_order_id <- NULL
    }
    
  }, error = function(e) {
    cat(sprintf("\n❌ TP ORDER ERROR: %s\n", e$message))
    tp_order_success <- FALSE
    tp_order_id <- NULL
  })
  
} else {
  cat("❌ TP functions not available!\n")
  cat("   Missing functions:\n")
  if (!available_functions$place_tp_simple) cat("   - place_tp_simple\n")
  if (!available_functions$get_current_positions) cat("   - get_current_positions\n")
  
  tp_order_success <- FALSE
  tp_order_id <- NULL
}

# ==========================================================================================================
# 🔍 VERIFICATION - CHECK PLACED ORDERS
# ==========================================================================================================

if (exists("tp_order_success") && tp_order_success) {
  cat("\n🔍 VERIFYING PLACED ORDERS...\n")
  
  tryCatch({
    Sys.sleep(2)  # Kurz warten damit Order im System ist
    
    if (exists("get_current_plan_orders")) {
      plan_orders <- get_current_plan_orders(ADA_SYMBOL)
      
      if (!is.null(plan_orders) && nrow(plan_orders) > 0) {
        cat("✅ Active plan orders found:\n")
        for (i in 1:nrow(plan_orders)) {
          order <- plan_orders[i, ]
          cat(sprintf("   Order %d: %s at %.4f USDT (Size: %s)\n", 
                      i, order$planType, as.numeric(order$triggerPrice), order$size))
        }
      } else {
        cat("⚠️ No active plan orders found (may take a moment to appear)\n")
      }
    }
  }, error = function(e) {
    cat("⚠️ Could not verify orders:", e$message, "\n")
  })
}

# ==========================================================================================================
# 🔄 Production-Ready Functions
# ==========================================================================================================


#complete_trading_analysis()


# ==========================================================================================================
# 🔄 RESTORE CONSOLE & SUMMARY
# ==========================================================================================================

# Console wiederherstellen
end_silent_mode()

# Final Summary
cat("\n🎉 FIXED EXECUTION COMPLETE!\n")
cat("============================\n")

if (exists("tp_order_success")) {
  if (tp_order_success) {
    cat("🎯 ADA TP Order: ✅ SUCCESSFULLY PLACED\n")
    if (!is.null(tp_order_id)) {
      cat(sprintf("   Order ID: %s\n", tp_order_id))
    }
    cat(sprintf("   TP Price: %.4f USDT\n", TP_PRICE))
    cat(sprintf("   Size: %.0f contracts (50%% of position)\n", tp_size))
    cat(sprintf("   Expected Profit: %.2f USDT\n", total_profit))
    cat("\n💡 Check your Bitget account to confirm the order!\n")
  } else {
    cat("🎯 ADA TP Order: ❌ FAILED\n")
    cat("   Please check the error messages above\n")
  }
} else {
  cat("🎯 ADA TP Order: ❌ NOT ATTEMPTED\n")
  cat("   Required functions were not available\n")
}

cat("\n📋 Next Steps:\n")
cat("   1. Check your Bitget trading interface\n")
cat("   2. Verify the TP order appears in 'Orders' tab\n")
cat("   3. Monitor position for execution\n")

# ==========================================================================================================
# 🎯 END OF FIXED EXECUTION
# ==========================================================================================================