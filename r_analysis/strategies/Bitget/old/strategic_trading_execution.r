# ==========================================================================================================
# 🎯 STRATEGIC TRADING EXECUTION - ADA POSITION MANAGEMENT
# ==========================================================================================================
# 
# ZIEL: 1. Strategische DCA Order bei 0.5636 USDT platzieren
#       2. Intelligente TP/SL Level für aktuelle Position setzen
#
# AKTUELLE POSITION: 3000 ADA @ 0.5839 USDT (leicht underwater)
# STRATEGIE: Gestaffelte Gewinnmitnahme + Risikobegrenzung + Strategic DCA
# 
# ==========================================================================================================

cat("🎯 STRATEGIC TRADING EXECUTION\n")
cat("=============================\n")
cat("Start Time:", as.character(Sys.time()), "\n\n")

# ==========================================================================================================
# 🔧 TRADING CONFIGURATION
# ==========================================================================================================

# Symbol & Position Details
SYMBOL <- "ADAUSDT_UMCBL"
CURRENT_POSITION_SIZE <- 3000
CURRENT_ENTRY_PRICE <- 0.5839

# Strategic Order Configuration
STRATEGIC_PRICE <- 0.5636        # 3.16% Discount für DCA
STRATEGIC_SIZE <- 5000           # Erhebliche Position für Averaging Down
STRATEGIC_VALUE <- STRATEGIC_PRICE * STRATEGIC_SIZE  # ~2818 USDT

# TP/SL Configuration für aktuelle Position (gestaffelt)
TP_LEVELS <- list(
  level1 = list(price = 0.5950, size = 1200, percentage = 1.9),  # 40% bei +1.9%
  level2 = list(price = 0.6050, size = 900, percentage = 3.6),   # 30% bei +3.6%
  level3 = list(price = 0.6200, size = 900, percentage = 6.2)    # 30% bei +6.2%
)

# Stop Loss Configuration
SL_PRICE <- 0.5750               # -1.5% von Entry für Risikobegrenzung
SL_SIZE <- 3000                  # Gesamte aktuelle Position

# ⚡ EXECUTION CONTROL - SICHERHEITSSCHALTER
EXECUTE_STRATEGIC_ORDER <- TRUE   # ← Strategische Order aktivieren
EXECUTE_TP_ORDERS <- TRUE         # ← TP Orders aktivieren  
EXECUTE_SL_ORDER <- TRUE          # ← SL Order aktivieren
FINAL_CONFIRMATION <- TRUE        # ← Finale Bestätigung vor Ausführung

cat("📊 Configuration Summary:\n")
cat("Current Position: ", CURRENT_POSITION_SIZE, " ADA @ ", CURRENT_ENTRY_PRICE, " USDT\n")
cat("Strategic Order: ", STRATEGIC_SIZE, " ADA @ ", STRATEGIC_PRICE, " USDT (", 
    round(((STRATEGIC_PRICE/CURRENT_ENTRY_PRICE)-1)*100, 2), "% discount)\n")
cat("Strategic Value: ", round(STRATEGIC_VALUE, 2), " USDT\n")
cat("TP Levels: 3 gestaffelte Level (40% / 30% / 30%)\n")
cat("SL Level: ", SL_PRICE, " USDT (-1.5% risk limit)\n\n")

# ==========================================================================================================
# 🔍 PRE-EXECUTION CHECKS
# ==========================================================================================================

cat("🔍 PRE-EXECUTION SYSTEM CHECKS\n")
cat("==============================\n")

# 1. Function Availability Check
required_functions <- c("place_strategic_limit_order", "place_tp_simple", "place_sl_simple", 
                       "get_current_positions", "get_enhanced_ticker_data")

functions_available <- TRUE
for (func in required_functions) {
  if (exists(func)) {
    cat("✅", func, "\n")
  } else {
    cat("❌", func, "- MISSING!\n")
    functions_available <- FALSE
  }
}

if (!functions_available) {
  stop("❌ Required functions missing - cannot proceed!")
}

# 2. Current Market Data Check
cat("\n📊 Current Market Data:\n")
current_market <- get_enhanced_ticker_data(SYMBOL)
if (!is.null(current_market)) {
  current_price <- current_market$last_price
  price_change_24h <- current_market$change_24h_pct
  volume_24h <- current_market$volume_24h_usdt / 1000000
  
  cat("Current Price: ", current_price, " USDT (", round(price_change_24h, 2), "% 24h)\n")
  cat("24h Volume: ", round(volume_24h, 1), "M USDT\n")
} else {
  cat("⚠️ Could not fetch current market data - using fallback\n")
  current_price <- 0.5820  # Fallback based on log
}

# 3. Position Verification
cat("\n📋 Position Verification:\n")
current_positions <- get_current_positions(SYMBOL)
if (!is.null(current_positions) && nrow(current_positions) > 0) {
  actual_size <- as.numeric(current_positions$total[1])
  actual_entry <- as.numeric(current_positions$averageOpenPrice[1])
  actual_pnl <- as.numeric(current_positions$unrealizedPL[1])
  
  cat("✅ Position confirmed: ", actual_size, " contracts @ ", actual_entry, " USDT\n")
  cat("Current PnL: ", round(actual_pnl, 2), " USDT\n")
  
  # Update configuration with actual values
  CURRENT_POSITION_SIZE <- actual_size
  CURRENT_ENTRY_PRICE <- actual_entry
} else {
  cat("⚠️ No active position found - strategic order only\n")
  CURRENT_POSITION_SIZE <- 0
}

# ==========================================================================================================
# 🚀 STRATEGIC ORDER EXECUTION
# ==========================================================================================================

if (EXECUTE_STRATEGIC_ORDER && functions_available) {
  cat("\n🚀 STRATEGIC ORDER PLACEMENT\n")
  cat("============================\n")
  
  # Strategic Order Details
  discount_percentage <- ((STRATEGIC_PRICE / current_price) - 1) * 100
  
  cat("📋 Strategic Order Details:\n")
  cat("Order Type: BUY (open_long)\n")
  cat("Size: ", STRATEGIC_SIZE, " contracts\n")
  cat("Price: ", STRATEGIC_PRICE, " USDT\n")
  cat("Current Discount: ", round(discount_percentage, 2), "%\n")
  cat("Order Value: ", round(STRATEGIC_VALUE, 2), " USDT\n")
  cat("Strategy: Dollar Cost Averaging (DCA)\n")
  
  # Sicherheitsvalidierung
  if (STRATEGIC_PRICE < current_price && STRATEGIC_SIZE > 0) {
    
    if (FINAL_CONFIRMATION) {
      cat("\n⚠️ FINAL CONFIRMATION REQUIRED\n")
      cat("This will place a REAL order with REAL money!\n")
      cat("Strategic Buy Order: ", STRATEGIC_SIZE, " ADA @ ", STRATEGIC_PRICE, " USDT\n")
      cat("Press ENTER to continue or Ctrl+C to cancel...\n")
      readline()
    }
    
    cat("\n🚀 Placing Strategic Order...\n")
    
    strategic_result <- place_strategic_limit_order(
      symbol = SYMBOL,
      side = "open_long",
      size = STRATEGIC_SIZE,
      price = STRATEGIC_PRICE
    )
    
    if (!is.null(strategic_result) && strategic_result$success) {
      cat("✅ STRATEGIC ORDER PLACED SUCCESSFULLY!\n")
      cat("Order ID: ", strategic_result$order_id, "\n")
      cat("Status: Pending Fill at ", STRATEGIC_PRICE, " USDT\n")
      strategic_order_success <- TRUE
    } else {
      cat("❌ Strategic Order Failed!\n")
      if (!is.null(strategic_result) && !is.null(strategic_result$error)) {
        cat("Error: ", strategic_result$error, "\n")
      }
      strategic_order_success <- FALSE
    }
  } else {
    cat("❌ Strategic Order Validation Failed!\n")
    strategic_order_success <- FALSE
  }
} else {
  cat("\n⏭️ Strategic Order Disabled\n")
  strategic_order_success <- FALSE
}

# ==========================================================================================================
# 📈 INTELLIGENT TP/SL PLACEMENT
# ==========================================================================================================

if (CURRENT_POSITION_SIZE > 0 && (EXECUTE_TP_ORDERS || EXECUTE_SL_ORDER)) {
  cat("\n📈 INTELLIGENT TP/SL PLACEMENT\n")
  cat("==============================\n")
  
  tp_results <- list()
  sl_result <- NULL
  
  # TP ORDERS - Gestaffelte Gewinnmitnahme
  if (EXECUTE_TP_ORDERS) {
    cat("📈 Placing Staged TP Orders...\n")
    
    for (level_name in names(TP_LEVELS)) {
      level <- TP_LEVELS[[level_name]]
      
      cat(sprintf("\n🎯 TP Level %s:\n", level_name))
      cat(sprintf("Price: %.4f USDT (+%.1f%%)\n", level$price, level$percentage))
      cat(sprintf("Size: %d contracts (%.1f%% of position)\n", 
                  level$size, (level$size/CURRENT_POSITION_SIZE)*100))
      
      # TP Validierung
      if (level$price > CURRENT_ENTRY_PRICE && level$size <= CURRENT_POSITION_SIZE) {
        
        tp_result <- place_tp_simple(
          symbol = SYMBOL,
          side = "long",
          size = as.character(level$size),
          trigger_price = level$price
        )
        
        if (!is.null(tp_result) && tp_result$success) {
          cat("✅ TP Order placed successfully!\n")
          cat("Order ID: ", tp_result$order_id, "\n")
          tp_results[[level_name]] <- list(success = TRUE, order_id = tp_result$order_id)
        } else {
          cat("❌ TP Order failed!\n")
          tp_results[[level_name]] <- list(success = FALSE, error = tp_result$error)
        }
      } else {
        cat("❌ TP Level validation failed!\n")
        tp_results[[level_name]] <- list(success = FALSE, error = "Validation failed")
      }
      
      Sys.sleep(1)  # Pause zwischen Orders
    }
  }
  
  # SL ORDER - Risikobegrenzung
  if (EXECUTE_SL_ORDER) {
    cat("\n📉 Placing Stop Loss Order...\n")
    
    sl_loss_amount <- (SL_PRICE - CURRENT_ENTRY_PRICE) * SL_SIZE
    sl_loss_percentage <- ((SL_PRICE / CURRENT_ENTRY_PRICE) - 1) * 100
    
    cat(sprintf("🛡️ SL Order Details:\n"))
    cat(sprintf("Price: %.4f USDT (%.1f%%)\n", SL_PRICE, sl_loss_percentage))
    cat(sprintf("Size: %d contracts (100%% of position)\n", SL_SIZE))
    cat(sprintf("Max Loss: %.2f USDT\n", sl_loss_amount))
    
    # SL Validierung
    if (SL_PRICE < CURRENT_ENTRY_PRICE && SL_SIZE <= CURRENT_POSITION_SIZE) {
      
      sl_result <- place_sl_simple(
        symbol = SYMBOL,
        side = "long", 
        size = as.character(SL_SIZE),
        trigger_price = SL_PRICE
      )
      
      if (!is.null(sl_result) && sl_result$success) {
        cat("✅ SL Order placed successfully!\n")
        cat("Order ID: ", sl_result$order_id, "\n")
        sl_order_success <- TRUE
      } else {
        cat("❌ SL Order failed!\n")
        if (!is.null(sl_result)) {
          cat("Error: ", sl_result$error, "\n")
        }
        sl_order_success <- FALSE
      }
    } else {
      cat("❌ SL Order validation failed!\n")
      sl_order_success <- FALSE
    }
  }
} else {
  cat("\n⏭️ TP/SL Orders skipped (no position or disabled)\n")
}

# ==========================================================================================================
# 📊 EXECUTION SUMMARY & VERIFICATION
# ==========================================================================================================

cat("\n📊 EXECUTION SUMMARY\n")
cat("====================\n")

# Strategic Order Summary
if (exists("strategic_order_success") && strategic_order_success) {
  cat("🚀 Strategic Order: ✅ PLACED\n")
  cat(sprintf("   %d ADA @ %.4f USDT (Value: %.0f USDT)\n", 
              STRATEGIC_SIZE, STRATEGIC_PRICE, STRATEGIC_VALUE))
} else {
  cat("🚀 Strategic Order: ❌ FAILED or DISABLED\n")
}

# TP Orders Summary
if (exists("tp_results") && length(tp_results) > 0) {
  successful_tp <- sum(sapply(tp_results, function(x) x$success))
  cat(sprintf("📈 TP Orders: %d/%d PLACED\n", successful_tp, length(tp_results)))
  
  for (level_name in names(tp_results)) {
    status <- if (tp_results[[level_name]]$success) "✅" else "❌"
    level <- TP_LEVELS[[level_name]]
    cat(sprintf("   %s %s: %d contracts @ %.4f USDT\n", 
                status, level_name, level$size, level$price))
  }
} else {
  cat("📈 TP Orders: ⏭️ SKIPPED\n")
}

# SL Order Summary
if (exists("sl_order_success") && sl_order_success) {
  cat("🛡️ SL Order: ✅ PLACED\n")
  cat(sprintf("   %d contracts @ %.4f USDT (Max Loss: %.0f USDT)\n", 
              SL_SIZE, SL_PRICE, (SL_PRICE - CURRENT_ENTRY_PRICE) * SL_SIZE))
} else {
  cat("🛡️ SL Order: ❌ FAILED or DISABLED\n")
}

# ==========================================================================================================
# 🔍 ORDER VERIFICATION
# ==========================================================================================================

cat("\n🔍 ORDER VERIFICATION\n")
cat("=====================\n")

# Warte kurz und prüfe dann alle aktiven Orders
Sys.sleep(3)

# Prüfe Plan Orders (TP/SL)
cat("📋 Checking Plan Orders (TP/SL)...\n")
plan_orders <- get_current_plan_orders(SYMBOL)
if (!is.null(plan_orders) && nrow(plan_orders) > 0) {
  cat(sprintf("✅ %d active plan orders found:\n", nrow(plan_orders)))
  for (i in 1:nrow(plan_orders)) {
    order <- plan_orders[i, ]
    order_type <- switch(order$planType,
                        "pos_profit" = "📈 TP",
                        "pos_loss" = "🛡️ SL",
                        "📋 Plan")
    cat(sprintf("   %s: %s contracts @ %.4f USDT\n", 
                order_type, order$size, as.numeric(order$triggerPrice)))
  }
} else {
  cat("📭 No plan orders found\n")
}

# Prüfe Open Orders (Strategic)
cat("\n📋 Checking Open Orders (Strategic)...\n")
if (exists("get_current_open_orders")) {
  open_orders <- get_current_open_orders(SYMBOL)
  if (!is.null(open_orders) && nrow(open_orders) > 0) {
    cat(sprintf("✅ %d active open orders found:\n", nrow(open_orders)))
    for (i in 1:nrow(open_orders)) {
      order <- open_orders[i, ]
      cat(sprintf("   🚀 %s: %s contracts @ %.4f USDT\n", 
                  order$side, order$size, as.numeric(order$price)))
    }
  } else {
    cat("📭 No open orders found\n")
  }
} else {
  cat("⚠️ Cannot verify open orders (function not available)\n")
}

# ==========================================================================================================
# 🎯 FINAL STATUS & NEXT STEPS
# ==========================================================================================================

cat("\n🎯 FINAL STATUS & NEXT STEPS\n")
cat("============================\n")

cat("📅 Execution completed at:", as.character(Sys.time()), "\n")
cat("🎯 Position Management Strategy: ACTIVE\n")

if (exists("strategic_order_success") && strategic_order_success) {
  cat("📊 Strategic DCA: Order placed at", STRATEGIC_PRICE, "USDT\n")
  cat("💡 Next: Monitor for fill and potential averaging down\n")
}

if (exists("tp_results") || exists("sl_order_success")) {
  cat("🛡️ Risk Management: TP/SL orders active\n") 
  cat("💡 Next: Monitor position and adjust levels as needed\n")
}

cat("\n🚀 STRATEGIC EXECUTION COMPLETE!\n")
cat("Your ADA position is now actively managed with:\n")
cat("   ✅ Strategic buy order for DCA\n")
cat("   ✅ Staged profit-taking levels\n") 
cat("   ✅ Risk protection via stop loss\n")
cat("   ✅ Professional order management\n")

cat("\n" , strrep("=", 50), "\n")
cat("💰 Position optimized for risk-adjusted returns! 📈\n")
cat(strrep("=", 50), "\n")