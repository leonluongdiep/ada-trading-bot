# ==========================================
# 📋 ADA ORDER ANALYSIS & MANAGEMENT
# ==========================================

# 1. AKTIVE ORDERS ABRUFEN
get_active_orders <- function() {
  cat("\n📋 Fetching active orders (TP/SL)...\n")
  cat("=========================================\n")
  
  query_params <- list(productType = "umcbl")
  
  data <- bitget_request("/api/mix/v1/plan/currentPlan", "GET", query_params)
  
  if (is.null(data) || data$code != "00000") {
    cat("❌ Failed to fetch orders\n")
    return(NULL)
  }
  
  if (length(data$data) == 0) {
    cat("📭 No active plan orders found.\n")
    return(NULL)
  }
  
  orders_df <- data$data
  
  cat(sprintf("📊 Found %d active orders\n\n", nrow(orders_df)))
  
  # Formatierte Ausgabe
  for (i in 1:nrow(orders_df)) {
    order <- orders_df[i, ]
    
    # Order Type Icon
    icon <- if(order$planType == "pos_profit") "🎯" else "🛑"
    type_name <- if(order$planType == "pos_profit") "Take Profit" else "Stop Loss"
    
    cat(sprintf("%s %s Order:\n", icon, type_name))
    cat(sprintf("   Symbol: %s\n", order$symbol))
    cat(sprintf("   Side: %s\n", order$side))
    cat(sprintf("   Size: %s contracts\n", order$size))
    cat(sprintf("   Trigger Price: %s USDT\n", order$triggerPrice))
    cat(sprintf("   Order ID: %s\n", order$orderId))
    cat(sprintf("   Status: %s\n", order$state))
    cat(sprintf("   Created: %s\n", 
                as.POSIXct(as.numeric(order$createTime)/1000, origin="1970-01-01")))
    cat("\n")
  }
  
  return(orders_df)
}

# 2. POSITION + ORDERS ANALYSE
analyze_ada_situation <- function() {
  cat("\n🔍 COMPLETE ADA SITUATION ANALYSIS\n")
  cat("==================================\n")
  
  # Current Position
  cat("\n💰 CURRENT POSITION:\n")
  positions_data <- bitget_request("/api/mix/v1/position/allPosition", "GET", 
                                   list(productType = "umcbl"))
  
  if (positions_data$code == "00000") {
    ada_pos <- positions_data$data[positions_data$data$symbol == "ADAUSDT_UMCBL" & 
                                     as.numeric(positions_data$data$total) > 0, ]
    
    if (nrow(ada_pos) > 0) {
      pos <- ada_pos[1, ]
      
      size <- as.numeric(pos$total)
      avg_price <- as.numeric(pos$averageOpenPrice)
      current_pnl <- as.numeric(pos$unrealizedPL)
      mark_price <- as.numeric(pos$markPrice)
      margin <- as.numeric(pos$margin)
      
      cat(sprintf("   Size: %s ADA\n", size))
      cat(sprintf("   Avg Price: %.8f USDT\n", avg_price))
      cat(sprintf("   Mark Price: %.8f USDT\n", mark_price))
      cat(sprintf("   Unrealized P&L: %.4f USDT\n", current_pnl))
      cat(sprintf("   Margin Used: %.4f USDT\n", margin))
      
      # Performance Calculation
      price_change_pct <- ((mark_price - avg_price) / avg_price) * 100
      
      cat(sprintf("   Price Change: %.2f%%\n", price_change_pct))
      
      if (current_pnl < 0) {
        cat("   📉 Position currently in LOSS\n")
      } else {
        cat("   📈 Position currently in PROFIT\n")
      }
    }
  }
  
  # Active Orders
  cat("\n📋 ACTIVE ORDERS:\n")
  orders <- get_active_orders()
  
  # Analysis
  cat("\n📊 SITUATION ANALYSIS:\n")
  if (is.null(orders)) {
    cat("   ⚠️  NO PROTECTION: No TP/SL orders active!\n")
    cat("   🚨 RISK: Position is unprotected\n")
    cat("   💡 RECOMMENDATION: Set protective orders immediately\n")
  } else {
    has_tp <- any(orders$planType == "pos_profit")
    has_sl <- any(orders$planType == "pos_loss")
    
    if (has_tp && has_sl) {
      cat("   ✅ PROTECTED: Both TP and SL orders active\n")
    } else if (has_tp) {
      cat("   ⚠️  PARTIAL PROTECTION: Only TP active, no SL\n")
    } else if (has_sl) {
      cat("   ⚠️  PARTIAL PROTECTION: Only SL active, no TP\n")
    }
  }
  
  return(list(position = ada_pos, orders = orders))
}

# 3. SMART ORDER RECOMMENDATIONS
recommend_orders <- function() {
  cat("\n🎯 SMART ORDER RECOMMENDATIONS\n")
  cat("==============================\n")
  
  # Get current position
  positions_data <- bitget_request("/api/mix/v1/position/allPosition", "GET", 
                                   list(productType = "umcbl"))
  
  if (positions_data$code == "00000") {
    ada_pos <- positions_data$data[positions_data$data$symbol == "ADAUSDT_UMCBL" & 
                                     as.numeric(positions_data$data$total) > 0, ]
    
    if (nrow(ada_pos) > 0) {
      pos <- ada_pos[1, ]
      
      size <- as.numeric(pos$total)
      avg_price <- as.numeric(pos$averageOpenPrice)
      mark_price <- as.numeric(pos$markPrice)
      current_pnl <- as.numeric(pos$unrealizedPL)
      
      cat(sprintf("Based on your position:\n"))
      cat(sprintf("   Average Price: %.8f USDT\n", avg_price))
      cat(sprintf("   Current Price: %.8f USDT\n", mark_price))
      cat(sprintf("   Current P&L: %.4f USDT\n", current_pnl))
      
      # Recommendations based on current situation
      if (current_pnl < 0) {
        # Position in loss - defensive strategy
        cat("\n🛡️  DEFENSIVE STRATEGY (Position in Loss):\n")
        
        # Conservative TP - just above break-even
        tp_conservative <- avg_price * 1.02  # 2% above avg
        tp_target <- avg_price * 1.05        # 5% above avg
        
        # Protective SL
        sl_tight <- avg_price * 0.95         # 5% below avg
        sl_loose <- avg_price * 0.90         # 10% below avg
        
        cat(sprintf("   📈 Conservative TP: %.6f USDT (+2%% from avg)\n", tp_conservative))
        cat(sprintf("   📈 Target TP: %.6f USDT (+5%% from avg)\n", tp_target))
        cat(sprintf("   📉 Tight SL: %.6f USDT (-5%% from avg)\n", sl_tight))
        cat(sprintf("   📉 Loose SL: %.6f USDT (-10%% from avg)\n", sl_loose))
        
        # Specific recommendation
        recommended_tp <- tp_conservative
        recommended_sl <- sl_tight
        
      } else {
        # Position in profit - aggressive strategy
        cat("\n🚀 AGGRESSIVE STRATEGY (Position in Profit):\n")
        
        # Trailing stops
        tp_aggressive <- mark_price * 1.05   # 5% above current
        sl_trailing <- mark_price * 0.98     # 2% below current (trailing)
        
        cat(sprintf("   📈 Aggressive TP: %.6f USDT (+5%% from current)\n", tp_aggressive))
        cat(sprintf("   📉 Trailing SL: %.6f USDT (-2%% from current)\n", sl_trailing))
        
        recommended_tp <- tp_aggressive
        recommended_sl <- sl_trailing
      }
      
      cat(sprintf("\n💡 RECOMMENDED ORDERS:\n"))
      cat(sprintf("   🎯 Take Profit: %.6f USDT\n", recommended_tp))
      cat(sprintf("   🛑 Stop Loss: %.6f USDT\n", recommended_sl))
      
      cat(sprintf("\n💻 EXECUTION COMMANDS:\n"))
      cat(sprintf("# Set Take Profit:\n"))
      cat(sprintf("place_take_profit('ADAUSDT_UMCBL', 'long', '%s', %.6f)\n\n", size, recommended_tp))
      cat(sprintf("# Set Stop Loss:\n"))
      cat(sprintf("place_stop_loss('ADAUSDT_UMCBL', 'long', '%s', %.6f)\n\n", size, recommended_sl))
      cat(sprintf("# Set Both at Once:\n"))
      cat(sprintf("place_tp_sl('ADAUSDT_UMCBL', 'long', '%s', %.6f, %.6f)\n", 
                  size, recommended_tp, recommended_sl))
      
      return(list(
        tp = recommended_tp,
        sl = recommended_sl,
        size = size,
        current_price = mark_price,
        avg_price = avg_price
      ))
    }
  }
}

# 4. CANCEL ALL ORDERS FUNCTION
cancel_all_ada_orders <- function() {
  cat("\n🗑️ CANCELLING ALL ADA ORDERS\n")
  cat("=============================\n")
  
  # Get active orders
  orders <- get_active_orders()
  
  if (is.null(orders) || nrow(orders) == 0) {
    cat("No orders to cancel.\n")
    return()
  }
  
  cancelled <- 0
  for (i in 1:nrow(orders)) {
    order <- orders[i, ]
    if (order$symbol == "ADAUSDT_UMCBL") {
      
      body <- list(
        orderId = as.character(order$orderId),
        symbol = order$symbol,
        marginCoin = "USDT"
      )
      
      result <- bitget_request("/api/mix/v1/plan/cancelPlan", "POST", body)
      
      if (!is.null(result) && result$code == "00000") {
        cat(sprintf("✅ Cancelled %s order: %s\n", 
                    ifelse(order$planType == "pos_profit", "TP", "SL"),
                    order$orderId))
        cancelled <- cancelled + 1
      } else {
        cat(sprintf("❌ Failed to cancel order: %s\n", order$orderId))
      }
      
      Sys.sleep(0.2)  # Kleine Pause zwischen Requests
    }
  }
  
  cat(sprintf("\n📊 Summary: %d orders cancelled.\n", cancelled))
}

# ==========================================
# 🚀 MAIN EXECUTION
# ==========================================

cat("🚀 ADA ORDER ANALYSIS READY!\n")
cat("=====================================\n")
cat("Available functions:\n")
cat("• get_active_orders()          - Show all active orders\n")
cat("• analyze_ada_situation()      - Complete analysis\n") 
cat("• recommend_orders()           - Smart recommendations\n")
cat("• cancel_all_ada_orders()      - Cancel all ADA orders\n")
cat("\nStart with: analyze_ada_situation()\n")

analyze_ada_situation()
