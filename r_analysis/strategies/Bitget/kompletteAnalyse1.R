# ==========================================
# 📋 BITGET ORDER MANAGEMENT & MONITORING
# ==========================================

# --- 1. Aktive Plan Orders überprüfen ---
check_active_orders <- function(symbol = NULL) {
  cat("\n📋 Checking active plan orders (TP/SL)...\n")
  cat("=========================================\n")
  
  query_params <- list(productType = "umcbl")
  if (!is.null(symbol)) {
    query_params$symbol <- symbol
  }
  
  data <- bitget_request("/api/mix/v1/plan/currentPlan", params = query_params)
  
  if (is.null(data) || data$code != "00000") {
    cat("❌ Failed to fetch orders\n")
    return(NULL)
  }
  
  if (length(data$data) == 0) {
    cat("📭 No active plan orders found.\n")
    return(NULL)
  }
  
  orders_df <- data$data
  
  # Formatierte Ausgabe
  for (i in 1:nrow(orders_df)) {
    order <- orders_df[i, ]
    
    # Order Type Icon
    icon <- if(order$planType == "pos_profit") "🎯" else "🛑"
    type_name <- if(order$planType == "pos_profit") "Take Profit" else "Stop Loss"
    
    cat(sprintf("\n%s %s Order:\n", icon, type_name))
    cat(sprintf("   Symbol: %s\n", order$symbol))
    cat(sprintf("   Side: %s\n", order$side))
    cat(sprintf("   Size: %s contracts\n", order$size))
    cat(sprintf("   Trigger Price: %s USDT\n", order$triggerPrice))
    cat(sprintf("   Order ID: %s\n", order$orderId))
    cat(sprintf("   Status: %s\n", order$state))
    cat(sprintf("   Created: %s\n", as.POSIXct(as.numeric(order$createTime)/1000, origin="1970-01-01")))
  }
  
  return(orders_df)
}

# --- 2. Order History abrufen ---
get_order_history <- function(symbol = NULL, limit = 20) {
  cat("\n📜 Fetching order history...\n")
  
  query_params <- list(
    productType = "umcbl",
    startTime = as.character(as.numeric(Sys.time() - 7*24*3600) * 1000),  # Letzte 7 Tage
    endTime = as.character(as.numeric(Sys.time()) * 1000),
    pageSize = as.character(limit)
  )
  
  if (!is.null(symbol)) {
    query_params$symbol <- symbol
  }
  
  data <- bitget_request("/api/mix/v1/plan/historyPlan", params = query_params)
  
  if (is.null(data) || data$code != "00000") {
    cat("❌ Failed to fetch history\n")
    return(NULL)
  }
  
  if (length(data$data) == 0) {
    cat("📭 No historical orders found.\n")
    return(NULL)
  }
  
  history_df <- data$data
  cat(sprintf("\n📊 Found %d historical orders\n", nrow(history_df)))
  
  return(history_df)
}

# --- 3. Order löschen ---
cancel_plan_order <- function(order_id, symbol) {
  cat(sprintf("\n🗑️ Cancelling order %s...\n", order_id))
  
  body <- list(
    orderId = as.character(order_id),
    symbol = symbol,
    marginCoin = "USDT",
    planType = "plan"  # Kann auch "pos_profit" oder "pos_loss" sein
  )
  
  data <- bitget_request("/api/mix/v1/plan/cancelPlan", method = "POST", params = body)
  
  if (is.null(data)) return(FALSE)
  
  if (data$code == "00000") {
    cat("✅ Order cancelled successfully!\n")
    return(TRUE)
  } else {
    cat(sprintf("❌ Failed to cancel: %s\n", data$msg))
    return(FALSE)
  }
}

# --- 4. Alle Orders für ein Symbol löschen ---
cancel_all_orders <- function(symbol) {
  cat(sprintf("\n🗑️ Cancelling all orders for %s...\n", symbol))
  
  # Erst alle aktiven Orders abrufen
  orders <- check_active_orders(symbol)
  
  if (is.null(orders) || nrow(orders) == 0) {
    cat("No orders to cancel.\n")
    return()
  }
  
  cancelled <- 0
  for (i in 1:nrow(orders)) {
    if (cancel_plan_order(orders$orderId[i], symbol)) {
      cancelled <- cancelled + 1
    }
    Sys.sleep(0.2)  # Kleine Pause zwischen Requests
  }
  
  cat(sprintf("\n✅ Cancelled %d orders.\n", cancelled))
}

# --- 5. Order Monitoring Dashboard ---
monitor_position <- function(symbol = "ADAUSDT_UMCBL") {
  cat("\n📊 POSITION MONITORING DASHBOARD\n")
  cat("=====================================\n")
  
  # Position abrufen
  cat("\n1️⃣ Current Position:\n")
  df_pos <- get_positions()
  
  # Aktive Orders
  cat("\n2️⃣ Active Orders:\n")
  orders <- check_active_orders(symbol)
  
  # Marktpreis
  cat("\n3️⃣ Current Market Price:\n")
  market_data <- bitget_request("/api/mix/v1/market/ticker", 
                                params = list(symbol = symbol, productType = "umcbl"))
  
  if (!is.null(market_data) && market_data$code == "00000") {
    current_price <- as.numeric(market_data$data$last)
    cat(sprintf("   Current Price: %s USDT\n", current_price))
    
    # Abstand zu TP/SL berechnen
    if (!is.null(orders) && nrow(orders) > 0) {
      cat("\n4️⃣ Distance to Orders:\n")
      for (i in 1:nrow(orders)) {
        order <- orders[i, ]
        trigger <- as.numeric(order$triggerPrice)
        distance <- abs(current_price - trigger)
        distance_pct <- (distance / current_price) * 100
        
        if (order$planType == "pos_profit") {
          cat(sprintf("   🎯 TP: %.2f%% away (%.4f USDT)\n", distance_pct, distance))
        } else {
          cat(sprintf("   🛑 SL: %.2f%% away (%.4f USDT)\n", distance_pct, distance))
        }
      }
    }
  }
}

# --- 6. Order Updates ---
update_tp_sl <- function(symbol, side, size, new_tp = NULL, new_sl = NULL) {
  cat("\n🔄 Updating TP/SL orders...\n")
  
  # Erst alte Orders löschen
  orders <- check_active_orders(symbol)
  
  if (!is.null(orders) && nrow(orders) > 0) {
    cat("Cancelling existing orders first...\n")
    for (i in 1:nrow(orders)) {
      order <- orders[i, ]
      if (!is.null(new_tp) && order$planType == "pos_profit" ||
          !is.null(new_sl) && order$planType == "pos_loss") {
        cancel_plan_order(order$orderId, symbol)
        Sys.sleep(0.2)
      }
    }
  }
  
  # Neue Orders platzieren
  if (!is.null(new_tp)) {
    place_take_profit(symbol, side, size, new_tp)
  }
  if (!is.null(new_sl)) {
    Sys.sleep(0.5)
    place_stop_loss(symbol, side, size, new_sl)
  }
}

# --- VERWENDUNG ---
cat("\n🚀 Order Management Functions ready!\n")
cat("\nUseful commands:\n")
cat("- check_active_orders('ADAUSDT_UMCBL')  # Check your orders\n")
cat("- monitor_position('ADAUSDT_UMCBL')     # Full position overview\n")
cat("- cancel_all_orders('ADAUSDT_UMCBL')    # Cancel all orders\n")
cat("- update_tp_sl('ADAUSDT_UMCBL', 'long', '2000', new_tp=0.65)  # Update TP\n")


# Ihre aktiven Orders anzeigen
check_active_orders('ADAUSDT_UMCBL')

# Komplette Position-Übersicht
monitor_position('ADAUSDT_UMCBL')
