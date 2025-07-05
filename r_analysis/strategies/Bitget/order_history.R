# ==========================================================================================================
# 📋 BITGET ORDER DETAILS API - KOMPLETTE FUNKTIONALITÄT
# ==========================================================================================================
# 
# Diese Funktionen ermöglichen detaillierte Order-Abfragen über die Bitget API
# 
# VERFÜGBARE FUNKTIONEN:
# - get_order_details_by_id()     # Einzelne Order Details
# - get_order_history()           # Order Verlauf
# - get_filled_orders()           # Ausgeführte Orders
# - get_order_status()            # Order Status Check
# - monitor_strategic_orders()    # Strategic Order Tracking
# 
# ==========================================================================================================

cat("📋 Loading Bitget Order Details API Functions...\n")

# ==========================================================================================================
# 🔍 ORDER DETAILS BY ID
# ==========================================================================================================

get_order_details_by_id <- function(symbol, order_id) {
  cat(sprintf("🔍 Fetching order details for ID: %s\n", order_id))
  
  # API Parameter
  params <- list(
    symbol = symbol,
    orderId = order_id
  )
  
  # API Call für Order Details
  result <- bitget_request("/api/mix/v1/order/detail", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    order_details <- result$data
    
    cat(sprintf("✅ Order Details retrieved:\n"))
    cat(sprintf("   Order ID: %s\n", order_details$orderId))
    cat(sprintf("   Symbol: %s\n", order_details$symbol))
    cat(sprintf("   Side: %s\n", order_details$side))
    cat(sprintf("   Size: %s\n", order_details$size))
    cat(sprintf("   Price: %s USDT\n", order_details$price))
    cat(sprintf("   Status: %s\n", order_details$state))
    cat(sprintf("   Fill Type: %s\n", order_details$fillPrice))
    cat(sprintf("   Create Time: %s\n", order_details$cTime))
    
    return(order_details)
  } else {
    error_msg <- if(!is.null(result)) result$msg else "Unknown error"
    cat(sprintf("❌ Failed to get order details: %s\n", error_msg))
    return(NULL)
  }
}

# ==========================================================================================================
# 📜 ORDER HISTORY (ALLE ORDERS)
# ==========================================================================================================

get_order_history <- function(symbol, start_time = NULL, end_time = NULL, limit = 100) {
  cat(sprintf("📜 Fetching order history for %s (limit: %d)\n", symbol, limit))
  
  # API Parameter
  params <- list(
    symbol = symbol,
    pageSize = as.character(limit)
  )
  
  # Optional time range
  if (!is.null(start_time)) {
    params$startTime <- as.character(as.numeric(start_time) * 1000)
  }
  if (!is.null(end_time)) {
    params$endTime <- as.character(as.numeric(end_time) * 1000)
  }
  
  # API Call für Order History
  result <- bitget_request("/api/mix/v1/order/history", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    orders <- result$data$orderList
    
    if (!is.null(orders) && length(orders) > 0) {
      cat(sprintf("✅ %d orders found in history\n", length(orders)))
      
      # Strukturiere Order History
      order_history <- data.frame(
        order_id = sapply(orders, function(x) x$orderId),
        symbol = sapply(orders, function(x) x$symbol),
        side = sapply(orders, function(x) x$side),
        size = sapply(orders, function(x) as.numeric(x$size)),
        price = sapply(orders, function(x) as.numeric(x$price)),
        filled_size = sapply(orders, function(x) as.numeric(x$fillSize)),
        status = sapply(orders, function(x) x$state),
        create_time = sapply(orders, function(x) as.POSIXct(as.numeric(x$cTime)/1000, origin="1970-01-01")),
        update_time = sapply(orders, function(x) as.POSIXct(as.numeric(x$uTime)/1000, origin="1970-01-01")),
        stringsAsFactors = FALSE
      )
      
      return(order_history)
    } else {
      cat("📭 No orders found in history\n")
      return(NULL)
    }
  } else {
    error_msg <- if(!is.null(result)) result$msg else "Unknown error"
    cat(sprintf("❌ Failed to get order history: %s\n", error_msg))
    return(NULL)
  }
}

# ==========================================================================================================
# ✅ FILLED ORDERS (AUSGEFÜHRTE ORDERS)
# ==========================================================================================================

get_filled_orders <- function(symbol, start_time = NULL, end_time = NULL, limit = 100) {
  cat(sprintf("✅ Fetching filled orders for %s (limit: %d)\n", symbol, limit))
  
  # API Parameter
  params <- list(
    symbol = symbol,
    pageSize = as.character(limit)
  )
  
  # Optional time range
  if (!is.null(start_time)) {
    params$startTime <- as.character(as.numeric(start_time) * 1000)
  }
  if (!is.null(end_time)) {
    params$endTime <- as.character(as.numeric(end_time) * 1000)
  }
  
  # API Call für Filled Orders
  result <- bitget_request("/api/mix/v1/order/fills", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    fills <- result$data$fillList
    
    if (!is.null(fills) && length(fills) > 0) {
      cat(sprintf("✅ %d filled orders found\n", length(fills)))
      
      # Strukturiere Filled Orders
      filled_orders <- data.frame(
        trade_id = sapply(fills, function(x) x$tradeId),
        order_id = sapply(fills, function(x) x$orderId),
        symbol = sapply(fills, function(x) x$symbol),
        side = sapply(fills, function(x) x$side),
        fill_size = sapply(fills, function(x) as.numeric(x$fillSize)),
        fill_price = sapply(fills, function(x) as.numeric(x$fillPrice)),
        fill_value = sapply(fills, function(x) as.numeric(x$fillValue)),
        fees = sapply(fills, function(x) as.numeric(x$fee)),
        fill_time = sapply(fills, function(x) as.POSIXct(as.numeric(x$cTime)/1000, origin="1970-01-01")),
        stringsAsFactors = FALSE
      )
      
      return(filled_orders)
    } else {
      cat("📭 No filled orders found\n")
      return(NULL)
    }
  } else {
    error_msg <- if(!is.null(result)) result$msg else "Unknown error"
    cat(sprintf("❌ Failed to get filled orders: %s\n", error_msg))
    return(NULL)
  }
}

# ==========================================================================================================
# 🔄 ORDER STATUS CHECK
# ==========================================================================================================

get_order_status <- function(symbol, order_id) {
  cat(sprintf("🔄 Checking status for order: %s\n", order_id))
  
  order_details <- get_order_details_by_id(symbol, order_id)
  
  if (!is.null(order_details)) {
    status <- order_details$state
    
    # Status interpretation
    status_info <- switch(status,
                          "new" = list(desc = "Order placed, waiting for fill", color = "🟡"),
                          "partial_filled" = list(desc = "Partially filled", color = "🟠"),
                          "filled" = list(desc = "Completely filled", color = "🟢"),
                          "cancelled" = list(desc = "Order cancelled", color = "🔴"),
                          "rejected" = list(desc = "Order rejected", color = "❌"),
                          list(desc = paste("Unknown status:", status), color = "⚪")
    )
    
    cat(sprintf("%s Status: %s (%s)\n", status_info$color, status, status_info$desc))
    
    # Zusätzliche Details
    if (!is.null(order_details$fillSize) && as.numeric(order_details$fillSize) > 0) {
      fill_percentage <- (as.numeric(order_details$fillSize) / as.numeric(order_details$size)) * 100
      cat(sprintf("📊 Fill Progress: %.1f%% (%s/%s)\n", 
                  fill_percentage, order_details$fillSize, order_details$size))
    }
    
    return(list(
      order_id = order_id,
      status = status,
      description = status_info$desc,
      details = order_details
    ))
  } else {
    return(NULL)
  }
}

# ==========================================================================================================
# 📊 STRATEGIC ORDER MONITORING
# ==========================================================================================================

monitor_strategic_orders <- function(symbol = "ADAUSDT_UMCBL") {
  cat(sprintf("📊 STRATEGIC ORDER MONITORING FOR %s\n", symbol))
  cat(sprintf("=====================================\n"))
  
  # Aktuelle Open Orders
  cat(sprintf("📋 Current Open Orders:\n"))
  open_orders <- get_current_open_orders(symbol)
  
  if (!is.null(open_orders) && length(open_orders) > 0) {
    for (i in 1:length(open_orders)) {
      order <- open_orders[[i]]
      cat(sprintf("   🚀 Order %d: %s %s @ %s USDT (ID: %s)\n", 
                  i, order$side, order$size, order$price, order$orderId))
      
      # Status für jede Order prüfen
      status_info <- get_order_status(symbol, order$orderId)
    }
  } else {
    cat(sprintf("   📭 No open orders found\n"))
  }
  
  # Plan Orders (TP/SL)
  cat(sprintf("\n📋 Current Plan Orders:\n"))
  plan_orders <- get_current_plan_orders(symbol)
  
  if (!is.null(plan_orders) && nrow(plan_orders) > 0) {
    for (i in 1:nrow(plan_orders)) {
      order <- plan_orders[i, ]
      order_type <- switch(order$planType,
                           "pos_profit" = "📈 TP",
                           "pos_loss" = "🛡️ SL",
                           "📋 Plan")
      cat(sprintf("   %s: %s @ %s USDT (ID: %s)\n", 
                  order_type, order$size, order$triggerPrice, order$orderId))
    }
  } else {
    cat(sprintf("   📭 No plan orders found\n"))
  }
  
  # Recent Fills
  cat(sprintf("\n✅ Recent Filled Orders (last 10):\n"))
  recent_fills <- get_filled_orders(symbol, limit = 10)
  
  if (!is.null(recent_fills) && nrow(recent_fills) > 0) {
    for (i in 1:min(nrow(recent_fills), 5)) {
      fill <- recent_fills[i, ]
      cat(sprintf("   💰 %s: %s @ %.4f USDT (%s)\n", 
                  fill$side, fill$fill_size, fill$fill_price, fill$fill_time))
    }
  } else {
    cat(sprintf("   📭 No recent fills found\n"))
  }
}

# ==========================================================================================================
# 🎯 QUICK ORDER LOOKUP
# ==========================================================================================================

quick_order_lookup <- function(order_id, symbol = "ADAUSDT_UMCBL") {
  cat(sprintf("🎯 QUICK ORDER LOOKUP\n"))
  cat(sprintf("Order ID: %s\n", order_id))
  cat(sprintf("Symbol: %s\n", symbol))
  cat(sprintf("=====================\n"))
  
  # Order Details abrufen
  details <- get_order_details_by_id(symbol, order_id)
  
  if (!is.null(details)) {
    # Status prüfen
    status_info <- get_order_status(symbol, order_id)
    
    # Zusammenfassung
    cat(sprintf("\n📊 ORDER SUMMARY:\n"))
    cat(sprintf("   Type: %s %s\n", details$side, details$orderType))
    cat(sprintf("   Size: %s contracts\n", details$size))
    cat(sprintf("   Price: %s USDT\n", details$price))
    cat(sprintf("   Status: %s\n", status_info$description))
    cat(sprintf("   Created: %s\n", details$cTime))
    
    return(details)
  } else {
    cat(sprintf("❌ Order not found or error occurred\n"))
    return(NULL)
  }
}

# ==========================================================================================================
# 📈 PERFORMANCE TRACKING
# ==========================================================================================================

track_order_performance <- function(symbol = "ADAUSDT_UMCBL", days = 7) {
  cat(sprintf("📈 ORDER PERFORMANCE TRACKING\n"))
  cat(sprintf("Symbol: %s | Last %d days\n", symbol, days))
  cat(sprintf("================================\n"))
  
  # Zeitbereich definieren
  end_time <- Sys.time()
  start_time <- end_time - (days * 24 * 3600)
  
  # Order History abrufen
  history <- get_order_history(symbol, start_time, end_time, limit = 200)
  
  if (!is.null(history) && nrow(history) > 0) {
    # Performance Statistiken
    total_orders <- nrow(history)
    filled_orders <- sum(history$status == "filled")
    cancelled_orders <- sum(history$status == "cancelled")
    
    cat(sprintf("📊 Order Statistics:\n"))
    cat(sprintf("   Total Orders: %d\n", total_orders))
    cat(sprintf("   Filled: %d (%.1f%%)\n", filled_orders, (filled_orders/total_orders)*100))
    cat(sprintf("   Cancelled: %d (%.1f%%)\n", cancelled_orders, (cancelled_orders/total_orders)*100))
    
    # Filled Orders Details
    fills <- get_filled_orders(symbol, start_time, end_time, limit = 200)
    
    if (!is.null(fills) && nrow(fills) > 0) {
      total_volume <- sum(fills$fill_value)
      total_fees <- sum(fills$fees)
      avg_fill_price <- mean(fills$fill_price)
      
      cat(sprintf("\n💰 Trading Volume:\n"))
      cat(sprintf("   Total Volume: %.2f USDT\n", total_volume))
      cat(sprintf("   Total Fees: %.2f USDT\n", total_fees))
      cat(sprintf("   Average Fill Price: %.4f USDT\n", avg_fill_price))
      cat(sprintf("   Fee Percentage: %.3f%%\n", (total_fees/total_volume)*100))
    }
    
    return(list(
      history = history,
      fills = fills,
      stats = list(
        total_orders = total_orders,
        filled_orders = filled_orders,
        cancelled_orders = cancelled_orders
      )
    ))
  } else {
    cat(sprintf("📭 No orders found in the specified period\n"))
    return(NULL)
  }
}

# ==========================================================================================================
# ✅ INSTALLATION COMPLETE
# ==========================================================================================================

cat("✅ BITGET ORDER DETAILS API LOADED!\n")
cat("====================================\n")
cat("🎯 Available Functions:\n")
cat("   get_order_details_by_id(symbol, order_id)\n")
cat("   get_order_history(symbol, start_time, end_time, limit)\n")
cat("   get_filled_orders(symbol, start_time, end_time, limit)\n")
cat("   get_order_status(symbol, order_id)\n")
cat("   monitor_strategic_orders(symbol)\n")
cat("   quick_order_lookup(order_id, symbol)\n")
cat("   track_order_performance(symbol, days)\n")

cat("\n📋 USAGE EXAMPLES:\n")
cat("==================\n")
cat("# Check specific order:\n")
cat("quick_order_lookup('1234567890', 'ADAUSDT_UMCBL')\n")
cat("\n# Monitor all your orders:\n")
cat("monitor_strategic_orders('ADAUSDT_UMCBL')\n")
cat("\n# Get order history:\n")
cat("history <- get_order_history('ADAUSDT_UMCBL', limit = 50)\n")
cat("\n# Track performance:\n")
cat("performance <- track_order_performance('ADAUSDT_UMCBL', days = 7)\n")

cat("\n🚀 Ready for comprehensive order tracking!\n")
