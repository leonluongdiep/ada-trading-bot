# ==========================================================================================================
# 🚀 STRATEGIC ORDER FUNCTION - NACHRÜSTUNG
# ==========================================================================================================
# 
# Diese Funktion ergänzt dein Trading System um die fehlende Strategic Order Funktionalität
# 
# VERWENDUNG:
# place_strategic_limit_order(symbol, side, size, price)
# 
# BEISPIEL:
# place_strategic_limit_order('ADAUSDT_UMCBL', 'open_long', 5000, 0.5636)
# 
# ==========================================================================================================

cat("🔧 Lade Strategic Order Funktion...\n")

# ==========================================================================================================
# 📋 STRATEGIC LIMIT ORDER FUNCTION
# ==========================================================================================================

place_strategic_limit_order <- function(symbol, side, size, price) {
  cat("🚀 STRATEGIC LIMIT ORDER PLACEMENT\n")
  cat("==================================\n")
  cat("   ⚠️  WARNING: PLACING REAL ORDER ON BITGET!\n")
  
  # Eingabe-Validierung
  if (missing(symbol) || missing(side) || missing(size) || missing(price)) {
    return(list(success = FALSE, error = "Missing required parameters"))
  }
  
  # Symbol-Präzision abrufen
  symbol_info <- get_symbol_precision(symbol)
  
  # Preis formatieren
  if (!is.null(symbol_info)) {
    formatted_price <- format_price_precise(price, symbol_info)
  } else {
    formatted_price <- sprintf("%.4f", round(price, 4))
  }
  
  cat(sprintf("📋 Strategic Order Details:\n"))
  cat(sprintf("   Symbol: %s\n", symbol))
  cat(sprintf("   Side: %s\n", side))
  cat(sprintf("   Size: %s contracts\n", size))
  cat(sprintf("   Price: %s USDT\n", formatted_price))
  
  # Order Body für Bitget V1 API erstellen
  # Basiert auf der working TP/SL Struktur aus deinem System
  order_body <- list(
    symbol = symbol,
    marginCoin = "USDT",
    side = side,                    # "open_long" or "open_short"
    orderType = "limit",            # Limit Order für Strategic Placement
    price = formatted_price,        # Formatierter Preis
    size = as.character(size),      # Kontraktanzahl als String
    timeInForceValue = "normal",    # Standard Time-in-Force
    presetTakeProfitPrice = "",     # Leer für Strategic Orders
    presetStopLossPrice = ""        # Leer für Strategic Orders
  )
  
  cat(sprintf("\n🔗 API Call Details:\n"))
  cat(sprintf("   Endpoint: /api/mix/v1/order/placeOrder\n"))
  cat(sprintf("   Method: POST\n"))
  cat(sprintf("   Order Type: Limit\n"))
  
  # API Request ausführen
  tryCatch({
    result <- bitget_request("/api/mix/v1/order/placeOrder", "POST", order_body)
    
    if (!is.null(result) && result$code == "00000") {
      cat(sprintf("✅ STRATEGIC ORDER PLACED SUCCESSFULLY!\n"))
      cat(sprintf("   Order ID: %s\n", result$data$orderId))
      cat(sprintf("   Client Order ID: %s\n", result$data$clientOid))
      
      return(list(
        success = TRUE,
        order_id = result$data$orderId,
        client_order_id = result$data$clientOid,
        symbol = symbol,
        side = side,
        size = size,
        price = formatted_price
      ))
      
    } else {
      error_msg <- if(!is.null(result)) result$msg else "Unknown API error"
      cat(sprintf("❌ STRATEGIC ORDER FAILED!\n"))
      cat(sprintf("   Error Code: %s\n", if(!is.null(result)) result$code else "N/A"))
      cat(sprintf("   Error Message: %s\n", error_msg))
      
      return(list(
        success = FALSE,
        error = error_msg,
        error_code = if(!is.null(result)) result$code else "UNKNOWN"
      ))
    }
    
  }, error = function(e) {
    cat(sprintf("❌ STRATEGIC ORDER ERROR: %s\n", e$message))
    return(list(
      success = FALSE,
      error = paste("Request error:", e$message)
    ))
  })
}

# ==========================================================================================================
# 🔍 HELPER FUNCTION: GET CURRENT OPEN ORDERS
# ==========================================================================================================

get_current_open_orders <- function(symbol = NULL) {
  cat("📋 Fetching current open orders...\n")
  
  # Parameter für API Call
  params <- list(productType = "umcbl")
  if (!is.null(symbol)) {
    params$symbol <- symbol
  }
  
  # API Request
  result <- bitget_request("/api/mix/v1/order/current", "GET", params)
  
  if (!is.null(result) && result$code == "00000") {
    if (!is.null(result$data) && length(result$data) > 0) {
      cat(sprintf("✅ %d open orders found\n", length(result$data)))
      return(result$data)
    } else {
      cat("📭 No open orders found\n")
      return(NULL)
    }
  } else {
    cat("❌ Failed to fetch open orders\n")
    return(NULL)
  }
}

# ==========================================================================================================
# 🛠️ HELPER FUNCTION: CANCEL ORDER (für Strategic Order Management)
# ==========================================================================================================

cancel_strategic_order <- function(symbol, order_id) {
  cat(sprintf("🗑️ Canceling order %s for %s...\n", order_id, symbol))
  
  cancel_body <- list(
    symbol = symbol,
    marginCoin = "USDT",
    orderId = order_id
  )
  
  result <- bitget_request("/api/mix/v1/order/cancel-order", "POST", cancel_body)
  
  if (!is.null(result) && result$code == "00000") {
    cat(sprintf("✅ Order %s canceled successfully\n", order_id))
    return(list(success = TRUE, order_id = order_id))
  } else {
    error_msg <- if(!is.null(result)) result$msg else "Unknown error"
    cat(sprintf("❌ Cancel failed: %s\n", error_msg))
    return(list(success = FALSE, error = error_msg))
  }
}

# ==========================================================================================================
# 🧪 STRATEGIC ORDER TEST FUNCTION
# ==========================================================================================================

test_strategic_order_function <- function() {
  cat("🧪 TESTING STRATEGIC ORDER FUNCTION\n")
  cat("===================================\n")
  
  # Test parameters
  test_symbol <- "ADAUSDT_UMCBL"
  test_side <- "open_long"
  test_size <- 5000
  test_price <- 0.5636
  
  cat(sprintf("Test Parameters:\n"))
  cat(sprintf("   Symbol: %s\n", test_symbol))
  cat(sprintf("   Side: %s\n", test_side))
  cat(sprintf("   Size: %d\n", test_size))
  cat(sprintf("   Price: %.4f USDT\n", test_price))
  
  cat(sprintf("\n⚠️ This will place a REAL order! Continue? (ENTER/Ctrl+C)\n"))
  readline()
  
  # Test the function
  result <- place_strategic_limit_order(test_symbol, test_side, test_size, test_price)
  
  cat(sprintf("\n📊 TEST RESULT:\n"))
  if (result$success) {
    cat(sprintf("✅ Function works! Order ID: %s\n", result$order_id))
  } else {
    cat(sprintf("❌ Function failed: %s\n", result$error))
  }
  
  return(result)
}

# ==========================================================================================================
# 🎯 QUICK STRATEGIC ORDER WRAPPER
# ==========================================================================================================

quick_strategic_ada <- function(size = 5000, price = 0.5636) {
  cat("⚡ QUICK STRATEGIC ADA ORDER\n")
  cat("===========================\n")
  cat(sprintf("Quick DCA Order: %d ADA @ %.4f USDT\n", size, price))
  
  return(place_strategic_limit_order('ADAUSDT_UMCBL', 'open_long', size, price))
}

# ==========================================================================================================
# ✅ INSTALLATION COMPLETE
# ==========================================================================================================

cat("✅ STRATEGIC ORDER FUNCTIONS LOADED!\n")
cat("====================================\n")
cat("🎯 Available Functions:\n")
cat("   place_strategic_limit_order(symbol, side, size, price)\n")
cat("   get_current_open_orders(symbol)\n") 
cat("   cancel_strategic_order(symbol, order_id)\n")
cat("   test_strategic_order_function()\n")
cat("   quick_strategic_ada(size, price)\n")
cat("\n🚀 Ready for Strategic Order placement!\n")

# ==========================================================================================================
# 📋 USAGE EXAMPLES
# ==========================================================================================================

cat("\n📋 USAGE EXAMPLES:\n")
cat("==================\n")
cat("# Deine geplante Strategic Order:\n")
cat("result <- place_strategic_limit_order('ADAUSDT_UMCBL', 'open_long', 5000, 0.5636)\n")
cat("\n# Quick version:\n")
cat("result <- quick_strategic_ada(5000, 0.5636)\n")
cat("\n# Check orders:\n")
cat("get_current_open_orders('ADAUSDT_UMCBL')\n")
cat("\n# Test function (safe test):\n")
cat("# test_strategic_order_function()  # Uncomment to test\n")

cat("\n🎯 Ready to complete your trading setup!\n")