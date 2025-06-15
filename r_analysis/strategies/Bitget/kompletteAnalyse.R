# ==========================================
# 🔧 BITGET API FIX - DEZIMALSTELLEN KORREKTUR
# ==========================================

# --- Helper Function für Preisformatierung ---
format_price <- function(price, decimals = 4) {
  # Rundet auf die gewünschte Anzahl Dezimalstellen
  # und gibt als String zurück (wichtig für API)
  sprintf(paste0("%.", decimals, "f"), round(price, decimals))
}

# --- 1. KORRIGIERTE Take Profit Function ---
place_take_profit <- function(symbol, side, size, trigger_price, order_type = "market") {
  cat(sprintf("\n📈 Placing Take Profit order for %s...\n", symbol))
  
  # WICHTIG: Preis auf 4 Dezimalstellen formatieren
  formatted_price <- format_price(trigger_price, 4)
  cat(sprintf("   Original price: %f -> Formatted: %s\n", trigger_price, formatted_price))
  
  trade_side <- if(side == "long") "close_long" else "close_short"
  
  body <- list(
    symbol = symbol,
    marginCoin = "USDT",
    planType = "pos_profit",
    triggerPrice = formatted_price,  # Formatierter Preis
    holdSide = side,
    side = trade_side,
    size = as.character(size),
    orderType = order_type,
    triggerType = "fill_price"
  )
  
  if (order_type != "market") {
    body$executePrice <- formatted_price
  }
  
  data <- bitget_request("/api/mix/v1/plan/placePlan", method = "POST", params = body)
  
  if (is.null(data)) return(FALSE)
  
  if (data$code == "00000") {
    cat(sprintf("✅ Take Profit order placed successfully at %s!\n", formatted_price))
    cat(sprintf("   Order ID: %s\n", data$data$orderId))
    return(TRUE)
  } else {
    cat(sprintf("❌ Failed: %s\n", data$msg))
    return(FALSE)
  }
}

# --- 2. KORRIGIERTE Stop Loss Function ---
place_stop_loss <- function(symbol, side, size, trigger_price, order_type = "market") {
  cat(sprintf("\n📉 Placing Stop Loss order for %s...\n", symbol))
  
  # WICHTIG: Preis auf 4 Dezimalstellen formatieren
  formatted_price <- format_price(trigger_price, 4)
  cat(sprintf("   Original price: %f -> Formatted: %s\n", trigger_price, formatted_price))
  
  trade_side <- if(side == "long") "close_long" else "close_short"
  
  body <- list(
    symbol = symbol,
    marginCoin = "USDT",
    planType = "pos_loss",
    triggerPrice = formatted_price,  # Formatierter Preis
    holdSide = side,
    side = trade_side,
    size = as.character(size),
    orderType = order_type,
    triggerType = "fill_price"
  )
  
  if (order_type != "market") {
    body$executePrice <- formatted_price
  }
  
  data <- bitget_request("/api/mix/v1/plan/placePlan", method = "POST", params = body)
  
  if (is.null(data)) return(FALSE)
  
  if (data$code == "00000") {
    cat(sprintf("✅ Stop Loss order placed successfully at %s!\n", formatted_price))
    cat(sprintf("   Order ID: %s\n", data$data$orderId))
    return(TRUE)
  } else {
    cat(sprintf("❌ Failed: %s\n", data$msg))
    return(FALSE)
  }
}

# --- 3. WRAPPER Function für beide Orders ---
place_tp_sl <- function(symbol, side, size, tp_price, sl_price) {
  cat("\n🎯 Placing TP/SL orders for", symbol, "...\n")
  cat(sprintf("   Take Profit: %.6f -> %s\n", tp_price, format_price(tp_price, 4)))
  cat(sprintf("   Stop Loss: %.6f -> %s\n", sl_price, format_price(sl_price, 4)))
  
  # TP Order
  tp_success <- place_take_profit(symbol, side, size, tp_price)
  Sys.sleep(0.5)  # Kurze Pause zwischen Orders
  
  # SL Order
  sl_success <- place_stop_loss(symbol, side, size, sl_price)
  
  if (tp_success && sl_success) {
    cat("\n✅ Both TP and SL orders placed successfully!\n")
  } else {
    cat("\n⚠️ Some orders failed. Check the messages above.\n")
  }
  
  return(list(tp = tp_success, sl = sl_success))
}

# --- 4. Symbol-spezifische Dezimalstellen ---
get_price_decimals <- function(symbol) {
  # Definiert die erlaubten Dezimalstellen pro Symbol
  decimals_map <- list(
    "BTCUSDT_UMCBL" = 2,    # BTC: 2 Dezimalstellen
    "ETHUSDT_UMCBL" = 2,    # ETH: 2 Dezimalstellen
    "ADAUSDT_UMCBL" = 4,    # ADA: 4 Dezimalstellen
    "DOGEUSDT_UMCBL" = 5    # DOGE: 5 Dezimalstellen
  )
  
  if (symbol %in% names(decimals_map)) {
    return(decimals_map[[symbol]])
  } else {
    return(4)  # Standard: 4 Dezimalstellen
  }
}

# --- 5. ERWEITERTE Version mit automatischer Erkennung ---
place_tp_sl_auto <- function(symbol, side, size, tp_price, sl_price) {
  # Erkennt automatisch die richtige Anzahl Dezimalstellen
  decimals <- get_price_decimals(symbol)
  
  cat("\n🎯 Placing TP/SL orders for", symbol, "...\n")
  cat(sprintf("   Symbol requires %d decimal places\n", decimals))
  
  # Preise formatieren
  tp_formatted <- format_price(tp_price, decimals)
  sl_formatted <- format_price(sl_price, decimals)
  
  cat(sprintf("   Take Profit: %.6f -> %s\n", tp_price, tp_formatted))
  cat(sprintf("   Stop Loss: %.6f -> %s\n", sl_price, sl_formatted))
  
  # Orders platzieren (nutzt intern die formatierten Preise)
  place_tp_sl(symbol, side, size, tp_price, sl_price)
}

# --- BEISPIEL VERWENDUNG ---
# Mit Ihrer aktuellen ADA Position:
# place_tp_sl('ADAUSDT_UMCBL', 'long', '2000', 0.644202, 0.594167)

# Oder mit automatischer Dezimalstellen-Erkennung:
# place_tp_sl_auto('ADAUSDT_UMCBL', 'long', '2000', 0.644202, 0.594167)

# --- TEST: Preis-Formatierung ---
test_formatting <- function() {
  cat("\n📊 Testing price formatting:\n")
  cat("=====================================\n")
  
  test_prices <- c(0.644202, 0.594167, 1.23456789, 0.5)
  
  for (price in test_prices) {
    cat(sprintf("Original: %.8f -> Formatted: %s\n", 
                price, format_price(price, 4)))
  }
  
  cat("\n📊 Symbol-specific formatting:\n")
  cat("=====================================\n")
  
  symbols <- c("BTCUSDT_UMCBL", "ADAUSDT_UMCBL", "DOGEUSDT_UMCBL")
  test_price <- 1.23456789
  
  for (symbol in symbols) {
    decimals <- get_price_decimals(symbol)
    formatted <- format_price(test_price, decimals)
    cat(sprintf("%s (%d decimals): %s\n", symbol, decimals, formatted))
  }
}

# Test ausführen:
# test_formatting()

# --- FINALE LÖSUNG für Ihr Problem ---
# Verwenden Sie diese Funktion mit Ihrer ADA Position:
cat("\n🚀 Ready to place your ADA orders with correct formatting!\n")
cat("Run: place_tp_sl('ADAUSDT_UMCBL', 'long', '2000', 0.644202, 0.594167)\n")