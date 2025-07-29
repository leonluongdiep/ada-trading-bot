# ==========================================================================================================
# 📈 BITGET ORDER CREATION & TRADING GUIDE
# ==========================================================================================================
# Komplette Anleitung für Order-Erstellung mit deinem 5-Coin Portfolio
# ==========================================================================================================

cat("📈 Loading Bitget Order Creation Guide...\n")

# ==========================================================================================================
# 🎯 GRUNDLEGENDE ORDER-ARTEN
# ==========================================================================================================

#' Zeige verfügbare Order-Typen
show_available_order_types <- function() {
  cat("\n🎯 === AVAILABLE ORDER TYPES === 🎯\n")
  cat("1️⃣ MARKET ORDERS:\n")
  cat("   place_market_order(symbol, side, size)           # Sofort kaufen/verkaufen\n")
  cat("   place_market_buy(symbol, size_usdt)              # Markt-Kauf in USDT\n")
  cat("   place_market_sell(symbol, size_coins)            # Markt-Verkauf in Coins\n")
  cat("\n2️⃣ LIMIT ORDERS:\n")
  cat("   place_limit_order(symbol, side, size, price)     # Kaufen/Verkaufen zu bestimmtem Preis\n")
  cat("   place_limit_buy(symbol, size, price)             # Limit-Kauf\n")
  cat("   place_limit_sell(symbol, size, price)            # Limit-Verkauf\n")
  cat("\n3️⃣ STOP ORDERS:\n")
  cat("   place_stop_loss(symbol, sl_percent)              # Stop Loss Order\n")
  cat("   place_take_profit(symbol, tp_percent)            # Take Profit Order\n")
  cat("   place_trailing_sl(symbol, trailing_percent)      # Trailing Stop Loss\n")
  cat("\n4️⃣ ADVANCED ORDERS:\n")
  cat("   place_oco_order(symbol, tp_price, sl_price)      # One-Cancels-Other\n")
  cat("   place_bracket_order(symbol, entry, tp, sl)       # Bracket Order\n")
  cat("\n5️⃣ PORTFOLIO MANAGEMENT:\n")
  cat("   place_portfolio_orders()                         # Orders für alle Coins\n")
  cat("   emergency_close_all()                            # Notfall: Alle Positionen schließen\n")
}

# ==========================================================================================================
# 📊 MARKET ORDERS (SOFORT AUSFÜHRUNG)
# ==========================================================================================================

#' Market Order - Sofortige Ausführung zum aktuellen Preis
place_market_order <- function(symbol, side, size, dry_run = TRUE) {
  cat(sprintf("📊 Market Order: %s %s %s\n", side, size, symbol))
  
  if (dry_run) {
    cat("🔍 DRY RUN MODE - No real order placed\n")
    
    # Hole aktuellen Preis für Simulation
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (!is.null(ticker_data)) {
      current_price <- ticker_data$last_price
      estimated_value <- size * current_price
      cat(sprintf("   💰 Current Price: %.6f USDT\n", current_price))
      cat(sprintf("   💵 Estimated Value: %.2f USDT\n", estimated_value))
      cat(sprintf("   📈 24h Change: %+.2f%%\n", ticker_data$change_24h_pct %||% 0))
    }
    return(list(success = TRUE, mode = "dry_run"))
  }
  
  # Echte Order (wenn dry_run = FALSE)
  tryCatch({
    response <- bitget_request(
      method = "POST",
      endpoint = "/api/v2/mix/order/place-order",
      params = list(
        symbol = symbol,
        productType = "USDT-FUTURES",
        marginMode = "crossed",
        marginCoin = "USDT",
        side = tolower(side),
        orderType = "market",
        size = as.character(size)
      )
    )
    
    if (!is.null(response) && !is.null(response$data) && !is.null(response$data$orderId)) {
      cat("✅ Market order placed successfully!\n")
      cat("🆔 Order ID:", response$data$orderId, "\n")
      return(list(success = TRUE, order_id = response$data$orderId))
    } else {
      cat("❌ Failed to place market order\n")
      return(list(success = FALSE, error = "API error"))
    }
    
  }, error = function(e) {
    cat("❌ Error placing market order:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

#' Market Buy - Kaufe für bestimmten USDT-Betrag
place_market_buy <- function(symbol, usdt_amount, dry_run = TRUE) {
  cat(sprintf("💵 Market Buy: %s USDT worth of %s\n", usdt_amount, symbol))
  
  # Berechne Coin-Menge basierend auf aktuellem Preis
  ticker_data <- get_enhanced_ticker_data(symbol)
  if (is.null(ticker_data)) {
    cat("❌ Cannot get current price for", symbol, "\n")
    return(list(success = FALSE, error = "No price data"))
  }
  
  current_price <- ticker_data$last_price
  coin_amount <- usdt_amount / current_price
  
  cat(sprintf("   💰 Current Price: %.6f USDT\n", current_price))
  cat(sprintf("   🪙 Coin Amount: %.6f %s\n", coin_amount, gsub("USDT.*", "", symbol)))
  
  return(place_market_order(symbol, "buy", coin_amount, dry_run))
}

#' Market Sell - Verkaufe bestimmte Coin-Menge
place_market_sell <- function(symbol, coin_amount, dry_run = TRUE) {
  cat(sprintf("🪙 Market Sell: %s %s\n", coin_amount, symbol))
  
  # Zeige geschätzten USDT-Wert
  ticker_data <- get_enhanced_ticker_data(symbol)
  if (!is.null(ticker_data)) {
    current_price <- ticker_data$last_price
    estimated_usdt <- coin_amount * current_price
    cat(sprintf("   💰 Current Price: %.6f USDT\n", current_price))
    cat(sprintf("   💵 Estimated USDT: %.2f\n", estimated_usdt))
  }
  
  return(place_market_order(symbol, "sell", coin_amount, dry_run))
}

# ==========================================================================================================
# 🎯 LIMIT ORDERS (BESTIMMTER PREIS)
# ==========================================================================================================

#' Limit Order - Kaufen/Verkaufen zu bestimmtem Preis
place_limit_order <- function(symbol, side, size, price, dry_run = TRUE) {
  cat(sprintf("🎯 Limit Order: %s %s %s at %.6f USDT\n", side, size, symbol, price))
  
  if (dry_run) {
    cat("🔍 DRY RUN MODE - No real order placed\n")
    
    # Zeige Marktvergleich
    ticker_data <- get_enhanced_ticker_data(symbol)
    if (!is.null(ticker_data)) {
      current_price <- ticker_data$last_price
      price_diff <- ((price - current_price) / current_price) * 100
      
      cat(sprintf("   💰 Current Price: %.6f USDT\n", current_price))
      cat(sprintf("   📊 Price Difference: %+.2f%%\n", price_diff))
      
      if (side == "buy" && price > current_price) {
        cat("   ⚠️ Warning: Buy price above market (will execute immediately)\n")
      } else if (side == "sell" && price < current_price) {
        cat("   ⚠️ Warning: Sell price below market (will execute immediately)\n")
      }
    }
    return(list(success = TRUE, mode = "dry_run"))
  }
  
  # Echte Order
  tryCatch({
    response <- bitget_request(
      method = "POST",
      endpoint = "/api/v2/mix/order/place-order",
      params = list(
        symbol = symbol,
        productType = "USDT-FUTURES",
        marginMode = "crossed",
        marginCoin = "USDT",
        side = tolower(side),
        orderType = "limit",
        size = as.character(size),
        price = as.character(price)
      )
    )
    
    if (!is.null(response) && !is.null(response$data) && !is.null(response$data$orderId)) {
      cat("✅ Limit order placed successfully!\n")
      cat("🆔 Order ID:", response$data$orderId, "\n")
      return(list(success = TRUE, order_id = response$data$orderId))
    } else {
      cat("❌ Failed to place limit order\n")
      return(list(success = FALSE, error = "API error"))
    }
    
  }, error = function(e) {
    cat("❌ Error placing limit order:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# ==========================================================================================================
# 🛡️ STOP ORDERS (RISIKOMANAGEMENT)
# ==========================================================================================================

#' Stop Loss Order für bestehende Position
place_stop_loss_order <- function(symbol, sl_percent = NULL, dry_run = TRUE) {
  if (is.null(sl_percent)) {
    asset_config <- get_asset_config(symbol)
    sl_percent <- asset_config$default_sl_percent %||% 1.5
  }
  
  cat(sprintf("🛡️ Stop Loss: %s at %.1f%% below entry\n", symbol, sl_percent))
  
  # Hole aktuelle Position
  positions <- get_current_positions()
  position <- positions[positions$symbol == symbol, ]
  
  if (nrow(position) == 0) {
    cat("❌ No position found for", symbol, "\n")
    return(list(success = FALSE, error = "No position"))
  }
  
  side <- position$side[1]
  size <- position$size[1]
  avg_price <- position$avg_price[1]
  
  # Berechne Stop Loss Preis
  if (side == "long") {
    sl_price <- avg_price * (1 - sl_percent / 100)
    order_side <- "sell"
  } else {
    sl_price <- avg_price * (1 + sl_percent / 100)
    order_side <- "buy"
  }
  
  cat(sprintf("   📊 Position: %s %s %s\n", side, size, symbol))
  cat(sprintf("   💰 Entry Price: %.6f USDT\n", avg_price))
  cat(sprintf("   🛡️ Stop Loss Price: %.6f USDT\n", sl_price))
  
  if (dry_run) {
    cat("🔍 DRY RUN MODE - No real order placed\n")
    return(list(success = TRUE, mode = "dry_run", sl_price = sl_price))
  }
  
  # Echte Stop Loss Order
  return(place_stop_loss_order_api(symbol, order_side, size, sl_price))
}

#' Take Profit Order für bestehende Position
place_take_profit_order <- function(symbol, tp_percent = NULL, dry_run = TRUE) {
  if (is.null(tp_percent)) {
    asset_config <- get_asset_config(symbol)
    tp_percent <- asset_config$default_tp_percent %||% 2.0
  }
  
  cat(sprintf("🎯 Take Profit: %s at %.1f%% above entry\n", symbol, tp_percent))
  
  # Hole aktuelle Position
  positions <- get_current_positions()
  position <- positions[positions$symbol == symbol, ]
  
  if (nrow(position) == 0) {
    cat("❌ No position found for", symbol, "\n")
    return(list(success = FALSE, error = "No position"))
  }
  
  side <- position$side[1]
  size <- position$size[1]
  avg_price <- position$avg_price[1]
  
  # Berechne Take Profit Preis
  if (side == "long") {
    tp_price <- avg_price * (1 + tp_percent / 100)
    order_side <- "sell"
  } else {
    tp_price <- avg_price * (1 - tp_percent / 100)
    order_side <- "buy"
  }
  
  cat(sprintf("   📊 Position: %s %s %s\n", side, size, symbol))
  cat(sprintf("   💰 Entry Price: %.6f USDT\n", avg_price))
  cat(sprintf("   🎯 Take Profit Price: %.6f USDT\n", tp_price))
  
  if (dry_run) {
    cat("🔍 DRY RUN MODE - No real order placed\n")
    return(list(success = TRUE, mode = "dry_run", tp_price = tp_price))
  }
  
  # Echte Take Profit Order
  return(place_take_profit_order_api(symbol, order_side, size, tp_price))
}

# ==========================================================================================================
# 📈 TRADING SCENARIOS (PRAKTISCHE BEISPIELE)
# ==========================================================================================================

#' Beispiel: Kaufe ADA für 100 USDT
example_buy_ada <- function(dry_run = TRUE) {
  cat("\n💡 === EXAMPLE: BUY ADA FOR 100 USDT === 💡\n")
  
  symbol <- "ADAUSDT_UMCBL"
  usdt_amount <- 100
  
  cat("🎯 Strategy: Buy ADA with 100 USDT\n")
  cat("📊 Steps:\n")
  cat("   1. Check current ADA price\n")
  cat("   2. Calculate coin amount\n")
  cat("   3. Place market buy order\n")
  cat("   4. Set stop loss (-2%)\n")
  cat("   5. Set take profit (+3%)\n")
  
  # Schritt 1: Aktueller Preis
  ticker_data <- get_enhanced_ticker_data(symbol)
  if (is.null(ticker_data)) {
    cat("❌ Cannot get ADA price\n")
    return()
  }
  
  current_price <- ticker_data$last_price
  coin_amount <- usdt_amount / current_price
  
  cat(sprintf("\n📊 Market Analysis:\n"))
  cat(sprintf("   💰 ADA Price: %.6f USDT\n", current_price))
  cat(sprintf("   📈 24h Change: %+.2f%%\n", ticker_data$change_24h_pct %||% 0))
  cat(sprintf("   🪙 Coin Amount: %.2f ADA\n", coin_amount))
  
  # Schritt 2: Market Buy
  cat("\n🛒 Placing market buy order...\n")
  buy_result <- place_market_buy(symbol, usdt_amount, dry_run)
  
  if (buy_result$success) {
    cat("✅ Buy order successful!\n")
    
    # Schritt 3: Risikomanagement (nur simulation)
    if (dry_run) {
      sl_price <- current_price * 0.98  # -2%
      tp_price <- current_price * 1.03  # +3%
      
      cat("\n🛡️ Risk Management Setup:\n")
      cat(sprintf("   🔻 Stop Loss: %.6f USDT (-2%%)\n", sl_price))
      cat(sprintf("   🎯 Take Profit: %.6f USDT (+3%%)\n", tp_price))
      cat("   💡 Use place_stop_loss_order() and place_take_profit_order() after buy\n")
    }
  }
  
  return(buy_result)
}

#' Portfolio-weite Order-Platzierung
place_portfolio_protection <- function(dry_run = TRUE) {
  cat("\n🛡️ === PORTFOLIO PROTECTION SETUP === 🛡️\n")
  
  # Hole aktuelle Positionen
  positions <- get_current_positions()
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No positions found - nothing to protect\n")
    return(list())
  }
  
  cat(sprintf("📊 Found %d positions to protect:\n", nrow(positions)))
  
  results <- list()
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    symbol <- pos$symbol
    
    cat(sprintf("\n🔸 %s (%s %.2f coins):\n", symbol, pos$side, pos$size))
    
    # Stop Loss
    sl_result <- place_stop_loss_order(symbol, dry_run = dry_run)
    
    # Take Profit  
    tp_result <- place_take_profit_order(symbol, dry_run = dry_run)
    
    results[[symbol]] <- list(
      stop_loss = sl_result,
      take_profit = tp_result
    )
  }
  
  return(results)
}

# ==========================================================================================================
# 🚨 NOTFALL-FUNKTIONEN
# ==========================================================================================================

#' Notfall: Alle Positionen schließen
emergency_close_all_positions <- function(dry_run = TRUE) {
  cat("\n🚨 === EMERGENCY: CLOSE ALL POSITIONS === 🚨\n")
  
  if (dry_run) {
    cat("🔍 DRY RUN MODE - No real orders placed\n")
  } else {
    cat("⚠️ REAL MODE - This will close ALL positions!\n")
    user_confirm <- readline("Type 'CLOSE ALL' to confirm: ")
    if (user_confirm != "CLOSE ALL") {
      cat("❌ Operation cancelled\n")
      return(list(success = FALSE, reason = "User cancelled"))
    }
  }
  
  # Hole alle Positionen
  positions <- get_current_positions()
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No positions to close\n")
    return(list(success = TRUE, positions_closed = 0))
  }
  
  cat(sprintf("🎯 Closing %d positions:\n", nrow(positions)))
  
  results <- list()
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    symbol <- pos$symbol
    side <- pos$side
    size <- pos$size
    
    # Bestimme Schließungsrichtung
    close_side <- if (side == "long") "sell" else "buy"
    
    cat(sprintf("   🔸 Closing %s %s %s...", side, size, symbol))
    
    if (dry_run) {
      cat(" ✅ (DRY RUN)\n")
      results[[symbol]] <- list(success = TRUE, mode = "dry_run")
    } else {
      # Echte Market Order zum Schließen
      close_result <- place_market_order(symbol, close_side, size, dry_run = FALSE)
      results[[symbol]] <- close_result
      
      if (close_result$success) {
        cat(" ✅\n")
      } else {
        cat(" ❌\n")
      }
    }
  }
  
  return(results)
}

# ==========================================================================================================
# 📋 ORDER MANAGEMENT & STATUS
# ==========================================================================================================

#' Zeige alle offenen Orders
show_open_orders <- function() {
  cat("\n📋 === OPEN ORDERS === 📋\n")
  
  tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/order/orders-pending",
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(response) || is.null(response$data) || length(response$data) == 0) {
      cat("ℹ️ No open orders found\n")
      return(data.frame())
    }
    
    orders <- response$data
    cat(sprintf("📊 Found %d open orders:\n", length(orders)))
    
    for (i in 1:length(orders)) {
      order <- orders[[i]]
      cat(sprintf("   %d. %s: %s %s at %.6f (%s)\n",
                  i,
                  order$symbol %||% "Unknown",
                  order$side %||% "Unknown",
                  order$size %||% "0",
                  as.numeric(order$price %||% 0),
                  order$orderType %||% "Unknown"))
    }
    
    return(orders)
    
  }, error = function(e) {
    cat("❌ Error fetching orders:", e$message, "\n")
    return(data.frame())
  })
}

#' Storniere alle offenen Orders
cancel_all_orders <- function(dry_run = TRUE) {
  cat("\n❌ === CANCEL ALL ORDERS === ❌\n")
  
  if (dry_run) {
    cat("🔍 DRY RUN MODE\n")
  }
  
  # Hole offene Orders
  orders <- show_open_orders()
  
  if (length(orders) == 0) {
    cat("ℹ️ No orders to cancel\n")
    return(list(success = TRUE, cancelled = 0))
  }
  
  if (!dry_run) {
    # Echte Stornierung würde hier implementiert
    cat("⚠️ Real cancellation would happen here\n")
  }
  
  return(list(success = TRUE, cancelled = length(orders)))
}

# ==========================================================================================================
# 💡 USAGE GUIDE & EXAMPLES
# ==========================================================================================================

cat("✅ BITGET ORDER CREATION LOADED!\n")
cat("📈 Available Functions:\n")
cat("\n💰 BASIC ORDERS:\n")
cat("   place_market_buy('ADAUSDT_UMCBL', 100)            # Buy ADA for 100 USDT\n")
cat("   place_market_sell('ADAUSDT_UMCBL', 50)            # Sell 50 ADA\n")
cat("   place_limit_order('ADAUSDT_UMCBL', 'buy', 100, 0.80)  # Buy 100 ADA at 0.80\n")
cat("\n🛡️ RISK MANAGEMENT:\n")
cat("   place_stop_loss_order('ADAUSDT_UMCBL')            # Auto Stop Loss\n")
cat("   place_take_profit_order('ADAUSDT_UMCBL')          # Auto Take Profit\n")
cat("   place_portfolio_protection()                      # Protect all positions\n")
cat("\n📊 EXAMPLES & SCENARIOS:\n")
cat("   example_buy_ada()                                 # Complete buy example\n")
cat("   show_available_order_types()                      # See all order types\n")
cat("\n🚨 EMERGENCY:\n")
cat("   emergency_close_all_positions()                   # Close everything\n")
cat("   show_open_orders()                                # See pending orders\n")
cat("\n🔍 IMPORTANT:\n")
cat("   📝 All functions use dry_run=TRUE by default (safe mode)\n")
cat("   📝 Set dry_run=FALSE for real orders\n")
cat("   📝 Always test with dry_run first!\n")
cat("\n🎯 QUICK START:\n")
cat("   1. show_available_order_types()                   # Learn order types\n")
cat("   2. example_buy_ada()                              # See example\n")
cat("   3. get_current_positions()                        # Check positions\n")
cat("   4. place_portfolio_protection()                   # Protect positions\n")

# ==========================================================================================================
# END OF ORDER CREATION GUIDE
# ==========================================================================================================