# ==========================================
# 🚀 Bitget Futures Data Dashboard (Fixed)
# ==========================================

# --- 1. Dependencies & Environment Setup ---
# Load required libraries
library(httr)
library(jsonlite)
library(openssl)

install.packages("jsonlite")


# --- 2. API Credentials (Direct Setup) ---
# Since .env loading has issues, set credentials directly
api_key    <- "bg_d563d86a3169b96e58b54f675f9a6514"
api_secret <- "aa079302da6e88419bac856c3d3d0465b2c9db2a48e76fef59ecb40c780614e7"
passphrase <- "freeding"

# Validate credentials
if (any(c(api_key, api_secret, passphrase) == "")) {
  stop("Missing API credentials.")
}

# Base URL for Bitget
base_url <- "https://api.bitget.com"

cat("🔐 API Credentials loaded successfully\n")
cat("🌐 Base URL:", base_url, "\n")

# --- 3. Core HTTP Helper ---
# Make authenticated requests to Bitget API
bitget_request <- function(path, method = "GET", params = NULL) {
  # Prepare timestamp
  ts <- as.character(round(as.numeric(Sys.time()) * 1000))
  
  # Construct prehash string
  query_str <- if (!is.null(params) && toupper(method) == "GET") {
    paste0("?", paste(names(params), params, sep = "=", collapse = "&"))
  } else ""
  prehash <- paste0(ts, toupper(method), path, query_str)

  # Body for POST
  body_json <- if (toupper(method) == "POST" && !is.null(params))
    toJSON(params, auto_unbox = TRUE) else ""

  # Sign with HMAC-SHA256
  sig_raw <- sha256(charToRaw(paste0(prehash, body_json)), key = charToRaw(api_secret))
  signature <- base64_enc(sig_raw)

  # Headers
  headers <- c(
    "ACCESS-KEY"       = api_key,
    "ACCESS-SIGN"      = signature,
    "ACCESS-TIMESTAMP" = ts,
    "ACCESS-PASSPHRASE"= passphrase,
    "Content-Type"     = "application/json"
  )

  # Perform request
  url <- paste0(base_url, path)
  resp <- if (toupper(method) == "GET") {
    GET(url, add_headers(.headers = headers), query = params, timeout(10))
  } else {
    VERB(method, url, add_headers(.headers = headers), body = body_json,
         encode = "json", timeout(10))
  }

  # Error handling
  if (http_error(resp)) {
    stop(sprintf("HTTP %s: %s", status_code(resp), content(resp, "text")))
  }

  fromJSON(content(resp, "text"), flatten = TRUE)
}

# --- 4. Asset Functions ---
# Fetch and display account assets
get_assets <- function() {
  cat("\n🔎 Fetching account assets...\n")
  res <- bitget_request("/api/mix/v1/account/accounts", "GET",
                         list(productType = "umcbl"))
  if (res$code != "00000") stop(res$msg)

  df <- res$data
  if (nrow(df) == 0) {
    cat("No assets found.\n")
    return()
  }

  # Print table header
  cat(sprintf("%-10s %-12s %-12s %-12s %-15s\n", 
              "Coin", "Available", "Locked", "Equity", "Unrealized PL"))
  cat(strrep("-", 65), "\n")

  # Iterate and print
  df[] <- lapply(df, as.character)
  apply(df, 1, function(row) {
    cat(sprintf("%-10s %-12s %-12s %-12s %-15s\n", row["marginCoin"],
                sprintf("%.4f", as.numeric(row["available"])),
                sprintf("%.4f", as.numeric(row["locked"])),
                sprintf("%.4f", as.numeric(row["equity"])),
                sprintf("%.4f", as.numeric(row["unrealizedPL"]) ) ) )
  })

  # Summary
  cat("\n📊 Summary:\n")
  cat(sprintf("Total Equity: %.4f USDT\n", as.numeric(df$equity[1])))
  cat(sprintf("Unrealized P&L: %.4f USDT\n", as.numeric(df$unrealizedPL[1])))
}

# --- 5. Position Functions ---
# Fetch and print all positions
get_positions <- function() {
  cat("\n🔎 Fetching all positions...\n")
  res <- bitget_request("/api/mix/v1/position/allPosition", "GET",
                         list(productType = "umcbl"))
  if (res$code != "00000") stop(res$msg)
  df <- res$data

  if (nrow(df) == 0) {
    cat("No open positions.\n")
    return(data.frame())
  }

  df[] <- lapply(df, as.character)
  for (i in seq_len(nrow(df))) {
    pos <- df[i, ]
    if (as.numeric(pos$total) == 0) next

    cat(sprintf("\n🔸 %s | %s | Size: %s | Avg Price: %s | PnL: %s\n",
                pos$symbol,
                ifelse(pos$holdSide=="long","🟢 LONG","🔴 SHORT"),
                pos$total,
                pos$averageOpenPrice,
                pos$unrealizedPL))
  }
  invisible(df)
}

# --- 6. Order Placement Functions ---
# Place Take Profit / Stop Loss plan orders
place_plan_order <- function(symbol, side, size, price, planType) {
  trade_side <- if (side == "long") "close_long" else "close_short"
  
  # Format price to 4 decimal places (critical for Bitget API)
  formatted_price <- sprintf("%.4f", price)
  
  body <- list(
    symbol     = symbol,
    marginCoin = "USDT",
    planType   = planType,
    triggerPrice= formatted_price,
    holdSide   = side,
    side       = trade_side,
    size       = as.character(size),
    orderType  = "market",
    triggerType= "fill_price"
  )

  cat(sprintf("📡 Placing %s order: %s at %s\n", 
              ifelse(planType=="pos_profit","TP","SL"), symbol, formatted_price))

  res <- bitget_request("/api/mix/v1/plan/placePlan", "POST", body)
  if (res$code != "00000") {
    cat(sprintf("❌ Order failed: %s\n", res$msg))
    return(FALSE)
  }
  cat(sprintf("✅ %s order %s placed (ID: %s)\n", 
              ifelse(planType=="pos_profit","TP","SL"), symbol, res$data$orderId))
  TRUE
}

# High-level wrapper to place both TP & SL
place_tp_sl <- function(symbol, side, size, tp, sl) {
  cat("\n🎯 Placing TP/SL for", symbol, "...\n")
  cat(sprintf("Position: %s %s, Size: %s\n", toupper(side), symbol, size))
  cat(sprintf("Take Profit: %.6f USDT\n", tp))
  cat(sprintf("Stop Loss: %.6f USDT\n", sl))
  
  ok1 <- place_plan_order(symbol, side, size, tp, "pos_profit")
  Sys.sleep(0.5)
  ok2 <- place_plan_order(symbol, side, size, sl, "pos_loss")

  if (all(c(ok1, ok2))) {
    cat("✅ Both TP and SL orders placed successfully!\n")
  } else {
    cat("⚠️ Some orders failed. Check messages above.\n")
  }
}

# --- 7. Get Plan Orders (TP/SL) ---
get_plan_orders <- function(symbol = NULL) {
  cat("\n📋 Fetching plan orders (TP/SL)...\n")
  
  query_params <- list(productType = "umcbl")
  if (!is.null(symbol)) {
    query_params$symbol <- symbol
  }
  
  res <- bitget_request("/api/mix/v1/plan/currentPlan", "GET", query_params)
  
  if (res$code != "00000") {
    cat(sprintf("❌ API Error: %s\n", res$msg))
    return()
  }
  
  if (length(res$data) == 0) {
    cat("📭 No active plan orders found.\n")
    return()
  }
  
  orders_data <- res$data
  
  for (i in 1:nrow(orders_data)) {
    order <- orders_data[i, ]
    
    order_type_display <- switch(order$planType,
                                 "pos_profit" = "🟢 Take Profit",
                                 "pos_loss" = "🔴 Stop Loss",
                                 order$planType)
    
    cat(sprintf("\n📄 %s: %s\n", order_type_display, order$symbol))
    cat(sprintf("   Size: %s | Trigger: %s USDT | Status: %s\n",
                order$size, order$triggerPrice, order$state))
    cat(sprintf("   Order ID: %s\n", order$orderId))
  }
}

# --- 8. Price Formatting Test ---
test_formatting <- function() {
  cat("\n📊 Testing price formatting:\n")
  cat("=====================================\n")
  
  test_prices <- c(0.644202, 0.594167, 1.23456789, 0.5)
  
  for (price in test_prices) {
    formatted <- sprintf("%.4f", price)
    cat(sprintf("Original: %.8f -> Formatted: %s\n", price, formatted))
  }
}

# --- 9. Main Dashboard ---
main <- function() {
  cat(strrep("=",40),"\n")
  cat("🚀 Bitget Futures Dashboard\n")
  cat(strrep("=",40),"\n")

  start <- Sys.time()
  get_assets()
  df_pos <- get_positions()

  cat(sprintf("\nCompleted in %.2f sec\n", as.numeric(Sys.time()-start)))
  
  return(df_pos)
}

# --- 10. Initialize ---
cat("🚀 Bitget API Script loaded successfully!\n")
cat("📊 Available functions:\n")
cat("   - get_assets(): Check account balance\n")
cat("   - get_positions(): Check open positions\n") 
cat("   - get_plan_orders(): Check TP/SL orders\n")
cat("   - place_tp_sl(symbol, side, size, tp, sl): Place TP/SL orders\n")
cat("   - test_formatting(): Test price formatting\n")
cat("   - main(): Run complete dashboard\n")

# Auto-run if desired
if (interactive()) {
  cat("\n🔧 Running test formatting...\n")
  test_formatting()
  
  cat("\n💰 Ready for your ADA position management!\n")
  cat("🎯 Your calculated TP/SL values:\n")
  cat("   Take Profit: 0.644202 USDT\n")
  cat("   Stop Loss: 0.594167 USDT\n")
  cat("\n⚠️ TO PLACE LIVE ORDERS:\n")
  cat("place_tp_sl('ADAUSDT_UMCBL', 'long', '2000', 0.644202, 0.594167)\n")
}