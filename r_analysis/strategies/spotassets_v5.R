# Bitget API Client in R - Erweitert für Positionen
library(httr)
library(jsonlite)
library(openssl)

# Load environment variables (if .env file exists)
if (file.exists(".env")) {
  tryCatch({
    if (require(dotenv, quietly = TRUE)) {
      load_dot_env()
    }
  }, error = function(e) {
    cat("Warning: Could not load .env file\n")
  })
}

# API credentials - you can either use .env file or set them directly here
api_key <- Sys.getenv("BITGET_API_KEY")
api_secret <- Sys.getenv("BITGET_API_SECRET")
passphrase <- Sys.getenv("BITGET_PASSPHRASE")

# Alternative: Set credentials directly (uncomment and fill in your values)
# api_key <- "your_api_key_here"
# api_secret <- "your_api_secret_here"
# passphrase <- "your_passphrase_here"

# Check if all credentials are available
if (api_key == "" || api_secret == "" || passphrase == "") {
  cat("❌ Missing API credentials.\n")
  cat("Please either:\n")
  cat("1. Create a .env file with your credentials, or\n")
  cat("2. Set the credentials directly in the script, or\n")
  cat("3. Set environment variables before running R\n")
  cat("\nExample .env file content:\n")
  cat("BITGET_API_KEY=your_api_key\n")
  cat("BITGET_API_SECRET=your_api_secret\n")
  cat("BITGET_PASSPHRASE=your_passphrase\n")
  stop("Missing credentials")
}

base_url <- "https://api.bitget.com"

# Function to make authenticated requests to Bitget API
bitget_request <- function(request_path, method = "GET", query_dict = NULL) {
  tryCatch({
    # Handle query parameters
    query <- ""
    if (!is.null(query_dict) && length(query_dict) > 0) {
      # For POST requests, query_dict goes into the body, not URL query params
      if (toupper(method) == "GET") {
        query_parts <- paste(names(query_dict), query_dict, sep = "=")
        query <- paste(query_parts, collapse = "&")
      }
    }
    
    # Generate timestamp (Unix timestamp in milliseconds)
    timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
    
    # Create sign target
    sign_target <- request_path
    if (toupper(method) == "GET" && query != "") {
      sign_target <- paste0(request_path, "?", query)
    }
    
    # Prepare body for POST requests
    body_content <- ""
    if (toupper(method) == "POST" && !is.null(query_dict)) {
      body_content <- toJSON(query_dict, auto_unbox = TRUE)
    }
    
    # Create message for signature
    message <- paste0(timestamp, toupper(method), sign_target, body_content)
    
    # Generate HMAC-SHA256 signature using openssl
    signature_raw <- sha256(charToRaw(message), key = charToRaw(api_secret))
    signature <- base64_enc(signature_raw)
    
    # Create headers
    headers <- c(
      "ACCESS-KEY" = api_key,
      "ACCESS-SIGN" = signature,
      "ACCESS-TIMESTAMP" = timestamp,
      "ACCESS-PASSPHRASE" = passphrase,
      "Content-Type" = "application/json",
      "locale" = "en-US"
    )
    
    # Make request
    url <- paste0(base_url, request_path)
    
    if (toupper(method) == "GET") {
      response <- GET(
        url = url,
        add_headers(.headers = headers),
        query = query_dict, # Use query for GET requests
        timeout(10)
      )
    } else { # POST, DELETE, etc.
      response <- VERB(
        verb = method,
        url = url,
        add_headers(.headers = headers),
        body = body_content, # Use body for POST requests
        encode = "json", # Ensure body is sent as JSON
        timeout(10)
      )
    }
    
    # Check for HTTP errors
    if (http_error(response)) {
      cat(sprintf("❌ HTTP Error (%d): %s\n", status_code(response), content(response, "text")))
      return(NULL)
    }
    
    # Parse JSON response
    return(fromJSON(content(response, "text"), flatten = TRUE))
    
  }, error = function(e) {
    if (grepl("Timeout", e$message)) {
      cat("❌ Network timeout error\n")
    } else {
      cat(sprintf("❌ Unexpected error: %s\n", e$message))
    }
    return(NULL)
  })
}

# Function to fetch account assets
get_assets <- function() {
  cat("\n🔎 Fetching account assets...\n")
  
  data <- bitget_request("/api/mix/v1/account/accounts", query_dict = list(productType = "umcbl"))
  
  if (is.null(data)) {
    return()
  }
  
  if (data$code != "00000") {
    cat(sprintf("❌ API Error: %s\n", data$msg))
    return()
  }
  
  cat("\n🔹 Account Assets (USDT-M):\n")
  
  # Create formatted table
  assets_data <- data$data
  
  if (nrow(assets_data) > 0) {
    # Process the data frame (API returns data frame with string values)
    df <- data.frame(
      Coin = assets_data$marginCoin,
      Available = sprintf("%.4f", as.numeric(assets_data$available)),
      Locked = sprintf("%.4f", as.numeric(assets_data$locked)),
      Equity = sprintf("%.4f", as.numeric(assets_data$equity)),
      UnrealizedPL = sprintf("%.4f", as.numeric(assets_data$unrealizedPL)), # Fixed: assets$unrealizedPL to assets_data$unrealizedPL
      stringsAsFactors = FALSE
    )
    
    # Print formatted table
    cat(sprintf("%-10s %15s %15s %15s %15s\n", "Coin", "Available", "Locked", "Equity", "Unrealized PL"))
    cat(paste(rep("-", 75), collapse = ""), "\n")
    
    for (i in 1:nrow(df)) {
      cat(sprintf("%-10s %15s %15s %15s %15s\n", 
                  df$Coin[i], df$Available[i], df$Locked[i], df$Equity[i], df$UnrealizedPL[i]))
    }
    
    # Additional summary info
    cat("\n📊 Summary:\n")
    cat(sprintf("Total Equity: %.4f USDT\n", as.numeric(assets_data$equity)))
    cat(sprintf("Available Balance: %.4f USDT\n", as.numeric(assets_data$available)))
    cat(sprintf("Locked Balance: %.4f USDT\n", as.numeric(assets_data$locked)))
    cat(sprintf("Unrealized P&L: %.4f USDT\n", as.numeric(assets_data$unrealizedPL)))
  } else {
    cat("No asset data found.\n")
  }
}

# Function to fetch all positions
get_positions <- function() {
  cat("\n🔎 Fetching all positions...\n")
  
  data <- bitget_request("/api/mix/v1/position/allPosition", query_dict = list(productType = "umcbl"))
  
  if (is.null(data)) {
    return(NULL) # Return NULL to indicate failure
  }
  
  if (data$code != "00000") {
    cat(sprintf("❌ API Error: %s\n", data$msg))
    return(NULL) # Return NULL to indicate failure
  }
  
  if (length(data$data) == 0 || nrow(data$data) == 0) {
    cat("📭 No open positions found.\n")
    return(data.frame()) # Return an empty data frame if no positions
  }
  
  cat("\n📈 Open Positions:\n")
  cat(paste(rep("=", 120), collapse = ""), "\n")
  
  positions_data <- data$data
  
  for (i in 1:nrow(positions_data)) {
    pos <- positions_data[i, ]
    
    # Skip positions with zero size
    if (as.numeric(pos$total) == 0) {
      next
    }
    
    cat(sprintf("\n🔸 Position %d:\n", i))
    cat(sprintf("Symbol: %s\n", pos$symbol))
    cat(sprintf("Side: %s %s\n", 
                ifelse(pos$holdSide == "long", "🟢 LONG", "🔴 SHORT"), 
                pos$holdSide))
    cat(sprintf("Size: %s\n", pos$total))
    cat(sprintf("Available: %s\n", pos$available))
    cat(sprintf("Locked: %s\n", pos$locked))
    
    # Price information
    cat(sprintf("Average Price: %s\n", pos$averageOpenPrice))
    cat(sprintf("Mark Price: %s\n", pos$markPrice))
    cat(sprintf("Liquidation Price: %s\n", pos$liquidationPrice))
    
    # P&L information
    unrealized_pnl <- as.numeric(pos$unrealizedPL)
    pnl_color <- ifelse(unrealized_pnl >= 0, "🟢", "🔴")
    cat(sprintf("Unrealized P&L: %s %s %s\n", pnl_color, pos$unrealizedPL, pos$marginCoin))
    
    # ROE (Return on Equity)
    if (!is.null(pos$unrealizedPLR) && pos$unrealizedPLR != "") {
      roe_pct <- as.numeric(pos$unrealizedPLR) * 100
      roe_color <- ifelse(roe_pct >= 0, "🟢", "🔴")
      cat(sprintf("ROE: %s %.2f%%\n", roe_color, roe_pct))
    }
    
    # Margin information
    cat(sprintf("Margin: %s %s\n", pos$margin, pos$marginCoin))
    cat(sprintf("Margin Mode: %s\n", pos$marginMode))
    
    # Leverage
    cat(sprintf("Leverage: %sx\n", pos$leverage))
    
    # Maintenance margin
    if (!is.null(pos$keepMarginRate) && pos$keepMarginRate != "") {
      maint_margin_rate <- as.numeric(pos$keepMarginRate) * 100
      cat(sprintf("Maintenance Margin Rate: %.2f%%\n", maint_margin_rate))
    }
    
    cat(paste(rep("-", 60), collapse = ""), "\n")
  }
  
  # Summary
  total_unrealized <- sum(as.numeric(positions_data$unrealizedPL), na.rm = TRUE)
  total_margin <- sum(as.numeric(positions_data$margin), na.rm = TRUE)
  
  cat(sprintf("\n📊 Positions Summary:\n"))
  cat(sprintf("Total Open Positions: %d\n", sum(as.numeric(positions_data$total) > 0)))
  cat(sprintf("Total Unrealized P&L: %.4f USDT\n", total_unrealized))
  cat(sprintf("Total Margin Used: %.4f USDT\n", total_margin))
  
  return(positions_data) # Return the fetched data
}

# Function to get specific position by symbol
get_position_by_symbol <- function(symbol) {
  cat(sprintf("\n🔎 Fetching position for %s...\n", symbol))
  
  data <- bitget_request("/api/mix/v1/position/singlePosition", 
                         query_dict = list(symbol = symbol, marginCoin = "USDT"))
  
  if (is.null(data)) {
    return(NULL)
  }
  
  if (data$code != "00000") {
    cat(sprintf("❌ API Error: %s\n", data$msg))
    return(NULL)
  }
  
  if (length(data$data) == 0) {
    cat(sprintf("📭 No position found for %s.\n", symbol))
    return(NULL)
  }
  
  pos_data <- data$data
  
  cat(sprintf("\n📈 Position Details for %s:\n", symbol))
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  for (i in 1:nrow(pos_data)) {
    pos <- pos_data[i, ]
    
    # Skip if no position
    if (as.numeric(pos$total) == 0) {
      next
    }
    
    cat(sprintf("Side: %s\n", ifelse(pos$holdSide == "long", "🟢 LONG", "🔴 SHORT")))
    cat(sprintf("Size: %s\n", pos$total))
    cat(sprintf("Available: %s\n", pos$available))
    cat(sprintf("Average Price: %s\n", pos$averageOpenPrice))
    cat(sprintf("Mark Price: %s\n", pos$markPrice))
    cat(sprintf("Liquidation Price: %s\n", pos$liquidationPrice))
    
    unrealized_pnl <- as.numeric(pos$unrealizedPL)
    pnl_color <- ifelse(unrealized_pnl >= 0, "🟢", "🔴")
    cat(sprintf("Unrealized P&L: %s %s USDT\n", pnl_color, pos$unrealizedPL))
    
    if (!is.null(pos$unrealizedPLR) && pos$unrealizedPLR != "") {
      roe_pct <- as.numeric(pos$unrealizedPLR) * 100
      roe_color <- ifelse(roe_pct >= 0, "🟢", "🔴")
      cat(sprintf("ROE: %s %.2f%%\n", roe_color, roe_pct))
    }
    
    cat(sprintf("Margin: %s USDT\n", pos$margin))
    cat(sprintf("Leverage: %sx\n", pos$leverage))
  }
  return(pos_data) # Return the position data
}

# Function to get only positions with non-zero size
get_active_positions <- function() {
  cat("\n🔎 Fetching active positions only...\n")
  
  data <- bitget_request("/api/mix/v1/position/allPosition", query_dict = list(productType = "umcbl"))
  
  if (is.null(data) || data$code != "00000") {
    cat("❌ Failed to fetch positions\n")
    return(data.frame())
  }
  
  if (length(data$data) == 0) {
    cat("📭 No positions found.\n")
    return(data.frame())
  }
  
  # Filter active positions
  active_positions <- data$data[as.numeric(data$data$total) > 0, ]
  
  if (nrow(active_positions) == 0) {
    cat("📭 No active positions found.\n")
    return(data.frame())
  }
  
  cat(sprintf("\n📈 %d Active Position(s):\n", nrow(active_positions)))
  
  for (i in 1:nrow(active_positions)) {
    pos <- active_positions[i, ]
    
    side_emoji <- ifelse(pos$holdSide == "long", "🟢", "🔴")
    pnl_num <- as.numeric(pos$unrealizedPL)
    pnl_emoji <- ifelse(pnl_num >= 0, "🟢", "🔴")
    
    cat(sprintf("\n%d. %s | %s %s | Size: %s | PnL: %s %s USDT\n",
                i, pos$symbol, side_emoji, toupper(pos$holdSide), 
                pos$total, pnl_emoji, pos$unrealizedPL))
    cat(sprintf("   Avg Price: %s | Mark: %s | Margin: %s USDT\n",
                pos$averageOpenPrice, pos$markPrice, pos$margin))
  }
  return(active_positions)
}




# Fixed Take Profit and Stop Loss functions for Bitget API

# Function to place Take Profit order - FIXED VERSION
place_take_profit <- function(symbol, side, size, trigger_price, order_type = "market") {
  cat(sprintf("\n📈 Placing Take Profit order for %s...\n", symbol))
  
  # Build the request body with all required fields
  body <- list(
    symbol = symbol,
    marginCoin = "USDT",
    planType = "pos_profit",
    triggerPrice = as.character(trigger_price),
    holdSide = side,
    size = as.character(size),
    orderType = order_type,
    triggerType = "fill_price"  # REQUIRED: Added missing trigger type
  )
  
  # Add executePrice for limit orders
  if (order_type != "market") {
    body$executePrice <- as.character(trigger_price)
  }
  
  data <- bitget_request("/api/mix/v1/plan/placePlan", method = "POST", query_dict = body)
  
  if (is.null(data)) {
    return(FALSE)
  }
  
  if (data$code == "00000") {
    cat(sprintf("✅ Take Profit order placed successfully!\n"))
    cat(sprintf("   Order ID: %s\n", data$data$orderId))
    cat(sprintf("   Symbol: %s\n", symbol))
    cat(sprintf("   Side: %s\n", side))
    cat(sprintf("   Size: %s\n", size))
    cat(sprintf("   Trigger Price: %s USDT\n", trigger_price))
    return(TRUE)
  } else {
    cat(sprintf("❌ Failed to place Take Profit order: %s\n", data$msg))
    return(FALSE)
  }
}

# Function to place Stop Loss order - FIXED VERSION
place_stop_loss <- function(symbol, side, size, trigger_price, order_type = "market") {
  cat(sprintf("\n📉 Placing Stop Loss order for %s...\n", symbol))
  
  # Build the request body with all required fields
  body <- list(
    symbol = symbol,
    marginCoin = "USDT",
    planType = "pos_loss",
    triggerPrice = as.character(trigger_price),
    holdSide = side,
    size = as.character(size),
    orderType = order_type,
    triggerType = "fill_price"  # REQUIRED: Added missing trigger type
  )
  
  # Add executePrice for limit orders
  if (order_type != "market") {
    body$executePrice <- as.character(trigger_price)
  }
  
  data <- bitget_request("/api/mix/v1/plan/placePlan", method = "POST", query_dict = body)
  
  if (is.null(data)) {
    return(FALSE)
  }
  
  if (data$code == "00000") {
    cat(sprintf("✅ Stop Loss order placed successfully!\n"))
    cat(sprintf("   Order ID: %s\n", data$data$orderId))
    cat(sprintf("   Symbol: %s\n", symbol))
    cat(sprintf("   Side: %s\n", side))
    cat(sprintf("   Size: %s\n", size))
    cat(sprintf("   Trigger Price: %s USDT\n", trigger_price))
    return(TRUE)
  } else {
    cat(sprintf("❌ Failed to place Stop Loss order: %s\n", data$msg))
    return(FALSE)
  }
}

# Enhanced function to place both TP and SL with better error handling
place_tp_sl_orders_enhanced <- function(symbol, side, size, tp_price, sl_price) {
  cat(sprintf("\n🎯 Placing TP/SL orders for %s position...\n", symbol))
  cat(sprintf("Position: %s %s, Size: %s\n", toupper(side), symbol, size))
  cat(sprintf("Take Profit: %s USDT, Stop Loss: %s USDT\n", tp_price, sl_price))
  
  success_count <- 0
  
  # Validate prices based on position side
  current_price <- 0.667  # You can get this from position data
  
  if (side == "long") {
    if (tp_price <= current_price) {
      cat("⚠️ Warning: Take Profit price should be higher than current price for LONG position\n")
    }
    if (sl_price >= current_price) {
      cat("⚠️ Warning: Stop Loss price should be lower than current price for LONG position\n")
    }
  } else {
    if (tp_price >= current_price) {
      cat("⚠️ Warning: Take Profit price should be lower than current price for SHORT position\n")
    }
    if (sl_price <= current_price) {
      cat("⚠️ Warning: Stop Loss price should be higher than current price for SHORT position\n")
    }
  }
  
  # Place Take Profit order
  if (place_take_profit(symbol, side, size, tp_price)) {
    success_count <- success_count + 1
  }
  
  # Small delay between orders
  Sys.sleep(0.5)
  
  # Place Stop Loss order
  if (place_stop_loss(symbol, side, size, sl_price)) {
    success_count <- success_count + 1
  }
  
  if (success_count == 2) {
    cat(sprintf("\n✅ Both TP and SL orders placed successfully for %s!\n", symbol))
  } else if (success_count == 1) {
    cat(sprintf("\n⚠️ Only one order was placed successfully for %s\n", symbol))
  } else {
    cat(sprintf("\n❌ Failed to place both orders for %s\n", symbol))
  }
  
  return(success_count)
}

# Test function to verify the fix
test_tp_sl_placement <- function() {
  cat("\n🧪 Testing TP/SL order placement with fixed functions...\n")
  
  # Test with your actual position data
  symbol <- "ADAUSDT_UMCBL"
  side <- "long"
  size <- "30"
  tp_price <- 0.7
  sl_price <- 0.55
  
  cat("\nAttempting to place orders with corrected parameters:\n")
  place_tp_sl_orders_enhanced(symbol, side, size, tp_price, sl_price)
}

# Alternative function using different trigger types (if fill_price doesn't work)
place_take_profit_alt <- function(symbol, side, size, trigger_price, order_type = "market") {
  cat(sprintf("\n📈 Placing Take Profit order (Alternative) for %s...\n", symbol))
  
  body <- list(
    symbol = symbol,
    marginCoin = "USDT",
    planType = "pos_profit",
    triggerPrice = as.character(trigger_price),
    holdSide = side,
    size = as.character(size),
    orderType = order_type,
    triggerType = "mark_price"  # Alternative trigger type
  )
  
  if (order_type != "market") {
    body$executePrice <- as.character(trigger_price)
  }
  
  data <- bitget_request("/api/mix/v1/plan/placePlan", method = "POST", query_dict = body)
  
  if (is.null(data)) {
    return(FALSE)
  }
  
  if (data$code == "00000") {
    cat(sprintf("✅ Take Profit order placed successfully!\n"))
    return(TRUE)
  } else {
    cat(sprintf("❌ Failed to place Take Profit order: %s\n", data$msg))
    return(FALSE)
  }
}


# Test the fix
test_tp_sl_placement()

# Or use the enhanced function directly
place_tp_sl_orders_enhanced("ADAUSDT_UMCBL", "long", "10", 0.7, 0.50)


# Example usage function for demonstration purposes
example_tp_sl_orders <- function() {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("📋 Example: How to place TP/SL orders", "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  cat("\n💡 Examples based on your screenshot:\n")
  cat("\n# For your ADAUSDT Long position:\n")
  cat("# Take Profit at 0.7 USDT (49.63% profit)\n")
  cat("place_take_profit('ADAUSDT_UMCBL', 'long', '29', 0.7)\n") # Note: This example uses hardcoded '29' for demonstration
  cat("\n# Stop Loss at 0.55 USDT (-175.29% loss)\n")
  cat("place_stop_loss('ADAUSDT_UMCBL', 'long', '29', 0.55)\n") # Note: This example uses hardcoded '29' for demonstration
  cat("\n# Or place both at once:\n")
  cat("place_tp_sl_orders('ADAUSDT_UMCBL', 'long', '29', 0.7, 0.55)\n") # Note: This example uses hardcoded '29' for demonstration
  cat("\n# Check your plan orders:\n")
  cat("get_plan_orders()\n")
  cat("\n# Cancel a specific order:\n")
  cat("cancel_plan_order('order_id_here', 'ADAUSDT_UMCBL')\n")
}

# Run main function if script is executed interactively or not sourced
if (interactive() || !exists("sourced_script")) {
  main()
  example_tp_sl_orders()
}
