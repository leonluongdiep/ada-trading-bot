# ==========================================
# 🚀 Bitget Futures Data Dashboard (Refactored)
# ==========================================

# --- 1. Dependencies & Environment Setup ---
# Load required libraries
library(httr)
library(jsonlite)
library(openssl)

# Load .env if available
if (file.exists(".env")) {
  tryCatch({
    if (require(dotenv, quietly = TRUE)) load_dot_env()
  }, error = function(e) {
    warning("Could not load .env file: ", e$message)
  })
}

# --- 2. API Credentials ---
api_key    <- Sys.getenv("BITGET_API_KEY")
api_secret <- Sys.getenv("BITGET_API_SECRET")
passphrase <- Sys.getenv("BITGET_PASSPHRASE")

# Validate credentials
if (any(c(api_key, api_secret, passphrase) == "")) {
  stop(
    "Missing API credentials.\n" ,
    "Define them in .env or export as environment variables."
  )
}

# Base URL for Bitget
base_url <- "https://api.bitget.com"


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
# ==========================================
# 🔧 SPOT PRICE FIX - DEBUG VERSION
# ==========================================

# Enhanced price function with debugging
get_ticker_price_debug <- function(symbol) {
  cat(sprintf("🔍 Debugging price for: %s\n", symbol))
  
  tryCatch({
    # Try different endpoints and formats
    endpoints_to_try <- list(
      list(path = "/api/spot/v1/market/ticker", symbol = symbol),
      list(path = "/api/spot/v1/market/ticker", symbol = paste0(symbol, "_SPBL")),
      list(path = "/api/v2/spot/market/tickers", symbol = symbol),
      list(path = "/api/spot/v1/market/tickers", symbol = symbol)
    )
    
    for (endpoint in endpoints_to_try) {
      cat(sprintf("📡 Trying: %s with symbol: %s\n", endpoint$path, endpoint$symbol))
      
      res <- tryCatch({
        bitget_request(endpoint$path, "GET", list(symbol = endpoint$symbol))
      }, error = function(e) {
        cat(sprintf("❌ Error: %s\n", e$message))
        return(NULL)
      })
      
      if (!is.null(res)) {
        cat("✅ Response received, structure:\n")
        cat(sprintf("Code: %s\n", res$code))
        if (!is.null(res$data)) {
          cat("Data fields:", paste(names(res$data), collapse = ", "), "\n")
          
          if (res$code == "00000") {
            # Try different price fields
            price_fields <- c("close", "lastPr", "last", "price", "closePrice")
            for (field in price_fields) {
              if (field %in% names(res$data)) {
                price <- as.numeric(res$data[[field]])
                if (!is.na(price) && price > 0) {
                  cat(sprintf("✅ Found price in field '%s': %.6f\n", field, price))
                  return(price)
                }
              }
            }
          }
        }
      }
    }
    
    cat("❌ No valid price found\n")
    return(0)
    
  }, error = function(e) {
    cat(sprintf("❌ Critical error: %s\n", e$message))
    return(0)
  })
}

# Alternative: Use tickers endpoint (gets all at once)
get_all_spot_tickers <- function() {
  cat("\n📊 Fetching ALL spot tickers...\n")
  
  tryCatch({
    res <- bitget_request("/api/spot/v1/market/tickers", "GET")
    
    if (res$code == "00000") {
      cat(sprintf("✅ Received %d tickers\n", nrow(res$data)))
      
      # Show first few for debugging
      cat("\nFirst 5 tickers:\n")
      if (nrow(res$data) > 0) {
        head_data <- head(res$data, 5)
        for (i in 1:nrow(head_data)) {
          ticker <- head_data[i, ]
          cat(sprintf("%s: %s\n", ticker$symbol, ticker$close))
        }
      }
      
      return(res$data)
    }
    
  }, error = function(e) {
    cat(sprintf("❌ Error: %s\n", e$message))
    return(data.frame())
  })
}

# Find XRP price from all tickers
find_xrp_price <- function() {
  cat("\n🔍 Finding XRP price...\n")
  
  tickers <- get_all_spot_tickers()
  
  if (nrow(tickers) > 0) {
    # Look for XRP pairs
    xrp_tickers <- tickers[grepl("XRP", tickers$symbol, ignore.case = TRUE), ]
    
    if (nrow(xrp_tickers) > 0) {
      cat("\n💎 Found XRP pairs:\n")
      for (i in 1:nrow(xrp_tickers)) {
        ticker <- xrp_tickers[i, ]
        cat(sprintf("  %s: %s\n", ticker$symbol, ticker$close))
      }
      
      # Try to find XRPUSDT
      xrp_usdt <- xrp_tickers[grepl("USDT", xrp_tickers$symbol), ]
      if (nrow(xrp_usdt) > 0) {
        price <- as.numeric(xrp_usdt$close[1])
        cat(sprintf("✅ XRP/USDT Price: %.6f\n", price))
        return(price)
      }
    }
  }
  
  return(0)
}

# Enhanced portfolio calculator with better price detection
calculate_portfolio_value_fixed <- function() {
  cat("\n💰 PORTFOLIO VALUE CALCULATOR (FIXED)\n")
  cat(strrep("=", 50), "\n")
  
  total_value <- 0
  
  # Futures value (unchanged)
  futures <- bitget_request("/api/mix/v1/account/accounts", "GET", list(productType = "umcbl"))
  if (futures$code == "00000" && nrow(futures$data) > 0) {
    futures_equity <- as.numeric(futures$data$equity[1])
    total_value <- total_value + futures_equity
    cat(sprintf("🔮 Futures Equity: %.4f USDT\n", futures_equity))
  }
  
  # Get all tickers once for efficiency
  cat("\n📊 Getting spot prices...\n")
  all_tickers <- get_all_spot_tickers()
  
  # Spot values with corrected prices
  spot <- bitget_request("/api/spot/v1/account/assets", "GET")
  if (spot$code == "00000" && nrow(spot$data) > 0) {
    spot_value <- 0
    df <- spot$data[as.numeric(spot$data$available) > 0 | as.numeric(spot$data$frozen) > 0, ]
    
    for (i in seq_len(nrow(df))) {
      coin <- df$coinName[i]
      balance <- as.numeric(df$available[i]) + as.numeric(df$frozen[i])
      
      if (coin == "USDT" || coin == "USDC") {
        coin_value <- balance
      } else {
        # Find price in all_tickers
        coin_ticker <- all_tickers[grepl(paste0("^", coin, "USDT"), all_tickers$symbol), ]
        
        if (nrow(coin_ticker) > 0) {
          price <- as.numeric(coin_ticker$close[1])
          coin_value <- balance * price
          cat(sprintf("💎 %s: %.8f × %.6f = %.4f USDT\n", coin, balance, price, coin_value))
        } else {
          coin_value <- 0
          cat(sprintf("⚠️ %s: %.8f × (price not found) = 0.0000 USDT\n", coin, balance))
        }
      }
      
      spot_value <- spot_value + coin_value
    }
    
    total_value <- total_value + spot_value
    cat(sprintf("\n💎 Total Spot Value: %.4f USDT\n", spot_value))
  }
  
  cat(strrep("-", 50), "\n")
  cat(sprintf("🎯 TOTAL PORTFOLIO: %.4f USDT\n", total_value))
  
  return(total_value)
}