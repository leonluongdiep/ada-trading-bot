# ------------ Paket-Installation für R 4.5.0 -----------#
version

cat('📦 Installing R packages for Bitget API...\n')
cat('R Version:', R.version.string, '\n')

# Essential packages for Bitget API
packages <- c('httr', 'jsonlite', 'openssl')

for(pkg in packages) {
  cat('Installing', pkg, '...\n')
  install.packages(pkg, repos='https://cran.r-project.org')
  cat('✅', pkg, 'installed\n')
}

cat('🎉 All packages installed successfully!\n')
"

# Verify installation
Rscript -e "
cat('=== VERIFYING INSTALLATION ===\n')
packages <- c('httr', 'jsonlite', 'openssl')

for(pkg in packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat('✅', pkg, ': OK\n')
  }, error = function(e) {
    cat('❌', pkg, ': FAILED\n')
  })
}

cat('=== VERIFICATION COMPLETE ===\n')



#---------------------------- Bitget API Script testen-----------------------------------------------#


# === TEST BITGET API CONNECTION ===

# Load required libraries
library(httr)
library(jsonlite)
library(openssl)

# API credentials (from your documentation)
api_key <- "bg_d563d86a3169b96e58b54f675f9a6514"
api_secret <- "aa079302da6e88419bac856c3d3d0465b2c9db2a48e76fef59ecb40c780614e7"
passphrase <- "freeding"
base_url <- "https://api.bitget.com"

cat("🔐 API credentials loaded\n")
cat("🌐 Base URL:", base_url, "\n")

# Test basic timestamp generation (critical for Bitget API)
timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
cat("⏰ Timestamp:", timestamp, "\n")

# Test HMAC-SHA256 signature (core Bitget requirement)
test_message <- "test"
signature_raw <- sha256(charToRaw(test_message), key = charToRaw(api_secret))
signature <- base64_enc(signature_raw)
cat("🔐 Signature test: OK\n")

cat("✅ Basic Bitget API setup ready!\n")


#------------------------------ Live Bitget API Connection------------------------------------------#


# === BITGET API REQUEST FUNCTION ===

bitget_request <- function(path, method = "GET", params = NULL) {
  # Generate timestamp
  ts <- as.character(round(as.numeric(Sys.time()) * 1000))
  
  # Build query string for GET requests
  query_str <- if (!is.null(params) && toupper(method) == "GET") {
    paste0("?", paste(names(params), params, sep = "=", collapse = "&"))
  } else ""
  
  # Create prehash string
  prehash <- paste0(ts, toupper(method), path, query_str)
  
  # Body for POST requests
  body_json <- if (toupper(method) == "POST" && !is.null(params)) {
    toJSON(params, auto_unbox = TRUE)
  } else ""
  
  # Generate signature
  sig_raw <- sha256(charToRaw(paste0(prehash, body_json)), key = charToRaw(api_secret))
  signature <- base64_enc(sig_raw)
  
  # Headers
  headers <- c(
    "ACCESS-KEY" = api_key,
    "ACCESS-SIGN" = signature,
    "ACCESS-TIMESTAMP" = ts,
    "ACCESS-PASSPHRASE" = passphrase,
    "Content-Type" = "application/json"
  )
  
  # Make request
  url <- paste0(base_url, path)
  
  if (toupper(method) == "GET") {
    response <- GET(url, add_headers(.headers = headers), query = params, timeout(10))
  } else {
    response <- VERB(method, url, add_headers(.headers = headers), 
                     body = body_json, encode = "json", timeout(10))
  }
  
  # Check for errors
  if (http_error(response)) {
    stop(sprintf("HTTP %s: %s", status_code(response), content(response, "text")))
  }
  
  # Parse JSON response
  fromJSON(content(response, "text"), flatten = TRUE)
}

# === TEST API CONNECTION ===
cat("📡 Testing Bitget API connection...\n")

# Test account assets endpoint
result <- bitget_request("/api/mix/v1/account/accounts", "GET", list(productType = "umcbl"))

if (result$code == "00000") {
  cat("✅ Bitget API connection successful!\n")
  cat("📊 Account data received\n")
} else {
  cat("❌ API Error:", result$msg, "\n")
}



#------------------------------ Credential-Loading and Live Bitget API Connection------------------------------------------#


# === LOAD SECURE CREDENTIALS FROM .ENV ===

# Install dotenv if needed
if (!require(dotenv, quietly = TRUE)) {
  install.packages("dotenv")
  library(dotenv)
}

# Load .env file
load_dot_env("C:/freeding/tbot202506/.env")

# Get credentials from environment variables
api_key <- Sys.getenv("BITGET_API_KEY")
api_secret <- Sys.getenv("BITGET_API_SECRET") 
passphrase <- Sys.getenv("BITGET_PASSPHRASE")
base_url <- "https://api.bitget.com"

# Verify credentials loaded
cat("🔐 Loading credentials from .env...\n")
cat("API Key:", substr(api_key, 1, 10), "...\n")
cat("API Secret:", substr(api_secret, 1, 10), "...\n") 
cat("Passphrase:", passphrase, "\n")

# Validate all credentials present
if (all(c(nchar(api_key) > 0, nchar(api_secret) > 0, nchar(passphrase) > 0))) {
  cat("✅ All Bitget credentials loaded securely!\n")
} else {
  cat("❌ Missing credentials\n")
  stop("Please check .env file")
}

# Update bitget_request function to use env credentials
bitget_request <- function(path, method = "GET", params = NULL) {
  ts <- as.character(round(as.numeric(Sys.time()) * 1000))
  
  query_str <- if (!is.null(params) && toupper(method) == "GET") {
    paste0("?", paste(names(params), params, sep = "=", collapse = "&"))
  } else ""
  
  prehash <- paste0(ts, toupper(method), path, query_str)
  
  body_json <- if (toupper(method) == "POST" && !is.null(params)) {
    toJSON(params, auto_unbox = TRUE)
  } else ""
  
  sig_raw <- sha256(charToRaw(paste0(prehash, body_json)), key = charToRaw(api_secret))
  signature <- base64_enc(sig_raw)
  
  headers <- c(
    "ACCESS-KEY" = api_key,
    "ACCESS-SIGN" = signature, 
    "ACCESS-TIMESTAMP" = ts,
    "ACCESS-PASSPHRASE" = passphrase,
    "Content-Type" = "application/json"
  )
  
  url <- paste0(base_url, path)
  
  if (toupper(method) == "GET") {
    response <- GET(url, add_headers(.headers = headers), query = params, timeout(10))
  } else {
    response <- VERB(method, url, add_headers(.headers = headers), 
                     body = body_json, encode = "json", timeout(10))
  }
  
  if (http_error(response)) {
    stop(sprintf("HTTP %s: %s", status_code(response), content(response, "text")))
  }
  
  fromJSON(content(response, "text"), flatten = TRUE)
}

# Test secure connection
cat("📡 Testing secure API connection...\n")
result <- bitget_request("/api/mix/v1/account/accounts", "GET", list(productType = "umcbl"))

if (result$code == "00000") {
  cat("✅ Secure Bitget API connection successful!\n")
} else {
  cat("❌ API Error:", result$msg, "\n")
}


#-----------------------------------------------------------------------------#


# === LIVE PORTFOLIO & POSITION CHECK ===

# Get account assets/balance
get_assets <- function() {
  cat("\n🔎 Fetching account assets...\n")
  res <- bitget_request("/api/mix/v1/account/accounts", "GET", list(productType = "umcbl"))
  
  if (res$code != "00000") stop(res$msg)
  
  df <- res$data
  if (nrow(df) == 0) {
    cat("No assets found.\n")
    return()
  }
  
  # Print formatted table
  cat(sprintf("%-10s %-12s %-12s %-12s %-15s\n", 
              "Coin", "Available", "Locked", "Equity", "Unrealized PL"))
  cat(strrep("-", 65), "\n")
  
  df[] <- lapply(df, as.character)
  apply(df, 1, function(row) {
    cat(sprintf("%-10s %-12s %-12s %-12s %-15s\n", row["marginCoin"],
                sprintf("%.4f", as.numeric(row["available"])),
                sprintf("%.4f", as.numeric(row["locked"])),
                sprintf("%.4f", as.numeric(row["equity"])),
                sprintf("%.4f", as.numeric(row["unrealizedPL"]))))
  })
  
  cat("\n📊 Summary:\n")
  cat(sprintf("Total Equity: %.4f USDT\n", as.numeric(df$equity[1])))
  cat(sprintf("Unrealized P&L: %.4f USDT\n", as.numeric(df$unrealizedPL[1])))
}

# Get all positions
get_positions <- function() {
  cat("\n🔎 Fetching all positions...\n")
  res <- bitget_request("/api/mix/v1/position/allPosition", "GET", list(productType = "umcbl"))
  
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

# Check existing TP/SL orders
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
  }
}

# === CHECK YOUR LIVE ADA POSITION ===
cat("💰 Checking your live Bitget portfolio...\n")
get_assets()
get_positions()
get_plan_orders("ADAUSDT_UMCBL")






