# === BITGET API CONNECTION ===

# Load required libraries
library(httr)
library(jsonlite)
library(openssl)  # ← This was missing!

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
  
  # Fixed: Using openssl::sha256 explicitly
  sig_raw <- openssl::sha256(charToRaw(paste0(prehash, body_json)), 
                             key = charToRaw(api_secret))
  signature <- openssl::base64_encode(sig_raw)
  
  headers <- c(
    "ACCESS-KEY" = api_key,
    "ACCESS-SIGN" = signature, 
    "ACCESS-TIMESTAMP" = ts,
    "ACCESS-PASSPHRASE" = passphrase,
    "Content-Type" = "application/json"
  )
  
  url <- paste0(base_url, path)
  
  tryCatch({
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
    
  }, error = function(e) {
    cat("❌ API Request Error:", e$message, "\n")
    return(NULL)
  })
}

# Test secure connection
cat("📡 Testing secure API connection...\n")
result <- bitget_request("/api/mix/v1/account/accounts", "GET", list(productType = "umcbl"))

if (!is.null(result) && result$code == "00000") {
  cat("✅ Secure Bitget API connection successful!\n")
  cat("📊 Account data retrieved!\n")
} else {
  cat("❌ API Error\n")
  if (!is.null(result)) {
    cat("Error message:", result$msg, "\n")
  }
}

# Quick test of your ADA position
cat("\n🔍 Checking ADA position...\n")
positions <- bitget_request("/api/mix/v1/position/allPosition", "GET", list(productType = "umcbl"))

if (!is.null(positions) && positions$code == "00000") {
  ada_pos <- positions$data[positions$data$symbol == "ADAUSDT_UMCBL" & as.numeric(positions$data$total) > 0, ]
  
  if (nrow(ada_pos) > 0) {
    cat("🎯 ADA Position Found:\n")
    cat("  Side:", ada_pos$holdSide[1], "\n")
    cat("  Size:", ada_pos$total[1], "\n") 
    cat("  P&L:", ada_pos$unrealizedPL[1], "USDT\n")
  } else {
    cat("📭 No active ADA position\n")
  }
}

