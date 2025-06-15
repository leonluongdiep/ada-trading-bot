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
  body <- list(
    symbol     = symbol,
    marginCoin = "USDT",
    planType   = planType,
    triggerPrice= as.character(price),
    holdSide   = side,
    side       = trade_side,
    size       = as.character(size),
    orderType  = "market",
    triggerType= "fill_price"
  )

  res <- bitget_request("/api/mix/v1/plan/placePlan", "POST", body)
  if (res$code != "00000") {
    warning(res$msg)
    return(FALSE)
  }
  message(sprintf("✅ %s order %s placed (ID: %s)", 
                  ifelse(planType=="pos_profit","TP","SL"), symbol, res$data$orderId))
  TRUE
}

# High-level wrapper to place both TP & SL
place_tp_sl <- function(symbol, side, size, tp, sl) {
  cat("\n🎯 Placing TP/SL for", symbol, "...\n")
  ok1 <- place_plan_order(symbol, side, size, tp, "pos_profit")
  Sys.sleep(0.5)
  ok2 <- place_plan_order(symbol, side, size, sl, "pos_loss")

  if (all(c(ok1, ok2))) cat("✅ Both orders placed!\n")
}


# --- 7. Main Dashboard ---
main <- function() {
  cat(strrep("=",40),"\n")
  cat("🚀 Bitget Futures Dashboard\n")
  cat(strrep("=",40),"\n")

  start <- Sys.time()
  get_assets()
  df_pos <- get_positions()

  # Example: auto TP/SL for ADA if position exists
  # ada <- subset(df_pos, symbol=="ADAUSDT_UMCBL" & as.numeric(total)>0)
  # if (nrow(ada)) {
  #   place_tp_sl(ada$symbol[1], ada$holdSide[1], ada$total[1], 0.9, 0.35)
  # }

  cat(sprintf("\nCompleted in %.2f sec\n", as.numeric(Sys.time()-start)))
}

# Run if interactive
if (interactive()) main()
