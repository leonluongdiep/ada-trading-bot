# Bitget API Client in R
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
      query_parts <- paste(names(query_dict), query_dict, sep = "=")
      query <- paste(query_parts, collapse = "&")
    }
    
    # Generate timestamp (Unix timestamp in milliseconds)
    timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
    
    # Create sign target
    sign_target <- if (query != "") paste0(request_path, "?", query) else request_path
    
    # Create message for signature
    message <- paste0(timestamp, toupper(method), sign_target)
    
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
        query = query_dict,
        timeout(10)
      )
    } else {
      response <- VERB(
        verb = method,
        url = url,
        add_headers(.headers = headers),
        query = query_dict,
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
      UnrealizedPL = sprintf("%.4f", as.numeric(assets_data$unrealizedPL)),
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

# Main function
main <- function() {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("🚀 Bitget Futures Data Dashboard", "\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  start_time <- Sys.time()
  
  # Run all queries
  get_assets()
  
  elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("\n✅ All queries completed in %.2f seconds\n\n", as.numeric(elapsed_time)))
}

# Run main function
if (interactive() || !exists("sourced_script")) {
  main()
}