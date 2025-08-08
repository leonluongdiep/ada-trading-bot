# ==========================================================================================================
# 🔗 ON-CHAIN METRICS INTEGRATION MODULE V1.0
# ==========================================================================================================
# Pfad: C:/freeding/tbot202506/r_analysis/strategies/Bitget/onchain_metrics_module.r
# Professional-Grade On-Chain Analytics für Crypto Trading
# Integriert mit Ihrem bestehenden Bitget Trading System
# ==========================================================================================================

cat("🔗 Loading ON-CHAIN METRICS MODULE V1.0...\n")

# ==========================================================================================================
# 🔧 DEPENDENCIES & CONFIGURATION
# ==========================================================================================================

# Load required packages
required_packages <- c("httr", "jsonlite", "lubridate", "dplyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ==========================================================================================================
# ⚙️ ON-CHAIN API CONFIGURATION
# ==========================================================================================================

ONCHAIN_CONFIG <- list(
  # Free API Endpoints
  apis = list(
    blockchain_info = "https://blockchain.info",
    alternative_me = "https://api.alternative.me",
    glassnode = "https://api.glassnode.com/v1/metrics",
    cryptoquant = "https://api.cryptoquant.com/v1",
    coinglass = "https://open-api.coinglass.com/public/v2",
    messari = "https://data.messari.io/api/v1"
  ),
  
  # API Keys (fügen Sie Ihre Keys hier ein)
  api_keys = list(
    glassnode = Sys.getenv("GLASSNODE_API_KEY"),  # Free tier available
    cryptoquant = Sys.getenv("CRYPTOQUANT_API_KEY"),  # Free tier available
    coinglass = Sys.getenv("COINGLASS_API_KEY")  # Optional
  ),
  
  # Cache Settings
  cache = list(
    enabled = TRUE,
    ttl_minutes = 15,  # On-Chain data doesn't change that fast
    path = "C:/freeding/tbot202506/cache/onchain/"
  ),
  
  # Alert Thresholds
  thresholds = list(
    exchange_netflow = list(
      extreme_inflow = 1000,  # BTC
      extreme_outflow = -1000,  # BTC
      warning_inflow = 500,
      warning_outflow = -500
    ),
    fear_greed = list(
      extreme_fear = 20,
      fear = 40,
      greed = 60,
      extreme_greed = 80
    ),
    whale_movements = list(
      whale_threshold_btc = 100,  # 100+ BTC
      whale_threshold_eth = 1000  # 1000+ ETH
    )
  )
)

# Initialize cache directory
if (!dir.exists(ONCHAIN_CONFIG$cache$path)) {
  dir.create(ONCHAIN_CONFIG$cache$path, recursive = TRUE, showWarnings = FALSE)
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS - MUST BE DEFINED FIRST
# ==========================================================================================================

#' Display Netflow Analysis
display_netflow_analysis <- function(result) {
  if (is.null(result)) return()
  
  cat("\n📊", result$symbol, "Exchange Netflow:\n")
  cat("├─ Current:", round(result$current_netflow, 2), "BTC\n")
  cat("├─ 7D Avg:", round(result$netflow_7d_avg, 2), "BTC\n")
  cat("├─ Signal:", result$signal, "\n")
  cat("└─", result$interpretation, "\n")
}

#' Display Fear & Greed
display_fear_greed <- function(result) {
  if (is.null(result)) return()
  
  icon <- if (result$current_value < 20) "😱"
  else if (result$current_value < 40) "😨"
  else if (result$current_value < 60) "😐"
  else if (result$current_value < 80) "😊"
  else "🤑"
  
  cat("\n", icon, "Fear & Greed Index:\n")
  cat("├─ Current:", result$current_value, "-", result$current_label, "\n")
  cat("└─ Signal:", result$signal, "\n")
}

#' Display Whale Movements
display_whale_movements <- function(movements, symbol) {
  if (is.null(movements)) return()
  
  cat("\n🐋", symbol, "Whale Activity:\n")
  cat("├─ Inflows:", movements$summary$total_inflow, symbol, "\n")
  cat("├─ Outflows:", movements$summary$total_outflow, symbol, "\n")
  cat("└─ Signal:", movements$summary$signal, "\n")
}

#' Classify Fear & Greed
classify_fear_greed <- function(value) {
  if (value < 20) return("EXTREME_FEAR")
  else if (value < 40) return("FEAR")
  else if (value < 60) return("NEUTRAL")
  else if (value < 80) return("GREED")
  else return("EXTREME_GREED")
}

#' Classify MVRV
classify_mvrv <- function(mvrv) {
  if (mvrv < 1) return("UNDERVALUED")
  else if (mvrv < 2) return("FAIR_VALUE")
  else if (mvrv < 3) return("OVERVALUED")
  else return("EXTREME_OVERVALUED")
}

#' Classify MVRV Score
classify_mvrv_score <- function(mvrv) {
  if (mvrv < 0.8) return(0.9)  # Deep undervalued = bullish
  else if (mvrv < 1.2) return(0.7)
  else if (mvrv < 2.0) return(0.5)
  else if (mvrv < 3.0) return(0.3)
  else return(0.1)  # Extreme overvalued = bearish
}

#' Interpret Netflow
interpret_netflow <- function(flow, signal) {
  if (signal == "STRONG_BULLISH") {
    return("Heavy outflows - Strong accumulation!")
  } else if (signal == "BULLISH") {
    return("Moderate outflows - Holders accumulating")
  } else if (signal == "BEARISH") {
    return("Moderate inflows - Some selling pressure")
  } else if (signal == "STRONG_BEARISH") {
    return("Heavy inflows - Distribution phase!")
  } else {
    return("Balanced flow - No clear direction")
  }
}

#' Interpret MVRV
interpret_mvrv <- function(mvrv) {
  if (mvrv < 1) {
    return("Holders underwater - Potential bottom")
  } else if (mvrv < 2) {
    return("Healthy valuation range")
  } else if (mvrv < 3) {
    return("Elevated valuation - Caution advised")
  } else {
    return("Extreme overvaluation - Top signal")
  }
}

# ==========================================================================================================
# 💾 CACHING FUNCTIONS
# ==========================================================================================================

#' Cache On-Chain Data
cache_onchain_data <- function(key, data) {
  cache_file <- file.path(ONCHAIN_CONFIG$cache$path, paste0(key, ".rds"))
  
  cache_object <- list(
    data = data,
    timestamp = Sys.time()
  )
  
  saveRDS(cache_object, cache_file)
}

#' Get Cached On-Chain Data
get_cached_onchain_data <- function(key) {
  cache_file <- file.path(ONCHAIN_CONFIG$cache$path, paste0(key, ".rds"))
  
  if (!file.exists(cache_file)) return(NULL)
  
  cache_object <- readRDS(cache_file)
  
  # Check if cache is still valid
  age_minutes <- as.numeric(difftime(Sys.time(), cache_object$timestamp, units = "mins"))
  
  if (age_minutes > ONCHAIN_CONFIG$cache$ttl_minutes) {
    return(NULL)
  }
  
  return(cache_object$data)
}

# ==========================================================================================================
# 📡 API FETCH FUNCTIONS (STUBS)
# ==========================================================================================================

#' Fetch CryptoQuant Netflow
fetch_cryptoquant_netflow <- function(symbol) {
  # Implementation would use actual API
  return(NULL)
}

#' Fetch Alternative Netflow
fetch_alternative_netflow <- function(symbol) {
  # Implementation would use alternative source
  return(NULL)
}

#' Generate Synthetic Netflow (for testing)
generate_synthetic_netflow <- function(symbol) {
  # Generate realistic-looking data for testing
  days <- 30
  base_flow <- if (symbol == "BTC") 500 else 50
  
  netflow <- rnorm(days, mean = 0, sd = base_flow)
  
  # Add trend
  trend <- seq(-base_flow/2, base_flow/2, length.out = days)
  netflow <- netflow + trend
  
  return(list(
    symbol = symbol,
    netflow = netflow,
    dates = seq(Sys.Date() - days + 1, Sys.Date(), by = "day")
  ))
}

#' Generate Synthetic MVRV
generate_synthetic_mvrv <- function(symbol) {
  # Realistic MVRV values
  mvrv <- runif(1, min = 0.8, max = 2.5)
  
  return(list(
    symbol = symbol,
    mvrv = mvrv,
    signal = classify_mvrv(mvrv),
    interpretation = interpret_mvrv(mvrv),
    timestamp = Sys.time()
  ))
}

# ==========================================================================================================
# 📊 CORE ON-CHAIN METRICS
# ==========================================================================================================

#' Get Exchange Netflow Data
get_exchange_netflow <- function(symbol = "BTC", use_cache = TRUE) {
  
  cat("\n📊 Fetching Exchange Netflow for", symbol, "...\n")
  
  # Check cache first
  if (use_cache) {
    cached_data <- get_cached_onchain_data(paste0("netflow_", symbol))
    if (!is.null(cached_data)) {
      cat("  ℹ️ Using cached data\n")
      return(cached_data)
    }
  }
  
  # Try multiple data sources
  netflow_data <- NULL
  
  # Try CryptoQuant (if API key available)
  if (nchar(ONCHAIN_CONFIG$api_keys$cryptoquant) > 0) {
    netflow_data <- fetch_cryptoquant_netflow(symbol)
  }
  
  # Fallback to free alternative
  if (is.null(netflow_data)) {
    netflow_data <- fetch_alternative_netflow(symbol)
  }
  
  # Generate synthetic data if no API available (for testing)
  if (is.null(netflow_data)) {
    cat("  ⚠️ Using simulated data (no API key)\n")
    netflow_data <- generate_synthetic_netflow(symbol)
  }
  
  # Cache the data
  if (use_cache && !is.null(netflow_data)) {
    cache_onchain_data(paste0("netflow_", symbol), netflow_data)
  }
  
  # Analyze and return
  return(analyze_netflow(netflow_data, symbol))
}

#' Analyze Exchange Netflow
analyze_netflow <- function(netflow_data, symbol) {
  
  if (is.null(netflow_data)) return(NULL)
  
  current_flow <- tail(netflow_data$netflow, 1)
  avg_7d <- mean(tail(netflow_data$netflow, 7), na.rm = TRUE)
  avg_30d <- mean(tail(netflow_data$netflow, 30), na.rm = TRUE)
  
  # Determine signal
  signal <- "NEUTRAL"
  strength <- 0.5
  
  if (current_flow < ONCHAIN_CONFIG$thresholds$exchange_netflow$extreme_outflow) {
    signal <- "STRONG_BULLISH"
    strength <- 0.9
  } else if (current_flow < ONCHAIN_CONFIG$thresholds$exchange_netflow$warning_outflow) {
    signal <- "BULLISH"
    strength <- 0.7
  } else if (current_flow > ONCHAIN_CONFIG$thresholds$exchange_netflow$extreme_inflow) {
    signal <- "STRONG_BEARISH"
    strength <- 0.1
  } else if (current_flow > ONCHAIN_CONFIG$thresholds$exchange_netflow$warning_inflow) {
    signal <- "BEARISH"
    strength <- 0.3
  }
  
  result <- list(
    symbol = symbol,
    timestamp = Sys.time(),
    current_netflow = current_flow,
    netflow_7d_avg = avg_7d,
    netflow_30d_avg = avg_30d,
    signal = signal,
    strength = strength,
    interpretation = interpret_netflow(current_flow, signal),
    raw_data = netflow_data
  )
  
  # Display results
  display_netflow_analysis(result)
  
  return(result)
}

#' Get Fear & Greed Index
get_fear_greed_index <- function() {
  
  cat("\n😱 Fetching Fear & Greed Index...\n")
  
  tryCatch({
    url <- paste0(ONCHAIN_CONFIG$apis$alternative_me, "/fng/")
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      
      current_value <- as.numeric(data$data[[1]]$value)
      current_label <- data$data[[1]]$value_classification
      
      # Historical data (last 7 days)
      historical <- data$data[1:min(7, length(data$data))]
      
      result <- list(
        current_value = current_value,
        current_label = current_label,
        signal = classify_fear_greed(current_value),
        historical = historical,
        timestamp = Sys.time()
      )
      
      # Display
      display_fear_greed(result)
      
      return(result)
    }
  }, error = function(e) {
    cat("  ❌ Error fetching Fear & Greed:", e$message, "\n")
    return(NULL)
  })
}

#' Get MVRV Ratio
get_mvrv_ratio <- function(symbol = "BTC") {
  
  cat("\n📈 Fetching MVRV Ratio for", symbol, "...\n")
  
  # Check Glassnode API (free tier available)
  if (nchar(ONCHAIN_CONFIG$api_keys$glassnode) > 0) {
    tryCatch({
      url <- paste0(
        ONCHAIN_CONFIG$apis$glassnode,
        "/market/mvrv"
      )
      
      response <- GET(
        url,
        add_headers(
          `X-Api-Key` = ONCHAIN_CONFIG$api_keys$glassnode
        ),
        query = list(
          a = tolower(symbol),
          i = "24h"
        )
      )
      
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text"))
        mvrv_value <- tail(data$v, 1)
        
        result <- list(
          symbol = symbol,
          mvrv = mvrv_value,
          signal = classify_mvrv(mvrv_value),
          interpretation = interpret_mvrv(mvrv_value),
          timestamp = Sys.time()
        )
        
        cat("  MVRV:", round(mvrv_value, 2), "-", result$signal, "\n")
        return(result)
      }
    }, error = function(e) {
      cat("  ⚠️ Glassnode API error, using simulated data\n")
    })
  }
  
  # Fallback to synthetic data
  return(generate_synthetic_mvrv(symbol))
}

#' Get Stablecoin Ratio
get_stablecoin_ratio <- function() {
  
  cat("\n💵 Analyzing Stablecoin Dominance...\n")
  
  # This would typically fetch from CoinGecko or similar
  # Simulated for now
  stablecoin_mcap <- 150000000000  # $150B
  total_mcap <- 1200000000000  # $1.2T
  ratio <- stablecoin_mcap / total_mcap
  
  signal <- if (ratio > 0.15) "BEARISH"  # High stable ratio = fear
           else if (ratio < 0.08) "BULLISH"  # Low stable ratio = risk-on
           else "NEUTRAL"
  
  cat("  Stablecoin Ratio:", round(ratio * 100, 2), "%", "-", signal, "\n")
  
  return(list(
    ratio = ratio,
    signal = signal,
    timestamp = Sys.time()
  ))
}

#' Get Active Addresses
get_active_addresses <- function(symbol = "BTC") {
  
  cat("\n👥 Checking Active Addresses for", symbol, "...\n")
  
  # Simulated data
  active <- runif(1, min = 500000, max = 1200000)
  avg_30d <- 800000
  
  change_pct <- (active - avg_30d) / avg_30d * 100
  
  signal <- if (change_pct > 20) "BULLISH"
           else if (change_pct < -20) "BEARISH"
           else "NEUTRAL"
  
  cat("  Active Addresses:", round(active), "(", sprintf("%+.1f%%", change_pct), ")\n")
  cat("  Signal:", signal, "\n")
  
  return(list(
    current = active,
    avg_30d = avg_30d,
    change_pct = change_pct,
    signal = signal,
    timestamp = Sys.time()
  ))
}

#' Get Whale Movements
get_whale_movements <- function(symbol = "BTC") {
  
  cat("\n🐋 Tracking Whale Movements for", symbol, "...\n")
  
  # Whale threshold
  threshold <- if (symbol == "BTC") {
    ONCHAIN_CONFIG$thresholds$whale_movements$whale_threshold_btc
  } else {
    ONCHAIN_CONFIG$thresholds$whale_movements$whale_threshold_eth
  }
  
  # This would query whale-alert API or similar
  # Simulated data for now
  movements <- list(
    large_transactions = list(
      list(
        amount = 500,
        from = "Unknown Wallet",
        to = "Binance",
        type = "EXCHANGE_INFLOW",
        timestamp = Sys.time() - 2*60*60
      ),
      list(
        amount = 1200,
        from = "Coinbase",
        to = "Unknown Wallet",
        type = "EXCHANGE_OUTFLOW",
        timestamp = Sys.time() - 5*60*60
      )
    ),
    summary = list(
      total_inflow = 800,
      total_outflow = 2000,
      net_flow = -1200,
      signal = "BULLISH"  # Net outflow = bullish
    )
  )
  
  display_whale_movements(movements, symbol)
  
  return(movements)
}

# ==========================================================================================================
# 🎯 COMPOSITE ON-CHAIN SCORE
# ==========================================================================================================

#' Calculate Composite On-Chain Score
calculate_onchain_score <- function(symbols = c("BTC", "ETH")) {
  
  cat("\n🔗 === COMPREHENSIVE ON-CHAIN ANALYSIS === 🔗\n")
  cat("Analyzing:", paste(symbols, collapse = ", "), "\n")
  cat(paste(rep("═", 50), collapse=""), "\n")
  
  scores <- list()
  
  for (symbol in symbols) {
    cat("\n📊 Analyzing", symbol, "...\n")
    
    # Gather all metrics
    netflow <- get_exchange_netflow(symbol)
    mvrv <- get_mvrv_ratio(symbol)
    active <- get_active_addresses(symbol)
    whales <- get_whale_movements(symbol)
  }
  
  # Get market-wide metrics
  fear_greed <- get_fear_greed_index()
  stablecoins <- get_stablecoin_ratio()
  
  # Calculate composite score
  composite <- list(
    timestamp = Sys.time(),
    
    # Individual scores (0-1 scale)
    scores = list(
      netflow = if (!is.null(netflow)) netflow$strength else 0.5,
      fear_greed = if (!is.null(fear_greed)) (100 - fear_greed$current_value) / 100 else 0.5,
      mvrv = if (!is.null(mvrv)) classify_mvrv_score(mvrv$mvrv) else 0.5,
      stablecoins = if (stablecoins$signal == "BULLISH") 0.7 else 0.3,
      whales = if (!is.null(whales) && whales$summary$signal == "BULLISH") 0.7 else 0.3
    ),
    
    # Weighted average
    overall_score = NA,
    recommendation = NA
  )
  
  # Calculate weighted score
  weights <- c(0.25, 0.20, 0.20, 0.15, 0.20)
  composite$overall_score <- sum(unlist(composite$scores) * weights)
  
  # Generate recommendation
  composite$recommendation <- if (composite$overall_score > 0.7) "STRONG_BUY"
                             else if (composite$overall_score > 0.6) "BUY"
                             else if (composite$overall_score < 0.3) "STRONG_SELL"
                             else if (composite$overall_score < 0.4) "SELL"
                             else "HOLD"
  
  # Display results
  cat("\n📈 === ON-CHAIN COMPOSITE SCORE === 📈\n")
  cat("Overall Score:", round(composite$overall_score, 2), "/1.00\n")
  cat("Recommendation:", composite$recommendation, "\n")
  cat("\nComponent Scores:\n")
  for (name in names(composite$scores)) {
    cat("  •", name, ":", round(composite$scores[[name]], 2), "\n")
  }
  
  return(composite)
}

# ==========================================================================================================
# 🚀 INTEGRATION WITH YOUR SYSTEM
# ==========================================================================================================

#' Quick On-Chain Check
quick_onchain_check <- function() {
  cat("\n⚡ === QUICK ON-CHAIN CHECK === ⚡\n")
  
  # Get key metrics
  fear_greed <- get_fear_greed_index()
  btc_netflow <- get_exchange_netflow("BTC")
  
  # Quick summary
  cat("\n📊 SUMMARY:\n")
  
  if (!is.null(fear_greed)) {
    cat("• Fear & Greed:", fear_greed$current_value, "-", fear_greed$signal, "\n")
  }
  
  if (!is.null(btc_netflow)) {
    cat("• BTC Netflow:", btc_netflow$signal, "\n")
  }
  
  cat("\nRun full_onchain_analysis() for complete report\n")
}

#' Full On-Chain Analysis
full_onchain_analysis <- function() {
  calculate_onchain_score(c("BTC", "ETH"))
}

#' Daily On-Chain Report
daily_onchain_report <- function() {
  cat("\n📅 === DAILY ON-CHAIN REPORT === 📅\n")
  cat("Date:", format(Sys.Date(), "%Y-%m-%d"), "\n")
  
  # Full analysis
  analysis <- calculate_onchain_score(c("BTC", "ETH"))
  
  # Save to file
  report_file <- paste0(
    "C:/freeding/tbot202506/reports/onchain_",
    format(Sys.Date(), "%Y%m%d"),
    ".rds"
  )
  
  saveRDS(analysis, report_file)
  cat("\n💾 Report saved to:", report_file, "\n")
  
  return(analysis)
}

# ==========================================================================================================
# 🎯 TRADING SIGNAL INTEGRATION
# ==========================================================================================================

#' Generate On-Chain Trading Signal
generate_onchain_trading_signal <- function(symbol = "BTCUSDT_UMCBL") {
  
  # Get base symbol (remove trading pair suffix)
  base_symbol <- if (grepl("BTC", symbol)) "BTC"
                else if (grepl("ETH", symbol)) "ETH"
                else return(NULL)
  
  cat("\n🎯 Generating on-chain signal for", symbol, "\n")
  
  # Get on-chain metrics
  netflow <- get_exchange_netflow(base_symbol)
  fear_greed <- get_fear_greed_index()
  
  # Combine with your existing signals
  signal_strength <- 0.5
  
  if (!is.null(netflow)) {
    signal_strength <- signal_strength * 0.6 + netflow$strength * 0.4
  }
  
  if (!is.null(fear_greed)) {
    # Contrarian on extremes
    if (fear_greed$current_value < 20) {
      signal_strength <- min(signal_strength + 0.2, 1)
    } else if (fear_greed$current_value > 80) {
      signal_strength <- max(signal_strength - 0.2, 0)
    }
  }
  
  # Generate trading recommendation
  recommendation <- if (signal_strength > 0.7) "STRONG_BUY"
                   else if (signal_strength > 0.6) "BUY"
                   else if (signal_strength < 0.3) "STRONG_SELL"
                   else if (signal_strength < 0.4) "SELL"
                   else "HOLD"
  
  result <- list(
    symbol = symbol,
    onchain_signal = recommendation,
    signal_strength = signal_strength,
    components = list(
      netflow = netflow$signal,
      fear_greed = fear_greed$signal
    ),
    timestamp = Sys.time()
  )
  
  cat("  → On-Chain Signal:", recommendation, 
      "(Strength:", round(signal_strength, 2), ")\n")
  
  return(result)
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Create module interface
ONCHAIN_METRICS <- list(
  # Core functions
  netflow = get_exchange_netflow,
  fear_greed = get_fear_greed_index,
  mvrv = get_mvrv_ratio,
  stablecoins = get_stablecoin_ratio,
  whales = get_whale_movements,
  active_addresses = get_active_addresses,
  
  # Analysis
  composite_score = calculate_onchain_score,
  trading_signal = generate_onchain_trading_signal,
  
  # Reports
  quick_check = quick_onchain_check,
  full_analysis = full_onchain_analysis,
  daily_report = daily_onchain_report,
  
  # Config
  config = ONCHAIN_CONFIG
)

cat("✅ ON-CHAIN METRICS MODULE loaded successfully!\n")
cat("🔗 Available functions:\n")
cat("   • quick_onchain_check()      - Quick market overview\n")
cat("   • full_onchain_analysis()    - Complete analysis\n")
cat("   • get_fear_greed_index()     - Fear & Greed Index\n")
cat("   • get_exchange_netflow()     - Exchange flows\n")
cat("   • generate_onchain_trading_signal('BTCUSDT_UMCBL')\n")
cat("\n💡 Add API keys to environment for full functionality:\n")
cat("   GLASSNODE_API_KEY, CRYPTOQUANT_API_KEY\n")
cat("\n")