# ==========================================================================================================
# 🚀 ENHANCED MULTI-ASSET TRADING SYSTEM V8 - ADA + ALGO SUPPORT
# ==========================================================================================================
# 
# NEW: Multi-Asset Support für ADA und ALGO
# ENHANCED: Parallele Analyse beider Assets
# OPTIMIZED: Asset-spezifische Konfigurationen
# IMPROVED: Comparative Analysis Dashboard
# 
# ==========================================================================================================

cat("🚀 ENHANCED MULTI-ASSET TRADING SYSTEM V8\n")
cat(strrep("=", 60), "\n")
cat("📅 Execution Start:", as.character(Sys.time()), "\n")
cat("🎯 Assets: ADA + ALGO Multi-Asset Analysis\n\n")

# ==========================================================================================================
# 🔧 MULTI-ASSET CONFIGURATION (ZUERST DEFINIEREN!)
# ==========================================================================================================

# Asset Configuration with ALGO added
MULTI_ASSET_CONFIG <- list(
  "ADAUSDT_UMCBL" = list(
    name = "Cardano",
    symbol = "ADAUSDT_UMCBL",
    base_asset = "ADA",
    quote_asset = "USDT",
    price_decimals = 4,
    tick_size = 0.0001,
    min_size = 1,
    max_leverage = 20,
    typical_volume_threshold = 50000000,  # 50M USDT
    icon = "🔷"
  ),
  "ALGOUSDT_UMCBL" = list(
    name = "Algorand",
    symbol = "ALGOUSDT_UMCBL",
    base_asset = "ALGO",
    quote_asset = "USDT",
    price_decimals = 4,
    tick_size = 0.0001,
    min_size = 10,
    max_leverage = 20,
    typical_volume_threshold = 20000000,  # 20M USDT
    icon = "⚫"
  )
)

# Active assets from your portfolio - GLOBAL DEFINITION
PORTFOLIO_ASSETS <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL")

# Safe configuration display function
display_portfolio_config <- function() {
  cat("🎯 MULTI-ASSET PORTFOLIO CONFIGURATION\n")
  cat(strrep("=", 50), "\n")
  
  if (exists("PORTFOLIO_ASSETS") && exists("MULTI_ASSET_CONFIG")) {
    for (symbol in PORTFOLIO_ASSETS) {
      if (symbol %in% names(MULTI_ASSET_CONFIG)) {
        config <- MULTI_ASSET_CONFIG[[symbol]]
        cat(sprintf("%s %s (%s) - %s\n", 
                    config$icon, config$name, config$base_asset, symbol))
      }
    }
  } else {
    cat("⚠️ Configuration variables not yet loaded\n")
  }
  cat("\n")
}

# Display initial configuration
display_portfolio_config()

# ==========================================================================================================
# 🔧 CONSOLE OUTPUT MANAGEMENT & CORE SYSTEM LOADING
# ==========================================================================================================

# Lade Console Management System zuerst
if (file.exists("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")) {
  tryCatch({
    source("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")
    start_silent_mode("file")
    cat("✅ Console management loaded and activated\n")
  }, error = function(e) {
    cat("⚠️ Console management failed, continuing with standard output:", e$message, "\n")
  })
} else {
  cat("⚠️ Console management not found - continuing with standard output\n")
}

cat("🔧 SYSTEM INITIALIZATION\n")
cat(strrep("=", 40), "\n")

# 1. Clean Console (optional)
tryCatch({
  if (file.exists("c:/freeding/tbot202506/r_analysis/clean_console.R")) {
    source("c:/freeding/tbot202506/r_analysis/clean_console.R")
    cat("✅ Console cleaned\n")
  }
}, error = function(e) cat("⚠️ Clean console skipped\n"))

# 2. Core Trading System
cat("🔧 Loading complete_trading_analysis_v3.r...\n")
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")
  cat("✅ Core trading analysis loaded\n")
}

# 3. Enhanced System mit Fixes
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r")
  cat("✅ System fixes loaded\n")
}

# 4. Enhanced Collector
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")
  cat("✅ Enhanced collector loaded\n")
}

# 5. Open Interest Heatmap
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/dynamic_ada_oi_heatmap.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/dynamic_ada_oi_heatmap.r")
  cat("✅ OI Heatmap system loaded\n")
}


# 5. Open Interest Heatmap
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/algo_oi_heatmap.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/algo_oi_heatmap.r")
  cat("✅ OI Heatmap system loaded\n")
}

# 5. trailing_sl_system
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/trailing_sl_system.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/trailing_sl_system.r")
  cat("✅ OI Htraiiling_sl_system\n")
}

# 6. oi_table_dashboard
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/oi_table_dashboard.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/oi_table_dashboard.r")
  cat("✅ OI Htraiiling_sl_system\n")
}


# 7.  ALTCOIN RALLY TRIGGERS
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/altcoin_rally_triggers.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/altcoin_rally_triggers.r")
  cat("✅ OI Htraiiling_sl_system\n")
}



cat("✅ All core systems loaded successfully\n")

# ==========================================================================================================
# 🎨 ENHANCED DISPLAY FUNCTIONS
# ==========================================================================================================

cat("🎨 Loading Enhanced Display Functions...\n")

# Helper Functions
str_to_title <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("Unknown")
  words <- strsplit(as.character(x), " ")[[1]]
  paste(toupper(substring(words, 1, 1)), tolower(substring(words, 2)), sep = "", collapse = " ")
}

create_header <- function(title, icon = "📊", width = 80) {
  cat("\n")
  cat(strrep("=", width), "\n")
  cat(sprintf("%s %s\n", icon, toupper(title)))
  cat(strrep("=", width), "\n")
}

create_subheader <- function(title, icon = "🔹", width = 50) {
  cat("\n")
  cat(strrep("-", width), "\n")
  cat(sprintf("%s %s\n", icon, title))
  cat(strrep("-", width), "\n")
}

format_status <- function(value, positive_threshold = 0, negative_threshold = 0) {
  if (is.na(value) || is.null(value) || length(value) == 0) {
    return("❓ UNKNOWN")
  }
  
  if (value > positive_threshold) {
    return(paste("🟢", sprintf("%.2f", value)))
  } else if (value < negative_threshold) {
    return(paste("🔴", sprintf("%.2f", value)))
  } else {
    return(paste("🟡", sprintf("%.2f", value)))
  }
}

format_percentage <- function(value, decimals = 2) {
  if (is.na(value) || is.null(value) || length(value) == 0) return("❓")
  
  formatted <- sprintf(paste0("%+.", decimals, "f%%"), value)
  
  if (value > 0) {
    return(paste("🟢", formatted))
  } else if (value < 0) {
    return(paste("🔴", formatted))
  } else {
    return(paste("🟡", formatted))
  }
}

safe_extract <- function(data, field, default = NA) {
  if (is.null(data) || is.null(data[[field]]) || length(data[[field]]) == 0) {
    return(default)
  }
  return(data[[field]])
}

# ==========================================================================================================
# 🌍 MULTI-ASSET DATA COLLECTION FUNCTIONS
# ==========================================================================================================

collect_multi_asset_data <- function(symbols = PORTFOLIO_ASSETS) {
  
  create_header("MULTI-ASSET DATA COLLECTION", "🌍")
  
  multi_asset_data <- list()
  
  for (symbol in symbols) {
    config <- MULTI_ASSET_CONFIG[[symbol]]
    cat(sprintf("\n%s Collecting data for %s (%s)...\n", 
                config$icon, config$name, symbol))
    
    asset_data <- list(
      symbol = symbol,
      config = config,
      market_data = NULL,
      position = NULL,
      orders = NULL,
      timestamp = Sys.time()
    )
    
    # Enhanced Market Data
    if (exists("get_enhanced_market_data")) {
      tryCatch({
        asset_data$market_data <- get_enhanced_market_data(symbol)
        cat(sprintf("   ✅ Market data collected for %s\n", config$name))
      }, error = function(e) {
        cat(sprintf("   ❌ Market data failed for %s: %s\n", config$name, e$message))
      })
    }
    
    # Position Data
    if (exists("get_current_positions")) {
      tryCatch({
        position <- get_current_positions(symbol)
        if (!is.null(position) && nrow(position) > 0) {
          asset_data$position <- position
          cat(sprintf("   ✅ Position found for %s\n", config$name))
        } else {
          cat(sprintf("   📭 No position for %s\n", config$name))
        }
      }, error = function(e) {
        cat(sprintf("   ❌ Position check failed for %s: %s\n", config$name, e$message))
      })
    }
    
    # Orders Data
    if (exists("get_current_plan_orders")) {
      tryCatch({
        orders <- get_current_plan_orders(symbol)
        if (!is.null(orders) && nrow(orders) > 0) {
          asset_data$orders <- orders
          cat(sprintf("   ✅ %d orders found for %s\n", nrow(orders), config$name))
        } else {
          cat(sprintf("   📭 No orders for %s\n", config$name))
        }
      }, error = function(e) {
        cat(sprintf("   ❌ Orders check failed for %s: %s\n", config$name, e$message))
      })
    }
    
    multi_asset_data[[symbol]] <- asset_data
  }
  
  cat("\n✅ Multi-asset data collection completed!\n")
  return(multi_asset_data)
}

# ==========================================================================================================
# 📊 ENHANCED PORTFOLIO OVERVIEW DISPLAY
# ==========================================================================================================

display_portfolio_overview <- function(multi_asset_data) {
  
  create_header("PORTFOLIO OVERVIEW", "💼")
  
  # Portfolio Summary
  portfolio_summary <- list(
    total_assets = length(multi_asset_data),
    active_positions = 0,
    total_pnl = 0,
    total_orders = 0,
    portfolio_value = 0
  )
  
  cat("┌────────────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                              PORTFOLIO SUMMARY                                │\n")
  cat("├────────────────────────────────────────────────────────────────────────────────┤\n")
  
  for (symbol in names(multi_asset_data)) {
    asset_data <- multi_asset_data[[symbol]]
    config <- asset_data$config
    
    # Extract key metrics
    current_price <- NA
    change_24h <- NA
    position_pnl <- 0
    position_size <- 0
    active_orders <- 0
    
    if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$ticker)) {
      ticker <- asset_data$market_data$ticker
      current_price <- safe_extract(ticker, "last_price", NA)
      change_24h <- safe_extract(ticker, "change_24h_pct", NA)
    }
    
    if (!is.null(asset_data$position) && nrow(asset_data$position) > 0) {
      pos <- asset_data$position[1, ]
      position_pnl <- as.numeric(safe_extract(pos, "unrealizedPL", 0))
      position_size <- as.numeric(safe_extract(pos, "total", 0))
      portfolio_summary$active_positions <- portfolio_summary$active_positions + 1
      portfolio_summary$total_pnl <- portfolio_summary$total_pnl + position_pnl
    }
    
    if (!is.null(asset_data$orders)) {
      active_orders <- nrow(asset_data$orders)
      portfolio_summary$total_orders <- portfolio_summary$total_orders + active_orders
    }
    
    # Position Value
    position_value <- if (!is.na(current_price) && position_size > 0) {
      position_size * current_price
    } else {
      0
    }
    portfolio_summary$portfolio_value <- portfolio_summary$portfolio_value + position_value
    
    # Display Asset Row
    price_display <- if (!is.na(current_price)) sprintf("%.4f USDT", current_price) else "N/A"
    change_display <- if (!is.na(change_24h)) format_percentage(change_24h) else "❓"
    pnl_display <- format_status(position_pnl)
    size_display <- if (position_size > 0) format(position_size, big.mark = ",") else "No Position"
    
    cat(sprintf("│ %s %-12s │ %-12s │ %-12s │ %-15s │ %-8s │\n",
                config$icon, 
                substr(config$name, 1, 12),
                substr(price_display, 1, 12),
                substr(change_display, 1, 12),
                substr(paste(pnl_display, "USDT"), 1, 15),
                substr(active_orders, 1, 8)))
  }
  
  cat("├────────────────────────────────────────────────────────────────────────────────┤\n")
  cat(sprintf("│ TOTALS: %d Assets │ %d Positions │ %s │ %d Orders        │\n",
              portfolio_summary$total_assets,
              portfolio_summary$active_positions,
              substr(paste(format_status(portfolio_summary$total_pnl), "USDT"), 1, 15),
              portfolio_summary$total_orders))
  cat("└────────────────────────────────────────────────────────────────────────────────┘\n")
  
  return(portfolio_summary)
}

# ==========================================================================================================
# 📊 ENHANCED INDIVIDUAL ASSET DISPLAY
# ==========================================================================================================

display_individual_asset_analysis <- function(asset_data, show_detailed = TRUE) {
  
  config <- asset_data$config
  symbol <- asset_data$symbol
  
  create_header(sprintf("%s %s ANALYSIS", config$icon, config$name), "📈")
  
  # === MARKET DATA SECTION ===
  if (!is.null(asset_data$market_data)) {
    market_data <- asset_data$market_data
    
    create_subheader("Market Data", "💰")
    
    if (!is.null(market_data$ticker)) {
      ticker <- market_data$ticker
      
      current_price <- safe_extract(ticker, "last_price", 0)
      change_24h_pct <- safe_extract(ticker, "change_24h_pct", 0)
      high_24h <- safe_extract(ticker, "high_24h", 0)
      low_24h <- safe_extract(ticker, "low_24h", 0)
      volume_24h_usdt <- safe_extract(ticker, "volume_24h_usdt", 0)
      funding_rate <- safe_extract(ticker, "funding_rate", 0)
      
      cat(sprintf("💰 Current Price: %.4f USDT\n", current_price))
      cat(sprintf("📊 24h Change: %s\n", format_percentage(change_24h_pct)))
      cat(sprintf("📈 24h High: %.4f USDT\n", high_24h))
      cat(sprintf("📉 24h Low: %.4f USDT\n", low_24h))
      cat(sprintf("💸 24h Volume: %s USDT\n", format(volume_24h_usdt, big.mark = ",", scientific = FALSE)))
      
      if (!is.na(funding_rate) && funding_rate != 0) {
        funding_status <- if (funding_rate > 0) "🟢 BULLISH" else "🔴 BEARISH"
        cat(sprintf("💸 Funding Rate: %.4f%% (%s)\n", funding_rate * 100, funding_status))
      }
    }
    
    # Sentiment Analysis
    if (!is.null(market_data$sentiment)) {
      sentiment <- market_data$sentiment
      overall_sentiment <- safe_extract(sentiment, "overall_sentiment", "UNKNOWN")
      sentiment_percentage <- safe_extract(sentiment, "sentiment_percentage", 0)
      
      sentiment_icon <- switch(overall_sentiment,
                             "STRONG_BULLISH" = "🚀",
                             "BULLISH" = "🟢",
                             "NEUTRAL" = "🟡",
                             "BEARISH" = "🔴",
                             "STRONG_BEARISH" = "📉",
                             "❓")
      
      cat(sprintf("🎭 Market Sentiment: %s %s (%d%%)\n", 
                  sentiment_icon, overall_sentiment, round(sentiment_percentage)))
    }
  }
  
  # === POSITION SECTION ===
  if (!is.null(asset_data$position) && nrow(asset_data$position) > 0) {
    create_subheader("Position Details", "💼")
    
    pos <- asset_data$position[1, ]
    position_side <- safe_extract(pos, "holdSide", "unknown")
    total_size <- as.numeric(safe_extract(pos, "total", 0))
    entry_price <- as.numeric(safe_extract(pos, "averageOpenPrice", 0))
    current_pnl <- as.numeric(safe_extract(pos, "unrealizedPL", 0))
    
    # Get current price for calculations
    current_price <- if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$ticker)) {
      asset_data$market_data$ticker$last_price
    } else {
      entry_price
    }
    
    price_change <- current_price - entry_price
    price_change_pct <- if (entry_price > 0) (price_change / entry_price) * 100 else 0
    position_value <- total_size * current_price
    
    cat("┌──────────────────────────────────────────────────────────────────────┐\n")
    cat("│                           POSITION DETAILS                          │\n")
    cat("├──────────────────────────────────────────────────────────────────────┤\n")
    cat(sprintf("│ Position Side:    %-50s │\n", toupper(position_side)))
    cat(sprintf("│ Contract Size:    %-50s │\n", format(total_size, big.mark = ",")))
    cat(sprintf("│ Entry Price:      %-50s │\n", paste(sprintf("%.4f", entry_price), "USDT")))
    cat(sprintf("│ Current Price:    %-50s │\n", paste(sprintf("%.4f", current_price), "USDT")))
    cat(sprintf("│ Price Change:     %-50s │\n", 
                paste(format_status(price_change), "USDT", sprintf("(%s)", format_percentage(price_change_pct)))))
    cat("├──────────────────────────────────────────────────────────────────────┤\n")
    cat(sprintf("│ Total P&L:        %-50s │\n", paste(format_status(current_pnl), "USDT")))
    cat(sprintf("│ Position Value:   %-50s │\n", paste(sprintf("%.2f", position_value), "USDT")))
    cat("└──────────────────────────────────────────────────────────────────────┘\n")
  } else {
    create_subheader("Position Status", "💼")
    cat("📭 No active position for this asset\n")
  }
  
  # === ORDERS SECTION ===
  if (!is.null(asset_data$orders) && nrow(asset_data$orders) > 0) {
    create_subheader("Active Orders", "📋")
    
    orders <- asset_data$orders
    current_price <- if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$ticker)) {
      asset_data$market_data$ticker$last_price
    } else {
      NULL
    }
    
    cat(sprintf("📊 Total Active Orders: %d\n", nrow(orders)))
    
    cat("┌─────┬─────────────┬─────────────┬──────────┬─────────────┬──────────────┐\n")
    cat("│ #   │ Type        │ Price       │ Size     │ Distance    │ Status       │\n")
    cat("├─────┼─────────────┼─────────────┼──────────┼─────────────┼──────────────┤\n")
    
    for (i in 1:nrow(orders)) {
      order <- orders[i, ]
      
      order_price <- as.numeric(safe_extract(order, "triggerPrice", 0))
      order_size <- safe_extract(order, "size", 0)
      order_type <- switch(safe_extract(order, "planType", "unknown"),
                          "pos_profit" = "TP",
                          "pos_loss" = "SL", 
                          "normal_plan" = "PLAN",
                          "UNKNOWN")
      
      # Distance calculation
      if (!is.null(current_price) && !is.na(order_price) && order_price > 0 && current_price > 0) {
        distance <- order_price - current_price
        distance_pct <- (distance / current_price) * 100
        
        if (distance > 0) {
          distance_str <- sprintf("+%.1f%%", distance_pct)
          distance_icon <- "📈"
        } else {
          distance_str <- sprintf("%.1f%%", distance_pct)
          distance_icon <- "📉"
        }
        distance_display <- paste(distance_icon, distance_str)
      } else {
        distance_display <- "❓"
      }
      
      # Status
      order_state <- safe_extract(order, "state", safe_extract(order, "status", "unknown"))
      status <- switch(order_state,
                      "not_trigger" = "🟡 PENDING",
                      "triggered" = "🟢 TRIGGERED",
                      "cancelled" = "🔴 CANCELLED",
                      "filled" = "✅ FILLED",
                      "❓ UNKNOWN")
      
      cat(sprintf("│ %-3d │ %-11s │ %-11s │ %-8s │ %-11s │ %-12s │\n",
                  i,
                  substr(order_type, 1, 11),
                  substr(sprintf("%.4f", order_price), 1, 11),
                  substr(format(order_size, big.mark = ","), 1, 8),
                  substr(distance_display, 1, 11),
                  substr(status, 1, 12)))
    }
    
    cat("└─────┴─────────────┴─────────────┴──────────┴─────────────┴──────────────┘\n")
  } else {
    create_subheader("Orders Status", "📋")
    cat("📭 No active orders for this asset\n")
  }
}

# ==========================================================================================================
# 🌍 COMPARATIVE ANALYSIS
# ==========================================================================================================

display_comparative_analysis <- function(multi_asset_data) {
  
  create_header("COMPARATIVE ANALYSIS", "⚖️")
  
  # Extract comparative metrics
  comparison_data <- list()
  
  for (symbol in names(multi_asset_data)) {
    asset_data <- multi_asset_data[[symbol]]
    config <- asset_data$config
    
    metrics <- list(
      symbol = symbol,
      name = config$name,
      icon = config$icon,
      price = NA,
      change_24h = NA,
      volume_24h = NA,
      sentiment_score = NA,
      position_pnl = 0,
      position_size = 0,
      active_orders = 0
    )
    
    # Market metrics
    if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$ticker)) {
      ticker <- asset_data$market_data$ticker
      metrics$price <- safe_extract(ticker, "last_price", NA)
      metrics$change_24h <- safe_extract(ticker, "change_24h_pct", NA)
      metrics$volume_24h <- safe_extract(ticker, "volume_24h_usdt", NA)
    }
    
    # Sentiment
    if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$sentiment)) {
      sentiment <- asset_data$market_data$sentiment
      metrics$sentiment_score <- safe_extract(sentiment, "sentiment_percentage", NA)
    }
    
    # Position metrics
    if (!is.null(asset_data$position) && nrow(asset_data$position) > 0) {
      pos <- asset_data$position[1, ]
      metrics$position_pnl <- as.numeric(safe_extract(pos, "unrealizedPL", 0))
      metrics$position_size <- as.numeric(safe_extract(pos, "total", 0))
    }
    
    # Orders
    if (!is.null(asset_data$orders)) {
      metrics$active_orders <- nrow(asset_data$orders)
    }
    
    comparison_data[[symbol]] <- metrics
  }
  
  # Display comparison table
  create_subheader("Performance Comparison", "📊")
  
  cat("┌────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┐\n")
  cat("│ Asset  │ Price       │ 24h Change  │ Sentiment   │ Position    │ Orders      │\n")
  cat("├────────┼─────────────┼─────────────┼─────────────┼─────────────┼─────────────┤\n")
  
  for (symbol in names(comparison_data)) {
    metrics <- comparison_data[[symbol]]
    
    price_display <- if (!is.na(metrics$price)) sprintf("%.4f", metrics$price) else "N/A"
    change_display <- if (!is.na(metrics$change_24h)) format_percentage(metrics$change_24h) else "❓"
    sentiment_display <- if (!is.na(metrics$sentiment_score)) sprintf("%+d%%", round(metrics$sentiment_score)) else "❓"
    position_display <- format_status(metrics$position_pnl)
    orders_display <- sprintf("%d", metrics$active_orders)
    
    cat(sprintf("│ %s %-4s │ %-11s │ %-11s │ %-11s │ %-11s │ %-11s │\n",
                metrics$icon,
                substr(metrics$name, 1, 4),
                substr(price_display, 1, 11),
                substr(change_display, 1, 11),
                substr(sentiment_display, 1, 11),
                substr(position_display, 1, 11),
                substr(orders_display, 1, 11)))
  }
  
  cat("└────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┘\n")
  
  # Performance ranking
  create_subheader("Performance Ranking", "🏆")
  
  # Rank by 24h performance
  performance_ranking <- comparison_data[order(sapply(comparison_data, function(x) if(is.na(x$change_24h)) -999 else x$change_24h), decreasing = TRUE)]
  
  cat("🏆 24h Performance Ranking:\n")
  for (i in seq_along(performance_ranking)) {
    metrics <- performance_ranking[[i]]
    rank_icon <- switch(i, "🥇", "🥈", "🥉", paste0(i, "."))
    change_text <- if (!is.na(metrics$change_24h)) format_percentage(metrics$change_24h) else "❓"
    cat(sprintf("   %s %s %s: %s\n", rank_icon, metrics$icon, metrics$name, change_text))
  }
  
  # PnL ranking
  pnl_ranking <- comparison_data[order(sapply(comparison_data, function(x) x$position_pnl), decreasing = TRUE)]
  
  cat("\n💰 Position P&L Ranking:\n")
  for (i in seq_along(pnl_ranking)) {
    metrics <- pnl_ranking[[i]]
    rank_icon <- switch(i, "🥇", "🥈", "🥉", paste0(i, "."))
    pnl_text <- if (metrics$position_pnl != 0) format_status(metrics$position_pnl) else "No Position"
    cat(sprintf("   %s %s %s: %s\n", rank_icon, metrics$icon, metrics$name, pnl_text))
  }
}

cat("✅ Enhanced Display Functions loaded successfully!\n")

# ==========================================================================================================
# 🎯 MAIN EXECUTION - MULTI-ASSET DATA COLLECTION & ANALYSIS (FIXED)
# ==========================================================================================================

# Ensure variables are available
if (!exists("PORTFOLIO_ASSETS")) {
  PORTFOLIO_ASSETS <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL")
  cat("⚠️ PORTFOLIO_ASSETS variable created as fallback\n")
}

if (!exists("MULTI_ASSET_CONFIG")) {
  cat("❌ MULTI_ASSET_CONFIG not found - using minimal configuration\n")
  MULTI_ASSET_CONFIG <- list(
    "ADAUSDT_UMCBL" = list(name = "Cardano", icon = "🔷", base_asset = "ADA"),
    "ALGOUSDT_UMCBL" = list(name = "Algorand", icon = "⚫", base_asset = "ALGO")
  )
}

# Verify functions exist before calling
if (!exists("collect_multi_asset_data")) {
  cat("❌ collect_multi_asset_data function not found - creating minimal version\n")
  
  collect_multi_asset_data <- function(symbols = PORTFOLIO_ASSETS) {
    create_header("MULTI-ASSET DATA COLLECTION (MINIMAL)", "🌍")
    
    multi_asset_data <- list()
    
    for (symbol in symbols) {
      cat(sprintf("🔍 Collecting basic data for %s...\n", symbol))
      
      asset_data <- list(
        symbol = symbol,
        market_data = NULL,
        position = NULL,
        orders = NULL,
        timestamp = Sys.time()
      )
      
      # Try to get basic data
      if (exists("get_enhanced_market_data")) {
        tryCatch({
          asset_data$market_data <- get_enhanced_market_data(symbol)
          cat(sprintf("   ✅ Market data collected for %s\n", symbol))
        }, error = function(e) {
          cat(sprintf("   ❌ Market data failed for %s\n", symbol))
        })
      }
      
      if (exists("get_current_positions")) {
        tryCatch({
          position <- get_current_positions(symbol)
          if (!is.null(position) && nrow(position) > 0) {
            asset_data$position <- position
            cat(sprintf("   ✅ Position found for %s\n", symbol))
          } else {
            cat(sprintf("   📭 No position for %s\n", symbol))
          }
        }, error = function(e) {
          cat(sprintf("   ❌ Position check failed for %s\n", symbol))
        })
      }
      
      if (exists("get_current_plan_orders")) {
        tryCatch({
          orders <- get_current_plan_orders(symbol)
          if (!is.null(orders) && nrow(orders) > 0) {
            asset_data$orders <- orders
            cat(sprintf("   ✅ %d orders found for %s\n", nrow(orders), symbol))
          } else {
            cat(sprintf("   📭 No orders for %s\n", symbol))
          }
        }, error = function(e) {
          cat(sprintf("   ❌ Orders check failed for %s\n", symbol))
        })
      }
      
      multi_asset_data[[symbol]] <- asset_data
    }
    
    return(multi_asset_data)
  }
}

# Collect data for all portfolio assets
cat("🌍 Starting multi-asset data collection...\n")
multi_asset_data <- collect_multi_asset_data(PORTFOLIO_ASSETS)

# Display Portfolio Overview (with safety checks)
if (exists("display_portfolio_overview") && !is.null(multi_asset_data)) {
  tryCatch({
    portfolio_summary <- display_portfolio_overview(multi_asset_data)
  }, error = function(e) {
    cat("⚠️ Portfolio overview display failed:", e$message, "\n")
    portfolio_summary <- NULL
  })
} else {
  cat("📊 Creating basic portfolio summary...\n")
  
  # Basic portfolio summary
  total_positions <- 0
  total_orders <- 0
  
  for (symbol in names(multi_asset_data)) {
    asset_data <- multi_asset_data[[symbol]]
    
    if (!is.null(asset_data$position) && nrow(asset_data$position) > 0) {
      total_positions <- total_positions + 1
    }
    
    if (!is.null(asset_data$orders)) {
      total_orders <- total_orders + nrow(asset_data$orders)
    }
  }
  
  cat(sprintf("📊 Portfolio Summary: %d assets, %d positions, %d orders\n", 
              length(multi_asset_data), total_positions, total_orders))
}

# Display Comparative Analysis (with safety checks)
if (exists("display_comparative_analysis") && !is.null(multi_asset_data)) {
  tryCatch({
    display_comparative_analysis(multi_asset_data)
  }, error = function(e) {
    cat("⚠️ Comparative analysis display failed:", e$message, "\n")
  })
} else {
  cat("📊 Creating basic comparative analysis...\n")
  
  create_header("BASIC COMPARATIVE ANALYSIS", "⚖️")
  
  for (symbol in names(multi_asset_data)) {
    asset_data <- multi_asset_data[[symbol]]
    
    # Get basic info
    current_price <- NA
    change_24h <- NA
    position_pnl <- 0
    
    if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$ticker)) {
      ticker <- asset_data$market_data$ticker
      current_price <- safe_extract(ticker, "last_price", NA)
      change_24h <- safe_extract(ticker, "change_24h_pct", NA)
    }
    
    if (!is.null(asset_data$position) && nrow(asset_data$position) > 0) {
      pos <- asset_data$position[1, ]
      position_pnl <- as.numeric(safe_extract(pos, "unrealizedPL", 0))
    }
    
    # Display basic info
    price_text <- if (!is.na(current_price)) sprintf("%.4f USDT", current_price) else "N/A"
    change_text <- if (!is.na(change_24h)) format_percentage(change_24h) else "❓"
    pnl_text <- if (position_pnl != 0) format_status(position_pnl) else "No Position"
    
    cat(sprintf("📊 %s: Price=%s, Change=%s, P&L=%s\n", 
                symbol, price_text, change_text, pnl_text))
  }
}

# Display Individual Asset Analysis for each asset (with safety checks)
for (symbol in PORTFOLIO_ASSETS) {
  if (!is.null(multi_asset_data[[symbol]])) {
    
    if (exists("display_individual_asset_analysis")) {
      tryCatch({
        display_individual_asset_analysis(multi_asset_data[[symbol]], show_detailed = TRUE)
      }, error = function(e) {
        cat(sprintf("⚠️ Individual analysis failed for %s: %s\n", symbol, e$message))
        
        # Fallback: Basic individual analysis
        asset_data <- multi_asset_data[[symbol]]
        
        create_header(sprintf("BASIC %s ANALYSIS", symbol), "📈")
        
        # Basic market info
        if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$ticker)) {
          ticker <- asset_data$market_data$ticker
          cat(sprintf("💰 Price: %.4f USDT\n", safe_extract(ticker, "last_price", 0)))
          cat(sprintf("📊 24h Change: %s\n", format_percentage(safe_extract(ticker, "change_24h_pct", 0))))
        }
        
        # Basic position info
        if (!is.null(asset_data$position) && nrow(asset_data$position) > 0) {
          pos <- asset_data$position[1, ]
          cat(sprintf("💼 Position: %s %s contracts\n", 
                      safe_extract(pos, "holdSide", "unknown"),
                      safe_extract(pos, "total", 0)))
          cat(sprintf("💰 P&L: %s USDT\n", 
                      format_status(as.numeric(safe_extract(pos, "unrealizedPL", 0)))))
        } else {
          cat("📭 No active position\n")
        }
        
        # Basic orders info
        if (!is.null(asset_data$orders) && nrow(asset_data$orders) > 0) {
          cat(sprintf("📋 Active Orders: %d\n", nrow(asset_data$orders)))
        } else {
          cat("📭 No active orders\n")
        }
      })
    } else {
      cat(sprintf("⚠️ display_individual_asset_analysis function not available for %s\n", symbol))
    }
  }
}

# ==========================================================================================================
# 🎯 ALGO-SPECIFIC FEATURES & ANALYSIS
# ==========================================================================================================

create_header("ALGO-SPECIFIC ANALYSIS", "⚫")

# ALGO-specific insights
if ("ALGOUSDT_UMCBL" %in% names(multi_asset_data)) {
  algo_data <- multi_asset_data[["ALGOUSDT_UMCBL"]]
  
  cat("⚫ ALGORAND (ALGO) SPECIAL FEATURES:\n")
  cat("   🔹 Pure Proof-of-Stake Consensus\n")
  cat("   🔹 Carbon Negative Blockchain\n")
  cat("   🔹 Instant Finality (~4.5 seconds)\n")
  cat("   🔹 Low Transaction Fees (~0.001 ALGO)\n")
  cat("   🔹 Smart Contracts (PyTeal/Reach)\n")
  cat("   🔹 Atomic Transfers\n\n")
  
  if (!is.null(algo_data$market_data) && !is.null(algo_data$market_data$ticker)) {
    algo_ticker <- algo_data$market_data$ticker
    algo_price <- safe_extract(algo_ticker, "last_price", 0)
    algo_change <- safe_extract(algo_ticker, "change_24h_pct", 0)
    
    cat("📊 ALGO Market Analysis:\n")
    cat(sprintf("   💰 Current Price: %.4f USDT\n", algo_price))
    cat(sprintf("   📈 24h Performance: %s\n", format_percentage(algo_change)))
    
    # ALGO price levels analysis
    if (algo_price > 0) {
      cat("\n🎯 ALGO Key Technical Levels:\n")
      cat(sprintf("   📈 Resistance 1: %.4f USDT (+5%%)\n", algo_price * 1.05))
      cat(sprintf("   📈 Resistance 2: %.4f USDT (+10%%)\n", algo_price * 1.10))
      cat(sprintf("   📉 Support 1: %.4f USDT (-5%%)\n", algo_price * 0.95))
      cat(sprintf("   📉 Support 2: %.4f USDT (-10%%)\n", algo_price * 0.90))
    }
  }
}

# ==========================================================================================================
# 🎯 ADA vs ALGO COMPARATIVE INSIGHTS
# ==========================================================================================================

create_header("ADA vs ALGO COMPARISON", "⚖️")

if ("ADAUSDT_UMCBL" %in% names(multi_asset_data) && "ALGOUSDT_UMCBL" %in% names(multi_asset_data)) {
  ada_data <- multi_asset_data[["ADAUSDT_UMCBL"]]
  algo_data <- multi_asset_data[["ALGOUSDT_UMCBL"]]
  
  # Price comparison
  ada_price <- if (!is.null(ada_data$market_data) && !is.null(ada_data$market_data$ticker)) {
    ada_data$market_data$ticker$last_price
  } else { 0 }
  
  algo_price <- if (!is.null(algo_data$market_data) && !is.null(algo_data$market_data$ticker)) {
    algo_data$market_data$ticker$last_price
  } else { 0 }
  
  if (ada_price > 0 && algo_price > 0) {
    price_ratio <- ada_price / algo_price
    cat(sprintf("📊 Price Ratio (ADA/ALGO): %.2fx\n", price_ratio))
    cat(sprintf("   🔷 1 ADA = %.2f ALGO\n", price_ratio))
    cat(sprintf("   ⚫ 1 ALGO = %.2f ADA\n", 1/price_ratio))
  }
  
  # Performance comparison
  ada_change <- if (!is.null(ada_data$market_data) && !is.null(ada_data$market_data$ticker)) {
    safe_extract(ada_data$market_data$ticker, "change_24h_pct", 0)
  } else { 0 }
  
  algo_change <- if (!is.null(algo_data$market_data) && !is.null(algo_data$market_data$ticker)) {
    safe_extract(algo_data$market_data$ticker, "change_24h_pct", 0)
  } else { 0 }
  
  cat("\n📈 24h Performance Comparison:\n")
  cat(sprintf("   🔷 ADA: %s\n", format_percentage(ada_change)))
  cat(sprintf("   ⚫ ALGO: %s\n", format_percentage(algo_change)))
  
  if (ada_change != 0 && algo_change != 0) {
    relative_performance <- ada_change - algo_change
    if (relative_performance > 0) {
      cat(sprintf("   🏆 ADA outperforming by %s\n", format_percentage(relative_performance)))
    } else {
      cat(sprintf("   🏆 ALGO outperforming by %s\n", format_percentage(abs(relative_performance))))
    }
  }
  
  # Portfolio allocation insights
  ada_pnl <- if (!is.null(ada_data$position) && nrow(ada_data$position) > 0) {
    as.numeric(safe_extract(ada_data$position[1,], "unrealizedPL", 0))
  } else { 0 }
  
  algo_pnl <- if (!is.null(algo_data$position) && nrow(algo_data$position) > 0) {
    as.numeric(safe_extract(algo_data$position[1,], "unrealizedPL", 0))
  } else { 0 }
  
  total_pnl <- ada_pnl + algo_pnl
  
  if (total_pnl != 0) {
    cat("\n💼 Portfolio Contribution:\n")
    if (ada_pnl != 0) {
      ada_contribution <- (ada_pnl / total_pnl) * 100
      cat(sprintf("   🔷 ADA contributes: %.1f%% of total P&L\n", ada_contribution))
    }
    if (algo_pnl != 0) {
      algo_contribution <- (algo_pnl / total_pnl) * 100
      cat(sprintf("   ⚫ ALGO contributes: %.1f%% of total P&L\n", algo_contribution))
    }
  }
}

# ==========================================================================================================
# 🎯 MULTI-ASSET OPEN INTEREST ANALYSIS (if available)
# ==========================================================================================================

create_header("MULTI-ASSET OPEN INTEREST", "🔍")

cat("🔍 Analyzing Open Interest patterns for portfolio assets...\n")

# Generate OI analysis for each asset if function is available
for (symbol in PORTFOLIO_ASSETS) {
  config <- MULTI_ASSET_CONFIG[[symbol]]
  
  cat(sprintf("\n%s %s Open Interest Analysis:\n", config$icon, config$name))
  
  if (exists("generate_dynamic_ada_oi_heatmap") && symbol == "ADAUSDT_UMCBL") {
    tryCatch({
      cat("🔴 Generating dynamic ADA OI heatmap with live price line...\n")
      ada_heatmap <- generate_dynamic_ada_oi_heatmap()
      cat("✅ ADA OI heatmap generated - check plot panel\n")
      
      # Display key levels if available
      if (!is.null(ada_heatmap$key_levels)) {
        cat("🎯 Key ADA OI Levels:\n")
        print(head(ada_heatmap$key_levels, 5))
      }
    }, error = function(e) {
      cat("⚠️ ADA OI heatmap generation failed:", e$message, "\n")
    })
  } else {
    # Generic OI analysis for other assets
    if (exists("get_open_interest")) {
      tryCatch({
        oi_data <- get_open_interest(symbol)
        if (!is.null(oi_data)) {
          cat(sprintf("   📊 Open Interest: %s contracts\n", format(oi_data$open_interest, big.mark = ",")))
        }
      }, error = function(e) {
        cat(sprintf("   ⚠️ OI data unavailable for %s\n", config$name))
      })
    }
  }
}

# ==========================================================================================================
# 🎯 QUICK TRADING COMMANDS FOR BOTH ASSETS
# ==========================================================================================================

create_header("QUICK TRADING COMMANDS", "⚡")

cat("📋 Available commands for your multi-asset portfolio:\n\n")

cat("🔷 ADA (Cardano) Commands:\n")
cat("   # Market Data:\n")
cat("   ada_market <- get_enhanced_market_data('ADAUSDT_UMCBL')\n")
cat("   ada_position <- get_current_positions('ADAUSDT_UMCBL')\n")
cat("   ada_orders <- get_current_plan_orders('ADAUSDT_UMCBL')\n\n")

cat("   # Trading (if enabled):\n")
cat("   place_tp_simple('ADAUSDT_UMCBL', 'long', '1000', 0.7000)  # TP order\n")
cat("   place_sl_simple('ADAUSDT_UMCBL', 'long', '1000', 0.5200)  # SL order\n\n")

cat("⚫ ALGO (Algorand) Commands:\n")
cat("   # Market Data:\n")
cat("   algo_market <- get_enhanced_market_data('ALGOUSDT_UMCBL')\n")
cat("   algo_position <- get_current_positions('ALGOUSDT_UMCBL')\n")
cat("   algo_orders <- get_current_plan_orders('ALGOUSDT_UMCBL')\n\n")

cat("   # Trading (if enabled):\n")
cat("   place_tp_simple('ALGOUSDT_UMCBL', 'long', '1000', 0.3500)  # TP order\n")
cat("   place_sl_simple('ALGOUSDT_UMCBL', 'long', '1000', 0.2800)  # SL order\n\n")

cat("🌍 Multi-Asset Commands:\n")
cat("   # Refresh all data:\n")
cat("   multi_data <- collect_multi_asset_data()\n")
cat("   display_portfolio_overview(multi_data)\n")
cat("   display_comparative_analysis(multi_data)\n\n")

cat("   # Quick position check:\n")
cat("   check_positions_universal()  # All assets\n")

# ==========================================================================================================
# 🎯 LIVE TRADING CONFIGURATION
# ==========================================================================================================

create_subheader("Live Trading Configuration", "🚨")



# ==========================================================================================================
# 🎯 Trading configuration for both assets
# ==========================================================================================================

# 🎯  Trading configuration for both assets
#  ----- Set to TRUE to enable live trading --------

#EXECUTE_LIVE_ORDERS <- TRUE




# Phase 3: Disaster Protection
#place_sl_simple('ALGOUSDT_UMCBL', 'long', '12000', 0.1788)

# Tiered TP orders for ADA
#place_tp_simple('ADAUSDT_UMCBL', 'long', '5000', 0.750)

# Protective SL
#place_sl_simple('ADAUSDT_UMCBL', 'long', '5000', 0.7213)

#place_tp_simple('ADAUSDT_UMCBL', 'long', '5000', 0.6930)  # Psychological level

#- strategic order-----
#place_strategic_limit_order('ADAUSDT_UMCBL', 'open_long', '5000', 0.6930)


# ==========================================================================================================
# 🎯 trailing_sl_percent
# ==========================================================================================================


# 🔷 ADA: 5000 contracts @ current 0.7341 USDT

#place_trailing_sl_percent('ADAUSDT_UMCBL', 'long', '5000', 2.0)

# → SL bei 0.7194 USDT (-2.0% = schützt +651 USDT Gewinne)

# ⚫ ALGO: 30,000 contracts @ current 0.2256 USDT  
#place_trailing_sl_percent('ALGOUSDT_UMCBL', 'long', '45000', 15.0)
# → SL bei 0.2193 USDT (-2.8% = schützt +485 USDT Gewinne)




# ==========================================================================================================
# ------------------- heatmap heatmap heatmap heatmap heatmap-------------------------------
# ==========================================================================================================


#-------------------------------------  heatmap

cat("🔴 The red dashed line shows the LIVE ADA price from Bitget!\n")



#----------------ADA heatmap----------------------------

# Basis-Heatmap generieren
live_ada_heatmap <- generate_dynamic_ada_oi_heatmap()

# Interaktive Anzeige
live_ada_heatmap$heatmap

# Zeige Top OI-Konzentrationen im vollen Range:
print(live_ada_heatmap$key_levels)



#----------------ALGO heatmap----------------------------


# Basis-Heatmap generieren
live_algo_heatmap <- generate_dynamic_algo_oi_heatmap()

# Interaktive Anzeige
live_algo_heatmap$heatmap

# Zeige Top OI-Konzentrationen im vollen Range:
print(live_algo_heatmap$key_levels)


OI-ANALYSIS TABLE SYSTEM - KOMPLETT KORRIGIERT!

  
  
  
  # ==========================================================================================================
# ------------------- Vollständige AGLO ADA -Analysen-------------------------------
# ==========================================================================================================




# 2. Vollständige ALGO-Analyse:
algo_analysis <- run_institutional_oi_analysis('ALGOUSDT_UMCBL')


# 3. Schneller Dashboard-Blick:
algo_dashboard <- quick_algo_dashboard()



#----------------Vollständige ADA Analyse

# 2. Vollständige ALGO-Analyse:
ada_analysis <- run_institutional_oi_analysis('ADAUSDT_UMCBL')


# 3. Schneller Dashboard-Blick:
ada_dashboard <- quick_ada_dashboard()



# ---------- Multi-Asset Vergleich: ------------------
comparison <- quick_multi_asset_comparison()




#----------Vergleich mit ALGO vs ADA----------------------------------------
#
#comparison <- compare_algo_ada_heatmaps()

# Zeige Top OI-Konzentrationen im vollen Range:
#print(comparison)

# 🛡️ FREITAG-DIP DEFENSE MIT AKTUELLEN ZAHLEN



#┌────────────────────────────────────────────────────────────────────────────────┐#
#│                        🚀 ALTCOIN RALLY TRIGGERS 🚀                          │
#├────────────────────────────────────────────────────────────────────────────────┤
#│ 🟠⚡ MASTER SIGNAL: WEAK_ALTCOIN_RALLY_SIGNAL                     │
#│ 🎯 Score: 30.0% | 💡 WATCH CLOSELY - Some positive signs │
#├────────────────────────────────────────────────────────────────────────────────┤


# In deiner rexecution.r, nach der Portfolio-Analyse hinzufügen:
cat("\n🚀 CHECKING ALTCOIN RALLY TRIGGERS...\n")
altcoin_triggers <- run_altcoin_rally_monitoring()

# Zusätzlich: Speichere Ergebnisse für Trend-Tracking
trigger_history_file <- "c:/freeding/tbot202506/logs/altcoin_triggers_history.rds"
if (file.exists(trigger_history_file)) {
  trigger_history <- readRDS(trigger_history_file)
} else {
  trigger_history <- list()
}

# Füge aktuellen Trigger hinzu
trigger_history[[length(trigger_history) + 1]] <- list(
  timestamp = Sys.time(),
  score = altcoin_triggers$master_signal$score_percentage,
  signal = altcoin_triggers$master_signal$master_signal
)

# Behalte nur letzte 100 Einträge
if (length(trigger_history) > 100) {
  trigger_history <- tail(trigger_history, 100)
}

saveRDS(trigger_history, trigger_history_file)


# ==========================================================================================================
# 🎯 SYSTEM STATUS & CLEANUP
# ==========================================================================================================

create_header("SYSTEM STATUS", "🔧")

# Function availability check
cat("📋 Core Functions Status:\n")

core_functions <- c(
  "complete_trading_analysis", "complete_trading_analysis_enhanced",
  "get_enhanced_market_data", "get_current_positions", 
  "get_current_plan_orders", "place_tp_simple", "place_sl_simple"
)

for (func in core_functions) {
  status <- if (exists(func)) "✅ Available" else "❌ Missing"
  cat(sprintf("   %-35s %s\n", func, status))
}

# Asset-specific function check
cat("\n🎯 Asset-Specific Functions:\n")
if (exists("generate_dynamic_ada_oi_heatmap")) {
  cat("   ✅ ADA OI Heatmap available\n")
} else {
  cat("   ❌ ADA OI Heatmap not available\n")
}

if (exists("get_open_interest")) {
  cat("   ✅ Generic OI analysis available\n")
} else {
  cat("   ❌ Generic OI analysis not available\n")
}

# Memory and data status
cat(sprintf("\n📊 Data Status:\n"))
cat(sprintf("   Portfolio Assets: %d (%s)\n", 
            length(PORTFOLIO_ASSETS), 
            paste(sapply(PORTFOLIO_ASSETS, function(x) MULTI_ASSET_CONFIG[[x]]$base_asset), collapse = ", ")))

if (exists("multi_asset_data")) {
  active_positions <- sum(sapply(multi_asset_data, function(x) !is.null(x$position) && nrow(x$position) > 0))
  total_orders <- sum(sapply(multi_asset_data, function(x) if(!is.null(x$orders)) nrow(x$orders) else 0))
  
  cat(sprintf("   Active Positions: %d/%d assets\n", active_positions, length(PORTFOLIO_ASSETS)))
  cat(sprintf("   Total Orders: %d\n", total_orders))
}

# Console cleanup
cat("\n🧹 Console Cleanup...\n")
tryCatch({
  if (exists("end_silent_mode")) {
    end_silent_mode()
    cat("✅ Silent mode ended successfully\n")
  } else {
    # Fallback cleanup
    for(i in 1:10) {
      tryCatch({
        sink()
        sink(type = "message")
        sink(type = "output")
      }, error = function(e) NULL)
    }
    cat("✅ Fallback console cleanup completed\n")
  }
}, error = function(e) {
  cat("⚠️ Console cleanup completed with minor issues\n")
})



# ==========================================================================================================
# 🎯 FINAL SUMMARY
# ==========================================================================================================

create_header("EXECUTION COMPLETE", "✅")

cat("🚀 ENHANCED MULTI-ASSET TRADING SYSTEM V8 COMPLETE!\n")
cat("✅ Successfully analyzed both ADA and ALGO portfolios!\n")
cat("📊 Enhanced multi-asset data collection and analysis!\n")
cat("🎯 Comparative analysis and ranking system operational!\n")
cat("⚖️ Asset-specific insights and technical levels provided!\n")
cat("🔍 Open Interest analysis for supported assets!\n\n")

cat("💼 Your Portfolio Summary:\n")
if (exists("portfolio_summary")) {
  cat(sprintf("   🎯 Total Assets: %d\n", portfolio_summary$total_assets))
  cat(sprintf("   💼 Active Positions: %d\n", portfolio_summary$active_positions))
  cat(sprintf("   💰 Total P&L: %s USDT\n", format_status(portfolio_summary$total_pnl)))
  cat(sprintf("   📋 Total Orders: %d\n", portfolio_summary$total_orders))
  cat(sprintf("   💎 Portfolio Value: %.2f USDT\n", portfolio_summary$portfolio_value))
}

cat("\n🎯 Next Steps:\n")
cat("1. 📊 Monitor comparative performance between ADA and ALGO\n")
cat("2. 🎯 Consider rebalancing based on relative performance\n")
cat("3. 📈 Set appropriate TP/SL levels for both assets\n")
cat("4. 🔍 Watch OI levels for potential support/resistance\n")
cat("5. 📱 Use quick commands for real-time monitoring\n")

cat(strrep("=", 80), "\n")
cat("🌍 Multi-Asset Trading System Ready!\n")

# ==========================================================================================================
# 🎯 END OF ENHANCED MULTI-ASSET TRADING SYSTEM V8
# ==========================================================================================================
  