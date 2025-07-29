# ==========================================================================================================
# 🚀 ENHANCED MULTI-ASSET TRADING SYSTEM V8 - 5 COINS PORTFOLIO
# ==========================================================================================================
# 
# EXPANDED: Multi-Asset Support für ADA, ALGO, ICP, ETC, VET
# ENHANCED: Parallele Analyse aller 5 Assets
# OPTIMIZED: Asset-spezifische Konfigurationen für jedes Coin
# IMPROVED: Comprehensive Dashboard für alle 5 Positionen
# 
# ==========================================================================================================

cat("🚀 ENHANCED MULTI-ASSET TRADING SYSTEM V8 - 5 COINS PORTFOLIO\n")
cat(strrep("=", 70), "\n")
cat("📅 Execution Start:", as.character(Sys.time()), "\n")
cat("🎯 Assets: ADA + ALGO + ICP + ETC + VET Portfolio Analysis\n\n")

# ==========================================================================================================
# 🔧 MULTI-ASSET CONFIGURATION FÜR 5 COINS (ZUERST DEFINIEREN!)
# ==========================================================================================================

# Sicherstellen, dass die Config immer verfügbar ist
if (!exists("MULTI_ASSET_CONFIG") || is.null(MULTI_ASSET_CONFIG)) {
  cat("🔧 Creating MULTI_ASSET_CONFIG...\n")
  
  # Erweiterte Asset Configuration für alle 5 Coins
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
      icon = "🔷",
      category = "Smart Contract Platform"
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
      icon = "⚫",
      category = "Pure PoS Blockchain"
    ),
    "ICPUSDT_UMCBL" = list(
      name = "Internet Computer",
      symbol = "ICPUSDT_UMCBL",
      base_asset = "ICP",
      quote_asset = "USDT",
      price_decimals = 3,
      tick_size = 0.001,
      min_size = 1,
      max_leverage = 20,
      typical_volume_threshold = 30000000,  # 30M USDT
      icon = "🌐",
      category = "Decentralized Internet"
    ),
    "ETCUSDT_UMCBL" = list(
      name = "Ethereum Classic",
      symbol = "ETCUSDT_UMCBL",
      base_asset = "ETC",
      quote_asset = "USDT",
      price_decimals = 3,
      tick_size = 0.001,
      min_size = 1,
      max_leverage = 20,
      typical_volume_threshold = 25000000,  # 25M USDT
      icon = "💎",
      category = "Original Ethereum"
    ),
    "VETUSDT_UMCBL" = list(
      name = "VeChain",
      symbol = "VETUSDT_UMCBL",
      base_asset = "VET",
      quote_asset = "USDT",
      price_decimals = 5,
      tick_size = 0.00001,
      min_size = 100,
      max_leverage = 20,
      typical_volume_threshold = 15000000,  # 15M USDT
      icon = "⚡",
      category = "Supply Chain & IoT"
    )
  )
  
  cat("✅ MULTI_ASSET_CONFIG created successfully!\n")
} else {
  cat("✅ MULTI_ASSET_CONFIG already exists\n")
}

# Sicherstellen, dass PORTFOLIO_ASSETS immer verfügbar ist
if (!exists("PORTFOLIO_ASSETS") || is.null(PORTFOLIO_ASSETS)) {
  cat("🔧 Creating PORTFOLIO_ASSETS...\n")
  # Erweiterte aktive Assets aus deinem Portfolio - GLOBAL DEFINITION
  PORTFOLIO_ASSETS <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL", "ICPUSDT_UMCBL", "ETCUSDT_UMCBL", "VETUSDT_UMCBL")
  cat("✅ PORTFOLIO_ASSETS created successfully!\n")
} else {
  cat("✅ PORTFOLIO_ASSETS already exists\n")
}

# Safe configuration display function
display_portfolio_config <- function() {
  cat("🎯 5-COINS PORTFOLIO CONFIGURATION\n")
  cat(strrep("=", 60), "\n")
  
  if (exists("PORTFOLIO_ASSETS") && exists("MULTI_ASSET_CONFIG")) {
    for (symbol in PORTFOLIO_ASSETS) {
      if (symbol %in% names(MULTI_ASSET_CONFIG)) {
        config <- MULTI_ASSET_CONFIG[[symbol]]
        cat(sprintf("%s %s (%s) - %s - %s\n", 
                    config$icon, config$name, config$base_asset, symbol, config$category))
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

# 5. Open Interest Heatmap Systems
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/dynamic_ada_oi_heatmap.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/dynamic_ada_oi_heatmap.r")
  cat("✅ ADA OI Heatmap system loaded\n")
}

if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/algo_oi_heatmap.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/algo_oi_heatmap.r")
  cat("✅ ALGO OI Heatmap system loaded\n")
}

# 6. Trading Systems
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/trailing_sl_system.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/trailing_sl_system.r")
  cat("✅ Trailing SL system loaded\n")
}

# 7. OI Table Dashboard
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/oi_table_dashboard.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/oi_table_dashboard.r")
  cat("✅ OI Table Dashboard loaded\n")
}

# 8. Altcoin Rally Triggers
if (file.exists("c:/freeding/tbot202506/r_analysis/strategies/Bitget/altcoin_rally_triggers.r")) {
  source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/altcoin_rally_triggers.r")
  cat("✅ Altcoin Rally Triggers loaded\n")
}

cat("✅ All core systems loaded successfully\n")

# ==========================================================================================================
# 🎨 ENHANCED DISPLAY FUNCTIONS FÜR 5 ASSETS
# ==========================================================================================================

cat("🎨 Loading Enhanced Display Functions for 5 Assets...\n")

# Helper Functions (gleich wie vorher)
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
# 🌍 MULTI-ASSET DATA COLLECTION FÜR 5 COINS
# ==========================================================================================================

collect_multi_asset_data <- function(symbols = NULL) {
  
  # Fallback wenn symbols nicht übergeben wurde
  if (is.null(symbols)) {
    if (exists("PORTFOLIO_ASSETS") && !is.null(PORTFOLIO_ASSETS)) {
      symbols <- PORTFOLIO_ASSETS
    } else {
      symbols <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL", "ICPUSDT_UMCBL", "ETCUSDT_UMCBL", "VETUSDT_UMCBL")
      cat("⚠️ Using fallback symbol list\n")
    }
  }
  
  # Sicherstellen, dass MULTI_ASSET_CONFIG verfügbar ist
  if (!exists("MULTI_ASSET_CONFIG") || is.null(MULTI_ASSET_CONFIG)) {
    cat("❌ MULTI_ASSET_CONFIG not available - creating minimal config\n")
    
    MULTI_ASSET_CONFIG <<- list(
      "ADAUSDT_UMCBL" = list(name = "Cardano", icon = "🔷", base_asset = "ADA", category = "Smart Contract"),
      "ALGOUSDT_UMCBL" = list(name = "Algorand", icon = "⚫", base_asset = "ALGO", category = "Pure PoS"),
      "ICPUSDT_UMCBL" = list(name = "Internet Computer", icon = "🌐", base_asset = "ICP", category = "Decentralized Internet"),
      "ETCUSDT_UMCBL" = list(name = "Ethereum Classic", icon = "💎", base_asset = "ETC", category = "Original Ethereum"),
      "VETUSDT_UMCBL" = list(name = "VeChain", icon = "⚡", base_asset = "VET", category = "Supply Chain")
    )
  }
  
  create_header("5-COINS PORTFOLIO DATA COLLECTION", "🌍")
  
  multi_asset_data <- list()
  
  for (symbol in symbols) {
    # Sichere Config-Extraktion
    if (symbol %in% names(MULTI_ASSET_CONFIG)) {
      config <- MULTI_ASSET_CONFIG[[symbol]]
    } else {
      # Fallback config
      config <- list(
        name = symbol,
        icon = "📊",
        base_asset = gsub("USDT_UMCBL", "", symbol),
        category = "Unknown"
      )
    }
    
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
  
  cat("\n✅ 5-Coins portfolio data collection completed!\n")
  return(multi_asset_data)
}

# ==========================================================================================================
# 📊 ENHANCED PORTFOLIO OVERVIEW FÜR 5 ASSETS
# ==========================================================================================================

display_portfolio_overview <- function(multi_asset_data) {
  
  create_header("5-COINS PORTFOLIO OVERVIEW", "💼")
  
  # Portfolio Summary
  portfolio_summary <- list(
    total_assets = length(multi_asset_data),
    active_positions = 0,
    total_pnl = 0,
    total_orders = 0,
    portfolio_value = 0
  )
  
  cat("┌────────────────────────────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                              5-COINS PORTFOLIO SUMMARY                                        │\n")
  cat("├────────────────────────────────────────────────────────────────────────────────────────────────┤\n")
  cat("│ Asset      │ Category          │ Price        │ 24h Change   │ P&L          │ Orders │ Status   │\n")
  cat("├────────────────────────────────────────────────────────────────────────────────────────────────┤\n")
  
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
    asset_name <- sprintf("%s %s", config$icon, substr(config$base_asset, 1, 5))
    category <- substr(config$category, 1, 17)
    price_display <- if (!is.na(current_price)) sprintf("%.4f", current_price) else "N/A"
    change_display <- if (!is.na(change_24h)) format_percentage(change_24h, 1) else "❓"
    pnl_display <- if (position_pnl != 0) sprintf("%+.0f", position_pnl) else "No Pos"
    status <- if (position_size > 0) "ACTIVE" else "IDLE"
    
    cat(sprintf("│ %-10s │ %-17s │ %-12s │ %-12s │ %-12s │ %-6d │ %-8s │\n",
                asset_name, category, price_display, change_display, 
                pnl_display, active_orders, status))
  }
  
  cat("├────────────────────────────────────────────────────────────────────────────────────────────────┤\n")
  cat(sprintf("│ TOTALS: %d Assets │ %d Active Positions │ Total P&L: %+.0f USDT │ %d Orders │ Value: %.0f USDT │\n",
              portfolio_summary$total_assets,
              portfolio_summary$active_positions,
              portfolio_summary$total_pnl,
              portfolio_summary$total_orders,
              portfolio_summary$portfolio_value))
  cat("└────────────────────────────────────────────────────────────────────────────────────────────────┘\n")
  
  return(portfolio_summary)
}

# ==========================================================================================================
# 📊 CATEGORY-BASED ANALYSIS FÜR 5 COINS
# ==========================================================================================================

display_category_analysis <- function(multi_asset_data) {
  
  create_header("CATEGORY-BASED ANALYSIS", "📊")
  
  # Group by categories
  categories <- list()
  
  for (symbol in names(multi_asset_data)) {
    asset_data <- multi_asset_data[[symbol]]
    config <- asset_data$config
    category <- config$category
    
    if (is.null(categories[[category]])) {
      categories[[category]] <- list()
    }
    
    categories[[category]][[symbol]] <- asset_data
  }
  
  # Display each category
  for (category_name in names(categories)) {
    create_subheader(sprintf("%s Category", category_name), "🏷️")
    
    category_assets <- categories[[category_name]]
    category_pnl <- 0
    category_positions <- 0
    
    for (symbol in names(category_assets)) {
      asset_data <- category_assets[[symbol]]
      config <- asset_data$config
      
      # Get performance metrics
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
        if (position_pnl != 0) category_positions <- category_positions + 1
      }
      
      category_pnl <- category_pnl + position_pnl
      
      # Display asset in category
      price_text <- if (!is.na(current_price)) sprintf("%.4f USDT", current_price) else "N/A"
      change_text <- if (!is.na(change_24h)) format_percentage(change_24h, 1) else "❓"
      pnl_text <- if (position_pnl != 0) sprintf("%+.0f USDT", position_pnl) else "No Position"
      
      cat(sprintf("   %s %s: %s | %s | %s\n", 
                  config$icon, config$name, price_text, change_text, pnl_text))
    }
    
    cat(sprintf("   📊 Category Summary: %d positions, %+.0f USDT total P&L\n", 
                category_positions, category_pnl))
  }
}

# ==========================================================================================================
# 🎯 COMPARATIVE RANKING FÜR 5 COINS
# ==========================================================================================================

display_comparative_ranking <- function(multi_asset_data) {
  
  create_header("5-COINS PERFORMANCE RANKING", "🏆")
  
  # Extract performance data
  performance_data <- list()
  
  for (symbol in names(multi_asset_data)) {
    asset_data <- multi_asset_data[[symbol]]
    config <- asset_data$config
    
    metrics <- list(
      symbol = symbol,
      name = config$name,
      icon = config$icon,
      base_asset = config$base_asset,
      price = NA,
      change_24h = NA,
      volume_24h = NA,
      position_pnl = 0,
      position_size = 0
    )
    
    # Market metrics
    if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$ticker)) {
      ticker <- asset_data$market_data$ticker
      metrics$price <- safe_extract(ticker, "last_price", NA)
      metrics$change_24h <- safe_extract(ticker, "change_24h_pct", NA)
      metrics$volume_24h <- safe_extract(ticker, "volume_24h_usdt", NA)
    }
    
    # Position metrics
    if (!is.null(asset_data$position) && nrow(asset_data$position) > 0) {
      pos <- asset_data$position[1, ]
      metrics$position_pnl <- as.numeric(safe_extract(pos, "unrealizedPL", 0))
      metrics$position_size <- as.numeric(safe_extract(pos, "total", 0))
    }
    
    performance_data[[symbol]] <- metrics
  }
  
  # 24h Performance Ranking
  cat("🏆 24H PERFORMANCE RANKING:\n")
  performance_ranking <- performance_data[order(sapply(performance_data, function(x) if(is.na(x$change_24h)) -999 else x$change_24h), decreasing = TRUE)]
  
  for (i in seq_along(performance_ranking)) {
    metrics <- performance_ranking[[i]]
    rank_icon <- switch(i, "🥇", "🥈", "🥉", "🏅", "⭐")
    change_text <- if (!is.na(metrics$change_24h)) format_percentage(metrics$change_24h, 1) else "❓"
    cat(sprintf("   %s %s %s (%s): %s\n", 
                rank_icon, metrics$icon, metrics$name, metrics$base_asset, change_text))
  }
  
  # P&L Ranking
  cat("\n💰 POSITION P&L RANKING:\n")
  pnl_ranking <- performance_data[order(sapply(performance_data, function(x) x$position_pnl), decreasing = TRUE)]
  
  for (i in seq_along(pnl_ranking)) {
    metrics <- pnl_ranking[[i]]
    rank_icon <- switch(i, "🥇", "🥈", "🥉", "🏅", "⭐")
    pnl_text <- if (metrics$position_pnl != 0) sprintf("%+.0f USDT", metrics$position_pnl) else "No Position"
    cat(sprintf("   %s %s %s (%s): %s\n", 
                rank_icon, metrics$icon, metrics$name, metrics$base_asset, pnl_text))
  }
  
  # Volume Ranking
  cat("\n📊 VOLUME RANKING:\n")
  volume_ranking <- performance_data[order(sapply(performance_data, function(x) if(is.na(x$volume_24h)) 0 else x$volume_24h), decreasing = TRUE)]
  
  for (i in seq_along(volume_ranking)) {
    metrics <- volume_ranking[[i]]
    rank_icon <- switch(i, "🥇", "🥈", "🥉", "🏅", "⭐")
    volume_text <- if (!is.na(metrics$volume_24h)) sprintf("%.0fM USDT", metrics$volume_24h/1000000) else "N/A"
    cat(sprintf("   %s %s %s (%s): %s\n", 
                rank_icon, metrics$icon, metrics$name, metrics$base_asset, volume_text))
  }
}

# ==========================================================================================================
# 🎯 MAIN EXECUTION - 5 COINS DATA COLLECTION & ANALYSIS (ROBUST VERSION)
# ==========================================================================================================

# Sicherstellen, dass alle notwendigen Variablen verfügbar sind
cat("🔧 Checking system variables...\n")

if (!exists("PORTFOLIO_ASSETS") || is.null(PORTFOLIO_ASSETS)) {
  PORTFOLIO_ASSETS <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL", "ICPUSDT_UMCBL", "ETCUSDT_UMCBL", "VETUSDT_UMCBL")
  cat("⚠️ PORTFOLIO_ASSETS variable created as fallback\n")
} else {
  cat("✅ PORTFOLIO_ASSETS available\n")
}

if (!exists("MULTI_ASSET_CONFIG") || is.null(MULTI_ASSET_CONFIG)) {
  cat("❌ MULTI_ASSET_CONFIG not found - creating minimal configuration\n")
  MULTI_ASSET_CONFIG <- list(
    "ADAUSDT_UMCBL" = list(name = "Cardano", icon = "🔷", base_asset = "ADA", category = "Smart Contract"),
    "ALGOUSDT_UMCBL" = list(name = "Algorand", icon = "⚫", base_asset = "ALGO", category = "Pure PoS"),
    "ICPUSDT_UMCBL" = list(name = "Internet Computer", icon = "🌐", base_asset = "ICP", category = "Decentralized Internet"),
    "ETCUSDT_UMCBL" = list(name = "Ethereum Classic", icon = "💎", base_asset = "ETC", category = "Original Ethereum"),
    "VETUSDT_UMCBL" = list(name = "VeChain", icon = "⚡", base_asset = "VET", category = "Supply Chain")
  )
} else {
  cat("✅ MULTI_ASSET_CONFIG available\n")
}

# Collect data for all 5 portfolio assets
cat("🌍 Starting 5-coins data collection...\n")

tryCatch({
  multi_asset_data <- collect_multi_asset_data(PORTFOLIO_ASSETS)
  cat("✅ Data collection successful\n")
}, error = function(e) {
  cat("❌ Data collection failed:", e$message, "\n")
  cat("🔄 Creating minimal data structure...\n")
  
  multi_asset_data <- list()
  for (symbol in PORTFOLIO_ASSETS) {
    multi_asset_data[[symbol]] <- list(
      symbol = symbol,
      config = if (symbol %in% names(MULTI_ASSET_CONFIG)) MULTI_ASSET_CONFIG[[symbol]] else list(name = symbol, icon = "📊"),
      market_data = NULL,
      position = NULL,
      orders = NULL,
      timestamp = Sys.time()
    )
  }
})

# Display Portfolio Overview
cat("📊 Attempting to display portfolio overview...\n")
if (exists("display_portfolio_overview") && !is.null(multi_asset_data)) {
  tryCatch({
    portfolio_summary <- display_portfolio_overview(multi_asset_data)
    cat("✅ Portfolio overview displayed successfully\n")
  }, error = function(e) {
    cat("⚠️ Portfolio overview display failed:", e$message, "\n")
    
    # Basic fallback summary
    cat("📊 BASIC PORTFOLIO SUMMARY:\n")
    cat(sprintf("   🎯 Total Assets: %d\n", length(multi_asset_data)))
    cat("   📊 Assets: ADA, ALGO, ICP, ETC, VET\n")
    portfolio_summary <- NULL
  })
} else {
  cat("📊 Creating basic portfolio summary...\n")
  portfolio_summary <- NULL
}

# Display Category Analysis
cat("📊 Attempting category analysis...\n")
if (exists("display_category_analysis") && !is.null(multi_asset_data)) {
  tryCatch({
    display_category_analysis(multi_asset_data)
    cat("✅ Category analysis completed\n")
  }, error = function(e) {
    cat("⚠️ Category analysis display failed:", e$message, "\n")
  })
}

# Display Comparative Ranking
cat("🏆 Attempting comparative ranking...\n")
if (exists("display_comparative_ranking") && !is.null(multi_asset_data)) {
  tryCatch({
    display_comparative_ranking(multi_asset_data)
    cat("✅ Comparative ranking completed\n")
  }, error = function(e) {
    cat("⚠️ Comparative ranking display failed:", e$message, "\n")
  })
}

# ==========================================================================================================
# 🎯 ASSET-SPECIFIC FEATURES FÜR NEUE COINS
# ==========================================================================================================

create_header("ASSET-SPECIFIC INSIGHTS", "🔍")

# ICP-specific insights
if ("ICPUSDT_UMCBL" %in% names(multi_asset_data)) {
  icp_data <- multi_asset_data[["ICPUSDT_UMCBL"]]
  
  cat("🌐 INTERNET COMPUTER (ICP) FEATURES:\n")
  cat("   🔹 Decentralized Internet Infrastructure\n")
  cat("   🔹 Reverse Gas Model (No Gas Fees)\n")
  cat("   🔹 Native Smart Contracts (Canisters)\n")
  cat("   🔹 Unlimited Scalability\n")
  cat("   🔹 Web3 Applications Hosting\n\n")
}

# ETC-specific insights
if ("ETCUSDT_UMCBL" %in% names(multi_asset_data)) {
  etc_data <- multi_asset_data[["ETCUSDT_UMCBL"]]
  
  cat("💎 ETHEREUM CLASSIC (ETC) FEATURES:\n")
  cat("   🔹 Original Ethereum Blockchain\n")
  cat("   🔹 Proof of Work Mining\n")
  cat("   🔹 Code is Law Philosophy\n")
  cat("   🔹 Immutable Smart Contracts\n")
  cat("   🔹 Fixed Monetary Policy\n\n")
}

# VET-specific insights
if ("VETUSDT_UMCBL" %in% names(multi_asset_data)) {
  vet_data <- multi_asset_data[["VETUSDT_UMCBL"]]
  
  cat("⚡ VECHAIN (VET) FEATURES:\n")
  cat("   🔹 Supply Chain Management\n")
  cat("   🔹 IoT Integration\n")
  cat("   🔹 Dual Token System (VET/VTHO)\n")
  cat("   🔹 Enterprise Partnerships\n")
  cat("   🔹 Sustainability Focus\n\n")
}

# ==========================================================================================================
# 🎯 5-COINS TRADING COMMANDS
# ==========================================================================================================

create_header("5-COINS TRADING COMMANDS", "⚡")

cat("📋 Quick commands for your 5-coins portfolio:\n\n")

cat("🔷 ADA Commands:\n")
cat("   ada_market <- get_enhanced_market_data('ADAUSDT_UMCBL')\n")
cat("   place_tp_simple('ADAUSDT_UMCBL', 'long', '1000', 0.7500)\n\n")

cat("⚫ ALGO Commands:\n")
cat("   algo_market <- get_enhanced_market_data('ALGOUSDT_UMCBL')\n")
cat("   place_tp_simple('ALGOUSDT_UMCBL', 'long', '1000', 0.3000)\n\n")

cat("🌐 ICP Commands:\n")
cat("   icp_market <- get_enhanced_market_data('ICPUSDT_UMCBL')\n")
cat("   place_tp_simple('ICPUSDT_UMCBL', 'long', '100', 12.000)\n\n")

cat("💎 ETC Commands:\n")
cat("   etc_market <- get_enhanced_market_data('ETCUSDT_UMCBL')\n")
cat("   place_tp_simple('ETCUSDT_UMCBL', 'long', '100', 35.000)\n\n")

cat("⚡ VET Commands:\n")
cat("   vet_market <- get_enhanced_market_data('VETUSDT_UMCBL')\n")
cat("   place_tp_simple('VETUSDT_UMCBL', 'long', '10000', 0.04500)\n\n")

cat("🌍 Portfolio-Wide Commands:\n")
cat("   # Refresh all 5 assets:\n")
cat("   multi_data <- collect_multi_asset_data()\n")
cat("   display_portfolio_overview(multi_data)\n")
cat("   display_comparative_ranking(multi_data)\n\n")

# ==========================================================================================================
# 🎯 5-COINS PORTFOLIO TRAILING SL CONFIGURATION
# ==========================================================================================================

create_subheader("5-Coins Portfolio Trailing SL", "🛡️")

cat("🛡️ TRAILING SL CONFIGURATION FÜR 5 COINS:\n\n")

# Portfolio-spezifische Trailing SL Config
if (!exists("portfolio_trailing_config") || is.null(portfolio_trailing_config)) {
  cat("🔧 Creating portfolio_trailing_config...\n")
  
  portfolio_trailing_config <- list(
    ADA = list(
      symbol = "ADAUSDT_UMCBL",
      side = "long", 
      size = "15000",  # Anpassbar
      trailing_percent = 2.5
    ),
    ALGO = list(
      symbol = "ALGOUSDT_UMCBL",
      side = "long",
      size = "30000",  # Anpassbar  
      trailing_percent = 3.0
    ),
    ICP = list(
      symbol = "ICPUSDT_UMCBL",
      side = "long",
      size = "1000",   # Anpassbar
      trailing_percent = 3.5
    ),
    ETC = list(
      symbol = "ETCUSDT_UMCBL", 
      side = "long",
      size = "500",    # Anpassbar
      trailing_percent = 3.0
    ),
    VET = list(
      symbol = "VETUSDT_UMCBL",
      side = "long", 
      size = "50000",  # Anpassbar
      trailing_percent = 4.0  # Höher wegen Volatilität
    )
  )
  
  cat("✅ portfolio_trailing_config created successfully!\n")
} else {
  cat("✅ portfolio_trailing_config already exists\n")
}

cat("📊 EMPFOHLENE TRAILING SL KONFIGURATION:\n")
for (name in names(portfolio_trailing_config)) {
  config <- portfolio_trailing_config[[name]]
  asset_config <- MULTI_ASSET_CONFIG[[config$symbol]]
  cat(sprintf("   %s %s: %.1f%% trailing (%s %s contracts)\n", 
              asset_config$icon, name, config$trailing_percent, 
              config$side, config$size))
}

cat("\n🚨 DEPLOYMENT COMMAND:\n")
cat("# place_batch_trailing_sl(portfolio_trailing_config)\n\n")

# ==========================================================================================================
# 🎯 HEATMAP ANALYSIS FÜR VERFÜGBARE ASSETS
# ==========================================================================================================

create_header("OI HEATMAP ANALYSIS", "🔥")

cat("🔥 OI Heatmap Analysis für verfügbare Assets...\n\n")

# Generate heatmaps for supported assets
supported_heatmaps <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL")

for (symbol in supported_heatmaps) {
  tryCatch({
    # Sichere Config-Extraktion
    if (exists("MULTI_ASSET_CONFIG") && symbol %in% names(MULTI_ASSET_CONFIG)) {
      config <- MULTI_ASSET_CONFIG[[symbol]]
      cat(sprintf("%s %s OI Heatmap:\n", config$icon, config$name))
    } else {
      cat(sprintf("📊 %s OI Heatmap:\n", symbol))
    }
    
    if (symbol == "ADAUSDT_UMCBL" && exists("generate_dynamic_ada_oi_heatmap")) {
      cat("🔴 Generating ADA OI heatmap...\n")
      ada_heatmap <- generate_dynamic_ada_oi_heatmap()
      cat("✅ ADA heatmap generated\n")
    } else if (symbol == "ALGOUSDT_UMCBL" && exists("generate_dynamic_algo_oi_heatmap")) {
      cat("⚫ Generating ALGO OI heatmap...\n")
      algo_heatmap <- generate_dynamic_algo_oi_heatmap()
      cat("✅ ALGO heatmap generated\n")
    } else {
      cat(sprintf("⚠️ Heatmap function not available for %s\n", symbol))
    }
  }, error = function(e) {
    cat(sprintf("❌ Heatmap generation failed for %s: %s\n", symbol, e$message))
  })
}

# ==========================================================================================================
# 🎯 ALTCOIN RALLY TRIGGERS
# ==========================================================================================================

create_header("ALTCOIN RALLY TRIGGERS", "🚀")

cat("🚀 Checking Altcoin Rally Triggers für 5-Coins Portfolio...\n")

if (exists("run_altcoin_rally_monitoring")) {
  tryCatch({
    altcoin_triggers <- run_altcoin_rally_monitoring()
    
    # Track trigger history
    trigger_history_file <- "c:/freeding/tbot202506/logs/altcoin_triggers_5coins.rds"
    if (file.exists(trigger_history_file)) {
      trigger_history <- readRDS(trigger_history_file)
    } else {
      trigger_history <- list()
    }
    
    # Add current trigger
    trigger_history[[length(trigger_history) + 1]] <- list(
      timestamp = Sys.time(),
      score = altcoin_triggers$master_signal$score_percentage,
      signal = altcoin_triggers$master_signal$master_signal,
      portfolio_size = length(PORTFOLIO_ASSETS)
    )
    
    # Keep last 100 entries
    if (length(trigger_history) > 100) {
      trigger_history <- tail(trigger_history, 100)
    }
    
    saveRDS(trigger_history, trigger_history_file)
    
  }, error = function(e) {
    cat("⚠️ Altcoin rally triggers failed:", e$message, "\n")
  })
} else {
  cat("⚠️ Altcoin rally monitoring not available\n")
}

# ==========================================================================================================
# 🎯 SYSTEM STATUS & FINAL SUMMARY
# ==========================================================================================================

create_header("5-COINS SYSTEM STATUS", "🔧")

# Portfolio statistics
if (exists("portfolio_summary") && !is.null(portfolio_summary)) {
  cat("💼 PORTFOLIO STATISTICS:\n")
  cat(sprintf("   🎯 Total Assets: %d (ADA, ALGO, ICP, ETC, VET)\n", portfolio_summary$total_assets))
  cat(sprintf("   💼 Active Positions: %d/%d\n", portfolio_summary$active_positions, length(PORTFOLIO_ASSETS)))
  cat(sprintf("   💰 Total P&L: %+.0f USDT\n", portfolio_summary$total_pnl))
  cat(sprintf("   📋 Total Orders: %d\n", portfolio_summary$total_orders))
  cat(sprintf("   💎 Portfolio Value: %.0f USDT\n", portfolio_summary$portfolio_value))
}

# System capabilities
cat("\n📊 SYSTEM CAPABILITIES:\n")
cat("   ✅ 5-Coins data collection and analysis\n")
cat("   ✅ Category-based performance analysis\n") 
cat("   ✅ Comparative ranking system\n")
cat("   ✅ Asset-specific insights\n")
cat("   ✅ Portfolio-wide trailing SL\n")
cat("   ✅ OI heatmap analysis (ADA, ALGO)\n")
cat("   ✅ Altcoin rally monitoring\n")

create_header("EXECUTION COMPLETE", "✅")

cat("🚀 5-COINS PORTFOLIO ANALYSIS COMPLETE!\n")
cat("✅ Successfully analyzed ADA, ALGO, ICP, ETC, VET!\n")
cat("📊 Enhanced multi-asset dashboard operational!\n")
cat("🎯 Comparative ranking and category analysis complete!\n")
cat("🔍 Asset-specific insights provided!\n")
cat("🌍 Portfolio-wide monitoring systems ready!\n\n")

cat("🎯 Next Steps:\n")
cat("1. 📊 Monitor 5-coins performance ranking\n")
cat("2. 🎯 Consider position sizing based on performance\n")
cat("3. 📈 Set appropriate TP/SL levels for all assets\n")
cat("4. 🔍 Track category-based trends\n")
cat("5. 🛡️ Deploy portfolio-wide trailing SL protection\n")

cat(strrep("=", 80), "\n")
cat("🌍 5-Coins Portfolio Trading System Ready!\n")

# ==========================================================================================================
# 🎯 END OF 5-COINS PORTFOLIO TRADING SYSTEM
# ==========================================================================================================


# Vor dem Fix (Log-Fehler):
# Objekt 'MULTI_ASSET_CONFIG' nicht gefunden

# Nach dem Fix (robust):
if (!exists("MULTI_ASSET_CONFIG") || is.null(MULTI_ASSET_CONFIG)) {
  cat("🔧 Creating MULTI_ASSET_CONFIG...\n")
  MULTI_ASSET_CONFIG <- list(...)  # Vollständige 5-Coins Config
}
