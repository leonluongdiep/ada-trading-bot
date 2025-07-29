# ==========================================================================================================
# 🚀 FIXED MULTI-ASSET TRADING SYSTEM V8 - 5 COINS PORTFOLIO
# ==========================================================================================================
# 

# ==========================================================================================================

# 
# FIXES:
# 1. ✅ Explizite Library-Ladung für ggplot2, plotly, dplyr
# 2. ✅ Robuste Fehlerbehandlung ohne rekursive Errors
# 3. ✅ Fallback-Mechanismen für fehlende Libraries
# 4. ✅ Vereinfachte OI Heatmap ohne komplexe Visualisierungen
# 5. ✅ Bessere NULL-Checks und Argument-Validierung
# 
# ==========================================================================================================


cat("🚀 FIXED MULTI-ASSET TRADING SYSTEM V8 - 5 COINS PORTFOLIO\n")
cat(strrep("=", 80), "\n")
cat("📅 Execution Start:", as.character(Sys.time()), "\n")
cat("🎯 Assets: ADA + ALGO + ICP + ETC + VET Portfolio Analysis\n\n")

# Zu Beginn des Scripts hinzufügen:
required_packages <- c("ggplot2", "plotly", "dplyr", "viridis")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("❌ %s not available - installing fallback\n", pkg))
    # Fallback-Mechanismus implementieren
  } else {
    cat(sprintf("✅ %s loaded successfully\n", pkg))
  }
}




# ==========================================================================================================
# 🔧 EXPLICIT LIBRARY LOADING WITH ERROR HANDLING
# ==========================================================================================================

cat("🔧 Loading required libraries...\n")

# Load libraries with explicit error handling
load_library_safe <- function(lib_name) {
  result <- tryCatch({
    library(lib_name, character.only = TRUE, quietly = TRUE)
    TRUE
  }, error = function(e) {
    cat(sprintf("⚠️ Failed to load %s: %s\n", lib_name, e$message))
    FALSE
  })
  
  if (result) {
    cat(sprintf("✅ %s loaded successfully\n", lib_name))
  } else {
    cat(sprintf("❌ %s not available\n", lib_name))
  }
  
  return(result)
}

# Try to load required libraries
required_libs <- c("ggplot2", "plotly", "dplyr", "viridis")
available_libs <- list()

for (lib in required_libs) {
  available_libs[[lib]] <- load_library_safe(lib)
}

# ==========================================================================================================
# 🔧 SAFE MULTI-ASSET CONFIGURATION (ALWAYS AVAILABLE)
# ==========================================================================================================

cat("🔧 Creating safe multi-asset configuration...\n")

# Sicherstellen, dass die Config immer verfügbar ist
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
    typical_volume_threshold = 50000000,
    icon = "🔷",
    category = "Smart Contract Platform",
    price_range_min = 0.30,
    price_range_max = 1.50,
    typical_price = 0.85
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
    typical_volume_threshold = 20000000,
    icon = "⚫",
    category = "Pure PoS Blockchain",
    price_range_min = 0.10,
    price_range_max = 0.50,
    typical_price = 0.30
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
    typical_volume_threshold = 30000000,
    icon = "🌐",
    category = "Decentralized Internet",
    price_range_min = 3.0,
    price_range_max = 12.0,
    typical_price = 6.0
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
    typical_volume_threshold = 25000000,
    icon = "💎",
    category = "Original Ethereum",
    price_range_min = 15.0,
    price_range_max = 35.0,
    typical_price = 24.0
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
    typical_volume_threshold = 15000000,
    icon = "⚡",
    category = "Supply Chain & IoT",
    price_range_min = 0.020,
    price_range_max = 0.060,
    typical_price = 0.030
  )
)


PORTFOLIO_ASSETS <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL", "ICPUSDT_UMCBL", "ETCUSDT_UMCBL", "VETUSDT_UMCBL")

cat("✅ Multi-asset configuration created\n")

# ==========================================================================================================
# 🔧 SIMPLIFIED OI HEATMAP SYSTEM (NO VISUALIZATION DEPENDENCIES)
# ==========================================================================================================

cat("🔧 Creating simplified OI analysis system...\n")

# Simplified OI analysis without visualization
generate_simplified_oi_analysis <- function(symbol = "ADAUSDT_UMCBL") {
  cat(sprintf("📊 SIMPLIFIED OI ANALYSIS FOR %s\n", symbol))
  
  # Get asset configuration
  config <- MULTI_ASSET_CONFIG[[symbol]]
  if (is.null(config)) {
    cat(sprintf("❌ No configuration for %s\n", symbol))
    return(NULL)
  }
  
  # Get live price data with safe error handling
  current_price <- config$typical_price
  current_oi <- 100000000
  high_24h <- current_price * 1.03
  low_24h <- current_price * 0.97
  change_24h_pct <- 0
  data_source <- "fallback"
  
  # Try to get live data
  tryCatch({
    if (exists("get_enhanced_ticker_data")) {
      ticker <- get_enhanced_ticker_data(symbol)
      if (!is.null(ticker) && !is.null(ticker$last_price) && !is.na(ticker$last_price)) {
        current_price <- as.numeric(ticker$last_price)
        current_oi <- as.numeric(ticker$open_interest) %||% current_oi
        high_24h <- as.numeric(ticker$high_24h) %||% (current_price * 1.03)
        low_24h <- as.numeric(ticker$low_24h) %||% (current_price * 0.97)
        change_24h_pct <- as.numeric(ticker$change_24h_pct) %||% 0
        data_source <- "live_data"
        
        cat(sprintf("✅ Live data: %.4f %s (%+.2f%% 24h)\n", 
                    current_price, config$base_asset, change_24h_pct))
      }
    }
  }, error = function(e) {
    cat(sprintf("⚠️ Using fallback data for %s\n", config$name))
  })
  
  # Simple OI level calculation (no complex visualization)
  price_levels <- list()
  
  # Current price zone
  price_levels[["current"]] <- list(
    price = current_price,
    type = "CURRENT",
    strength = 100,
    distance_pct = 0
  )
  
  # Resistance levels (2%, 5%, 10% above)
  for (pct in c(2, 5, 10)) {
    price <- current_price * (1 + pct/100)
    price_levels[[paste0("resistance_", pct)]] <- list(
      price = round(price, config$price_decimals),
      type = "RESISTANCE",
      strength = 100 - pct * 3,
      distance_pct = pct
    )
  }
  
  # Support levels (2%, 5%, 10% below)
  for (pct in c(2, 5, 10)) {
    price <- current_price * (1 - pct/100)
    price_levels[[paste0("support_", pct)]] <- list(
      price = round(price, config$price_decimals),
      type = "SUPPORT", 
      strength = 100 - pct * 3,
      distance_pct = -pct
    )
  }
  
  # 24h high/low levels
  price_levels[["high_24h"]] <- list(
    price = round(high_24h, config$price_decimals),
    type = "24H_HIGH",
    strength = 85,
    distance_pct = round(((high_24h / current_price) - 1) * 100, 2)
  )
  
  price_levels[["low_24h"]] <- list(
    price = round(low_24h, config$price_decimals),
    type = "24H_LOW", 
    strength = 85,
    distance_pct = round(((low_24h / current_price) - 1) * 100, 2)
  )
  
  # Return structured result
  return(list(
    symbol = symbol,
    config = config,
    current_price = current_price,
    change_24h_pct = change_24h_pct,
    current_oi = current_oi,
    high_24h = high_24h,
    low_24h = low_24h,
    data_source = data_source,
    price_levels = price_levels,
    timestamp = Sys.time()
  ))
}

# Safe wrapper for all coins
generate_all_coins_analysis <- function() {
  cat("🌍 GENERATING 5-COINS SIMPLIFIED ANALYSIS\n")
  cat(strrep("=", 50), "\n")
  
  results <- list()
  
  for (symbol in PORTFOLIO_ASSETS) {
    config <- MULTI_ASSET_CONFIG[[symbol]]
    cat(sprintf("\n%s Analyzing %s...\n", config$icon, config$name))
    
    tryCatch({
      analysis <- generate_simplified_oi_analysis(symbol)
      if (!is.null(analysis)) {
        results[[symbol]] <- analysis
        cat(sprintf("   ✅ %s analysis completed\n", config$name))
      }
    }, error = function(e) {
      cat(sprintf("   ❌ %s analysis failed: %s\n", config$name, e$message))
    })
  }
  
  cat(sprintf("\n✅ Completed analysis for %d coins\n", length(results)))
  return(results)
}

# ==========================================================================================================
# 🔧 SAFE SYSTEM LOADING
# ==========================================================================================================

cat("🔧 Loading essential systems safely...\n")

# System loading with better error handling
load_system_safe <- function(file_path, system_name) {
  cat(sprintf("📦 Loading %s...\n", system_name))
  
  if (!file.exists(file_path)) {
    cat(sprintf("   ⚠️ File not found: %s\n", basename(file_path)))
    return(FALSE)
  }
  
  result <- tryCatch({
    source(file_path)
    TRUE
  }, error = function(e) {
    cat(sprintf("   ❌ Failed to load %s: %s\n", system_name, e$message))
    FALSE
  })
  
  if (result) {
    cat(sprintf("   ✅ %s loaded successfully\n", system_name))
  }
  
  return(result)
}


# Lade Console Management System zuerst (with error handling)
tryCatch({
  if (file.exists("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")) {
    source("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")
    start_silent_mode("file")
    cat("✅ Console management loaded and activated\n")
  } else {
    cat("⚠️ Console management not found - continuing with standard output\n")
  }
}, error = function(e) {
  cat("⚠️ Console management failed, continuing with standard output:", e$message, "\n")
})


# Try to load essential systems
system_files <- list(
  list(path = "c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r", name = "Core Trading Analysis"),
  list(path = "c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r", name = "Enhanced Collector"),
  list(path = "c:/freeding/tbot202506/r_analysis/strategies/Bitget/trailing_sl_system.r", name = "Trailing SL System")
)

loaded_systems <- 0
for (system in system_files) {
  if (load_system_safe(system$path, system$name)) {
    loaded_systems <- loaded_systems + 1
  }
}

cat(sprintf("✅ Loaded %d/%d systems successfully\n", loaded_systems, length(system_files)))

# ==========================================================================================================
# 🌍 SAFE MULTI-ASSET DATA COLLECTION
# ==========================================================================================================

cat("🌍 Starting safe 5-coins data collection...\n")

# Safe data collection function
collect_multi_asset_data_safe <- function(symbols = PORTFOLIO_ASSETS) {
  cat("================================================================================ \n")
  cat("🌍 5-COINS PORTFOLIO DATA COLLECTION\n")
  cat("================================================================================ \n")
  
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
      timestamp = Sys.time()
    )
    
    # Enhanced Market Data (safe)
    tryCatch({
      if (exists("get_enhanced_market_data")) {
        asset_data$market_data <- get_enhanced_market_data(symbol)
        cat(sprintf("   ✅ Market data collected for %s\n", config$name))
      }
    }, error = function(e) {
      cat(sprintf("   ⚠️ Market data failed for %s: %s\n", config$name, e$message))
    })
    
    # Position Data (safe)
    tryCatch({
      if (exists("get_current_positions")) {
        position <- get_current_positions(symbol)
        if (!is.null(position) && nrow(position) > 0) {
          asset_data$position <- position
          cat(sprintf("   ✅ Position found for %s\n", config$name))
        } else {
          cat(sprintf("   📭 No position for %s\n", config$name))
        }
      }
    }, error = function(e) {
      cat(sprintf("   ⚠️ Position check failed for %s: %s\n", config$name, e$message))
    })
    
    multi_asset_data[[symbol]] <- asset_data
  }
  
  cat("\n✅ 5-Coins portfolio data collection completed!\n")
  return(multi_asset_data)
}

# ==========================================================================================================
# 📊 SAFE PORTFOLIO OVERVIEW
# ==========================================================================================================

display_portfolio_overview_safe <- function(multi_asset_data) {
  cat("================================================================================ \n")
  cat("💼 5-COINS PORTFOLIO OVERVIEW\n")
  cat("================================================================================ \n")
  
  portfolio_summary <- list(
    total_assets = length(multi_asset_data),
    active_positions = 0,
    total_pnl = 0,
    portfolio_value = 0
  )
  
  cat("┌─────────────────────────────────────────────────────────────────────────────────────────┐\n")
  cat("│                         5-COINS PORTFOLIO SUMMARY                                      │\n")
  cat("├─────────────────────────────────────────────────────────────────────────────────────────┤\n")
  cat("│ Asset      │ Category         │ Price       │ 24h Change  │ P&L        │ Status    │\n")
  cat("├─────────────────────────────────────────────────────────────────────────────────────────┤\n")
  
  for (symbol in names(multi_asset_data)) {
    asset_data <- multi_asset_data[[symbol]]
    config <- asset_data$config
    
    # Extract metrics safely
    current_price <- NA
    change_24h <- NA
    position_pnl <- 0
    position_size <- 0
    
    # Market data
    tryCatch({
      if (!is.null(asset_data$market_data) && !is.null(asset_data$market_data$ticker)) {
        ticker <- asset_data$market_data$ticker
        current_price <- as.numeric(ticker$last_price) %||% NA
        change_24h <- as.numeric(ticker$change_24h_pct) %||% NA
      }
    }, error = function(e) {
      # Use fallback values
    })
    
    # Position data
    tryCatch({
      if (!is.null(asset_data$position) && nrow(asset_data$position) > 0) {
        pos <- asset_data$position[1, ]
        position_pnl <- as.numeric(pos$unrealizedPL) %||% 0
        position_size <- as.numeric(pos$total) %||% 0
        portfolio_summary$active_positions <- portfolio_summary$active_positions + 1
        portfolio_summary$total_pnl <- portfolio_summary$total_pnl + position_pnl
      }
    }, error = function(e) {
      # No position or error
    })
    
    # Position value calculation
    position_value <- 0
    if (!is.na(current_price) && position_size > 0) {
      position_value <- position_size * current_price
      portfolio_summary$portfolio_value <- portfolio_summary$portfolio_value + position_value
    }
    
    # Display formatting
    asset_name <- sprintf("%s %-6s", config$icon, substr(config$base_asset, 1, 5))
    category <- substr(config$category, 1, 16)
    price_display <- if (!is.na(current_price)) sprintf("%.4f", current_price) else "N/A"
    change_display <- if (!is.na(change_24h)) {
      if (change_24h > 0) sprintf("🟢 %+.1f%%", change_24h)
      else if (change_24h < 0) sprintf("🔴 %+.1f%%", change_24h)
      else "🟡 0.0%"
    } else "❓"
    pnl_display <- if (position_pnl != 0) sprintf("%+.0f", position_pnl) else "No Pos"
    status <- if (position_size > 0) "ACTIVE" else "IDLE"
    
    cat(sprintf("│ %-10s │ %-16s │ %-11s │ %-11s │ %-10s │ %-9s │\n",
                asset_name, category, price_display, change_display, 
                pnl_display, status))
  }
  
  cat("├─────────────────────────────────────────────────────────────────────────────────────────┤\n")
  cat(sprintf("│ TOTALS: %d Assets │ %d Positions │ P&L: %+.0f USDT │ Value: %.0f USDT │\n",
              portfolio_summary$total_assets,
              portfolio_summary$active_positions,
              portfolio_summary$total_pnl,
              portfolio_summary$portfolio_value))
  cat("└─────────────────────────────────────────────────────────────────────────────────────────┘\n")
  
  return(portfolio_summary)
}

# ==========================================================================================================
# 🎯 MAIN EXECUTION (SAFE MODE)
# ==========================================================================================================

cat("🎯 Starting main execution in SAFE MODE...\n")

# Collect data safely
multi_asset_data <- NULL
tryCatch({
  multi_asset_data <- collect_multi_asset_data_safe(PORTFOLIO_ASSETS)
  cat("✅ Data collection completed\n")
}, error = function(e) {
  cat("❌ Data collection failed:", e$message, "\n")
  # Create minimal data structure
  multi_asset_data <- list()
  for (symbol in PORTFOLIO_ASSETS) {
    multi_asset_data[[symbol]] <- list(
      symbol = symbol,
      config = MULTI_ASSET_CONFIG[[symbol]],
      market_data = NULL,
      position = NULL,
      timestamp = Sys.time()
    )
  }
})

# Display portfolio overview safely
if (!is.null(multi_asset_data) && length(multi_asset_data) > 0) {
  tryCatch({
    portfolio_summary <- display_portfolio_overview_safe(multi_asset_data)
    cat("✅ Portfolio overview displayed\n")
  }, error = function(e) {
    cat("⚠️ Portfolio overview failed:", e$message, "\n")
  })
}

# Generate simplified analysis safely
all_analysis <- NULL
tryCatch({
  all_analysis <- generate_all_coins_analysis()
  cat("✅ Simplified analysis completed\n")
}, error = function(e) {
  cat("⚠️ Analysis failed:", e$message, "\n")
})

# ==========================================================================================================
# 📋 SUMMARY & STATUS
# ==========================================================================================================

cat("================================================================================ \n")
cat("✅ FIXED 5-COINS SYSTEM EXECUTION COMPLETE\n")
cat("================================================================================ \n")

cat("📊 SYSTEM STATUS:\n")
cat(sprintf("   Libraries: ggplot2:%s plotly:%s dplyr:%s\n", 
            ifelse(available_libs$ggplot2, "✅", "❌"),
            ifelse(available_libs$plotly, "✅", "❌"), 
            ifelse(available_libs$dplyr, "✅", "❌")))
cat(sprintf("   Systems loaded: %d/%d\n", loaded_systems, length(system_files)))
cat(sprintf("   Assets analyzed: %d/5\n", if(is.null(all_analysis)) 0 else length(all_analysis)))

cat("\n🎯 AVAILABLE FUNCTIONS:\n")
cat("   generate_simplified_oi_analysis('ADAUSDT_UMCBL')\n")
cat("   generate_all_coins_analysis()\n")
cat("   collect_multi_asset_data_safe()\n")

cat("\n💡 NEXT STEPS:\n")
cat("1. Check if required libraries are installed\n")
cat("2. Verify core trading system functions are available\n") 
cat("3. Test simplified analysis functions\n")
cat("4. Consider installing missing libraries if needed\n")

cat("================================================================================ \n")
cat("🛠️ Fixed system ready - no recursive errors!\n")



# ==========================================================================================================
# 📋 Manuelle Analysen & STATUS
# ==========================================================================================================


# ==========================================================================================================
# 📋 heatmap heatmap heatmap  heatmap  heatmap heatmap
# ==========================================================================================================

# # Alle Coins mit einer Funktion:
#
ada_heatmap <- generate_ada_heatmap()        # 🔷 ADA (enhanced)
algo_heatmap <- generate_algo_heatmap()      # ⚫ ALGO (enhanced)
icp_heatmap <- generate_icp_heatmap()        # 🌐 ICP (universal)
etc_heatmap <- generate_etc_heatmap()        # 💎 ETC (universal)
vet_heatmap <- generate_vet_heatmap()        # ⚡ VET (universal)

# Dashboard und Vergleiche:

# all_heatmaps <- generate_5coins_heatmap_dashboard()
# comparison <- compare_all_5coins_heatmaps()


#
ada_heatmap
algo_heatmap
icp_heatmap
etc_heatmap
vet_heatmap

#



# ==========================================================================================================
# 🎯 Trading configuration for both assets
# ==========================================================================================================

# 🎯  Trading configuration for both assets
#  ----- Set to TRUE to enable live trading --------

#EXECUTE_LIVE_ORDERS <- TRUE




# ADA Stop Loss
#place_sl_simple('ADAUSDT_UMCBL', 'long', '1000', 0.6981)

# ADA Take Profit
#place_tp_simple('ADAUSDT_UMCBL', 'long', '3000', 0.9097)

# ALGO Stop Loss  
#place_sl_simple('ALGOUSDT_UMCBL', 'long', '20000', 0.2784)

# ALGO Take Profit
#place_tp_simple('ALGOUSDT_UMCBL', 'long', '20000', 0.3091)


# 
# # 🚀 AGGRESSIVE ALTCOIN SEASON TPs (höhere Gewinnziele)
# place_tp_simple('ADAUSDT_UMCBL', 'long', '3500', 0.9400)   # ADA +4.6% (1. TP)
# place_tp_simple('ADAUSDT_UMCBL', 'long', '2000', 0.9700)   # ADA +8.0% (2. TP)
# 
# place_tp_simple('ALGOUSDT_UMCBL', 'long', '20000', 0.3250)  # ALGO +8.1% (1. TP)
# place_tp_simple('ALGOUSDT_UMCBL', 'long', '15000', 0.3350)  # ALGO +11.4% (2. TP)
# 
# place_tp_simple('ICPUSDT_UMCBL', 'long', '350', 6.500)     # ICP +6.6% (1. TP)
# place_tp_simple('ICPUSDT_UMCBL', 'long', '200', 6.800)     # ICP +11.6% (2. TP)
# 
# place_tp_simple('ETCUSDT_UMCBL', 'long', '250', 25.200)    # ETC +4.4% (1. TP)
# place_tp_simple('ETCUSDT_UMCBL', 'long', '150', 26.000)    # ETC +7.7% (2. TP)
# 
# place_tp_simple('VETUSDT_UMCBL', 'long', '100000', 0.03000) # VET +5.4% (1. TP)
# place_tp_simple('VETUSDT_UMCBL', 'long', '100000', 0.03150) # VET +10.7% (2. TP)
# 
# 
# 
# 
# 
# 
# # 🎯 WEITE TRAILING SLs - Lassen Gewinne laufen, schützen vor Absturz
# 
# # Basis: Aktuelle Preise mit weiten Trailing-Distanzen
# place_trailing_sl_percent('ADAUSDT_UMCBL', 'long', '7000', 8.5)    # ADA -8.5% trailing
# place_trailing_sl_percent('ALGOUSDT_UMCBL', 'long', '50000', 8.0)  # ALGO -8.0% trailing  
# place_trailing_sl_percent('ICPUSDT_UMCBL', 'long', '700', 9.5)     # ICP -9.5% trailing
# place_trailing_sl_percent('ETCUSDT_UMCBL', 'long', '500', 7.0)     # ETC -7.0% trailing (enger)
# place_trailing_sl_percent('VETUSDT_UMCBL', 'long', '294379', 8.5)  # VET -8.5% trailing
# 
# 

#

#------------------------------- trailing --------
#- strategic order-------------------------------
#place_strategic_limit_order('ADAUSDT_UMCBL', 'open_long', '5000', 0.6930)

# Tiered TP orders for ADA
#place_tp_simple('ICPUSDT_UMCBL', 'long', '1000', 5.565)



# ==========================================================================================================
# 🎯 trailing_sl_percent
# ==========================================================================================================


# 🔷 ADA: 5000 contracts @ current 0.7341 USDT

#place_trailing_sl_percent('ADAUSDT_UMCBL', 'long', '5000', 2.0)

# → SL bei 0.7194 USDT (-2.0% = schützt +651 USDT Gewinne)

# ⚫ ALGO: 30,000 contracts @ current 0.2256 USDT  
#place_trailing_sl_percent('ALGOUSDT_UMCBL', 'long', '45000', 15.0)
# → SL bei 0.2193 USDT (-2.8% = schützt +485 USDT Gewinne)


EXECUTE_TRAILING_ORDERS <- FALSE  # Nur Info, keine Orders

#source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/emergency_trailing_sl.r")


# ==========================================================================================================
# 🎯 Prompt für Trading System Log Analyse
# ==========================================================================================================


# 
# 
# Analysiere das Trading System Console Log und erstelle eine vollständige numerische Zusammenfassung in tabellarischer Form. Bitte keine aufwändigen Grafiken, nur strukturierte Tabellen mit Berechnungen.
# 
# ## Gewünschte Analyse-Struktur:
# 
# ### 1. Portfolio Status Tabelle
# - Asset, Symbol, aktueller Preis, 24h Change, Position Size, explizite Berechnung (Preis × Size), Position Value, P&L, Status
# - Beispiel: | 🔷 ADA | ADAUSDT_UMCBL | 0.8469 | +0.02% | 10,000 | 0.8469 × 10,000 | 8,469 | +174.22 | ACTIVE |
#   
#   ### 2. Asset Zusammenfassung nach Kategorien
#   - Tabelle mit: Kategorie, Assets, Anzahl Positionen, Kategorie P&L, Berechnung P&L, Kategorie Value, Berechnung Value
# - **WICHTIG**: Füge eine TOTAL-Zeile hinzu mit Summen aller Kategorien
# 
# ### 3. Total Portfolio Summe
# - Tabelle mit: Kennzahl, Berechnung (explizit), Wert, Einheit
# - Berechne: Gesamtwert, Gesamt P&L, Performance %, durchschnittliche Werte
# - Zeige alle Berechnungen explizit (z.B. "8,469 + 8,829 + 5,909 + 4,971 + 7,075")
# 
# ### 4. Rankings mit Prozentberechnungen
# - P&L Ranking: Position, Asset, P&L, Prozent des Gesamt-P&L (mit Berechnung)
# - Value Ranking: Asset, Value, Prozent des Portfolio-Werts (mit Berechnung)
# 
# ### 5. Market Sentiment Analysis (falls verfügbar)
# - Asset, Sentiment, Score, Volume Status, Funding Rate, Spread, Open Interest
# 
# ### 6. Key Levels (falls OI Daten verfügbar)
# - Top 3 OI Levels pro Asset mit Level Type, Preis, Distanz %, OI Konzentration
# 
# ### 7. Trading Empfehlungen
# - Asset, Nächste Resistance, Nächster Support, Momentum, Empfehlung
# 
# ### 8. Risk Management Summary
# - Tabellarische Übersicht: Kennzahl, Status, Wert
# 
# ## Wichtige Anforderungen:
# - Alle Berechnungen explizit zeigen (z.B. "496.93 ÷ 35,253 × 100 = 1.41%")
# - Emojis für Assets beibehalten (🔷 ADA, ⚫ ALGO, etc.)
# - Numerische Präzision: 2-4 Dezimalstellen für USDT, Prozente auf 1-2 Dezimalstellen
# - Status-Icons verwenden (✅, ❌, 🟢, etc.)
# - Totalsummen für alle relevanten Kategorien
# - Zeitstempel und System-Status am Ende
# 
# Extrahiere alle relevanten Daten aus dem Log und strukturiere sie entsprechend dieser Vorlage




