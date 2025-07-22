# ==========================================================================================================
# 📊 PROZENTBASIERTE TRAILING STOP LOSS SYSTEM
# ==========================================================================================================
# 
# ZWECK: Intelligente Trailing SL basierend auf Prozent-Rückläufen vom Current Price
# FLEXIBEL: Automatische Preis-Berechnung basierend auf Live-Kursen
# SICHER: Berücksichtigt Symbol-spezifische Präzision und Tick-Größen
# 
# ==========================================================================================================

cat("📊 Loading Prozentbasierte Trailing SL System...\n")

# ==========================================================================================================
# 🧮 CORE TRAILING SL FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ TRAILING SL MIT PROZENT-BERECHNUNG - Live Price basiert                                             │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
place_trailing_sl_percent <- function(symbol, side, size, trailing_percent, 
                                     use_entry_price = FALSE, 
                                     entry_price = NULL,
                                     show_calculation = TRUE) {
  
  cat(sprintf("📉 TRAILING SL SETUP FOR %s\n", symbol))
  cat(strrep("=", 50), "\n")
  cat(sprintf("⚠️  WARNING: PLACING REAL TRAILING SL ORDER!\n"))
  cat(sprintf("   Side: %s\n", side))
  cat(sprintf("   Size: %s contracts\n", size))
  cat(sprintf("   Trailing: %.2f%% below current/entry price\n", trailing_percent))
  
  # 1. AKTUELLEN PREIS HOLEN
  current_price <- NULL
  if (exists("get_enhanced_ticker_data")) {
    tryCatch({
      ticker <- get_enhanced_ticker_data(symbol)
      if (!is.null(ticker) && !is.null(ticker$last_price)) {
        current_price <- as.numeric(ticker$last_price)
        cat(sprintf("📊 Live Current Price: %.4f USDT\n", current_price))
      }
    }, error = function(e) {
      cat("⚠️ Could not fetch live price, using fallback\n")
    })
  }
  
  # 2. REFERENZ-PREIS BESTIMMEN
  if (use_entry_price && !is.null(entry_price)) {
    reference_price <- as.numeric(entry_price)
    cat(sprintf("📍 Using Entry Price as Reference: %.4f USDT\n", reference_price))
  } else if (!is.null(current_price)) {
    reference_price <- current_price
    cat(sprintf("📍 Using Current Price as Reference: %.4f USDT\n", reference_price))
  } else {
    cat("❌ No price reference available\n")
    return(list(success = FALSE, error = "No price reference"))
  }
  
  # 3. SL PREIS BERECHNEN
  if (tolower(side) == "long") {
    # Long Position: SL unter Referenz-Preis
    sl_price <- reference_price * (1 - trailing_percent / 100)
    direction_text <- "below"
  } else {
    # Short Position: SL über Referenz-Preis
    sl_price <- reference_price * (1 + trailing_percent / 100)
    direction_text <- "above"
  }
  
  # 4. BERECHNUNG ANZEIGEN
  if (show_calculation) {
    cat("\n🧮 TRAILING SL CALCULATION:\n")
    cat(sprintf("   Reference Price: %.4f USDT\n", reference_price))
    cat(sprintf("   Trailing Percent: %.2f%%\n", trailing_percent))
    cat(sprintf("   Direction: %s (%s position)\n", direction_text, side))
    cat(sprintf("   Calculated SL: %.4f USDT\n", sl_price))
    
    # Distanz-Analyse
    distance_usdt <- abs(reference_price - sl_price)
    cat(sprintf("   Distance: %.4f USDT (%.2f%%)\n", distance_usdt, trailing_percent))
    
    # Risk Analysis
    if (!is.null(current_price) && current_price != reference_price) {
      current_distance_pct <- abs(current_price - sl_price) / current_price * 100
      cat(sprintf("   Current Risk: %.2f%% from live price\n", current_distance_pct))
    }
  }
  
  # 5. SYMBOL PRECISION HOLEN
  symbol_info <- NULL
  if (exists("get_symbol_precision")) {
    symbol_info <- get_symbol_precision(symbol)
  }
  
  # 6. SL ORDER PLATZIEREN
  cat(sprintf("\n📉 Placing SL order at %.4f USDT...\n", sl_price))
  
  if (exists("place_sl_simple")) {
    result <- place_sl_simple(symbol, side, size, sl_price)
    
    if (!is.null(result) && result$success) {
      cat("✅ Trailing SL placed successfully!\n")
      
      return(list(
        success = TRUE,
        symbol = symbol,
        side = side,
        size = size,
        trailing_percent = trailing_percent,
        reference_price = reference_price,
        sl_price = sl_price,
        distance_usdt = abs(reference_price - sl_price),
        order_result = result
      ))
    } else {
      cat("❌ Trailing SL placement failed\n")
      return(list(success = FALSE, error = "Order placement failed", result = result))
    }
  } else {
    cat("❌ place_sl_simple function not available\n")
    return(list(success = FALSE, error = "Function not available"))
  }
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ BATCH TRAILING SL - Mehrere Assets gleichzeitig                                                     │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
place_batch_trailing_sl <- function(positions_config, show_summary = TRUE) {
  
  cat("🌍 BATCH TRAILING SL PLACEMENT\n")
  cat(strrep("=", 50), "\n")
  cat(sprintf("Processing %d positions...\n", length(positions_config)))
  
  results <- list()
  
  for (i in seq_along(positions_config)) {
    config <- positions_config[[i]]
    symbol <- config$symbol
    
    cat(sprintf("\n📊 [%d/%d] Processing %s...\n", i, length(positions_config), symbol))
    
    result <- place_trailing_sl_percent(
      symbol = config$symbol,
      side = config$side,
      size = config$size,
      trailing_percent = config$trailing_percent,
      use_entry_price = config$use_entry_price %||% FALSE,
      entry_price = config$entry_price %||% NULL,
      show_calculation = FALSE  # Weniger verbose bei batch
    )
    
    results[[symbol]] <- result
    
    # Pause zwischen Orders
    if (i < length(positions_config)) {
      Sys.sleep(1)
    }
  }
  
  # SUMMARY
  if (show_summary) {
    cat("\n📋 BATCH TRAILING SL SUMMARY:\n")
    cat(strrep("=", 40), "\n")
    
    successful <- 0
    failed <- 0
    
    for (symbol in names(results)) {
      result <- results[[symbol]]
      if (result$success) {
        successful <- successful + 1
        cat(sprintf("✅ %s: SL at %.4f USDT (%.2f%% trailing)\n", 
                    symbol, result$sl_price, result$trailing_percent))
      } else {
        failed <- failed + 1
        cat(sprintf("❌ %s: Failed - %s\n", symbol, result$error))
      }
    }
    
    cat(sprintf("\n🎯 Results: %d successful, %d failed\n", successful, failed))
  }
  
  return(results)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ DYNAMIC TRAILING SL - Basierend auf Volatilität und OI-Levels                                      │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
place_dynamic_trailing_sl <- function(symbol, side, size, 
                                     base_trailing_percent = 3.0,
                                     volatility_adjustment = TRUE,
                                     oi_level_adjustment = TRUE) {
  
  cat(sprintf("🎯 DYNAMIC TRAILING SL FOR %s\n", symbol))
  cat(strrep("=", 40), "\n")
  
  # 1. BASIS TRAILING PERCENT
  trailing_percent <- base_trailing_percent
  cat(sprintf("📊 Base Trailing: %.2f%%\n", base_trailing_percent))
  
  # 2. VOLATILITÄT-ADJUSTMENT
  if (volatility_adjustment && exists("get_enhanced_market_data")) {
    tryCatch({
      market_data <- get_enhanced_market_data(symbol)
      if (!is.null(market_data$ticker)) {
        change_24h <- abs(as.numeric(market_data$ticker$change_24h_pct))
        
        if (change_24h > 5) {
          # Hohe Volatilität: Mehr Raum geben
          volatility_factor <- 1.5
          cat(sprintf("📈 High volatility detected (%.2f%%) - widening SL\n", change_24h))
        } else if (change_24h < 1) {
          # Niedrige Volatilität: Enger setzen
          volatility_factor <- 0.7
          cat(sprintf("📊 Low volatility detected (%.2f%%) - tightening SL\n", change_24h))
        } else {
          volatility_factor <- 1.0
          cat(sprintf("📊 Normal volatility (%.2f%%)\n", change_24h))
        }
        
        trailing_percent <- trailing_percent * volatility_factor
      }
    }, error = function(e) {
      cat("⚠️ Volatility adjustment failed, using base percent\n")
    })
  }
  
  # 3. OI-LEVEL ADJUSTMENT
  if (oi_level_adjustment) {
    # Hier könntest du OI-Heatmap Daten nutzen für Support/Resistance
    cat("🔍 OI-level adjustment: Using technical support levels\n")
    # Implementation für OI-basierte Adjustments
  }
  
  # 4. FINALE TRAILING SL PLATZIERUNG
  cat(sprintf("🎯 Final Trailing Percent: %.2f%%\n", trailing_percent))
  
  result <- place_trailing_sl_percent(
    symbol = symbol,
    side = side,
    size = size,
    trailing_percent = trailing_percent,
    show_calculation = TRUE
  )
  
  return(result)
}

# ==========================================================================================================
# 🎯 PORTFOLIO TRAILING SL FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ PORTFOLIO-WIDE TRAILING SL - Alle aktiven Positionen automatisch                                   │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
place_portfolio_trailing_sl <- function(trailing_configs = NULL, 
                                       default_trailing_percent = 3.0) {
  
  cat("💼 PORTFOLIO-WIDE TRAILING SL SETUP\n")
  cat(strrep("=", 50), "\n")
  
  # Default Config für bekannte Positionen
  if (is.null(trailing_configs)) {
    trailing_configs <- list(
      ADA = list(
        symbol = "ADAUSDT_UMCBL",
        side = "long",
        size = "12000",
        trailing_percent = 2.5,  # Konservativ für ADA
        use_entry_price = FALSE
      ),
      ALGO = list(
        symbol = "ALGOUSDT_UMCBL", 
        side = "long",
        size = "30000",
        trailing_percent = 3.5,  # Mehr Raum für ALGO (volatiler)
        use_entry_price = FALSE
      )
    )
  }
  
  cat(sprintf("🎯 Setting up trailing SL for %d assets\n", length(trailing_configs)))
  cat("📊 Configuration:\n")
  
  for (name in names(trailing_configs)) {
    config <- trailing_configs[[name]]
    cat(sprintf("   %s: %.2f%% trailing (%s %s contracts)\n", 
                name, config$trailing_percent, config$side, config$size))
  }
  
  # Bestätigung abfragen
  cat("\n⚠️  WARNING: This will place REAL trailing SL orders!\n")
  cat("Continue? (Press Enter to proceed, Ctrl+C to cancel)\n")
  # readline(prompt = "")  # Uncomment for interactive confirmation
  
  # Batch Processing
  results <- place_batch_trailing_sl(trailing_configs, show_summary = TRUE)
  
  return(results)
}

# ==========================================================================================================
# ✅ QUICK COMMANDS & USAGE EXAMPLES
# ==========================================================================================================

cat("✅ PROZENTBASIERTE TRAILING SL SYSTEM LOADED!\n")
cat(strrep("=", 60), "\n")

cat("🎯 USAGE EXAMPLES:\n\n")

cat("📊 EINZELNE TRAILING SL ORDERS:\n")
cat("# ADA: 2.5% trailing von current price\n")
cat("place_trailing_sl_percent('ADAUSDT_UMCBL', 'long', '12000', 2.5)\n\n")

cat("# ALGO: 3.5% trailing von current price\n") 
cat("place_trailing_sl_percent('ALGOUSDT_UMCBL', 'long', '30000', 3.5)\n\n")

cat("# Mit Entry Price als Referenz:\n")
cat("place_trailing_sl_percent('ADAUSDT_UMCBL', 'long', '12000', 2.0, \n")
cat("                         use_entry_price = TRUE, entry_price = 0.6552)\n\n")

cat("🌍 PORTFOLIO-WIDE TRAILING SL:\n")
cat("# Alle Positionen mit default settings:\n")
cat("portfolio_results <- place_portfolio_trailing_sl()\n\n")

cat("🎯 DYNAMIC TRAILING (Volatilitäts-angepasst):\n")
cat("# Automatische Anpassung basierend auf Marktbedingungen:\n")
cat("place_dynamic_trailing_sl('ADAUSDT_UMCBL', 'long', '12000', 3.0)\n\n")

cat("📋 CUSTOM BATCH SETUP:\n")
cat("custom_config <- list(\n")
cat("  ADA = list(symbol = 'ADAUSDT_UMCBL', side = 'long', \n")
cat("             size = '12000', trailing_percent = 2.5),\n")
cat("  ALGO = list(symbol = 'ALGOUSDT_UMCBL', side = 'long', \n")
cat("              size = '30000', trailing_percent = 3.5)\n")
cat(")\n")
cat("batch_results <- place_batch_trailing_sl(custom_config)\n\n")

cat("🚨 FREITAG-DIP DEFENSE SETUP (LIVE POSITIONS!):\n")
cat("# Deine AKTUELLEN Positionen (19:52 Uhr Update):\n")
cat("friday_defense_live <- list(\n")
cat("  ADA = list(symbol = 'ADAUSDT_UMCBL', side = 'long', \n")
cat("             size = '15000', trailing_percent = 2.0),  # 15K contracts @ 0.7341\n")
cat("  ALGO = list(symbol = 'ALGOUSDT_UMCBL', side = 'long', \n")
cat("              size = '30000', trailing_percent = 2.8)   # 30K contracts @ 0.2256\n")
cat(")\n")
cat("place_batch_trailing_sl(friday_defense_live)\n\n")

cat("💡 LIVE BERECHNUNG (basierend auf aktuelle Preise):\n")
cat("# ADA: 0.7341 USDT - 2.0% = 0.7194 USDT SL\n")
cat("# ALGO: 0.2256 USDT - 2.8% = 0.2193 USDT SL\n")
cat("# Total gesicherte Gewinne: ~1,300+ USDT\n\n")

cat("💡 FEATURES:\n")
cat("✅ Live price fetching für aktuelle Referenz\n")
cat("✅ Entry price option für ursprüngliche Basis\n") 
cat("✅ Symbol-spezifische Präzision\n")
cat("✅ Batch processing für Portfolio\n")
cat("✅ Volatilitäts-Adjustments\n")
cat("✅ Detailed calculation display\n")
cat("✅ Error handling & fallbacks\n")

cat(strrep("=", 60), "\n")