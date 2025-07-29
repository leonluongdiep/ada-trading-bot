# ==========================================================================================================
# 🔧 FIX FÜR FEHLENDE COINS IM PORTFOLIO
# ==========================================================================================================
# Löst: Nur 2 von 5 Coins werden angezeigt
# ==========================================================================================================

cat("🔧 Loading fix for missing coins...\n")

# ==========================================================================================================
# 🔍 DIAGNOSE DAS PORTFOLIO-PROBLEM
# ==========================================================================================================

#' Prüfe aktuellen Portfolio-Status
diagnose_portfolio_status <- function() {
  cat("\n🔍 === PORTFOLIO DIAGNOSIS === 🔍\n")
  
  # Prüfe PORTFOLIO_ASSETS Variable
  cat("📊 Current PORTFOLIO_ASSETS:\n")
  if (exists("PORTFOLIO_ASSETS")) {
    cat("   Length:", length(PORTFOLIO_ASSETS), "\n")
    cat("   Contents:", paste(PORTFOLIO_ASSETS, collapse = ", "), "\n")
    for (i in 1:length(PORTFOLIO_ASSETS)) {
      cat(sprintf("   %d. %s\n", i, PORTFOLIO_ASSETS[i]))
    }
  } else {
    cat("   ❌ PORTFOLIO_ASSETS does not exist!\n")
  }
  
  # Prüfe MULTI_ASSET_CONFIG
  cat("\n⚙️ Asset Configurations:\n")
  if (exists("MULTI_ASSET_CONFIG")) {
    cat("   Configured assets:", length(MULTI_ASSET_CONFIG), "\n")
    cat("   Available configs:", paste(names(MULTI_ASSET_CONFIG), collapse = ", "), "\n")
  } else {
    cat("   ❌ MULTI_ASSET_CONFIG does not exist!\n")
  }
  
  # Teste jede Coin einzeln
  cat("\n🧪 Testing individual coins:\n")
  expected_coins <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL", "ICPUSDT_UMCBL", "ETCUSDT_UMCBL", "VETUSDT_UMCBL")
  
  for (coin in expected_coins) {
    cat(sprintf("   Testing %s...", coin))
    
    ticker_data <- tryCatch({
      get_enhanced_ticker_data(coin)
    }, error = function(e) {
      cat(" ❌ Error:", e$message, "\n")
      NULL
    })
    
    if (!is.null(ticker_data) && !is.null(ticker_data$last_price) && ticker_data$last_price > 0) {
      cat(sprintf(" ✅ %.6f USDT\n", ticker_data$last_price))
    } else {
      cat(" ❌ No data\n")
    }
  }
  
  return(list(
    portfolio_assets = if (exists("PORTFOLIO_ASSETS")) PORTFOLIO_ASSETS else NULL,
    config_assets = if (exists("MULTI_ASSET_CONFIG")) names(MULTI_ASSET_CONFIG) else NULL
  ))
}

# ==========================================================================================================
# 🔧 REPARIERE DAS PORTFOLIO
# ==========================================================================================================

#' Setze Portfolio korrekt mit allen 5 Coins
fix_portfolio_completely <- function() {
  cat("\n🔧 === FIXING PORTFOLIO COMPLETELY === 🔧\n")
  
  # Setze die korrekten 5 Coins
  correct_portfolio <- c(
    "ADAUSDT_UMCBL",   # ADA
    "ALGOUSDT_UMCBL",  # ALGO  
    "ICPUSDT_UMCBL",   # ICP
    "ETCUSDT_UMCBL",   # ETC
    "VETUSDT_UMCBL"    # VET
  )
  
  cat("🎯 Setting portfolio to 5 coins:\n")
  for (i in 1:length(correct_portfolio)) {
    cat(sprintf("   %d. %s\n", i, correct_portfolio[i]))
  }
  
  # Aktualisiere global
  PORTFOLIO_ASSETS <<- correct_portfolio
  
  cat("✅ Portfolio updated globally\n")
  
  # Prüfe ob Update erfolgreich war
  cat("\n📊 Verification:\n")
  cat("   PORTFOLIO_ASSETS length:", length(PORTFOLIO_ASSETS), "\n")
  cat("   Contents:", paste(PORTFOLIO_ASSETS, collapse = ", "), "\n")
  
  # Teste alle Coins
  cat("\n🧪 Testing all 5 coins:\n")
  success_count <- 0
  
  for (coin in correct_portfolio) {
    cat(sprintf("   Testing %s...", coin))
    
    ticker_data <- tryCatch({
      get_enhanced_ticker_data(coin)
    }, error = function(e) NULL)
    
    if (!is.null(ticker_data) && !is.null(ticker_data$last_price) && ticker_data$last_price > 0) {
      cat(sprintf(" ✅ %.6f USDT\n", ticker_data$last_price))
      success_count <- success_count + 1
    } else {
      cat(" ❌ Failed\n")
    }
  }
  
  cat(sprintf("\n📊 Results: %d/5 coins working\n", success_count))
  
  if (success_count == 5) {
    cat("🎉 ALL 5 COINS WORKING!\n")
  } else {
    cat("⚠️ Some coins have issues\n")
  }
  
  return(correct_portfolio)
}

# ==========================================================================================================
# 🔧 VERBESSERTE MARKET CHECK FUNKTION
# ==========================================================================================================

#' Verbesserte Market Check die garantiert alle Coins zeigt
complete_market_check <- function() {
  cat("\n💯 === COMPLETE MARKET CHECK (ALL 5 COINS) === 💯\n")
  
  # Stelle sicher dass Portfolio korrekt ist
  if (length(PORTFOLIO_ASSETS) != 5) {
    cat("⚠️ Portfolio incomplete, fixing...\n")
    fix_portfolio_completely()
  }
  
  # Hole Daten für alle 5 Coins
  all_results <- list()
  
  for (coin in PORTFOLIO_ASSETS) {
    cat(sprintf("📡 Fetching %s...", coin))
    
    # Verwende execute_silent um Daten zu holen
    coin_results <- tryCatch({
      execute_silent(coin, "full")
    }, error = function(e) {
      cat(" ❌ Error\n")
      NULL
    })
    
    if (!is.null(coin_results)) {
      cat(" ✅\n")
      all_results[[coin]] <- coin_results
    } else {
      cat(" ❌\n")
      # Fallback: Direkte API-Anfrage
      ticker_data <- tryCatch({
        get_enhanced_ticker_data(coin)
      }, error = function(e) NULL)
      
      if (!is.null(ticker_data)) {
        # Erstelle minimale Datenstruktur
        all_results[[coin]] <- list(
          market_data = list()
        )
        all_results[[coin]]$market_data[[coin]] <- list(
          current_price = ticker_data$last_price,
          change_24h = ticker_data$change_24h_pct %||% 0,
          volume_24h = ticker_data$volume_24h_usdt %||% 0
        )
        cat("   📊 Using fallback data\n")
      }
    }
  }
  
  # Zeige Ergebnisse
  cat("\n📊 === COMPLETE RESULTS === 📊\n")
  
  for (coin in names(all_results)) {
    result <- all_results[[coin]]
    
    # Extrahiere Preisdaten
    if (!is.null(result$market_data) && !is.null(result$market_data[[coin]])) {
      data <- result$market_data[[coin]]
      
      price <- data$current_price %||% 0
      change <- data$change_24h %||% 0
      volume <- data$volume_24h %||% 0
      
      # Trend-Icon
      trend_icon <- if (change > 1) "📈" else 
        if (change > 0) "🔼" else 
          if (change > -1) "➡️" else 
            if (change > -3) "🔽" else "📉"
      
      cat(sprintf("✅ %s: %.6f USDT | 24h: %+.2f%% %s | Vol: %.1fM\n",
                  coin, price, change, trend_icon, volume/1000000))
      
    } else {
      cat(sprintf("❌ %s: No data available\n", coin))
    }
  }
  
  cat(sprintf("\n⏱️ Executed at: %s\n", format(Sys.time(), "%H:%M:%S")))
  cat(sprintf("📊 Coins displayed: %d/5\n", length(all_results)))
  
  return(all_results)
}

# ==========================================================================================================
# 🔧 EINFACHER PREIS-CHECK FÜR ALLE 5 COINS
# ==========================================================================================================

#' Einfacher Preis-Check ohne komplexe Strukturen
simple_5_coin_check <- function() {
  cat("\n💰 === SIMPLE 5-COIN PRICE CHECK === 💰\n")
  
  coins <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL", "ICPUSDT_UMCBL", "ETCUSDT_UMCBL", "VETUSDT_UMCBL")
  
  for (coin in coins) {
    cat(sprintf("📡 %s...", coin))
    
    ticker_data <- tryCatch({
      get_enhanced_ticker_data(coin)
    }, error = function(e) {
      cat(" ❌\n")
      NULL
    })
    
    if (!is.null(ticker_data) && !is.null(ticker_data$last_price) && ticker_data$last_price > 0) {
      price <- ticker_data$last_price
      change <- ticker_data$change_24h_pct %||% 0
      volume <- ticker_data$volume_24h_usdt %||% 0
      
      # Trend-Icon
      trend_icon <- if (change > 1) "📈" else 
        if (change > 0) "🔼" else 
          if (change > -1) "➡️" else 
            if (change > -3) "🔽" else "📉"
      
      cat(sprintf(" ✅ %.6f USDT (%+.2f%% %s)\n", price, change, trend_icon))
    } else {
      cat(" ❌ No data\n")
    }
  }
  
  cat(sprintf("\n⏱️ Updated: %s\n", format(Sys.time(), "%H:%M:%S")))
}

# ==========================================================================================================
# 🔧 FORCE UPDATE PORTFOLIO
# ==========================================================================================================

#' Erzwinge Portfolio-Update und teste alles
force_portfolio_update <- function() {
  cat("\n💪 === FORCING PORTFOLIO UPDATE === 💪\n")
  
  # Schritt 1: Diagnose
  cat("1️⃣ Running diagnosis...\n")
  diagnosis <- diagnose_portfolio_status()
  
  # Schritt 2: Portfolio reparieren
  cat("\n2️⃣ Fixing portfolio...\n")
  fixed_portfolio <- fix_portfolio_completely()
  
  # Schritt 3: Vollständiger Test
  cat("\n3️⃣ Running complete check...\n")
  complete_results <- complete_market_check()
  
  # Schritt 4: Einfacher Test
  cat("\n4️⃣ Running simple check...\n")
  simple_5_coin_check()
  
  cat("\n✅ FORCE UPDATE COMPLETED!\n")
  cat("💡 Try: quick_market_check() again\n")
  
  return(list(
    diagnosis = diagnosis,
    fixed_portfolio = fixed_portfolio,
    complete_results = complete_results
  ))
}

# ==========================================================================================================
# 💡 USAGE INSTRUCTIONS
# ==========================================================================================================

cat("✅ MISSING COINS FIX LOADED!\n")
cat("🔧 Available Functions:\n")
cat("   diagnose_portfolio_status()              # Check what's wrong\n")
cat("   fix_portfolio_completely()               # Fix portfolio setup\n")
cat("   complete_market_check()                  # Check all 5 coins\n")
cat("   simple_5_coin_check()                    # Simple price check\n")
cat("   force_portfolio_update()                 # Fix everything\n")
cat("\n🚀 RECOMMENDED:\n")
cat("   force_portfolio_update()                 # Run this to fix everything!\n")
cat("\n💡 THEN TRY:\n")
cat("   quick_market_check()                     # Should show all 5 coins\n")

# ==========================================================================================================
# END OF MISSING COINS FIX
# ==========================================================================================================