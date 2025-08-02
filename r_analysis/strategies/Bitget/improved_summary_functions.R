# ==========================================================================================================
# 📊 VERBESSERTE SUMMARY-FUNKTIONEN - FIX FÜR LIVE-DATEN
# ==========================================================================================================
# Löst das Problem mit leeren Zusammenfassungen
# ==========================================================================================================

cat("📊 Loading improved summary functions...\n")

# ==========================================================================================================
# 🔧 VERBESSERTE EXECUTE_SUMMARY_ONLY FUNKTION
# ==========================================================================================================

#' Verbesserte Summary mit korrekter Datenextraktion
execute_summary_only_fixed <- function(symbols = NULL, mode = "full") {
  cat("🔄 Running improved analysis...\n")
  
  # Führe stille Analyse durch
  results <- execute_silent(symbols, mode)
  
  if (is.null(results)) {
    cat("❌ No results received\n")
    return(invisible(NULL))
  }
  
  # Zeige nur Zusammenfassung basierend auf tatsächlicher Datenstruktur
  cat("\n📊 === LIVE MARKET SUMMARY === 📊\n")
  
  # Prüfe verschiedene Datenstrukturen
  if (!is.null(results$quick_data)) {
    # Struktur 1: quick_data (von execute_quick_check)
    display_quick_data_summary(results$quick_data)
  } else if (!is.null(results$market_data)) {
    # Struktur 2: market_data (von execute_full_analysis)
    display_market_data_summary(results$market_data, results)
  } else {
    cat("❌ No recognizable data structure found\n")
    cat("🔍 Available fields:", paste(names(results), collapse = ", "), "\n")
  }
  
  cat(sprintf("\n⏱️ Executed at: %s\n", format(Sys.time(), "%H:%M:%S")))
  return(invisible(results))
}

#' Zeige Quick Data Summary
display_quick_data_summary <- function(quick_data) {
  for (symbol in names(quick_data)) {
    data <- quick_data[[symbol]]
    
    if (!is.null(data$price) && data$price > 0) {
      # Trend-Icon
      trend_icon <- if (data$change_24h > 1) "📈" else 
        if (data$change_24h > 0) "🔼" else 
          if (data$change_24h > -1) "➡️" else 
            if (data$change_24h > -3) "🔽" else "📉"
      
      cat(sprintf("✅ %s: %.4f USDT | 24h: %+.2f%% %s | Vol: %.1fM\n",
                  symbol, data$price, data$change_24h, trend_icon, data$volume/1000000))
    } else {
      cat(sprintf("❌ %s: No valid data\n", symbol))
    }
  }
}

#' Zeige Market Data Summary (von Full Analysis)
display_market_data_summary <- function(market_data, full_results) {
  for (symbol in names(market_data)) {
    data <- market_data[[symbol]]
    
    if (!is.null(data$current_price) && data$current_price > 0) {
      # Trend-Icon
      trend_icon <- if (data$change_24h > 1) "📈" else 
        if (data$change_24h > 0) "🔼" else 
          if (data$change_24h > -1) "➡️" else 
            if (data$change_24h > -3) "🔽" else "📉"
      
      # Zusätzliche Info aus Technical Analysis
      trend_info <- ""
      if (!is.null(full_results$technical_analysis[[symbol]])) {
        tech <- full_results$technical_analysis[[symbol]]
        trend_info <- sprintf(" | RSI: %.0f", tech$rsi %||% 50)
      }
      
      # Sentiment Info
      sentiment_info <- ""
      if (!is.null(full_results$sentiment_analysis[[symbol]])) {
        sent <- full_results$sentiment_analysis[[symbol]]
        sentiment_info <- sprintf(" | Sentiment: %s", sent$overall_sentiment %||% "UNKNOWN")
      }
      
      cat(sprintf("✅ %s: %.4f USDT | 24h: %+.2f%% %s | Vol: %.1fM%s%s\n",
                  symbol, data$current_price, data$change_24h, trend_icon, 
                  data$volume_24h/1000000, trend_info, sentiment_info))
    } else {
      cat(sprintf("❌ %s: No valid data\n", symbol))
    }
  }
}

# ==========================================================================================================
# 🔧 VERBESSERTE DAILY MARKET CHECK
# ==========================================================================================================

#' Verbesserte tägliche Marktprüfung
daily_market_check_fixed <- function(symbols = NULL) {
  cat("\n🌅 === IMPROVED DAILY MARKET CHECK === 🌅\n")
  
  # 1. Schnelle Übersicht mit korrekter Funktion
  cat("1️⃣ Getting quick overview...\n")
  summary_results <- execute_summary_only_fixed(symbols, "full")
  
  # 2. Detaillierte Analyse mit korrekter Datenextraktion
  cat("\n2️⃣ Detailed analysis...\n")
  if (!is.null(summary_results)) {
    analyze_results_fixed(summary_results)
  }
  
  # 3. Speichere Daten mit korrekter Struktur
  cat("3️⃣ Saving daily data...\n")
  if (!is.null(summary_results)) {
    save_results_to_csv_fixed(summary_results)
  }
  
  cat("\n✅ Daily market check completed!\n")
  return(summary_results)
}

#' Verbesserte Ergebnis-Analyse
analyze_results_fixed <- function(results) {
  if (is.null(results)) {
    cat("❌ No results to analyze\n")
    return()
  }
  
  cat("\n📊 === DETAILED ANALYSIS === 📊\n")
  
  # Analysiere basierend auf verfügbarer Datenstruktur
  if (!is.null(results$market_data)) {
    # Full analysis results
    for (symbol in names(results$market_data)) {
      analyze_full_symbol_data(symbol, results)
    }
  } else if (!is.null(results$quick_data)) {
    # Quick analysis results
    for (symbol in names(results$quick_data)) {
      analyze_quick_symbol_data(symbol, results$quick_data[[symbol]])
    }
  } else {
    cat("❌ No recognizable data structure for analysis\n")
  }
}

#' Analysiere vollständige Symbol-Daten
analyze_full_symbol_data <- function(symbol, results) {
  cat(sprintf("\n🔸 %s FULL ANALYSIS:\n", symbol))
  
  # Market Data
  market <- results$market_data[[symbol]]
  if (!is.null(market)) {
    trend <- if (market$change_24h > 2) "📈 STRONG UP" else 
      if (market$change_24h > 0) "🔼 UP" else 
        if (market$change_24h > -2) "➡️ STABLE" else 
          if (market$change_24h > -5) "🔽 DOWN" else "📉 STRONG DOWN"
    
    vol_status <- if (market$volume_24h > 100000000) "🔥 HIGH" else 
      if (market$volume_24h > 50000000) "📊 NORMAL" else "📉 LOW"
    
    cat(sprintf("   💰 Price: %.4f USDT\n", market$current_price))
    cat(sprintf("   📈 24h Change: %+.2f%% %s\n", market$change_24h, trend))
    cat(sprintf("   📊 Volume: %.1fM USDT %s\n", market$volume_24h/1000000, vol_status))
    cat(sprintf("   📏 Price Position: %.1f%% of 24h range\n", market$price_position * 100))
  }
  
  # Technical Analysis
  tech <- results$technical_analysis[[symbol]]
  if (!is.null(tech)) {
    cat(sprintf("   🧮 RSI: %.0f | Trend: %s\n", tech$rsi %||% 50, tech$trend %||% "UNKNOWN"))
  }
  
  # Sentiment Analysis
  sent <- results$sentiment_analysis[[symbol]]
  if (!is.null(sent)) {
    cat(sprintf("   🎭 Sentiment: %s (Score: %.3f)\n", 
                sent$overall_sentiment %||% "UNKNOWN", 
                sent$sentiment_score %||% 0.5))
    cat(sprintf("   💡 Recommendation: %s\n", sent$recommendation %||% "HOLD"))
  }
  
  # Overall Recommendation
  overall_rec <- results$recommendations[[symbol]]
  if (!is.null(overall_rec)) {
    cat(sprintf("   🎯 Overall: %s\n", overall_rec))
  }
}

#' Analysiere schnelle Symbol-Daten
analyze_quick_symbol_data <- function(symbol, data) {
  cat(sprintf("\n🔸 %s QUICK ANALYSIS:\n", symbol))
  
  if (!is.null(data$price) && data$price > 0) {
    trend <- if (data$change_24h > 2) "📈 STRONG UP" else 
      if (data$change_24h > 0) "🔼 UP" else 
        if (data$change_24h > -2) "➡️ STABLE" else 
          if (data$change_24h > -5) "🔽 DOWN" else "📉 STRONG DOWN"
    
    vol_status <- if (data$volume > 100000000) "🔥 HIGH" else 
      if (data$volume > 50000000) "📊 NORMAL" else "📉 LOW"
    
    cat(sprintf("   💰 Price: %.4f USDT\n", data$price))
    cat(sprintf("   📈 24h Change: %+.2f%% %s\n", data$change_24h, trend))
    cat(sprintf("   📊 Volume: %.1fM USDT %s\n", data$volume/1000000, vol_status))
    cat(sprintf("   ⏰ Updated: %s\n", data$timestamp))
  } else {
    cat("   ❌ No valid price data available\n")
  }
}

#' Verbesserte CSV-Speicherung
save_results_to_csv_fixed <- function(results, filename = NULL) {
  if (is.null(results)) {
    cat("❌ No results to save\n")
    return()
  }
  
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("market_data_", timestamp, ".csv")
  }
  
  # Bestimme Datenquelle und konvertiere zu DataFrame
  df_list <- list()
  
  if (!is.null(results$market_data)) {
    # Full analysis data
    for (symbol in names(results$market_data)) {
      data <- results$market_data[[symbol]]
      
      # Zusätzliche Daten sammeln
      tech_data <- results$technical_analysis[[symbol]]
      sent_data <- results$sentiment_analysis[[symbol]]
      
      df_list[[symbol]] <- data.frame(
        Symbol = symbol,
        Price = data$current_price %||% 0,
        Change_24h = data$change_24h %||% 0,
        Volume_USDT = data$volume_24h %||% 0,
        Price_Position = data$price_position %||% 0.5,
        RSI = tech_data$rsi %||% 50,
        Trend = tech_data$trend %||% "UNKNOWN",
        Sentiment = sent_data$overall_sentiment %||% "NEUTRAL",
        Sentiment_Score = sent_data$sentiment_score %||% 0.5,
        Recommendation = sent_data$recommendation %||% "HOLD",
        Timestamp = as.character(data$timestamp %||% Sys.time()),
        stringsAsFactors = FALSE
      )
    }
  } else if (!is.null(results$quick_data)) {
    # Quick analysis data
    for (symbol in names(results$quick_data)) {
      data <- results$quick_data[[symbol]]
      df_list[[symbol]] <- data.frame(
        Symbol = symbol,
        Price = data$price %||% 0,
        Change_24h = data$change_24h %||% 0,
        Volume_USDT = data$volume %||% 0,
        Status = data$status %||% "UNKNOWN",
        Timestamp = as.character(data$timestamp %||% Sys.time()),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(df_list) > 0) {
    df <- do.call(rbind, df_list)
    
    tryCatch({
      write.csv(df, filename, row.names = FALSE)
      cat("✅ Results saved to:", filename, "\n")
      cat("📁 Columns:", paste(names(df), collapse = ", "), "\n")
    }, error = function(e) {
      cat("❌ Error saving CSV:", e$message, "\n")
    })
  } else {
    cat("❌ No data to save\n")
  }
}

# ==========================================================================================================
# 🎯 QUICK ACCESS FUNKTIONEN
# ==========================================================================================================

#' Schneller Marktcheck mit verbesserter Ausgabe
quick_market_check <- function() {
  execute_summary_only_fixed()
}

#' Vollständiger Tagescheck mit allen Verbesserungen
full_daily_check <- function() {
  daily_market_check_fixed()
}

#' Nur Preise anzeigen (minimal)
prices_only <- function(symbols = NULL) {
  cat("💰 === CURRENT PRICES === 💰\n")
  results <- execute_silent(symbols, "quick")
  
  if (!is.null(results$quick_data)) {
    for (symbol in names(results$quick_data)) {
      data <- results$quick_data[[symbol]]
      if (!is.null(data$price) && data$price > 0) {
        cat(sprintf("%s: %.4f USDT (%+.2f%%)\n", 
                    symbol, data$price, data$change_24h))
      }
    }
  }
}

# ==========================================================================================================
# 💡 USAGE EXAMPLES
# ==========================================================================================================

cat("✅ IMPROVED SUMMARY FUNCTIONS LOADED!\n")
cat("🔧 Available Improved Functions:\n")
cat("   execute_summary_only_fixed()             # Fixed summary function\n")
cat("   daily_market_check_fixed()               # Fixed daily check\n")
cat("   quick_market_check()                     # Quick market overview\n")
cat("   full_daily_check()                       # Complete daily routine\n")
cat("   prices_only()                            # Just prices\n")
cat("\n💡 Recommended Usage:\n")
cat("   quick_market_check()                     # For quick checks\n")
cat("   full_daily_check()                       # For daily routine\n")
cat("   prices_only()                            # For minimal info\n")

# ==========================================================================================================
# END OF IMPROVED SUMMARY FUNCTIONS
# ==========================================================================================================