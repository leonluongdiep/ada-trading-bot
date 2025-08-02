# ==========================================================================================================
# 📊 ALGO HEATMAP SCORE ERKLÄRUNG - WIE DIE WERTE BERECHNET WERDEN
# ==========================================================================================================
# Detaillierte Erklärung aller Scores und deren Bedeutung
# ==========================================================================================================

cat("📚 Loading ALGO Heatmap Score Explanation...\n")

# ==========================================================================================================
# 📖 SCORE ERKLÄRUNGEN
# ==========================================================================================================

#' Erkläre alle Heatmap Scores im Detail
explain_heatmap_scores <- function() {
  cat("📚 === HEATMAP SCORE ERKLÄRUNG === 📚\n\n")
  
  cat("🎯 === 1. INTEREST SCORE (Interesse-Score) === 🎯\n")
  cat("➤ WAS ES BEDEUTET:\n")
  cat("   Der Interest Score zeigt an, wie 'interessant' ein bestimmtes Preis-Level für Trader ist.\n")
  cat("   Je höher der Score, desto wahrscheinlicher ist es, dass an diesem Level gehandelt wird.\n\n")
  
  cat("➤ WIE ES BERECHNET WIRD:\n")
  cat("   1. DISTANZ ZUM AKTUELLEN PREIS (40% Gewichtung):\n")
  cat("      - Je näher am aktuellen Preis, desto höher der Score\n")
  cat("      - Formel: exp(-distance_factor * 8) * 0.4\n")
  cat("      - Beispiel: Bei 1% Distanz = hoher Score, bei 5% Distanz = niedriger Score\n\n")
  
  cat("   2. DISTANZ ZU 24H HIGH (30% Gewichtung):\n")
  cat("      - Levels nahe dem 24h-Hoch sind interessant (Widerstand)\n")
  cat("      - Formel: exp(-distance_to_high * 15) * 0.3\n\n")
  
  cat("   3. DISTANZ ZU 24H LOW (30% Gewichtung):\n")
  cat("      - Levels nahe dem 24h-Tief sind interessant (Unterstützung)\n")
  cat("      - Formel: exp(-distance_to_low * 15) * 0.3\n\n")
  
  cat("   4. RUNDE ZAHLEN BONUS:\n")
  cat("      - Preise wie 0.2500, 0.2600 erhalten 1.5x Multiplikator\n")
  cat("      - Psychologische Levels sind wichtiger für Trader\n\n")
  
  cat("🎯 === 2. SUPPORT vs RESISTANCE === 🎯\n")
  cat("➤ KLASSIFIZIERUNG:\n")
  cat("   - RESISTANCE: Preis-Level ÜBER dem aktuellen Preis\n")
  cat("   - SUPPORT: Preis-Level UNTER dem aktuellen Preis\n\n")
  
  cat("➤ BEDEUTUNG:\n")
  cat("   - RESISTANCE: Potential verkauft zu werden (Widerstand)\n")
  cat("   - SUPPORT: Potential gekauft zu werden (Unterstützung)\n\n")
  
  cat("🎯 === 3. VOLUME ADJUSTED SCORE === 🎯\n")
  cat("➤ WAS ES BEDEUTET:\n")
  cat("   Der finale Score nach Anpassung an das Handelsvolumen.\n")
  cat("   Hohe Volumes verstärken die Wichtigkeit der Levels.\n\n")
  
  cat("➤ BERECHNUNG:\n")
  cat("   - Volume Factor = min(2.0, volume_24h / 10M)\n")
  cat("   - Final Score = Interest Score × Volume Factor\n")
  cat("   - Beispiel: Bei 20M Volume = 2x Verstärkung\n")
  cat("   - Beispiel: Bei 5M Volume = 0.5x Abschwächung\n\n")
  
  cat("🎯 === 4. HEATMAP VISUALISIERUNG === 🎯\n")
  cat("➤ ASCII BALKEN:\n")
  cat("   - Jeder █ Block = ~8.33% des maximalen Scores\n")
  cat("   - 12 Blöcke = 100% Score\n")
  cat("   - Längere Balken = wichtigere Preis-Levels\n\n")
  
  cat("➤ BEISPIEL INTERPRETATION:\n")
  cat("   0.2650 │████████████│ 92%  ← SEHR WICHTIGES LEVEL!\n")
  cat("   0.2620 │██████      │ 48%  ← MODERATES LEVEL\n")
  cat("   0.2590 │████        │ 32%  ← SCHWACHES LEVEL\n\n")
}

#' Zeige ein praktisches Berechnungsbeispiel
show_calculation_example <- function() {
  cat("🧮 === PRAKTISCHES BERECHNUNGSBEISPIEL === 🧮\n\n")
  
  # Beispiel-Parameter
  current_price <- 0.2580
  high_24h <- 0.2650
  low_24h <- 0.2510
  volume_24h <- 15000000  # 15M USDT
  target_price <- 0.2620  # Beispiel-Level
  
  cat("📊 BEISPIEL-SITUATION:\n")
  cat("   Aktueller ALGO Preis: ", current_price, " USDT\n")
  cat("   24h High: ", high_24h, " USDT\n")
  cat("   24h Low: ", low_24h, " USDT\n")
  cat("   24h Volume: ", volume_24h / 1000000, " M USDT\n")
  cat("   Zu bewertender Preis: ", target_price, " USDT\n\n")
  
  cat("🔢 SCHRITT-FÜR-SCHRITT BERECHNUNG:\n\n")
  
  # 1. Distanzen berechnen
  distance_current <- abs(target_price - current_price) / current_price
  distance_high <- abs(target_price - high_24h) / current_price
  distance_low <- abs(target_price - low_24h) / current_price
  
  cat("1️⃣ DISTANZ-BERECHNUNGEN:\n")
  cat("   Distanz zu aktuellem Preis: ", round(distance_current * 100, 2), "%\n")
  cat("   Distanz zu 24h High: ", round(distance_high * 100, 2), "%\n")
  cat("   Distanz zu 24h Low: ", round(distance_low * 100, 2), "%\n\n")
  
  # 2. Gewichtungen berechnen
  current_weight <- exp(-distance_current * 8) * 0.4
  high_weight <- exp(-distance_high * 15) * 0.3
  low_weight <- exp(-distance_low * 15) * 0.3
  
  cat("2️⃣ GEWICHTUNGS-BERECHNUNGEN:\n")
  cat("   Current Weight: exp(-", round(distance_current, 4), " × 8) × 0.4 = ", round(current_weight, 4), "\n")
  cat("   High Weight: exp(-", round(distance_high, 4), " × 15) × 0.3 = ", round(high_weight, 4), "\n")
  cat("   Low Weight: exp(-", round(distance_low, 4), " × 15) × 0.3 = ", round(low_weight, 4), "\n\n")
  
  # 3. Runde Zahlen Check
  round_factor <- ifelse(abs(target_price - round(target_price, 2)) < 0.001, 1.5, 1.0)
  cat("3️⃣ RUNDE ZAHLEN BONUS:\n")
  cat("   Ist ", target_price, " eine runde Zahl? ", ifelse(round_factor > 1, "JA (1.5x Bonus)", "NEIN (1.0x)"), "\n\n")
  
  # 4. Base Interest Score
  base_score <- (current_weight + high_weight + low_weight) * round_factor
  cat("4️⃣ BASE INTEREST SCORE:\n")
  cat("   (", round(current_weight, 4), " + ", round(high_weight, 4), " + ", round(low_weight, 4), ") × ", round_factor, "\n")
  cat("   = ", round(base_score, 4), "\n\n")
  
  # 5. Volume Adjustment
  volume_factor <- min(2.0, volume_24h / 10000000)
  final_score <- base_score * volume_factor
  
  cat("5️⃣ VOLUME ADJUSTMENT:\n")
  cat("   Volume Factor: min(2.0, ", volume_24h / 1000000, "M / 10M) = ", round(volume_factor, 4), "\n")
  cat("   Final Score: ", round(base_score, 4), " × ", round(volume_factor, 4), " = ", round(final_score, 4), "\n\n")
  
  # 6. Interpretation
  cat("6️⃣ INTERPRETATION:\n")
  percentage <- round(final_score * 100)
  if (final_score > 0.8) {
    interpretation <- "🔥 SEHR WICHTIGES LEVEL - Hohe Wahrscheinlichkeit für Reaktion"
  } else if (final_score > 0.6) {
    interpretation <- "⚡ WICHTIGES LEVEL - Moderate Wahrscheinlichkeit für Reaktion"
  } else if (final_score > 0.4) {
    interpretation <- "📊 DURCHSCHNITTLICHES LEVEL - Geringe Wahrscheinlichkeit"
  } else {
    interpretation <- "📉 SCHWACHES LEVEL - Sehr geringe Bedeutung"
  }
  
  cat("   Score von ", round(final_score, 4), " (", percentage, "%) bedeutet:\n")
  cat("   ", interpretation, "\n\n")
  
  # 7. Support/Resistance
  level_type <- ifelse(target_price > current_price, "RESISTANCE", "SUPPORT")
  cat("7️⃣ LEVEL-TYP:\n")
  cat("   Da ", target_price, " > ", current_price, " ist es ein ", level_type, " Level\n")
  
  if (level_type == "RESISTANCE") {
    cat("   → Erwartung: Preis könnte bei ", target_price, " auf Verkaufsdruck stoßen\n")
  } else {
    cat("   → Erwartung: Preis könnte bei ", target_price, " auf Kaufunterstützung treffen\n")
  }
}

#' Erkläre die praktische Anwendung
explain_practical_usage <- function() {
  cat("\n💡 === PRAKTISCHE ANWENDUNG === 💡\n\n")
  
  cat("🎯 WIE SIE DIE SCORES NUTZEN KÖNNEN:\n\n")
  
  cat("1️⃣ TRADING LEVELS IDENTIFIZIEREN:\n")
  cat("   - Scores > 80%: Sehr wahrscheinliche Reaktionszonen\n")
  cat("   - Scores 60-80%: Moderate Reaktionszonen\n")
  cat("   - Scores < 40%: Weniger wichtige Levels\n\n")
  
  cat("2️⃣ ENTRY/EXIT STRATEGIEN:\n")
  cat("   - RESISTANCE Levels: Potentielle Verkaufszonen\n")
  cat("   - SUPPORT Levels: Potentielle Kaufzonen\n")
  cat("   - Hohe Scores = höhere Wahrscheinlichkeit für Preisreaktion\n\n")
  
  cat("3️⃣ RISK MANAGEMENT:\n")
  cat("   - Stop Loss unter/über wichtigen Levels setzen\n")
  cat("   - Take Profit vor starken Widerständen platzieren\n")
  cat("   - Position Size an Level-Stärke anpassen\n\n")
  
  cat("4️⃣ BEISPIEL TRADING PLAN:\n")
  cat("   Current Price: 0.2580 USDT\n")
  cat("   \n")
  cat("   🔴 RESISTANCE @ 0.2650 (Score: 92%) - STRONG SELL ZONE\n")
  cat("   │   → Take Profit hier setzen\n")
  cat("   │   → Short Entry bei Rejection\n")
  cat("   │\n")
  cat("   🟡 Moderate @ 0.2620 (Score: 65%) - WATCH ZONE\n")
  cat("   │   → Partial Take Profit möglich\n")
  cat("   │\n")
  cat("   📍 CURRENT @ 0.2580 ←\n")
  cat("   │\n")
  cat("   🟢 SUPPORT @ 0.2550 (Score: 78%) - STRONG BUY ZONE\n")
  cat("       → Long Entry bei Bounce\n")
  cat("       → Stop Loss unter diesem Level\n\n")
  
  cat("⚠️ WICHTIGE HINWEISE:\n")
  cat("   - Scores sind WAHRSCHEINLICHKEITEN, keine Garantien\n")
  cat("   - Kombinieren Sie mit anderen Analysen (TA, Fundamentals)\n")
  cat("   - Berücksichtigen Sie Marktbedingungen und Volatilität\n")
  cat("   - Höhere Volumes = verlässlichere Levels\n")
}

#' Zeige Live-Beispiel mit aktuellen ALGO Daten
show_live_algo_example <- function() {
  cat("\n🔷 === LIVE ALGO BEISPIEL === 🔷\n\n")
  
  # Hole aktuelle ALGO Daten
  ticker_data <- get_enhanced_ticker_data_safe("ALGOUSDT_UMCBL")
  
  if (is.null(ticker_data)) {
    cat("❌ Kann keine aktuellen ALGO Daten abrufen\n")
    return()
  }
  
  current_price <- ticker_data$last_price
  high_24h <- ticker_data$high_24h %||% (current_price * 1.05)
  low_24h <- ticker_data$low_24h %||% (current_price * 0.95)
  volume_24h <- ticker_data$volume_24h_usdt %||% 10000000
  
  cat("📊 AKTUELLE ALGO DATEN:\n")
  cat("   Preis: ", round(current_price, 4), " USDT\n")
  cat("   24h High: ", round(high_24h, 4), " USDT\n")
  cat("   24h Low: ", round(low_24h, 4), " USDT\n")
  cat("   24h Volume: ", round(volume_24h / 1000000, 1), " M USDT\n\n")
  
  # Berechne einige Key Levels
  resistance_1 <- current_price * 1.02  # +2%
  resistance_2 <- high_24h              # 24h High
  support_1 <- current_price * 0.98     # -2%
  support_2 <- low_24h                  # 24h Low
  
  levels <- list(
    list(price = resistance_2, name = "24H HIGH RESISTANCE"),
    list(price = resistance_1, name = "NEAR RESISTANCE"),
    list(price = support_1, name = "NEAR SUPPORT"),
    list(price = support_2, name = "24H LOW SUPPORT")
  )
  
  cat("🎯 BERECHNUNG FÜR KEY LEVELS:\n\n")
  
  for (i in 1:length(levels)) {
    level <- levels[[i]]
    target_price <- level$price
    
    cat("📍 ", level$name, " @ ", round(target_price, 4), " USDT:\n")
    
    # Berechne Score
    distance_current <- abs(target_price - current_price) / current_price
    distance_high <- abs(target_price - high_24h) / current_price
    distance_low <- abs(target_price - low_24h) / current_price
    
    current_weight <- exp(-distance_current * 8) * 0.4
    high_weight <- exp(-distance_high * 15) * 0.3
    low_weight <- exp(-distance_low * 15) * 0.3
    
    round_factor <- ifelse(abs(target_price - round(target_price, 2)) < 0.001, 1.5, 1.0)
    base_score <- (current_weight + high_weight + low_weight) * round_factor
    
    volume_factor <- min(2.0, volume_24h / 10000000)
    final_score <- base_score * volume_factor
    
    level_type <- ifelse(target_price > current_price, "RESISTANCE", "SUPPORT")
    distance_pct <- round(((target_price - current_price) / current_price) * 100, 1)
    
    cat("   Distanz: ", distance_pct, "%\n")
    cat("   Score: ", round(final_score, 3), " (", round(final_score * 100), "%)\n")
    cat("   Typ: ", level_type, "\n")
    
    if (final_score > 0.7) {
      cat("   💡 STARKES LEVEL - Hohe Reaktionswahrscheinlichkeit\n")
    } else if (final_score > 0.5) {
      cat("   💡 MODERATES LEVEL - Mittlere Reaktionswahrscheinlichkeit\n")
    } else {
      cat("   💡 SCHWACHES LEVEL - Geringe Reaktionswahrscheinlichkeit\n")
    }
    cat("\n")
  }
}

# ==========================================================================================================
# 🎯 HAUPT-ERKLÄRUNGS-FUNKTION
# ==========================================================================================================

#' Komplette Erklärung aller Heatmap Aspekte
explain_algo_heatmap_complete <- function() {
  cat("🎓 === KOMPLETTE ALGO HEATMAP ERKLÄRUNG === 🎓\n\n")
  
  # 1. Grundlagen erklären
  explain_heatmap_scores()
  
  # 2. Berechnungsbeispiel zeigen
  show_calculation_example()
  
  # 3. Praktische Anwendung
  explain_practical_usage()
  
  # 4. Live Beispiel mit aktuellen Daten
  show_live_algo_example()
  
  cat("\n🎯 === ZUSAMMENFASSUNG === 🎯\n")
  cat("✅ Interest Score = Kombination aus Distanz zu Key-Levels + Volume\n")
  cat("✅ RESISTANCE/SUPPORT = Position relativ zum aktuellen Preis\n")
  cat("✅ Höhere Scores = wichtigere Trading-Levels\n")
  cat("✅ Runde Zahlen erhalten Bonus-Multiplikator\n")
  cat("✅ Volume verstärkt die Bedeutung der Levels\n\n")
  
  cat("💡 NUTZEN SIE DIE HEATMAP FÜR:\n")
  cat("   🎯 Entry/Exit Point Identifikation\n")
  cat("   🛡️ Stop Loss / Take Profit Platzierung\n")
  cat("   📊 Risk/Reward Ratio Optimierung\n")
  cat("   ⚡ Breakout/Breakdown Vorhersagen\n")
}

# ==========================================================================================================
# 🚀 QUICK ACCESS FUNKTIONEN
# ==========================================================================================================

#' Schnelle Score-Erklärung
explain_scores <- function() {
  explain_heatmap_scores()
}

#' Zeige Berechnungsbeispiel
show_example <- function() {
  show_calculation_example()
}

#' Komplette Erklärung
explain_all <- function() {
  explain_algo_heatmap_complete()
}

cat("✅ HEATMAP SCORE ERKLÄRUNG GELADEN!\n")
cat("📚 Detaillierte Erklärungen aller Berechnungen verfügbar\n\n")
cat("💡 VERFÜGBARE ERKLÄRUNGS-FUNKTIONEN:\n")
cat("   explain_scores()                  # Grundlagen der Score-Berechnung\n")
cat("   show_example()                    # Schritt-für-Schritt Beispiel\n")
cat("   explain_practical_usage()         # Praktische Trading-Anwendung\n")
cat("   show_live_algo_example()          # Live-Beispiel mit aktuellen ALGO Daten\n")
cat("   explain_all()                     # KOMPLETTE Erklärung (empfohlen!)\n\n")
cat("🚀 STARTEN SIE MIT:\n")
cat("   explain_all()                     # Vollständige Erklärung\n")