# ==========================================================================================================
# 🎯 STRATEGIC ADA TRADING SETUP - COMPLETE R SCRIPT
# ==========================================================================================================
# 
# ZWECK: Strategische Order bei 0.5636 USDT + intelligente TP/SL Level
# MODUS: Live Trading (echte Orders!)
# POSITION: 3000 ADA @ 0.5839 USDT
# 
# WICHTIG: Führe JEDEN ABSCHNITT EINZELN aus und prüfe das Ergebnis!
# ==========================================================================================================

cat("🎯 STRATEGIC ADA TRADING SETUP GESTARTET\n")
cat("========================================\n")
cat("Start Zeit:", as.character(Sys.time()), "\n")
cat("⚠️ ACHTUNG: Dieses Script platziert ECHTE ORDERS!\n\n")


# ==========================================================================================================
# 🔧 Clean Console
# ==========================================================================================================


# 1. Clean Console (optional)
tryCatch({
  source("c:/freeding/tbot202506/r_analysis/clean_console.R")
}, error = function(e) cat("⚠️ Clean console skipped\n"))


# ==========================================================================================================
# 📋 SCHRITT 1: SYSTEM VORBEREITUNG & LADEN
# ==========================================================================================================
# 
# Hier laden wir alle benötigten Systeme und prüfen die Verfügbarkeit aller Funktionen
# 

cat("📋 SCHRITT 1: SYSTEM SETUP\n")
cat("===========================\n")

# 1.1 Trading System laden (in korrekter Reihenfolge)
cat("🔧 Lade Trading System Komponenten...\n")

source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")
cat("✅ Complete Trading Analysis geladen\n")

source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r") 
cat("✅ System Fixes geladen\n")

source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")
cat("✅ Enhanced Collector geladen\n")


source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/strategic_orders.r")
cat("✅ Estrategicn order\n")


# 1.2 Funktions-Check
cat("\n🔍 Prüfe kritische Funktionen...\n")

required_functions <- c(
  "get_current_positions",
  "place_strategic_limit_order", 
  "place_tp_simple",
  "place_sl_simple",
  "get_current_plan_orders",
  "get_enhanced_ticker_data"
)

all_functions_ok <- TRUE
for (func in required_functions) {
  if (exists(func)) {
    cat(sprintf("✅ %s\n", func))
  } else {
    cat(sprintf("❌ %s - FEHLT!\n", func))
    all_functions_ok <- FALSE
  }
}

if (!all_functions_ok) {
  stop("❌ Kritische Funktionen fehlen - kann nicht fortfahren!")
}

cat("\n✅ Alle Systeme bereit!\n")

# ⏸️ PAUSE: Prüfe dass alle Funktionen ✅ zeigen bevor du fortsetzt!
cat("\n⏸️ CHECKPOINT 1: Sind alle Funktionen ✅? Falls ja, drücke ENTER zum Fortfahren...\n")
readline()

# ==========================================================================================================
# 📊 SCHRITT 2: AKTUELLE POSITION UND MARKT ANALYSIEREN
# ==========================================================================================================
# 
# Hier holen wir die aktuellen Daten zu deiner Position und dem Markt
#

cat("\n📊 SCHRITT 2: POSITION & MARKT ANALYSE\n")
cat("======================================\n")

# 2.1 Aktuelle ADA Position abrufen
cat("📋 Lade aktuelle ADA Position...\n")

ada_position <- get_current_positions('ADAUSDT_UMCBL')

if (!is.null(ada_position) && nrow(ada_position) > 0) {
  # Position Details extrahieren
  position_side <- ada_position$holdSide[1]
  position_size <- as.numeric(ada_position$total[1])
  entry_price <- as.numeric(ada_position$averageOpenPrice[1])
  current_pnl <- as.numeric(ada_position$unrealizedPL[1])
  
  cat(sprintf("✅ Position gefunden:\n"))
  cat(sprintf("   Seite: %s\n", position_side))
  cat(sprintf("   Größe: %.0f Kontrakte\n", position_size))
  cat(sprintf("   Entry: %.4f USDT\n", entry_price))
  cat(sprintf("   P&L: %.2f USDT (%s)\n", current_pnl, 
              if(current_pnl >= 0) "🟢 GEWINN" else "🔴 VERLUST"))
  
  # Globale Variablen für weitere Schritte setzen
  CURRENT_POSITION_SIZE <- position_size
  CURRENT_ENTRY_PRICE <- entry_price
  CURRENT_POSITION_SIDE <- position_side
  
} else {
  cat("❌ Keine ADA Position gefunden!\n")
  stop("Kann ohne aktive Position nicht fortfahren")
}

# 2.2 Aktuelle Marktdaten abrufen
cat(sprintf("\n📈 Lade aktuelle Marktdaten...\n"))

market_data <- get_enhanced_ticker_data('ADAUSDT_UMCBL')

if (!is.null(market_data)) {
  current_price <- market_data$last_price
  change_24h <- market_data$change_24h_pct
  volume_24h <- market_data$volume_24h_usdt / 1000000
  
  cat(sprintf("📊 Marktinformation:\n"))
  cat(sprintf("   Aktueller Preis: %.4f USDT\n", current_price))
  cat(sprintf("   24h Änderung: %.2f%%\n", change_24h))
  cat(sprintf("   24h Volumen: %.1fM USDT\n", volume_24h))
  
  CURRENT_MARKET_PRICE <- current_price
  
} else {
  cat("⚠️ Verwende Fallback Marktdaten\n")
  CURRENT_MARKET_PRICE <- 0.5820  # Fallback aus deinen Logs
}

# 2.3 Bestehende Orders prüfen
cat(sprintf("\n📋 Prüfe bestehende Orders...\n"))

existing_orders <- get_current_plan_orders('ADAUSDT_UMCBL')
if (!is.null(existing_orders) && nrow(existing_orders) > 0) {
  cat(sprintf("⚠️ %d bestehende Plan-Orders gefunden!\n", nrow(existing_orders)))
  for (i in 1:nrow(existing_orders)) {
    order <- existing_orders[i, ]
    cat(sprintf("   %s: %s @ %.4f USDT\n", 
                order$planType, order$size, as.numeric(order$triggerPrice)))
  }
  cat("💡 Überlege bestehende Orders zu stornieren\n")
} else {
  cat("✅ Keine bestehenden Orders - freie Bahn\n")
}

# ⏸️ PAUSE: Prüfe die Position und Marktdaten
cat(sprintf("\n⏸️ CHECKPOINT 2: Position und Markt korrekt? Falls ja, ENTER...\n"))
cat(sprintf("   Position: %.0f ADA @ %.4f USDT\n", CURRENT_POSITION_SIZE, CURRENT_ENTRY_PRICE))
cat(sprintf("   Aktuell: %.4f USDT (%.2f USDT P&L)\n", CURRENT_MARKET_PRICE, current_pnl))
readline()

# ==========================================================================================================
# 🚀 SCHRITT 3: STRATEGIC ORDER PLATZIEREN (DCA BEI 0.5636 USDT)
# ==========================================================================================================
# 
# Hier platzieren wir die strategische Buy-Order für Dollar Cost Averaging
#

cat("\n🚀 SCHRITT 3: STRATEGIC ORDER (DCA)\n")
cat("===================================\n")

# 3.1 Strategic Order Parameter definieren
STRATEGIC_SYMBOL <- 'ADAUSDT_UMCBL'
STRATEGIC_PRICE <- 0.498     # Zielpreis für DCA (3.16% günstiger)
STRATEGIC_SIZE <- 3000         # Anzahl Kontrakte
STRATEGIC_SIDE <- "open_long"  # Neue Long Position öffnen

# Berechnungen
strategic_value <- STRATEGIC_PRICE * STRATEGIC_SIZE
discount_percentage <- ((STRATEGIC_PRICE / CURRENT_MARKET_PRICE) - 1) * 100

cat(sprintf("📋 Strategic Order Konfiguration:\n"))
cat(sprintf("   Symbol: %s\n", STRATEGIC_SYMBOL))
cat(sprintf("   Aktion: %s (%s)\n", STRATEGIC_SIDE, "Neue Long Position"))
cat(sprintf("   Größe: %d Kontrakte\n", STRATEGIC_SIZE))
cat(sprintf("   Preis: %.4f USDT\n", STRATEGIC_PRICE))
cat(sprintf("   Wert: %.2f USDT\n", strategic_value))
cat(sprintf("   Rabatt: %.2f%% vom aktuellen Preis\n", discount_percentage))
cat(sprintf("   Strategie: Dollar Cost Averaging (DCA)\n"))

# 3.2 Sicherheitsvalidierung
cat(sprintf("\n🔍 Sicherheitsprüfung...\n"))

# Validierungen
price_ok <- STRATEGIC_PRICE < CURRENT_MARKET_PRICE
size_ok <- STRATEGIC_SIZE > 0 && STRATEGIC_SIZE <= 10000
value_ok <- strategic_value >= 1000 && strategic_value <= 5000
discount_ok <- abs(discount_percentage) >= 2.0

cat(sprintf("   Preis unter Markt: %s (%.4f < %.4f)\n", 
            if(price_ok) "✅" else "❌", STRATEGIC_PRICE, CURRENT_MARKET_PRICE))
cat(sprintf("   Größe vernünftig: %s (%d Kontrakte)\n", 
            if(size_ok) "✅" else "❌", STRATEGIC_SIZE))
cat(sprintf("   Wert angemessen: %s (%.0f USDT)\n", 
            if(value_ok) "✅" else "❌", strategic_value))
cat(sprintf("   Rabatt ausreichend: %s (%.2f%%)\n", 
            if(discount_ok) "✅" else "❌", abs(discount_percentage)))

all_validations_ok <- price_ok && size_ok && value_ok && discount_ok

if (!all_validations_ok) {
  cat("❌ Validierung fehlgeschlagen - Strategic Order wird übersprungen!\n")
  strategic_order_success <- FALSE
} else {
  cat("✅ Alle Validierungen bestanden - bereit zum Platzieren\n")
  
  # 3.3 Finale Bestätigung
  cat(sprintf("\n⚠️ FINALE BESTÄTIGUNG FÜR STRATEGIC ORDER\n"))
  cat(sprintf("==========================================\n"))
  cat(sprintf("Du bist dabei eine ECHTE ORDER zu platzieren:\n\n"))
  cat(sprintf("📋 Order Details:\n"))
  cat(sprintf("   KAUFE: %d ADA Kontrakte\n", STRATEGIC_SIZE))
  cat(sprintf("   PREIS: %.4f USDT\n", STRATEGIC_PRICE))
  cat(sprintf("   WERT: %.2f USDT\n", strategic_value))
  cat(sprintf("   RABATT: %.2f%% vom Marktpreis\n", discount_percentage))
  cat(sprintf("\n🚨 Dies ist eine ECHTE ORDER mit ECHTEM GELD!\n"))
  cat(sprintf("Drücke ENTER um fortzufahren oder Ctrl+C zum Abbrechen...\n"))
  
  readline()
  
  # 3.4 Strategic Order platzieren
  cat(sprintf("\n🚀 PLATZIERE STRATEGIC ORDER...\n"))
  
  strategic_result <- place_strategic_limit_order(
    symbol = STRATEGIC_SYMBOL,
    side = STRATEGIC_SIDE,
    size = STRATEGIC_SIZE,
    price = STRATEGIC_PRICE
  )
  
  # Ergebnis prüfen
  if (!is.null(strategic_result) && strategic_result$success) {
    cat(sprintf("✅ STRATEGIC ORDER ERFOLGREICH PLATZIERT!\n"))
    cat(sprintf("   Order ID: %s\n", strategic_result$order_id))
    cat(sprintf("   Status: Wartend bei %.4f USDT\n", STRATEGIC_PRICE))
    cat(sprintf("   Wird gefüllt wenn ADA auf Zielpreis fällt\n"))
    
    strategic_order_success <- TRUE
    strategic_order_id <- strategic_result$order_id
    
  } else {
    cat(sprintf("❌ STRATEGIC ORDER FEHLGESCHLAGEN!\n"))
    if (!is.null(strategic_result) && !is.null(strategic_result$error)) {
      cat(sprintf("   Fehler: %s\n", strategic_result$error))
    }
    strategic_order_success <- FALSE
    strategic_order_id <- NULL
  }
}

# ⏸️ PAUSE: Strategic Order Status prüfen
cat(sprintf("\n⏸️ CHECKPOINT 3: Strategic Order Status prüfen\n"))
if (exists("strategic_order_success") && strategic_order_success) {
  cat(sprintf("✅ Strategic Order platziert - bereit für TP/SL Setup\n"))
} else {
  cat(sprintf("❌ Strategic Order nicht platziert - trotzdem mit TP/SL fortfahren?\n"))
}
cat(sprintf("ENTER zum Fortfahren...\n"))
readline()

# ==========================================================================================================
# 📈 SCHRITT 4: TAKE PROFIT ORDERS PLATZIEREN (GESTAFFELTE GEWINNMITNAHME)
# ==========================================================================================================
# 
# Hier setzen wir 3 gestaffelte TP-Level für optimale Gewinnmitnahme
#

cat(sprintf("\n📈 SCHRITT 4: TAKE PROFIT ORDERS\n"))
cat(sprintf("=================================\n"))

# 4.1 TP Level konfigurieren
cat(sprintf("📊 Konfiguriere TP Level basierend auf deiner Position...\n"))
cat(sprintf("   Position: %.0f ADA @ %.4f USDT\n", CURRENT_POSITION_SIZE, CURRENT_ENTRY_PRICE))

# TP Level definieren (gestaffelt für optimale Gewinnmitnahme)
tp_levels <- list(
  level1 = list(
    name = "Konservativ TP",
    price = 0.5950,
    size = round(CURRENT_POSITION_SIZE * 0.40),  # 40% der Position
    percentage = ((0.5950 / CURRENT_ENTRY_PRICE) - 1) * 100
  ),
  level2 = list(
    name = "Moderat TP", 
    price = 0.6050,
    size = round(CURRENT_POSITION_SIZE * 0.30),  # 30% der Position
    percentage = ((0.6050 / CURRENT_ENTRY_PRICE) - 1) * 100
  ),
  level3 = list(
    name = "Aggressiv TP",
    price = 0.6200,
    size = round(CURRENT_POSITION_SIZE * 0.30),  # 30% der Position  
    percentage = ((0.6200 / CURRENT_ENTRY_PRICE) - 1) * 100
  )
)

# TP Konfiguration anzeigen
cat(sprintf("\n📈 Take Profit Level:\n"))
total_tp_size <- 0
for (level_name in names(tp_levels)) {
  level <- tp_levels[[level_name]]
  total_tp_size <- total_tp_size + level$size
  profit_amount <- (level$price - CURRENT_ENTRY_PRICE) * level$size
  
  cat(sprintf("   %s: %d Kontrakte @ %.4f USDT (+%.1f%% = +%.0f USDT)\n", 
              level$name, level$size, level$price, level$percentage, profit_amount))
}

cat(sprintf("\n📊 Abdeckung: %d/%d Kontrakte (%.1f%% der Position)\n", 
            total_tp_size, CURRENT_POSITION_SIZE, 
            (total_tp_size/CURRENT_POSITION_SIZE)*100))

# 4.2 TP Orders platzieren
cat(sprintf("\n🚀 Platziere TP Orders...\n"))

tp_results <- list()

for (level_name in names(tp_levels)) {
  level <- tp_levels[[level_name]]
  
  cat(sprintf("\n🎯 Platziere %s...\n", level$name))
  cat(sprintf("   Preis: %.4f USDT (+%.1f%%)\n", level$price, level$percentage))
  cat(sprintf("   Größe: %d Kontrakte\n", level$size))
  
  expected_profit <- (level$price - CURRENT_ENTRY_PRICE) * level$size
  cat(sprintf("   Erwarteter Gewinn: %.2f USDT\n", expected_profit))
  
  # TP Order platzieren
  tp_result <- place_tp_simple(
    symbol = 'ADAUSDT_UMCBL',
    side = CURRENT_POSITION_SIDE,
    size = as.character(level$size),
    trigger_price = level$price
  )
  
  # Ergebnis bewerten
  if (!is.null(tp_result) && tp_result$success) {
    cat(sprintf("   ✅ ERFOLGREICH - Order ID: %s\n", tp_result$order_id))
    tp_results[[level_name]] <- list(success = TRUE, order_id = tp_result$order_id, level = level)
  } else {
    cat(sprintf("   ❌ FEHLGESCHLAGEN\n"))
    if (!is.null(tp_result) && !is.null(tp_result$error)) {
      cat(sprintf("   Fehler: %s\n", tp_result$error))
    }
    tp_results[[level_name]] <- list(success = FALSE, error = "Order fehlgeschlagen", level = level)
  }
  
  # Pause zwischen Orders (API Rate Limiting)
  Sys.sleep(1.5)
}

# TP Summary
successful_tp_orders <- sum(sapply(tp_results, function(x) x$success))
cat(sprintf("\n📊 TP ORDERS ZUSAMMENFASSUNG: %d/%d erfolgreich\n", 
            successful_tp_orders, length(tp_results)))

# ⏸️ PAUSE: TP Orders Status prüfen
cat(sprintf("\n⏸️ CHECKPOINT 4A: TP Orders Status prüfen\n"))
cat(sprintf("✅ %d von %d TP Orders erfolgreich platziert\n", successful_tp_orders, length(tp_results)))
cat(sprintf("ENTER für Stop Loss Setup...\n"))
readline()

# ==========================================================================================================
# 🛡️ SCHRITT 5: STOP LOSS ORDER PLATZIEREN (RISIKOSCHUTZ)
# ==========================================================================================================
# 
# Hier setzen wir eine Stop Loss Order als Risikobegrenzung
#

cat(sprintf("\n🛡️ SCHRITT 5: STOP LOSS ORDER\n"))
cat(sprintf("==============================\n"))

# 5.1 SL Konfiguration
sl_config <- list(
  name = "Risiko-Schutz SL",
  price = 0.5750,
  size = CURRENT_POSITION_SIZE,  # 100% der Position schützen
  percentage = ((0.5750 / CURRENT_ENTRY_PRICE) - 1) * 100
)

max_loss_amount <- (sl_config$price - CURRENT_ENTRY_PRICE) * sl_config$size

cat(sprintf("🛡️ Stop Loss Konfiguration:\n"))
cat(sprintf("   Name: %s\n", sl_config$name))
cat(sprintf("   Preis: %.4f USDT (%.1f%%)\n", sl_config$price, sl_config$percentage))
cat(sprintf("   Größe: %d Kontrakte (100%% Schutz)\n", sl_config$size))
cat(sprintf("   Maximaler Verlust: %.2f USDT\n", max_loss_amount))

# 5.2 SL Order platzieren
cat(sprintf("\n🚀 Platziere Stop Loss Order...\n"))

sl_result <- place_sl_simple(
  symbol = 'ADAUSDT_UMCBL',
  side = CURRENT_POSITION_SIDE,
  size = as.character(sl_config$size),
  trigger_price = sl_config$price
)

# Ergebnis bewerten
if (!is.null(sl_result) && sl_result$success) {
  cat(sprintf("✅ STOP LOSS ERFOLGREICH PLATZIERT!\n"))
  cat(sprintf("   Order ID: %s\n", sl_result$order_id))
  cat(sprintf("   Schutz aktiv bei %.4f USDT\n", sl_config$price))
  
  sl_order_success <- TRUE
  sl_order_id <- sl_result$order_id
  
} else {
  cat(sprintf("❌ STOP LOSS FEHLGESCHLAGEN!\n"))
  if (!is.null(sl_result) && !is.null(sl_result$error)) {
    cat(sprintf("   Fehler: %s\n", sl_result$error))
  }
  sl_order_success <- FALSE
  sl_order_id <- NULL
}

# ⏸️ PAUSE: SL Order Status prüfen
cat(sprintf("\n⏸️ CHECKPOINT 4B: Stop Loss Status prüfen\n"))
if (exists("sl_order_success") && sl_order_success) {
  cat(sprintf("✅ Stop Loss als Risikoschutz aktiv\n"))
} else {
  cat(sprintf("❌ Stop Loss nicht platziert - Position ungeschützt!\n"))
}
cat(sprintf("ENTER für Verifikation...\n"))
readline()

# ==========================================================================================================
# 🔍 SCHRITT 6: ORDER VERIFIKATION UND STATUS ÜBERSICHT
# ==========================================================================================================
# 
# Hier prüfen wir alle platzierten Orders und erstellen eine Übersicht
#

cat(sprintf("\n🔍 SCHRITT 6: ORDER VERIFIKATION\n"))
cat(sprintf("=================================\n"))

# 6.1 Kurz warten für Order-Registrierung
cat(sprintf("⏳ Warte auf Order-Registrierung im System...\n"))
Sys.sleep(3)

# 6.2 Plan Orders prüfen (TP/SL)
cat(sprintf("📋 Plan Orders (TP/SL) Verifikation:\n"))
final_plan_orders <- get_current_plan_orders('ADAUSDT_UMCBL')

if (!is.null(final_plan_orders) && nrow(final_plan_orders) > 0) {
  cat(sprintf("✅ %d aktive Plan Orders gefunden:\n", nrow(final_plan_orders)))
  
  for (i in 1:nrow(final_plan_orders)) {
    order <- final_plan_orders[i, ]
    order_type_display <- switch(order$planType,
                                 "pos_profit" = "📈 Take Profit",
                                 "pos_loss" = "🛡️ Stop Loss", 
                                 "📋 Plan Order")
    
    cat(sprintf("   %s: %s Kontrakte @ %.4f USDT\n", 
                order_type_display, order$size, as.numeric(order$triggerPrice)))
  }
} else {
  cat(sprintf("❌ Keine Plan Orders gefunden - möglicherweise Platzierungsprobleme!\n"))
}

# 6.3 Open Orders prüfen (Strategic)
cat(sprintf("\n📋 Open Orders (Strategic) Verifikation:\n"))
if (exists("get_current_open_orders")) {
  final_open_orders <- get_current_open_orders('ADAUSDT_UMCBL')
  
  if (!is.null(final_open_orders) && nrow(final_open_orders) > 0) {
    cat(sprintf("✅ %d aktive Open Orders gefunden:\n", nrow(final_open_orders)))
    
    for (i in 1:nrow(final_open_orders)) {
      order <- final_open_orders[i, ]
      cat(sprintf("   🚀 %s: %s Kontrakte @ %.4f USDT\n", 
                  order$side, order$size, as.numeric(order$price)))
    }
  } else {
    cat(sprintf("❌ Keine Open Orders gefunden\n"))
    if (exists("strategic_order_success") && strategic_order_success) {
      cat(sprintf("⚠️ Strategic Order wurde platziert aber ist nicht sichtbar\n"))
    }
  }
} else {
  cat(sprintf("ℹ️ Open Orders Verifikation nicht verfügbar\n"))
}

# ==========================================================================================================
# 📊 SCHRITT 7: FINALE ZUSAMMENFASSUNG UND MONITORING SETUP
# ==========================================================================================================
# 
# Abschließende Übersicht und Setup für laufende Überwachung
#

cat(sprintf("\n📊 SCHRITT 7: FINALE ZUSAMMENFASSUNG\n"))
cat(sprintf("====================================\n"))

# 7.1 Ausführungs-Übersicht
execution_end <- Sys.time()
cat(sprintf("⏱️ Setup abgeschlossen um: %s\n", as.character(execution_end)))

# Strategic Order Status
cat(sprintf("\n🚀 Strategic Order Status:\n"))
if (exists("strategic_order_success") && strategic_order_success) {
  cat(sprintf("   ✅ PLATZIERT - %d ADA @ %.4f USDT\n", STRATEGIC_SIZE, STRATEGIC_PRICE))
  cat(sprintf("   💰 Wert: %.2f USDT\n", STRATEGIC_PRICE * STRATEGIC_SIZE))
} else {
  cat(sprintf("   ❌ NICHT PLATZIERT oder FEHLGESCHLAGEN\n"))
}

# TP Orders Status
cat(sprintf("\n📈 Take Profit Orders Status:\n"))
if (exists("tp_results") && length(tp_results) > 0) {
  successful_tp <- sum(sapply(tp_results, function(x) x$success))
  cat(sprintf("   📊 Erfolgsrate: %d/%d Orders platziert\n", successful_tp, length(tp_results)))
  
  for (level_name in names(tp_results)) {
    result <- tp_results[[level_name]]
    status_icon <- if(result$success) "✅" else "❌"
    cat(sprintf("   %s %s: %d Kontrakte @ %.4f USDT\n", 
                status_icon, result$level$name, result$level$size, result$level$price))
  }
} else {
  cat(sprintf("   ❌ KEINE TP ORDERS VERSUCHT\n"))
}

# SL Order Status
cat(sprintf("\n🛡️ Stop Loss Order Status:\n"))
if (exists("sl_order_success") && sl_order_success) {
  cat(sprintf("   ✅ PLATZIERT - %d Kontrakte @ %.4f USDT\n", 
              sl_config$size, sl_config$price))
  cat(sprintf("   🛡️ Max Verlust: %.2f USDT (%.1f%%)\n", 
              max_loss_amount, sl_config$percentage))
} else {
  cat(sprintf("   ❌ NICHT PLATZIERT - POSITION UNGESCHÜTZT!\n"))
}

# 7.2 Monitoring Funktion erstellen
cat(sprintf("\n📱 Monitoring Setup...\n"))

# Einfache Monitoring Funktion definieren
monitor_ada_position <- function() {
  cat(sprintf("\n📊 ADA POSITION MONITOR - %s\n", Sys.time()))
  cat(sprintf("=====================================\n"))
  
  # Position Status
  pos <- get_current_positions('ADAUSDT_UMCBL')
  if (!is.null(pos) && nrow(pos) > 0) {
    current_pnl <- as.numeric(pos$unrealizedPL[1])
    pnl_status <- if(current_pnl >= 0) "🟢 GEWINN" else "🔴 VERLUST"
    cat(sprintf("💰 Position P&L: %.2f USDT (%s)\n", current_pnl, pnl_status))
  }
  
  # Aktueller Marktpreis
  market <- get_enhanced_ticker_data('ADAUSDT_UMCBL')
  if (!is.null(market)) {
    trend_icon <- if(market$change_24h_pct >= 0) "📈" else "📉"
    cat(sprintf("💱 Aktueller Preis: %.4f USDT (%s %.2f%% 24h)\n", 
                market$last_price, trend_icon, market$change_24h_pct))
    
    # Strategic Order Distanz
    strategic_distance <- ((0.5636 / market$last_price) - 1) * 100
    cat(sprintf("🎯 Strategic Distanz: %.2f%% zum Ziel (%.4f USDT)\n", 
                strategic_distance, 0.5636))
  }
  
  # Aktive Orders Anzahl
  plan_orders <- get_current_plan_orders('ADAUSDT_UMCBL')
  plan_count <- if(!is.null(plan_orders)) nrow(plan_orders) else 0
  cat(sprintf("📋 Aktive Orders: %d Plan Orders\n", plan_count))
}

cat(sprintf("✅ Monitoring Funktion erstellt!\n"))
cat(sprintf("Verwende: monitor_ada_position() für Status-Check\n"))

# ==========================================================================================================
# 🎯 SETUP ABGESCHLOSSEN - WAS PASSIERT JETZT?
# ==========================================================================================================

cat(sprintf("\n🎯 SETUP ERFOLGREICH ABGESCHLOSSEN!\n"))
cat(sprintf("===================================\n"))

cat(sprintf("✅ Deine ADA Position ist jetzt professionell verwaltet:\n"))
cat(sprintf("   🚀 Strategic DCA Order bei 0.5636 USDT (falls platziert)\n"))
cat(sprintf("   📈 Gestaffelte Gewinnmitnahme über 3 TP Level\n"))
cat(sprintf("   🛡️ Risikoschutz via Stop Loss (falls platziert)\n"))
cat(sprintf("   📱 Monitoring Funktion für Überwachung\n"))

cat(sprintf("\n💡 Was passiert jetzt:\n"))
cat(sprintf("   📊 Überwache mit: monitor_ada_position()\n"))
cat(sprintf("   🎯 Strategic Order füllt bei ADA = 0.5636 USDT\n"))
cat(sprintf("   📈 TP Orders werden automatisch bei Gewinn-Levels ausgeführt\n"))
cat(sprintf("   🛡️ SL Order schützt vor größeren Verlusten\n"))
cat(sprintf("   📱 Prüfe 2-3x täglich, aber nicht obsessiv!\n"))

cat(sprintf("\n🏆 HERZLICHEN GLÜCKWUNSCH!\n"))
cat(sprintf("Du hast ein professionelles Trading-Setup wie institutionelle Trader! 🚀💰\n"))

# Test der Monitoring Funktion
cat(sprintf("\n🔍 INITIAL MONITORING TEST:\n"))
monitor_ada_position()

cat(sprintf("\n✅ SCRIPT KOMPLETT ABGESCHLOSSEN! ✅\n"))



# Zeige alle verfügbaren Order-Funktionen:
ls()[grep("order|Order|place", ls(), ignore.case = TRUE)]

monitor_ada_position()
