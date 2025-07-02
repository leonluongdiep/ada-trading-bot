# ==========================================================================================================
# 🔧 FLOWGRAPH FIX - Für r_trading_flowgraph.r
# ==========================================================================================================

# PROBLEM: live_nodes DataFrame hat keine 'value' Spalte
# LÖSUNG: Fügen Sie 'value' Spalte hinzu oder verwenden Sie Fallback in mutate()

# ==========================================================================================================
# OPTION 1: FIX in create_live_trading_flowgraph() Funktion
# ==========================================================================================================

# ERSETZEN Sie diesen Code (ca. Zeile 270):
# live_nodes <- data.frame(
#   id = 1:10,
#   label = c("System", "ADA Analysis", "BTC Analysis", "ETH Analysis",
#             "Ticker Data", "Sentiment", "Technical", "Positions", "Signals", "Output"),
#   stringsAsFactors = FALSE
# )

# MIT diesem Code:
live_nodes <- data.frame(
  id = 1:10,
  label = c("System", "ADA Analysis", "BTC Analysis", "ETH Analysis",
            "Ticker Data", "Sentiment", "Technical", "Positions", "Signals", "Output"),
  # ✅ FIX: value Spalte hinzufügen
  value = c(100, 90, 85, 80, 95, 88, 92, 85, 90, 95),
  # ✅ FIX: Weitere benötigte Spalten hinzufügen
  status = c("success", "bullish", "neutral", "bearish", "success", "success", "success", "success", "success", "success"),
  group = c("system", "ada", "btc", "eth", "data", "data", "analysis", "monitoring", "output", "output"),
  details = c("Live System", "Live ADA Data", "Live BTC Data", "Live ETH Data", 
              "Real Ticker", "Live Sentiment", "Live Analysis", "Live Positions", "Live Signals", "Final Output"),
  stringsAsFactors = FALSE
)

# ==========================================================================================================
# OPTION 2: SICHERE FIX in create_visnetwork_flowgraph() Funktion  
# ==========================================================================================================

# ERSETZEN Sie diesen Code (ca. Zeile 150):
# vis_nodes <- nodes %>%
#   mutate(
#     color = case_when(...),
#     size = pmax(20, value * 0.4),  # ❌ PROBLEM HIER
#     shape = case_when(...)
#   )

# MIT diesem Code:
vis_nodes <- nodes %>%
  mutate(
    # Colors based on status
    color = case_when(
      status == "success" ~ "#27ae60",
      status == "partial" ~ "#f39c12", 
      status == "error" ~ "#e74c3c",
      status == "bullish" ~ "#9b59b6",
      status == "neutral" ~ "#95a5a6", 
      status == "bearish" ~ "#e74c3c",
      TRUE ~ "#3498db"
    ),
    # ✅ SICHERE SIZES: Prüfung ob 'value' existiert
    size = if("value" %in% names(nodes)) {
      pmax(20, value * 0.4)
    } else {
      case_when(
        group == "system" ~ 35,
        group == "hub" ~ 40,
        group %in% c("ada", "btc", "eth") ~ 30,
        group == "data" ~ 25,
        group == "analysis" ~ 30,
        TRUE ~ 25
      )
    },
    # Shapes
    shape = case_when(
      group == "system" ~ "box",
      group == "hub" ~ "diamond", 
      group %in% c("ada", "btc", "eth") ~ "circle",
      group == "data" ~ "triangle",
      group == "analysis" ~ "star",
      TRUE ~ "ellipse"
    ),
    # Tooltips - sichere Spalten-Prüfung
    title = if("details" %in% names(nodes)) {
      paste0("<b>", label, "</b><br>", 
             "Status: ", if("status" %in% names(nodes)) status else "unknown", "<br>",
             "Details:<br>", gsub("\n", "<br>", details))
    } else {
      paste0("<b>", label, "</b><br>Live Trading Node")
    }
  )

# ==========================================================================================================
# OPTION 3: PLOTLY FIX (ca. Zeile 370)
# ==========================================================================================================

# ERSETZEN Sie:
# marker = list(
#   size = pmax(20, nodes$value * 0.6),  # ❌ PROBLEM
#   color = node_colors,
#   line = list(color = "white", width = 2)
# ),

# MIT:
marker = list(
  size = if("value" %in% names(nodes)) {
    pmax(20, nodes$value * 0.6)
  } else {
    rep(30, nrow(nodes))  # Fallback Größe
  },
  color = node_colors,
  line = list(color = "white", width = 2)
),

# ==========================================================================================================
# SCHNELLSTE LÖSUNG: UNIVERSAL PATCH
# ==========================================================================================================

# Fügen Sie diese Funktion am ANFANG der Datei hinzu (nach den Libraries):

ensure_required_columns <- function(nodes_df) {
  # Stellt sicher, dass alle benötigten Spalten existieren
  
  if (!"value" %in% names(nodes_df)) {
    nodes_df$value <- rep(30, nrow(nodes_df))
  }
  
  if (!"status" %in% names(nodes_df)) {
    nodes_df$status <- rep("success", nrow(nodes_df))
  }
  
  if (!"group" %in% names(nodes_df)) {
    nodes_df$group <- rep("default", nrow(nodes_df))
  }
  
  if (!"details" %in% names(nodes_df)) {
    nodes_df$details <- paste("Node", nodes_df$id)
  }
  
  return(nodes_df)
}

# Dann am ANFANG jeder Flowgraph-Funktion:
# nodes <- ensure_required_columns(system_data$nodes)

# ==========================================================================================================
# TEST NACH DEM FIX
# ==========================================================================================================

# Testen Sie nach dem Fix:
test_flowgraph_fix <- function() {
  cat("🧪 Testing flowgraph fix...\n")
  
  tryCatch({
    # Test alle Varianten
    vis_graph <- create_visnetwork_flowgraph()
    cat("✅ visNetwork: OK\n")
    
    plotly_graph <- create_plotly_flowgraph()
    cat("✅ Plotly: OK\n")
    
    # Test mit Live-Daten
    live_graph <- create_live_trading_flowgraph()
    cat("✅ Live graph: OK\n")
    
    cat("🎉 ALL FLOWGRAPH FIXES SUCCESSFUL!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Still error:", e$message, "\n")
    return(FALSE)
  })
}
