# ==========================================================================================================
# 🚀 R TRADING SYSTEM FLOW GRAPH GENERATOR
# ==========================================================================================================
# 
# ZWECK: Erstellt interaktive Flowgraphs des Trading Systems basierend auf Console Logs
# BIBLIOTHEKEN: DiagrammeR, visNetwork, plotly für verschiedene Visualisierungen
# INTEGRATION: Kompatibel mit Ihrem bestehenden Trading System
# 
# ==========================================================================================================

cat("📊 Loading Trading System Flow Graph Generator...\n")

# ==========================================================================================================
# 📚 REQUIRED LIBRARIES
# ==========================================================================================================

# Installiere und lade benötigte Bibliotheken
required_packages <- c("DiagrammeR", "visNetwork", "plotly", "htmlwidgets", "dplyr", "jsonlite")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("📦 Installing %s...\n", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("✅ All libraries loaded successfully!\n")

# ==========================================================================================================
# 🎯 TRADING SYSTEM DATA STRUCTURE
# ==========================================================================================================

# Definiere Trading System Komponenten basierend auf Console Log
create_trading_system_data <- function() {
  
  # Nodes (Systemkomponenten)
  nodes <- data.frame(
    id = 1:15,
    label = c(
      "🔧 System Init",
      "📝 Script Loading", 
      "🔍 Function Check",
      "🎯 Multi-Asset Hub",
      "💜 ADA Analysis",
      "🟠 BTC Analysis", 
      "🔵 ETH Analysis",
      "📈 Ticker Data",
      "📚 Orderbook",
      "🔄 Trades Analysis",
      "🧮 Sentiment Calc",
      "🧮 Technical Analysis",
      "🎯 Position Check",
      "📋 Results Output",
      "🔄 Console Restore"
    ),
    group = c(
      "system", "system", "system", "hub",
      "ada", "btc", "eth", 
      "data", "data", "data", "data",
      "analysis", "monitoring", "output", "system"
    ),
    status = c(
      "success", "success", "partial", "success",
      "bullish", "neutral", "bearish",
      "success", "success", "success", "success", 
      "success", "success", "success", "error"
    ),
    details = c(
      "Console Management\nAPI Credentials\nUTF-8 Encoding",
      "4 Core Scripts\nTrading Analysis\nSystem Fixes",
      "7/12 Functions\nAPI Ready\nPartial Load",
      "3 Assets\nParallel Analysis\nLive Data",
      "Price: $0.5527\nSignal: BULLISH\nSentiment: 60%",
      "Price: $106,872\nSignal: NEUTRAL\nSentiment: 20%", 
      "Price: $2,408\nSignal: SELL\nSentiment: -20%",
      "Live Prices\n24h Volume\nFunding Rate",
      "Bid/Ask Ratio\nSpread Analysis\nLiquidity",
      "50 Recent Trades\nBuy/Sell Ratio\nVolume Analysis",
      "5 Factors\nScore Calculation\nClassification",
      "RSI, SMA, MACD\nBollinger Bands\nSignal Generation",
      "ADA: 6000 contracts\nPnL: -$95\nBTC/ETH: None",
      "3 Assets Complete\nRankings\nTrading Signals",
      "Log Saved\nMinor Issue\nFixed in v2"
    ),
    value = c(100, 95, 58, 100, 75, 60, 40, 90, 85, 88, 82, 92, 95, 100, 20)
  )
  
  # Edges (Verbindungen zwischen Komponenten)
  edges <- data.frame(
    from = c(1, 2, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 9, 10, 11, 12, 13, 3, 14),
    to =   c(2, 3, 4, 5, 6, 7, 8, 9, 8, 9, 8, 9, 11, 11, 11, 11, 13, 14, 14, 15),
    type = c(
      "success", "success", "partial", "flow", "flow", "flow",
      "data", "data", "data", "data", "data", "data", 
      "analysis", "analysis", "analysis", "analysis",
      "monitoring", "output", "output", "error"
    ),
    width = c(3, 3, 2, 2, 2, 2, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 2, 2, 2, 2, 2, 3, 2, 1)
  )
  
  return(list(nodes = nodes, edges = edges))
}

# ==========================================================================================================
# 📊 DIAGRAMMER FLOWGRAPH
# ==========================================================================================================

create_diagrammer_flowgraph <- function(system_data = NULL) {
  
  if (is.null(system_data)) {
    system_data <- create_trading_system_data()
  }
  
  cat("🎨 Creating DiagrammeR flowgraph...\n")
  
  # DOT Graph Definition
  dot_graph <- "
  digraph TradingSystem {
    
    # Graph attributes
    graph [layout = dot, rankdir = TB, bgcolor = '#2c3e50', fontcolor = white]
    
    # Node defaults
    node [shape = box, style = 'rounded,filled', fontname = Arial, fontcolor = white, fontsize = 10]
    
    # Edge defaults  
    edge [color = '#3498db', fontcolor = white, fontsize = 8]
    
    # System Layer
    subgraph cluster_0 {
      label = '🔧 System Layer'
      color = '#34495e'
      style = filled
      fontcolor = white
      
      init [label = '🔧 System Init\\nConsole & API', fillcolor = '#27ae60']
      scripts [label = '📝 Script Loading\\n4 Core Scripts', fillcolor = '#27ae60'] 
      functions [label = '🔍 Function Check\\n7/12 Available', fillcolor = '#f39c12']
    }
    
    # Analysis Hub
    hub [label = '🎯 Multi-Asset Hub\\n3 Assets Parallel', fillcolor = '#3498db']
    
    # Asset Analysis Layer
    subgraph cluster_1 {
      label = '📈 Asset Analysis'
      color = '#34495e' 
      style = filled
      fontcolor = white
      
      ada [label = '💜 ADA\\n$0.5527\\nBULLISH 60%', fillcolor = '#9b59b6']
      btc [label = '🟠 BTC\\n$106,872\\nNEUTRAL 20%', fillcolor = '#f39c12']
      eth [label = '🔵 ETH\\n$2,408\\nSELL -20%', fillcolor = '#3498db']
    }
    
    # Data Collection Layer
    subgraph cluster_2 {
      label = '📊 Data Collection'
      color = '#34495e'
      style = filled 
      fontcolor = white
      
      ticker [label = '📈 Ticker\\nLive Prices', fillcolor = '#27ae60']
      orderbook [label = '📚 Orderbook\\nSpread Analysis', fillcolor = '#27ae60']
      trades [label = '🔄 Trades\\n50 Recent', fillcolor = '#27ae60']
      sentiment [label = '🧮 Sentiment\\n5 Factors', fillcolor = '#27ae60']
    }
    
    # Analysis & Output Layer
    technical [label = '🧮 Technical\\nRSI, SMA, MACD', fillcolor = '#2ecc71']
    positions [label = '🎯 Positions\\nADA: 6000\\nPnL: -$95', fillcolor = '#e67e22']
    results [label = '📋 Results\\n3 Assets Complete', fillcolor = '#27ae60']
    console [label = '🔄 Console\\nRestore (Fixed)', fillcolor = '#e74c3c']
    
    # Flow connections
    init -> scripts -> functions -> hub
    hub -> {ada btc eth}
    {ada btc eth} -> {ticker orderbook trades}
    {ticker orderbook trades sentiment} -> technical
    hub -> positions
    {technical positions} -> results
    functions -> results
    results -> console
    
  }"
  
  # Render graph
  graph <- grViz(dot_graph)
  
  # Save as HTML widget
  html_file <- paste0("c:/freeding/tbot202506/reports/trading_flowgraph_", 
                      format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  
  tryCatch({
    dir.create(dirname(html_file), recursive = TRUE, showWarnings = FALSE)
    saveWidget(graph, html_file, selfcontained = TRUE)
    cat("💾 DiagrammeR graph saved:", basename(html_file), "\n")
  }, error = function(e) {
    cat("⚠️ Could not save HTML file\n")
  })
  
  return(graph)
}

# ==========================================================================================================
# 🌐 VISNETWORK INTERACTIVE GRAPH  
# ==========================================================================================================

create_visnetwork_flowgraph <- function(system_data = NULL) {
  
  if (is.null(system_data)) {
    system_data <- create_trading_system_data()
  }
  
  cat("🌐 Creating visNetwork interactive graph...\n")
  
  nodes <- system_data$nodes
  edges <- system_data$edges
  
  # Prepare nodes for visNetwork
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
      # Sizes based on value
      size = pmax(20, value * 0.4),
      # Shapes
      shape = case_when(
        group == "system" ~ "box",
        group == "hub" ~ "diamond", 
        group %in% c("ada", "btc", "eth") ~ "circle",
        group == "data" ~ "triangle",
        group == "analysis" ~ "star",
        TRUE ~ "ellipse"
      ),
      # Tooltips
      title = paste0("<b>", label, "</b><br>", 
                     "Status: ", status, "<br>",
                     "Details:<br>", gsub("\n", "<br>", details))
    )
  
  # Prepare edges for visNetwork
  vis_edges <- edges %>%
    mutate(
      # Colors based on type
      color = case_when(
        type == "success" ~ "#27ae60",
        type == "partial" ~ "#f39c12",
        type == "error" ~ "#e74c3c", 
        type == "flow" ~ "#3498db",
        type == "data" ~ "#1abc9c",
        type == "analysis" ~ "#9b59b6",
        TRUE ~ "#95a5a6"
      ),
      # Arrows
      arrows = "to",
      # Smooth curves
      smooth = TRUE
    )
  
  # Create visNetwork
  vis_graph <- visNetwork(vis_nodes, vis_edges, width = "100%", height = "600px") %>%
    visOptions(highlightNearest = TRUE, 
               selectedBy = "group",
               manipulation = FALSE) %>%
    visLayout(randomSeed = 123) %>%
    visPhysics(stabilization = TRUE,
               solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = -200)) %>%
    visInteraction(dragNodes = TRUE, 
                   dragView = TRUE, 
                   zoomView = TRUE) %>%
    visLegend(useGroups = FALSE, 
              addNodes = data.frame(
                label = c("System", "Assets", "Data", "Analysis", "Success", "Error"),
                color = c("#34495e", "#9b59b6", "#1abc9c", "#2ecc71", "#27ae60", "#e74c3c"),
                shape = c("box", "circle", "triangle", "star", "box", "box")
              )) %>%
    visEvents(click = "function(nodes) {
                alert('Node clicked: ' + nodes.nodes);
              }")
  
  # Save as HTML
  html_file <- paste0("c:/freeding/tbot202506/reports/trading_visnetwork_", 
                      format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  
  tryCatch({
    dir.create(dirname(html_file), recursive = TRUE, showWarnings = FALSE)
    saveWidget(vis_graph, html_file, selfcontained = TRUE)
    cat("💾 visNetwork graph saved:", basename(html_file), "\n")
  }, error = function(e) {
    cat("⚠️ Could not save visNetwork file\n")
  })
  
  return(vis_graph)
}

# ==========================================================================================================
# 📈 PLOTLY NETWORK GRAPH
# ==========================================================================================================

create_plotly_flowgraph <- function(system_data = NULL) {
  
  if (is.null(system_data)) {
    system_data <- create_trading_system_data()
  }
  
  cat("📈 Creating Plotly network graph...\n")
  
  nodes <- system_data$nodes
  edges <- system_data$edges
  
  # Calculate layout positions (simple force-directed)
  set.seed(123)
  n_nodes <- nrow(nodes)
  
  # Create positions based on groups
  positions <- nodes %>%
    mutate(
      x = case_when(
        group == "system" ~ row_number() * 2,
        group == "hub" ~ 4,
        group == "ada" ~ 2,
        group == "btc" ~ 4, 
        group == "eth" ~ 6,
        group == "data" ~ (row_number() - 8) * 1.5 + 3,
        group == "analysis" ~ 5,
        group == "monitoring" ~ 6,
        group == "output" ~ 7,
        TRUE ~ row_number()
      ),
      y = case_when(
        group == "system" ~ 5,
        group == "hub" ~ 4,
        group %in% c("ada", "btc", "eth") ~ 3,
        group == "data" ~ 2,
        group == "analysis" ~ 2.5,
        group == "monitoring" ~ 3.5,
        group == "output" ~ 5,
        TRUE ~ 1
      )
    )
  
  # Create edge traces
  edge_traces <- list()
  
  for (i in 1:nrow(edges)) {
    from_pos <- positions[positions$id == edges$from[i], ]
    to_pos <- positions[positions$id == edges$to[i], ]
    
    edge_traces[[i]] <- list(
      x = c(from_pos$x, to_pos$x, NA),
      y = c(from_pos$y, to_pos$y, NA),
      mode = "lines",
      type = "scatter",
      line = list(color = "#3498db", width = edges$width[i]),
      showlegend = FALSE,
      hoverinfo = "none"
    )
  }
  
  # Create node trace
  node_colors <- case_when(
    nodes$status == "success" ~ "#27ae60",
    nodes$status == "partial" ~ "#f39c12",
    nodes$status == "error" ~ "#e74c3c", 
    nodes$status == "bullish" ~ "#9b59b6",
    nodes$status == "neutral" ~ "#95a5a6",
    nodes$status == "bearish" ~ "#e74c3c",
    TRUE ~ "#3498db"
  )
  
  node_trace <- list(
    x = positions$x,
    y = positions$y,
    mode = "markers+text",
    type = "scatter",
    text = nodes$label,
    textposition = "middle center",
    marker = list(
      size = pmax(20, nodes$value * 0.6),
      color = node_colors,
      line = list(color = "white", width = 2)
    ),
    hovertext = paste0("<b>", nodes$label, "</b><br>",
                       "Status: ", nodes$status, "<br>",
                       "Details: ", gsub("\n", "<br>", nodes$details)),
    hoverinfo = "text",
    showlegend = FALSE
  )
  
  # Create plotly figure
  plotly_graph <- plot_ly() %>%
    add_trace(data = node_trace) %>%
    layout(
      title = list(
        text = "🚀 Trading System Flow Graph",
        font = list(color = "white", size = 20)
      ),
      plot_bgcolor = "#2c3e50",
      paper_bgcolor = "#2c3e50", 
      font = list(color = "white"),
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      showlegend = FALSE,
      margin = list(t = 50, b = 20, l = 20, r = 20)
    )
  
  # Add edge traces
  for (edge_trace in edge_traces) {
    plotly_graph <- plotly_graph %>% add_trace(data = edge_trace)
  }
  
  # Save as HTML
  html_file <- paste0("c:/freeding/tbot202506/reports/trading_plotly_", 
                      format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  
  tryCatch({
    dir.create(dirname(html_file), recursive = TRUE, showWarnings = FALSE)
    saveWidget(plotly_graph, html_file, selfcontained = TRUE)
    cat("💾 Plotly graph saved:", basename(html_file), "\n")
  }, error = function(e) {
    cat("⚠️ Could not save Plotly file\n")
  })
  
  return(plotly_graph)
}

# ==========================================================================================================
# 🎯 LIVE DATA INTEGRATION
# ==========================================================================================================

create_live_trading_flowgraph <- function(analysis_results = NULL) {
  
  cat("📊 Creating live trading flowgraph from analysis results...\n")
  
  if (is.null(analysis_results)) {
    cat("⚠️ No analysis results provided, using sample data\n")
    return(create_visnetwork_flowgraph())
  }
  
  # Extract live data from analysis results
  live_nodes <- data.frame(
    id = 1:10,
    label = c("System", "ADA Analysis", "BTC Analysis", "ETH Analysis",
              "Ticker Data", "Sentiment", "Technical", "Positions", "Signals", "Output"),
    stringsAsFactors = FALSE
  )
  
  # Update with live data if available
  if (!is.null(analysis_results$market_data_results)) {
    for (symbol in names(analysis_results$market_data_results)) {
      market_data <- analysis_results$market_data_results[[symbol]]
      
      if (!is.null(market_data$ticker)) {
        # Update node with live price and sentiment
        asset_name <- switch(symbol,
                             "ADAUSDT_UMCBL" = "ADA",
                             "BTCUSDT_UMCBL" = "BTC", 
                             "ETHUSDT_UMCBL" = "ETH")
        
        price <- market_data$ticker$last_price
        sentiment <- if (!is.null(market_data$sentiment)) {
          market_data$sentiment$overall_sentiment
        } else "UNKNOWN"
        
        # Update label with live data
        node_index <- which(grepl(asset_name, live_nodes$label))
        if (length(node_index) > 0) {
          live_nodes$label[node_index] <- sprintf("%s\\n$%s\\n%s", 
                                                  asset_name, 
                                                  format(price, big.mark = ","),
                                                  sentiment)
        }
      }
    }
  }
  
  # Create system data with live updates
  live_system_data <- list(
    nodes = live_nodes,
    edges = data.frame(
      from = c(1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      to = c(2, 3, 4, 5, 5, 5, 6, 7, 8, 9, 10),
      type = rep("live", 11)
    )
  )
  
  return(create_visnetwork_flowgraph(live_system_data))
}

# ==========================================================================================================
# 🚀 MAIN EXECUTION FUNCTIONS
# ==========================================================================================================

# Alle Flowgraphs erstellen
generate_all_flowgraphs <- function(analysis_results = NULL) {
  
  cat("🚀 GENERATING ALL TRADING FLOWGRAPHS\n")
  cat("====================================\n")
  
  start_time <- Sys.time()
  
  # 1. DiagrammeR Graph
  cat("\n1️⃣ Creating DiagrammeR flowgraph...\n")
  diagrammer_graph <- create_diagrammer_flowgraph()
  
  # 2. visNetwork Graph  
  cat("\n2️⃣ Creating visNetwork interactive graph...\n")
  visnetwork_graph <- create_visnetwork_flowgraph()
  
  # 3. Plotly Graph
  cat("\n3️⃣ Creating Plotly network graph...\n")
  plotly_graph <- create_plotly_flowgraph()
  
  # 4. Live Data Graph (falls verfügbar)
  if (!is.null(analysis_results)) {
    cat("\n4️⃣ Creating live data flowgraph...\n")
    live_graph <- create_live_trading_flowgraph(analysis_results)
  } else {
    live_graph <- NULL
  }
  
  execution_time <- difftime(Sys.time(), start_time, units = "secs")
  
  cat("\n✅ ALL FLOWGRAPHS GENERATED!\n")
  cat("============================\n")
  cat(sprintf("⏱️ Total time: %.2f seconds\n", as.numeric(execution_time)))
  cat("📁 Files saved to: c:/freeding/tbot202506/reports/\n")
  cat("🌐 Open HTML files in browser for interactive viewing\n")
  
  return(list(
    diagrammer = diagrammer_graph,
    visnetwork = visnetwork_graph,
    plotly = plotly_graph,
    live = live_graph,
    execution_time = as.numeric(execution_time)
  ))
}

# Quick single graph
quick_flowgraph <- function(type = "visnetwork") {
  
  switch(type,
         "diagrammer" = create_diagrammer_flowgraph(),
         "visnetwork" = create_visnetwork_flowgraph(), 
         "plotly" = create_plotly_flowgraph(),
         create_visnetwork_flowgraph()  # default
  )
}

# ==========================================================================================================
# ✅ USAGE INSTRUCTIONS
# ==========================================================================================================

cat("✅ TRADING FLOWGRAPH GENERATOR LOADED!\n")
cat("======================================\n")
cat("🎯 QUICK START:\n\n")

cat("📊 SINGLE GRAPHS:\n")
cat("   diagrammer_graph <- create_diagrammer_flowgraph()\n")
cat("   visnetwork_graph <- create_visnetwork_flowgraph()\n") 
cat("   plotly_graph <- create_plotly_flowgraph()\n\n")

cat("🚀 ALL GRAPHS AT ONCE:\n")
cat("   all_graphs <- generate_all_flowgraphs()\n\n")

cat("📈 WITH LIVE DATA:\n")
cat("   # First run your trading analysis:\n")
cat("   results <- optimized_execution_fixed()\n")
cat("   # Then create flowgraph with live data:\n")
cat("   live_graphs <- generate_all_flowgraphs(results)\n\n")

cat("⚡ QUICK SINGLE GRAPH:\n")
cat("   quick_graph <- quick_flowgraph('visnetwork')\n\n")

cat("📂 OUTPUT:\n")
cat("   ✅ HTML files saved to reports/ directory\n") 
cat("   ✅ Interactive graphs viewable in browser\n")
cat("   ✅ Self-contained files (no internet required)\n\n")

cat("🎨 GRAPH TYPES:\n")
cat("   📊 DiagrammeR: Professional flowcharts\n")
cat("   🌐 visNetwork: Interactive network graphs\n") 
cat("   📈 Plotly: Dynamic network visualization\n")

# ==========================================================================================================
# 🎯 END OF FLOWGRAPH GENERATOR
# ==========================================================================================================