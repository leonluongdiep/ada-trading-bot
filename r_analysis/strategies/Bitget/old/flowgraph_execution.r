
# Script laden
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/flowgraph_specific_fix.r")

# Einzelne Graphs erstellen
diagrammer_graph <- create_diagrammer_flowgraph()
visnetwork_graph <- create_visnetwork_flowgraph() 
plotly_graph <- create_plotly_flowgraph()


test_graph <- create_visnetwork_flowgraph()

# ALLE Graphs auf einmal
all_graphs <- generate_all_flowgraphs()

# Quick single graph
quick_graph <- quick_flowgraph("visnetwork")


# Nach Ihrer Trading-Analyse
enhanced_analysis <- complete_trading_analysis_enhanced('ADAUSDT_UMCBL')

# Flowgraph mit echten Daten erstellen
live_flowgraph <- create_live_trading_flowgraph(enhanced_analysis)





# Erstellen UND sofort öffnen:
quick_view_flowgraph <- function() {
  test_graph <- create_visnetwork_flowgraph()
  
  # Finde neueste HTML-Datei
  reports_dir <- "c:/freeding/tbot202506/reports/"
  html_files <- list.files(reports_dir, 
                           pattern = "trading_visnetwork.*\\.html$", 
                           full.names = TRUE)
  
  if(length(html_files) > 0) {
    latest <- html_files[which.max(file.mtime(html_files))]
    browseURL(latest)
    cat("🌐 Graph opened in browser:", basename(latest), "\n")
  }
  
  return(test_graph)
}

# Verwendung:
quick_view_flowgraph()

