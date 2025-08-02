# ==========================================================================================================
# 📊 GGPLOT2 ALGO HEATMAP VISUALISIERUNG
# ==========================================================================================================
# Professionelle Visualisierung der Heatmap-Daten mit verschiedenen Plot-Optionen
# ==========================================================================================================

# Benötigte Libraries laden
if (!require(ggplot2, quietly = TRUE)) install.packages("ggplot2")
if (!require(dplyr, quietly = TRUE)) install.packages("dplyr")
if (!require(scales, quietly = TRUE)) install.packages("scales")
if (!require(viridis, quietly = TRUE)) install.packages("viridis")
if (!require(plotly, quietly = TRUE)) install.packages("plotly")
if (!require(gridExtra, quietly = TRUE)) install.packages("gridExtra")

library(ggplot2)
library(dplyr)
library(scales)
library(viridis)
library(plotly)
library(gridExtra)

cat("📊 Loading ggplot2 ALGO Heatmap Visualization...\n")

# ==========================================================================================================
# 🎨 HEATMAP VISUALISIERUNG FUNKTIONEN
# ==========================================================================================================

#' Hauptfunktion für ALGO Heatmap Visualisierung
visualize_algo_heatmap <- function(heatmap_data, current_price = NULL, save_plots = TRUE) {
  
  cat("🎨 === CREATING ALGO HEATMAP VISUALIZATIONS === 🎨\n")
  
  # Aktuellen Preis schätzen falls nicht angegeben
  if (is.null(current_price)) {
    current_price <- median(heatmap_data$price_level)
    cat("📍 Estimated current price:", round(current_price, 4), "USDT\n")
  } else {
    cat("📍 Current price:", round(current_price, 4), "USDT\n")
  }
  
  # Daten vorbereiten
  plot_data <- prepare_heatmap_data(heatmap_data, current_price)
  
  # Verschiedene Visualisierungen erstellen
  plots <- list()
  
  # 1. Klassische Heatmap
  plots$heatmap <- create_classic_heatmap(plot_data, current_price)
  
  # 2. Barplot mit Scores
  plots$barplot <- create_score_barplot(plot_data, current_price)
  
  # 3. Lollipop Chart
  plots$lollipop <- create_lollipop_chart(plot_data, current_price)
  
  # 4. Kombinierte Analyse
  plots$combined <- create_combined_analysis(plot_data, current_price)
  
  # 5. Interaktiver Plot
  plots$interactive <- create_interactive_plot(plot_data, current_price)
  
  # Plots anzeigen
  display_all_plots(plots)
  
  # Optional: Plots speichern
  if (save_plots) {
    save_heatmap_plots(plots)
  }
  
  return(plots)
}

#' Daten für Plotting vorbereiten
prepare_heatmap_data <- function(heatmap_data, current_price) {
  
  plot_data <- heatmap_data %>%
    mutate(
      # Support/Resistance klassifizieren
      level_type = ifelse(price_level > current_price, "Resistance", "Support"),
      
      # Distanz zum aktuellen Preis
      distance_pct = ((price_level - current_price) / current_price) * 100,
      
      # Intensitäts-Kategorien
      intensity_category = case_when(
        volume_adjusted_score >= 0.8 ~ "Very High",
        volume_adjusted_score >= 0.6 ~ "High", 
        volume_adjusted_score >= 0.4 ~ "Medium",
        TRUE ~ "Low"
      ),
      
      # Score normalisiert (0-100%)
      score_percent = volume_adjusted_score * 100,
      
      # Preis-Labels
      price_label = paste0("$", format(round(price_level, 4), nsmall = 4)),
      
      # Tooltips für interaktive Plots
      tooltip = paste0(
        "Price: ", price_label, "\n",
        "Score: ", round(volume_adjusted_score, 3), "\n",
        "Type: ", level_type, "\n",
        "Distance: ", round(distance_pct, 1), "%"
      )
    ) %>%
    arrange(price_level)
  
  return(plot_data)
}

#' 1. Klassische Heatmap (Tile-basiert)
create_classic_heatmap <- function(plot_data, current_price) {
  
  p <- ggplot(plot_data, aes(x = 1, y = price_level, fill = volume_adjusted_score)) +
    geom_tile(width = 0.8, height = diff(range(plot_data$price_level))/nrow(plot_data)) +
    
    # Aktuellen Preis markieren
    geom_hline(yintercept = current_price, color = "white", size = 2, linetype = "dashed") +
    geom_hline(yintercept = current_price, color = "red", size = 1.5, linetype = "dashed") +
    
    # Farbskala
    scale_fill_viridis_c(
      name = "Interest\nScore",
      option = "plasma",
      trans = "sqrt",
      labels = scales::percent_format(scale = 100)
    ) +
    
    # Y-Achse formatieren
    scale_y_continuous(
      name = "ALGO Price (USDT)",
      labels = scales::dollar_format(prefix = "$", accuracy = 0.0001),
      breaks = pretty(plot_data$price_level, n = 8)
    ) +
    
    # Theme und Labels
    labs(
      title = "🔷 ALGO Price Interest Heatmap",
      subtitle = paste0("Current Price: $", round(current_price, 4), " USDT"),
      caption = "Higher intensity = Higher probability of price reaction"
    ) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "red"),
      legend.position = "right"
    ) +
    
    # Annotations für Top-Levels
    annotate("text", x = 1.4, y = current_price, 
             label = "← Current Price", color = "red", size = 4, hjust = 0)
  
  return(p)
}

#' 2. Barplot mit Scores
create_score_barplot <- function(plot_data, current_price) {
  
  p <- ggplot(plot_data, aes(x = reorder(price_label, price_level), y = score_percent, 
                            fill = level_type)) +
    geom_col(alpha = 0.8, width = 0.7) +
    
    # Scores als Text hinzufügen
    geom_text(aes(label = paste0(round(score_percent, 0), "%")), 
              hjust = -0.1, size = 3, color = "black") +
    
    # Farben
    scale_fill_manual(
      name = "Level Type",
      values = c("Support" = "#2E8B57", "Resistance" = "#DC143C"),
      labels = c("Support" = "🟢 Support", "Resistance" = "🔴 Resistance")
    ) +
    
    # Achsen
    scale_y_continuous(
      name = "Interest Score (%)",
      limits = c(0, max(plot_data$score_percent) * 1.1),
      labels = scales::percent_format(scale = 1)
    ) +
    
    # Theme
    labs(
      title = "🎯 ALGO Interest Scores by Price Level",
      subtitle = paste0("Current Price: $", round(current_price, 4), " USDT"),
      x = "Price Level (USDT)",
      caption = "Higher scores indicate stronger potential reaction zones"
    ) +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    
    coord_flip()
  
  return(p)
}

#' 3. Lollipop Chart
create_lollipop_chart <- function(plot_data, current_price) {
  
  # Top 10 Levels highlighten
  top_levels <- plot_data %>%
    arrange(desc(volume_adjusted_score)) %>%
    slice_head(n = 10)
  
  p <- ggplot(plot_data, aes(x = volume_adjusted_score, y = reorder(price_label, price_level))) +
    
    # Lollipop sticks
    geom_segment(aes(x = 0, xend = volume_adjusted_score, 
                    y = reorder(price_label, price_level), 
                    yend = reorder(price_label, price_level),
                    color = level_type), 
                size = 1.2, alpha = 0.7) +
    
    # Lollipop heads
    geom_point(aes(color = level_type, size = volume_adjusted_score), alpha = 0.8) +
    
    # Top Levels hervorheben
    geom_point(data = top_levels, 
              aes(x = volume_adjusted_score, y = reorder(price_label, price_level)),
              color = "gold", size = 4, shape = 21, stroke = 2) +
    
    # Farben und Größen
    scale_color_manual(
      name = "Level Type",
      values = c("Support" = "#2E8B57", "Resistance" = "#DC143C")
    ) +
    
    scale_size_continuous(
      name = "Score",
      range = c(2, 6),
      guide = "none"
    ) +
    
    scale_x_continuous(
      name = "Interest Score",
      labels = scales::percent_format(scale = 100),
      limits = c(0, max(plot_data$volume_adjusted_score) * 1.05)
    ) +
    
    # Theme
    labs(
      title = "🎪 ALGO Interest Levels - Lollipop View",
      subtitle = paste0("Current Price: $", round(current_price, 4), " | Gold circles = Top 10 levels"),
      y = "Price Level (USDT)",
      caption = "Distance from left axis indicates interest strength"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "top",
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(size = 9)
    )
  
  return(p)
}

#' 4. Kombinierte Analyse (Dual-Y-Axis)
create_combined_analysis <- function(plot_data, current_price) {
  
  # Basis Plot
  p1 <- ggplot(plot_data, aes(x = price_level)) +
    
    # Interest Score als Fläche
    geom_area(aes(y = interest_score), fill = "lightblue", alpha = 0.5) +
    geom_line(aes(y = interest_score), color = "blue", size = 1) +
    
    # Volume Adjusted Score als Linie
    geom_line(aes(y = volume_adjusted_score), color = "red", size = 1.5) +
    geom_point(aes(y = volume_adjusted_score, color = level_type), size = 2) +
    
    # Aktueller Preis
    geom_vline(xintercept = current_price, color = "darkgreen", 
               size = 2, linetype = "dashed", alpha = 0.8) +
    
    # Farben
    scale_color_manual(
      name = "Level Type",
      values = c("Support" = "#2E8B57", "Resistance" = "#DC143C")
    ) +
    
    # Achsen
    scale_x_continuous(
      name = "ALGO Price (USDT)",
      labels = scales::dollar_format(prefix = "$", accuracy = 0.0001)
    ) +
    
    scale_y_continuous(
      name = "Interest Score",
      labels = scales::percent_format(scale = 100)
    ) +
    
    # Theme
    labs(
      title = "📈 ALGO Combined Interest Analysis",
      subtitle = paste0("Blue Area: Base Interest | Red Line: Volume Adjusted | Green Line: Current Price ($", 
                       round(current_price, 4), ")"),
      caption = "Red line shows final scores after volume adjustment"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    
    # Annotations
    annotate("text", x = current_price, y = max(plot_data$volume_adjusted_score) * 0.9,
             label = paste0("Current: $", round(current_price, 4)), 
             color = "darkgreen", size = 4, hjust = 0.5)
  
  return(p1)
}

#' 5. Interaktiver Plot (mit plotly)
create_interactive_plot <- function(plot_data, current_price) {
  
  p <- ggplot(plot_data, aes(x = price_level, y = volume_adjusted_score, 
                            text = tooltip, color = level_type)) +
    
    # Hauptdaten
    geom_point(aes(size = interest_score), alpha = 0.7) +
    geom_line(alpha = 0.5, size = 0.8) +
    
    # Aktueller Preis
    geom_vline(xintercept = current_price, color = "black", 
               size = 1, linetype = "dashed") +
    
    # Top Levels hervorheben
    geom_point(data = plot_data %>% filter(volume_adjusted_score >= quantile(volume_adjusted_score, 0.8)),
              aes(x = price_level, y = volume_adjusted_score),
              color = "gold", size = 4, alpha = 0.8, shape = 17) +
    
    # Farben und Größen
    scale_color_manual(
      name = "Level Type",
      values = c("Support" = "#2E8B57", "Resistance" = "#DC143C")
    ) +
    
    scale_size_continuous(
      name = "Base Interest",
      range = c(2, 8)
    ) +
    
    # Achsen
    scale_x_continuous(
      name = "ALGO Price (USDT)",
      labels = scales::dollar_format(prefix = "$", accuracy = 0.0001)
    ) +
    
    scale_y_continuous(
      name = "Volume Adjusted Score",
      labels = scales::percent_format(scale = 100)
    ) +
    
    # Theme
    labs(
      title = "🔍 Interactive ALGO Heatmap Analysis",
      subtitle = "Hover over points for detailed information",
      caption = "Gold triangles = Top 20% strongest levels"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "right"
    )
  
  # In interaktiven Plot konvertieren
  interactive_plot <- ggplotly(p, tooltip = "text") %>%
    layout(
      title = list(text = "🔍 Interactive ALGO Heatmap Analysis", 
                  font = list(size = 16)),
      annotations = list(
        x = current_price, y = max(plot_data$volume_adjusted_score) * 0.9,
        text = paste0("Current Price: $", round(current_price, 4)),
        showarrow = TRUE, arrowhead = 2, arrowsize = 1,
        bgcolor = "white", bordercolor = "black"
      )
    )
  
  return(interactive_plot)
}

# ==========================================================================================================
# 📊 UTILITY FUNKTIONEN
# ==========================================================================================================

#' Alle Plots anzeigen
display_all_plots <- function(plots) {
  
  cat("\n📊 Displaying all visualizations...\n")
  
  # Statische Plots in Grid anzeigen
  static_plots <- list(plots$heatmap, plots$barplot, plots$lollipop, plots$combined)
  
  tryCatch({
    # 2x2 Grid
    grid_plot <- grid.arrange(grobs = static_plots, ncol = 2, nrow = 2,
                             top = "🔷 ALGO Heatmap Analysis - Multiple Views")
    print(grid_plot)
  }, error = function(e) {
    cat("📊 Displaying plots individually...\n")
    print(plots$heatmap)
    print(plots$barplot)
    print(plots$lollipop)
    print(plots$combined)
  })
  
  # Interaktiver Plot separat
  cat("\n🔍 Interactive plot available in: plots$interactive\n")
  cat("   Use: print(plots$interactive) to display\n")
}

#' Plots speichern
save_heatmap_plots <- function(plots, output_dir = "algo_heatmap_plots") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Statische Plots speichern
  plot_names <- c("heatmap", "barplot", "lollipop", "combined")
  
  for (name in plot_names) {
    filename <- file.path(output_dir, paste0("algo_", name, "_", timestamp, ".png"))
    
    tryCatch({
      ggsave(filename, plots[[name]], width = 12, height = 8, dpi = 300)
      cat("✅ Saved:", basename(filename), "\n")
    }, error = function(e) {
      cat("❌ Error saving", name, ":", e$message, "\n")
    })
  }
  
  # Interaktiven Plot als HTML speichern
  if ("interactive" %in% names(plots)) {
    html_filename <- file.path(output_dir, paste0("algo_interactive_", timestamp, ".html"))
    
    tryCatch({
      htmlwidgets::saveWidget(plots$interactive, html_filename)
      cat("✅ Saved interactive plot:", basename(html_filename), "\n")
    }, error = function(e) {
      cat("❌ Error saving interactive plot:", e$message, "\n")
    })
  }
  
  cat("\n📁 All plots saved to:", output_dir, "\n")
}

# ==========================================================================================================
# 🚀 QUICK ACCESS FUNKTIONEN
# ==========================================================================================================

#' Schnelle Visualisierung mit Beispieldaten
quick_viz_example <- function() {
  
  # Beispieldaten (wie in Ihrer Ausgabe)
  example_data <- data.frame(
    price_level = c(0.2322900, 0.2336136, 0.2349372, 0.2362608, 0.2375844, 
                   0.2389079, 0.2402315, 0.2415551, 0.2428787, 0.2442023,
                   0.2455259, 0.2468495, 0.2481731, 0.2494967, 0.2508203, 0.2521438),
    interest_score = c(0.2905030, 0.3068871, 0.3242947, 0.3427955, 0.3624645,
                      0.3833820, 0.6084515, 0.4293140, 0.4545205, 0.4813605,
                      0.5099487, 0.5404080, 0.5728709, 0.9112190, 0.9665791, 0.6837553),
    volume_adjusted_score = c(0.3365227, 0.3555023, 0.3756674, 0.3970990, 0.4198838,
                             0.4441150, 0.7048386, 0.4973233, 0.5265228, 0.5576147,
                             0.5907315, 0.6260161, 0.6636215, 1.0555686, 1.1196986, 0.7920715)
  )
  
  current_price <- 0.2450  # Geschätzter aktueller Preis
  
  return(visualize_algo_heatmap(example_data, current_price))
}

#' Verwende Ihre echten Heatmap-Daten
visualize_my_heatmap <- function(heatmap_result, current_price = NULL) {
  
  if (is.null(heatmap_result) || is.null(heatmap_result$heatmap_data)) {
    cat("❌ No heatmap data found. Please provide heatmap_result$heatmap_data\n")
    return(NULL)
  }
  
  heatmap_data <- heatmap_result$heatmap_data
  
  if (is.null(current_price)) {
    current_price <- heatmap_result$current_price %||% median(heatmap_data$price_level)
  }
  
  return(visualize_algo_heatmap(heatmap_data, current_price))
}

cat("✅ GGPLOT2 HEATMAP VISUALIZATION LOADED!\n")
cat("🎨 Professional visualization functions ready\n\n")
cat("💡 USAGE OPTIONS:\n")
cat("   # Option 1: Mit Ihren echten Daten\n")
cat("   heatmap_result <- algo_heatmap()  # Heatmap erstellen\n")
cat("   plots <- visualize_my_heatmap(heatmap_result)\n")
cat("   print(plots$interactive)  # Interaktiver Plot\n\n")
cat("   # Option 2: Mit Beispieldaten testen\n")
cat("   plots <- quick_viz_example()\n\n")
cat("   # Option 3: Direkt mit Daten\n")
cat("   plots <- visualize_algo_heatmap(your_data, current_price = 0.2580)\n\n")
cat("🎯 VERFÜGBARE PLOTS:\n")
cat("   plots$heatmap      # Klassische Heatmap\n")
cat("   plots$barplot      # Score Barplot\n")
cat("   plots$lollipop     # Lollipop Chart\n") 
cat("   plots$combined     # Kombinierte Analyse\n")
cat("   plots$interactive  # Interaktiver Plot\n\n")
cat("💾 AUTOMATISCHES SPEICHERN: Alle Plots werden als PNG/HTML gespeichert\n")