# ==========================================================================================================
# 📊 TRADING MONITORING DASHBOARD - MINIMALIST MATHEMATICAL STYLE
# ==========================================================================================================
# 
# DESIGN PHILOSOPHY:
# - Schwarz-weiß mit grau-Abstufungen (mathematiker-freundlich)
# - Klare, präzise Visualisierungen ohne Ablenkung
# - Grid-Layout für systematische Übersicht
# - Fokus auf Daten, nicht auf Design
# 
# ==========================================================================================================

library(ggplot2)
library(gridExtra)
library(scales)

# ==========================================================================================================
# 🎨 MATHEMATICAL THEME DEFINITION
# ==========================================================================================================

theme_mathematical <- function() {
  theme_minimal() +
  theme(
    # Background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90", size = 0.3, linetype = "dotted"),
    panel.grid.minor = element_line(color = "grey95", size = 0.2, linetype = "dotted"),
    
    # Text
    text = element_text(family = "mono", color = "black", size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 9, color = "grey30", hjust = 0.5),
    axis.title = element_text(size = 9, color = "grey20"),
    axis.text = element_text(size = 8, color = "black"),
    
    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    legend.key.size = unit(0.4, "cm"),
    
    # Facets
    strip.text = element_text(size = 9, face = "bold", color = "black"),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    
    # Margins
    plot.margin = margin(5, 5, 5, 5, "mm")
  )
}

# Mathematical color palette (grayscale with minimal accents)
math_colors <- c(
  primary = "black",
  secondary = "grey30", 
  tertiary = "grey60",
  background = "white",
  grid = "grey90",
  positive = "grey20",
  negative = "grey50",
  neutral = "grey70"
)

# ==========================================================================================================
# 📊 CORE VISUALIZATION FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ 1. POSITION STATUS GAUGE                                                                             │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
plot_position_status <- function(analysis_result) {
  
  # Extract position data
  if (!is.null(analysis_result$analysis_results$current_position)) {
    pos <- analysis_result$analysis_results$current_position[1, ]
    
    size <- as.numeric(pos$total)
    entry_price <- as.numeric(pos$averageOpenPrice)
    pnl <- as.numeric(pos$unrealizedPL)
    side <- pos$holdSide
    
    # Get current price
    current_price <- if (!is.null(analysis_result$analysis_results$market_data$ticker)) {
      analysis_result$analysis_results$market_data$ticker$last_price
    } else {
      entry_price * 1.01  # fallback
    }
    
    pnl_pct <- ((current_price / entry_price) - 1) * 100
    
    # Create position summary data
    pos_data <- data.frame(
      metric = c("Size", "Entry", "Current", "P&L", "P&L %"),
      value = c(
        paste(format(size, big.mark = ","), "contracts"),
        paste(format(entry_price, nsmall = 4), "USDT"),
        paste(format(current_price, nsmall = 4), "USDT"),
        paste(format(pnl, nsmall = 2), "USDT"),
        paste(format(pnl_pct, nsmall = 2), "%")
      ),
      color = c("neutral", "neutral", "neutral", 
                ifelse(pnl >= 0, "positive", "negative"),
                ifelse(pnl_pct >= 0, "positive", "negative"))
    )
    
  } else {
    pos_data <- data.frame(
      metric = "Status",
      value = "No Active Position",
      color = "neutral"
    )
  }
  
  # Create text plot
  ggplot(pos_data, aes(x = 1, y = seq_along(metric), label = paste(metric, ":", value))) +
    geom_text(aes(color = color), size = 3.5, hjust = 0, family = "mono") +
    scale_color_manual(values = math_colors) +
    xlim(0.5, 5) +
    labs(title = "Position Status", 
         subtitle = ifelse(exists("side"), paste("Direction:", toupper(side)), "")) +
    theme_mathematical() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ 2. MARKET DATA SUMMARY                                                                               │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
plot_market_summary <- function(analysis_result) {
  
  if (!is.null(analysis_result$analysis_results$market_data$ticker)) {
    ticker <- analysis_result$analysis_results$market_data$ticker
    
    market_data <- data.frame(
      metric = c("Price", "24h Change", "24h Volume", "Spread", "Funding"),
      value = c(
        paste(format(ticker$last_price, nsmall = 4), "USDT"),
        paste(format(ticker$change_24h_pct, nsmall = 2), "%"),
        paste(format(ticker$volume_24h_usdt / 1e6, nsmall = 1), "M USDT"),
        if (!is.null(analysis_result$analysis_results$market_data$orderbook)) {
          paste(format(analysis_result$analysis_results$market_data$orderbook$spread_pct, nsmall = 4), "%")
        } else "N/A",
        paste(format(ticker$funding_rate * 100, nsmall = 4), "%")
      ),
      color = c("neutral", 
                ifelse(ticker$change_24h_pct >= 0, "positive", "negative"),
                "neutral", "neutral",
                ifelse(ticker$funding_rate >= 0, "positive", "negative"))
    )
  } else {
    market_data <- data.frame(
      metric = "Status",
      value = "No Market Data",
      color = "neutral"
    )
  }
  
  ggplot(market_data, aes(x = 1, y = seq_along(metric), label = paste(metric, ":", value))) +
    geom_text(aes(color = color), size = 3.5, hjust = 0, family = "mono") +
    scale_color_manual(values = math_colors) +
    xlim(0.5, 5) +
    labs(title = "Market Data", subtitle = "Live Market Information") +
    theme_mathematical() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ 3. TECHNICAL INDICATORS CHART                                                                        │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
plot_technical_indicators <- function(analysis_result) {
  
  if (!is.null(analysis_result$analysis_results$technical_analysis$indicators)) {
    indicators <- tail(analysis_result$analysis_results$technical_analysis$indicators, 20)
    
    # RSI Plot
    rsi_plot <- ggplot(indicators, aes(x = seq_along(rsi_14))) +
      geom_line(aes(y = rsi_14), color = math_colors$primary, size = 0.7) +
      geom_hline(yintercept = c(30, 70), color = math_colors$secondary, 
                 linetype = "dashed", size = 0.4) +
      geom_hline(yintercept = 50, color = math_colors$tertiary, 
                 linetype = "dotted", size = 0.3) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      labs(title = "RSI(14)", y = "RSI", x = "Periods") +
      theme_mathematical()
    
    # Price and SMA Plot
    price_plot <- ggplot(indicators, aes(x = seq_along(close))) +
      geom_line(aes(y = close), color = math_colors$primary, size = 0.8) +
      geom_line(aes(y = sma_10), color = math_colors$secondary, size = 0.6, linetype = "dashed") +
      geom_line(aes(y = sma_20), color = math_colors$tertiary, size = 0.6, linetype = "dotted") +
      labs(title = "Price & Moving Averages", y = "Price (USDT)", x = "Periods") +
      theme_mathematical()
    
    grid.arrange(price_plot, rsi_plot, ncol = 1)
    
  } else {
    # Fallback empty plot
    ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = "No Technical Data Available"), 
                size = 4, color = math_colors$secondary) +
      labs(title = "Technical Indicators") +
      theme_mathematical() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  }
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ 4. MULTI-TIMEFRAME PERFORMANCE                                                                       │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
plot_timeframe_performance <- function(analysis_result) {
  
  if (!is.null(analysis_result$analysis_results$timeframe_analysis)) {
    tf_data <- analysis_result$analysis_results$timeframe_analysis
    
    # Extract performance metrics
    performance_data <- data.frame(
      timeframe = character(),
      win_rate = numeric(),
      total_pnl = numeric(),
      trading_score = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (tf_name in names(tf_data)) {
      tf <- tf_data[[tf_name]]
      days <- as.numeric(gsub("days_", "", tf_name))
      
      if (!is.null(tf$metrics)) {
        performance_data <- rbind(performance_data, data.frame(
          timeframe = paste(days, "d"),
          win_rate = tf$metrics$win_rate$win_rate_pct,
          total_pnl = tf$metrics$profit_loss$total_pnl,
          trading_score = tf$metrics$efficiency$trading_score,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if (nrow(performance_data) > 0) {
      # Win Rate Chart
      win_rate_plot <- ggplot(performance_data, aes(x = timeframe, y = win_rate)) +
        geom_col(fill = math_colors$tertiary, color = math_colors$primary, width = 0.7) +
        geom_hline(yintercept = 50, color = math_colors$secondary, linetype = "dashed") +
        geom_text(aes(label = paste0(round(win_rate, 1), "%")), 
                  vjust = -0.3, size = 3, family = "mono") +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
        labs(title = "Win Rate by Timeframe", y = "Win Rate (%)", x = "Timeframe") +
        theme_mathematical()
      
      # P&L Chart
      pnl_plot <- ggplot(performance_data, aes(x = timeframe, y = total_pnl)) +
        geom_col(fill = ifelse(performance_data$total_pnl >= 0, 
                              math_colors$positive, math_colors$negative),
                 color = math_colors$primary, width = 0.7) +
        geom_hline(yintercept = 0, color = math_colors$primary, size = 0.5) +
        geom_text(aes(label = paste0(ifelse(total_pnl >= 0, "+", ""), round(total_pnl, 1))),
                  vjust = ifelse(performance_data$total_pnl >= 0, -0.3, 1.3), 
                  size = 3, family = "mono") +
        labs(title = "P&L by Timeframe", y = "P&L (USDT)", x = "Timeframe") +
        theme_mathematical()
      
      grid.arrange(win_rate_plot, pnl_plot, ncol = 2)
    } else {
      ggplot() +
        geom_text(aes(x = 0.5, y = 0.5, label = "No Timeframe Data Available"), 
                  size = 4, color = math_colors$secondary) +
        labs(title = "Multi-Timeframe Performance") +
        theme_mathematical() +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()
        )
    }
  }
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ 5. TRADING SIGNALS DISPLAY                                                                           │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
plot_trading_signals <- function(analysis_result) {
  
  signals_data <- data.frame(
    signal_type = character(),
    signal_value = character(),
    confidence = character(),
    stringsAsFactors = FALSE
  )
  
  # Technical signals
  if (!is.null(analysis_result$analysis_results$technical_analysis$signals)) {
    tech_signals <- analysis_result$analysis_results$technical_analysis$signals
    
    signals_data <- rbind(signals_data, data.frame(
      signal_type = c("Overall", "RSI", "Trend", "MACD"),
      signal_value = c(
        tech_signals$overall_signal,
        tech_signals$rsi_signal,
        tech_signals$sma_signal,
        tech_signals$macd_signal
      ),
      confidence = c("High", "Medium", "Medium", "Low"),
      stringsAsFactors = FALSE
    ))
  }
  
  # Market sentiment
  if (!is.null(analysis_result$analysis_results$enhanced_analysis$enhanced_market_data$sentiment)) {
    sentiment <- analysis_result$analysis_results$enhanced_analysis$enhanced_market_data$sentiment
    
    signals_data <- rbind(signals_data, data.frame(
      signal_type = "Sentiment",
      signal_value = sentiment$overall_sentiment,
      confidence = paste0(round(sentiment$sentiment_percentage), "%"),
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(signals_data) > 0) {
    ggplot(signals_data, aes(x = 1, y = seq_along(signal_type))) +
      geom_text(aes(label = paste(signal_type, ":", signal_value, "(", confidence, ")")),
                size = 3.5, hjust = 0, family = "mono", color = math_colors$primary) +
      xlim(0.5, 8) +
      labs(title = "Trading Signals", subtitle = "Current Signal Overview") +
      theme_mathematical() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  } else {
    ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = "No Signals Available"), 
                size = 4, color = math_colors$secondary) +
      labs(title = "Trading Signals") +
      theme_mathematical() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  }
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ 6. EFFICIENCY METRICS                                                                                │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
plot_efficiency_metrics <- function(analysis_result) {
  
  if (!is.null(analysis_result$summary)) {
    summary_data <- analysis_result$summary
    
    metrics_data <- data.frame(
      metric = c("Efficiency Score", "Market Sentiment", "Technical Signal", "Primary Rec."),
      value = c(
        paste(summary_data$efficiency_score, "/100"),
        summary_data$market_sentiment,
        summary_data$technical_signal,
        gsub("_", " ", summary_data$primary_recommendation)
      ),
      score = c(summary_data$efficiency_score, 
                if(!is.null(summary_data$sentiment_score)) summary_data$sentiment_score else 50,
                case_when(
                  summary_data$technical_signal == "BUY" ~ 75,
                  summary_data$technical_signal == "SELL" ~ 25,
                  TRUE ~ 50
                ),
                case_when(
                  grepl("BUY", summary_data$primary_recommendation) ~ 75,
                  grepl("SELL", summary_data$primary_recommendation) ~ 25,
                  TRUE ~ 50
                ))
    )
    
    ggplot(metrics_data, aes(x = metric, y = score)) +
      geom_col(fill = math_colors$tertiary, color = math_colors$primary, width = 0.6) +
      geom_text(aes(label = value), vjust = -0.3, size = 3, family = "mono") +
      geom_hline(yintercept = 50, color = math_colors$secondary, linetype = "dashed") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
      labs(title = "System Metrics", y = "Score/Rating", x = "") +
      theme_mathematical() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = "No Efficiency Data Available"), 
                size = 4, color = math_colors$secondary) +
      labs(title = "Efficiency Metrics") +
      theme_mathematical() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  }
}

# ==========================================================================================================
# 🎯 MASTER DASHBOARD FUNCTION
# ==========================================================================================================

create_trading_dashboard <- function(analysis_result, save_path = NULL) {
  
  cat("📊 Creating mathematical trading dashboard...\n")
  
  # Create individual plots
  p1 <- plot_position_status(analysis_result)
  p2 <- plot_market_summary(analysis_result)
  p3 <- plot_trading_signals(analysis_result)
  p4 <- plot_efficiency_metrics(analysis_result)
  
  # Create technical plots (these might be more complex)
  tryCatch({
    p5 <- plot_technical_indicators(analysis_result)
  }, error = function(e) {
    p5 <<- ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Technical indicators unavailable"), 
                size = 4) +
      theme_mathematical() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), 
            axis.title = element_blank(), panel.grid = element_blank())
  })
  
  tryCatch({
    p6 <- plot_timeframe_performance(analysis_result)
  }, error = function(e) {
    p6 <<- ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Timeframe data unavailable"), 
                size = 4) +
      theme_mathematical() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), 
            axis.title = element_blank(), panel.grid = element_blank())
  })
  
  # Create dashboard layout
  dashboard <- grid.arrange(
    arrangeGrob(p1, p2, ncol = 2, top = "Status Overview"),
    arrangeGrob(p3, p4, ncol = 2, top = "Signals & Metrics"),
    p5,
    p6,
    ncol = 1,
    heights = c(1, 1, 1.2, 1.2),
    top = textGrob("Trading System Monitoring Dashboard", 
                   gp = gpar(fontsize = 16, fontface = "bold", fontfamily = "mono"))
  )
  
  # Save if requested
  if (!is.null(save_path)) {
    ggsave(save_path, dashboard, width = 16, height = 12, units = "in", dpi = 300, bg = "white")
    cat("✅ Dashboard saved to:", save_path, "\n")
  }
  
  return(dashboard)
}

# ==========================================================================================================
# 📊 QUICK MONITORING FUNCTIONS
# ==========================================================================================================

# Quick position overview
quick_position_view <- function(analysis_result) {
  p1 <- plot_position_status(analysis_result)
  p2 <- plot_market_summary(analysis_result)
  
  grid.arrange(p1, p2, ncol = 2, 
               top = textGrob("Quick Position Overview", 
                             gp = gpar(fontsize = 14, fontface = "bold", fontfamily = "mono")))
}

# Quick technical overview
quick_technical_view <- function(analysis_result) {
  p1 <- plot_trading_signals(analysis_result)
  p2 <- plot_efficiency_metrics(analysis_result)
  
  grid.arrange(p1, p2, ncol = 2,
               top = textGrob("Quick Technical Overview", 
                             gp = gpar(fontsize = 14, fontface = "bold", fontfamily = "mono")))
}

# ==========================================================================================================
# 🎯 USAGE EXAMPLES
# ==========================================================================================================

cat("📊 MATHEMATICAL TRADING DASHBOARD LOADED!\n")
cat("=========================================\n")
cat("\n📈 USAGE EXAMPLES:\n")
cat("# 1. Complete Dashboard:\n")
cat("dashboard <- create_trading_dashboard(ada_master_analysis_fixed)\n")
cat("# Save to file:\n")
cat("create_trading_dashboard(ada_master_analysis_fixed, 'dashboard.png')\n")
cat("\n# 2. Quick Views:\n")
cat("quick_position_view(ada_master_analysis_fixed)\n")
cat("quick_technical_view(ada_master_analysis_fixed)\n")
cat("\n# 3. Individual Plots:\n")
cat("plot_position_status(ada_master_analysis_fixed)\n")
cat("plot_market_summary(ada_master_analysis_fixed)\n")
cat("plot_trading_signals(ada_master_analysis_fixed)\n")
cat("\n🎨 DESIGN FEATURES:\n")
cat("✅ Minimalist black & white design\n")
cat("✅ Monospace font for precision\n")
cat("✅ Grid-based systematic layout\n")
cat("✅ Focus on data, not decoration\n")
cat("✅ Mathematical clarity\n")
cat("✅ High-DPI export capability\n")
cat("=========================================\n")

# Korrigieren Sie die Farbdefinition direkt:
math_colors <- list(
  primary = "black",
  secondary = "grey30", 
  tertiary = "grey60",
  background = "white",
  grid = "grey90",
  positive = "grey20",
  negative = "grey50",
  neutral = "grey70"
)

# Dann Dashboard erstellen:
dashboard <- create_trading_dashboard(ada_master_analysis_fixed)
# Laden Sie das Dashboard-System
#source("path/to/trading_monitoring_dashboard.r")

# Erstellen Sie das komplette Dashboard
dashboard <- create_trading_dashboard(ada_master_analysis_fixed)

# Speichern Sie es als hochqualitative PNG
create_trading_dashboard(ada_master_analysis_fixed, "trading_dashboard.png")

# Oder verwenden Sie schnelle Übersichten
quick_position_view(ada_master_analysis_fixed)
quick_technical_view(ada_master_analysis_fixed)
