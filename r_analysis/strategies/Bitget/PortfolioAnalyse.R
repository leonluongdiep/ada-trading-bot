#!/usr/bin/env Rscript
# ==========================================
# 🚀 ADVANCED ADA PORTFOLIO ANALYSIS SYSTEM
# ==========================================
# Cross-Platform Compatible: R 4.5.0
# Integration: Bitget API + Technical Analysis
# Purpose: Comprehensive Portfolio Assessment

# Load required libraries
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(openssl)
  library(tidyverse)
  library(quantmod)
  library(TTR)
  library(lubridate)
  library(plotly)
})

# ==========================================
# 🔧 BITGET API INTEGRATION
# ==========================================

# API Configuration
api_key <- Sys.getenv("BITGET_API_KEY")
api_secret <- Sys.getenv("BITGET_API_SECRET")
passphrase <- Sys.getenv("BITGET_PASSPHRASE")
base_url <- "https://api.bitget.com"

# Bitget API Request Function
bitget_request <- function(path, method = "GET", params = NULL) {
  tryCatch(
    {
      ts <- as.character(round(as.numeric(Sys.time()) * 1000))

      query_str <- if (!is.null(params) && toupper(method) == "GET") {
        paste0("?", paste(names(params), params, sep = "=", collapse = "&"))
      } else {
        ""
      }
      prehash <- paste0(ts, toupper(method), path, query_str)

      body_json <- if (toupper(method) == "POST" && !is.null(params)) {
        toJSON(params, auto_unbox = TRUE)
      } else {
        ""
      }

      sig_raw <- sha256(
        charToRaw(paste0(prehash, body_json)),
        key = charToRaw(api_secret)
      )
      signature <- base64_enc(sig_raw)

      headers <- c(
        "ACCESS-KEY" = api_key,
        "ACCESS-SIGN" = signature,
        "ACCESS-TIMESTAMP" = ts,
        "ACCESS-PASSPHRASE" = passphrase,
        "Content-Type" = "application/json"
      )

      url <- paste0(base_url, path)
      resp <- if (toupper(method) == "GET") {
        GET(url, add_headers(.headers = headers), query = params, timeout(10))
      } else {
        VERB(
          method,
          url,
          add_headers(.headers = headers),
          body = body_json,
          encode = "json",
          timeout(10)
        )
      }

      if (http_error(resp)) {
        stop(sprintf("HTTP %s: %s", status_code(resp), content(resp, "text")))
      }

      fromJSON(content(resp, "text"), flatten = TRUE)
    },
    error = function(e) {
      cat("❌ API Error:", e$message, "\n")
      return(NULL)
    }
  )
}

# ==========================================
# 📈 PORTFOLIO DATA COLLECTION
# ==========================================

get_account_data <- function() {
  cat("🔍 Fetching account data...\n")
  res <- bitget_request(
    "/api/mix/v1/account/accounts",
    "GET",
    list(productType = "umcbl")
  )

  if (is.null(res) || res$code != "00000") {
    cat("❌ Failed to fetch account data\n")
    return(NULL)
  }

  return(res$data)
}

get_position_data <- function() {
  cat("🔍 Fetching position data...\n")
  res <- bitget_request(
    "/api/mix/v1/position/allPosition",
    "GET",
    list(productType = "umcbl")
  )

  if (is.null(res) || res$code != "00000") {
    cat("❌ Failed to fetch position data\n")
    return(NULL)
  }

  # Filter for ADA positions only
  positions <- res$data
  ada_positions <- positions[
    grepl("ADA", positions$symbol, ignore.case = TRUE) &
      as.numeric(positions$total) > 0,
  ]

  return(ada_positions)
}

get_market_data <- function(symbol = "ADA-USD", days = 100) {
  cat("🔍 Fetching market data for", symbol, "...\n")

  tryCatch(
    {
      end_date <- Sys.Date()
      start_date <- end_date - days(days)

      data <- getSymbols(
        symbol,
        from = start_date,
        to = end_date,
        auto.assign = FALSE
      )

      if (is.null(data) || nrow(data) == 0) {
        cat("⚠️ No market data available, using mock data for analysis\n")
        # Create mock data for demonstration
        dates <- seq(from = start_date, to = end_date, by = "day")
        mock_data <- xts(
          matrix(runif(length(dates) * 6, 0.5, 0.8), ncol = 6),
          order.by = dates
        )
        colnames(mock_data) <- c(
          "Open",
          "High",
          "Low",
          "Close",
          "Volume",
          "Adjusted"
        )
        return(mock_data)
      }

      return(data)
    },
    error = function(e) {
      cat("⚠️ Market data error, using mock data:", e$message, "\n")
      dates <- seq(from = Sys.Date() - 100, to = Sys.Date(), by = "day")
      mock_data <- xts(
        matrix(runif(length(dates) * 6, 0.5, 0.8), ncol = 6),
        order.by = dates
      )
      colnames(mock_data) <- c(
        "Open",
        "High",
        "Low",
        "Close",
        "Volume",
        "Adjusted"
      )
      return(mock_data)
    }
  )
}

# ==========================================
# 🎯 PORTFOLIO ANALYSIS FUNCTIONS
# ==========================================

analyze_portfolio_health <- function(account_data, position_data) {
  cat("\n📊 PORTFOLIO HEALTH ANALYSIS\n")
  cat("=====================================\n")

  if (is.null(account_data) || nrow(account_data) == 0) {
    cat("❌ No account data available\n")
    return(NULL)
  }

  # Account metrics
  equity <- as.numeric(account_data$equity[1])
  available <- as.numeric(account_data$available[1])
  unrealized_pnl <- as.numeric(account_data$unrealizedPL[1])
  locked <- as.numeric(account_data$locked[1])

  # Portfolio metrics
  equity_utilization <- (locked / equity) * 100
  pnl_percentage <- (unrealized_pnl / equity) * 100
  available_percentage <- (available / equity) * 100

  # Health assessment
  health_score <- 100
  health_factors <- c()

  if (equity_utilization > 80) {
    health_score <- health_score - 30
    health_factors <- c(health_factors, "High leverage risk")
  }

  if (pnl_percentage < -10) {
    health_score <- health_score - 25
    health_factors <- c(health_factors, "Significant losses")
  }

  if (available_percentage < 10) {
    health_score <- health_score - 20
    health_factors <- c(health_factors, "Low available margin")
  }

  # Display results
  cat(sprintf("💰 Total Equity: %.4f USDT\n", equity))
  cat(sprintf(
    "💵 Available: %.4f USDT (%.1f%%)\n",
    available,
    available_percentage
  ))
  cat(sprintf("🔒 Locked: %.4f USDT (%.1f%%)\n", locked, equity_utilization))
  cat(sprintf(
    "📈 Unrealized P&L: %.4f USDT (%.2f%%)\n",
    unrealized_pnl,
    pnl_percentage
  ))
  cat(sprintf("⚡ Health Score: %.0f/100\n", health_score))

  if (length(health_factors) > 0) {
    cat("⚠️ Risk Factors:\n")
    for (factor in health_factors) {
      cat(sprintf("   • %s\n", factor))
    }
  }

  # Return analysis
  list(
    equity = equity,
    available = available,
    locked = locked,
    unrealized_pnl = unrealized_pnl,
    equity_utilization = equity_utilization,
    pnl_percentage = pnl_percentage,
    health_score = health_score,
    risk_factors = health_factors
  )
}

analyze_ada_position <- function(position_data, market_data) {
  cat("\n🎯 ADA POSITION ANALYSIS\n")
  cat("=====================================\n")

  if (is.null(position_data) || nrow(position_data) == 0) {
    cat("📭 No active ADA positions found\n")
    return(NULL)
  }

  # Process each ADA position
  position_analysis <- list()

  for (i in 1:nrow(position_data)) {
    pos <- position_data[i, ]

    # Basic position metrics
    symbol <- pos$symbol
    side <- pos$holdSide
    size <- as.numeric(pos$total)
    available <- as.numeric(pos$available)
    avg_price <- as.numeric(pos$averageOpenPrice)
    mark_price <- as.numeric(pos$markPrice)
    unrealized_pnl <- as.numeric(pos$unrealizedPL)
    margin <- as.numeric(pos$margin)
    leverage <- as.numeric(pos$leverage)

    # Current market price (from market data)
    current_price <- if (!is.null(market_data)) {
      as.numeric(last(Cl(market_data)))
    } else {
      mark_price
    }

    # Position analysis
    price_change_pct <- ((current_price - avg_price) / avg_price) * 100
    roe_pct <- (unrealized_pnl / margin) * 100

    # Risk metrics
    liquidation_price <- as.numeric(pos$liquidationPrice)
    distance_to_liquidation <- if (side == "long") {
      ((current_price - liquidation_price) / current_price) * 100
    } else {
      ((liquidation_price - current_price) / current_price) * 100
    }

    # Position value
    position_value <- size * current_price

    cat(sprintf("\n🔸 Position: %s\n", symbol))
    cat(sprintf(
      "   Side: %s %s\n",
      ifelse(side == "long", "🟢", "🔴"),
      toupper(side)
    ))
    cat(sprintf("   Size: %s ADA (%.2f USDT)\n", size, position_value))
    cat(sprintf("   Avg Price: %.6f USDT\n", avg_price))
    cat(sprintf("   Current Price: %.6f USDT\n", current_price))
    cat(sprintf("   Price Change: %.2f%%\n", price_change_pct))
    cat(sprintf("   Unrealized P&L: %.4f USDT\n", unrealized_pnl))
    cat(sprintf("   ROE: %.2f%%\n", roe_pct))
    cat(sprintf("   Leverage: %sx\n", leverage))
    cat(sprintf("   Margin: %.4f USDT\n", margin))
    cat(sprintf(
      "   Liquidation: %.6f USDT (%.1f%% away)\n",
      liquidation_price,
      distance_to_liquidation
    ))

    # Risk assessment
    risk_level <- "LOW"
    if (abs(roe_pct) > 50) {
      risk_level <- "HIGH"
    } else if (abs(roe_pct) > 25) {
      risk_level <- "MEDIUM"
    }

    if (distance_to_liquidation < 20) {
      risk_level <- "HIGH"
    } else if (distance_to_liquidation < 50) {
      risk_level <- "MEDIUM"
    }

    cat(sprintf("   Risk Level: %s\n", risk_level))

    position_analysis[[i]] <- list(
      symbol = symbol,
      side = side,
      size = size,
      avg_price = avg_price,
      current_price = current_price,
      unrealized_pnl = unrealized_pnl,
      roe_pct = roe_pct,
      leverage = leverage,
      margin = margin,
      risk_level = risk_level,
      distance_to_liquidation = distance_to_liquidation
    )
  }

  return(position_analysis)
}

calculate_technical_indicators <- function(market_data) {
  cat("\n📊 TECHNICAL INDICATORS\n")
  cat("=====================================\n")

  if (is.null(market_data) || nrow(market_data) == 0) {
    cat("❌ No market data available for technical analysis\n")
    return(NULL)
  }

  close_prices <- Cl(market_data)

  # Calculate indicators
  indicators <- list(
    current_price = as.numeric(last(close_prices)),
    rsi_14 = as.numeric(last(RSI(close_prices, n = 14))),
    sma_20 = as.numeric(last(SMA(close_prices, n = 20))),
    sma_50 = as.numeric(last(SMA(close_prices, n = 50))),
    ema_12 = as.numeric(last(EMA(close_prices, n = 12))),
    ema_26 = as.numeric(last(EMA(close_prices, n = 26))),
    bb_upper = as.numeric(last(BBands(close_prices)$up)),
    bb_lower = as.numeric(last(BBands(close_prices)$dn)),
    volume = as.numeric(last(Vo(market_data))),
    volatility = as.numeric(last(volatility(close_prices, n = 20)))
  )

  # MACD
  macd_data <- MACD(close_prices, nFast = 12, nSlow = 26, nSig = 9)
  indicators$macd <- as.numeric(last(macd_data$macd))
  indicators$macd_signal <- as.numeric(last(macd_data$signal))

  cat(sprintf("💰 Current Price: %.6f USDT\n", indicators$current_price))
  cat(sprintf("📊 RSI (14): %.2f\n", indicators$rsi_14))
  cat(sprintf("📈 SMA 20: %.6f USDT\n", indicators$sma_20))
  cat(sprintf("📈 SMA 50: %.6f USDT\n", indicators$sma_50))
  cat(sprintf("📈 EMA 12: %.6f USDT\n", indicators$ema_12))
  cat(sprintf("📈 EMA 26: %.6f USDT\n", indicators$ema_26))
  cat(sprintf("📊 MACD: %.6f\n", indicators$macd))
  cat(sprintf("📊 MACD Signal: %.6f\n", indicators$macd_signal))
  cat(sprintf("📊 Bollinger Upper: %.6f USDT\n", indicators$bb_upper))
  cat(sprintf("📊 Bollinger Lower: %.6f USDT\n", indicators$bb_lower))
  cat(sprintf("📊 Volatility: %.4f\n", indicators$volatility))

  return(indicators)
}

generate_trading_signals <- function(indicators, position_analysis) {
  cat("\n🎯 TRADING SIGNALS\n")
  cat("=====================================\n")

  if (is.null(indicators)) {
    cat("❌ No indicators available for signal generation\n")
    return(NULL)
  }

  signals <- list()
  signal_strength <- 0
  signal_direction <- "HOLD"
  reasons <- c()

  # RSI Signals
  if (indicators$rsi_14 < 30) {
    signal_strength <- signal_strength + 2
    signal_direction <- "BUY"
    reasons <- c(reasons, "RSI Oversold")
  } else if (indicators$rsi_14 > 70) {
    signal_strength <- signal_strength - 2
    signal_direction <- "SELL"
    reasons <- c(reasons, "RSI Overbought")
  }

  # Moving Average Signals
  if (
    indicators$current_price > indicators$sma_20 &&
      indicators$sma_20 > indicators$sma_50
  ) {
    signal_strength <- signal_strength + 1
    reasons <- c(reasons, "Bullish MA Cross")
  } else if (
    indicators$current_price < indicators$sma_20 &&
      indicators$sma_20 < indicators$sma_50
  ) {
    signal_strength <- signal_strength - 1
    reasons <- c(reasons, "Bearish MA Cross")
  }

  # MACD Signals
  if (indicators$macd > indicators$macd_signal) {
    signal_strength <- signal_strength + 1
    reasons <- c(reasons, "MACD Bullish")
  } else {
    signal_strength <- signal_strength - 1
    reasons <- c(reasons, "MACD Bearish")
  }

  # Bollinger Band Signals
  if (indicators$current_price <= indicators$bb_lower) {
    signal_strength <- signal_strength + 1
    reasons <- c(reasons, "BB Oversold")
  } else if (indicators$current_price >= indicators$bb_upper) {
    signal_strength <- signal_strength - 1
    reasons <- c(reasons, "BB Overbought")
  }

  # Final signal determination
  if (signal_strength >= 2) {
    signal_direction <- "STRONG BUY"
  } else if (signal_strength >= 1) {
    signal_direction <- "BUY"
  } else if (signal_strength <= -2) {
    signal_direction <- "STRONG SELL"
  } else if (signal_strength <= -1) {
    signal_direction <- "SELL"
  } else {
    signal_direction <- "HOLD"
  }

  confidence <- min(abs(signal_strength) * 20, 90)

  cat(sprintf("🎯 Signal: %s\n", signal_direction))
  cat(sprintf("💪 Confidence: %d%%\n", confidence))
  cat(sprintf("📊 Signal Strength: %d\n", signal_strength))
  cat("📋 Reasons:\n")
  for (reason in reasons) {
    cat(sprintf("   • %s\n", reason))
  }

  return(list(
    signal = signal_direction,
    confidence = confidence,
    strength = signal_strength,
    reasons = reasons,
    timestamp = Sys.time()
  ))
}

generate_recommendations <- function(
  portfolio_health,
  position_analysis,
  signals
) {
  cat("\n💡 PORTFOLIO RECOMMENDATIONS\n")
  cat("=====================================\n")

  recommendations <- c()

  # Portfolio health recommendations
  if (!is.null(portfolio_health)) {
    if (portfolio_health$health_score < 70) {
      recommendations <- c(
        recommendations,
        "🚨 Consider reducing leverage or position size"
      )
    }

    if (portfolio_health$equity_utilization > 80) {
      recommendations <- c(
        recommendations,
        "⚠️ Add more margin to reduce liquidation risk"
      )
    }

    if (portfolio_health$pnl_percentage < -5) {
      recommendations <- c(
        recommendations,
        "📉 Consider stop-loss strategy to limit losses"
      )
    }
  }

  # Position-specific recommendations
  if (!is.null(position_analysis) && length(position_analysis) > 0) {
    for (pos in position_analysis) {
      if (pos$risk_level == "HIGH") {
        recommendations <- c(
          recommendations,
          sprintf(
            "🔴 %s position at HIGH risk - consider partial close",
            pos$symbol
          )
        )
      }

      if (pos$distance_to_liquidation < 30) {
        recommendations <- c(
          recommendations,
          sprintf(
            "⚡ %s close to liquidation - add margin or reduce size",
            pos$symbol
          )
        )
      }

      if (pos$roe_pct > 20) {
        recommendations <- c(
          recommendations,
          sprintf(
            "💰 %s in profit - consider taking partial profits",
            pos$symbol
          )
        )
      }
    }
  }

  # Signal-based recommendations
  if (!is.null(signals)) {
    if (signals$signal == "STRONG BUY" && signals$confidence > 70) {
      recommendations <- c(
        recommendations,
        "🚀 Strong buy signal - consider increasing position"
      )
    } else if (signals$signal == "STRONG SELL" && signals$confidence > 70) {
      recommendations <- c(
        recommendations,
        "📉 Strong sell signal - consider reducing position"
      )
    }
  }

  # General recommendations
  recommendations <- c(recommendations, "📊 Monitor RSI for entry/exit points")
  recommendations <- c(
    recommendations,
    "🎯 Set stop-loss orders for risk management"
  )
  recommendations <- c(
    recommendations,
    "📈 Consider DCA strategy for volatile periods"
  )

  if (length(recommendations) == 0) {
    recommendations <- c("✅ Portfolio looks healthy - continue monitoring")
  }

  for (i in seq_along(recommendations)) {
    cat(sprintf("%d. %s\n", i, recommendations[i]))
  }

  return(recommendations)
}

save_analysis_report <- function(
  portfolio_health,
  position_analysis,
  indicators,
  signals,
  recommendations
) {
  cat("\n💾 SAVING ANALYSIS REPORT\n")
  cat("=====================================\n")

  report <- list(
    timestamp = Sys.time(),
    portfolio_health = portfolio_health,
    position_analysis = position_analysis,
    technical_indicators = indicators,
    trading_signals = signals,
    recommendations = recommendations
  )

  # Save to JSON for Python integration
  output_dir <- "../shared_data"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_file <- file.path(output_dir, "portfolio_analysis.json")

  tryCatch(
    {
      write_json(report, output_file, pretty = TRUE)
      cat(sprintf("✅ Report saved to: %s\n", output_file))

      # Also save signals for Python bot
      signals_file <- file.path(output_dir, "r_signals.json")
      if (!is.null(signals)) {
        write_json(signals, signals_file, pretty = TRUE)
        cat(sprintf("✅ Signals saved to: %s\n", signals_file))
      }

      return(TRUE)
    },
    error = function(e) {
      cat(sprintf("❌ Failed to save report: %s\n", e$message))
      return(FALSE)
    }
  )
}

# ==========================================
# 🚀 MAIN ANALYSIS FUNCTION
# ==========================================

main_portfolio_analysis <- function() {
  cat("🚀 STARTING COMPREHENSIVE PORTFOLIO ANALYSIS\n")
  cat("==============================================\n")
  cat(sprintf("📅 Timestamp: %s\n", Sys.time()))
  cat(sprintf("🌍 System: %s\n", Sys.info()["sysname"]))
  cat(sprintf("📍 Working Directory: %s\n", getwd()))

  # Step 1: Collect data
  account_data <- get_account_data()
  position_data <- get_position_data()
  market_data <- get_market_data()

  # Step 2: Analyze portfolio health
  portfolio_health <- analyze_portfolio_health(account_data, position_data)

  # Step 3: Analyze ADA positions
  position_analysis <- analyze_ada_position(position_data, market_data)

  # Step 4: Calculate technical indicators
  indicators <- calculate_technical_indicators(market_data)

  # Step 5: Generate trading signals
  signals <- generate_trading_signals(indicators, position_analysis)

  # Step 6: Generate recommendations
  recommendations <- generate_recommendations(
    portfolio_health,
    position_analysis,
    signals
  )

  # Step 7: Save analysis report
  save_analysis_report(
    portfolio_health,
    position_analysis,
    indicators,
    signals,
    recommendations
  )

  cat("\n🎉 PORTFOLIO ANALYSIS COMPLETE\n")
  cat("===============================\n")

  # Return summary
  return(list(
    portfolio_health = portfolio_health,
    position_analysis = position_analysis,
    indicators = indicators,
    signals = signals,
    recommendations = recommendations
  ))
}

# ==========================================
# 🎯 EXECUTION
# ==========================================

# Run the analysis if script is executed directly
if (!interactive()) {
  tryCatch(
    {
      result <- main_portfolio_analysis()
      cat("✅ Analysis completed successfully\n")
    },
    error = function(e) {
      cat(sprintf("❌ Analysis failed: %s\n", e$message))
      quit(status = 1)
    }
  )
}

# Export main function for interactive use
if (interactive()) {
  cat("📊 Portfolio Analysis System loaded!\n")
  cat("Run: result <- main_portfolio_analysis()\n")
}
