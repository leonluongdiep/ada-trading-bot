# ==========================================================================================================
# 🎯 TRADING SYSTEM MAIN EXECUTION HUB V3
# ==========================================================================================================
# Pfad: C:\freeding\tbot202506\r_analysis\riskassetm\core\main.r
# Konsolidiert aus: trading_execution_hub_v2_with_console_mgmt.r
# ==========================================================================================================

cat("🎯 Loading Trading System Main Hub V3...\n")

# ==========================================================================================================
# 🔧 LOAD CORE COMPONENTS
# ==========================================================================================================

# Load configuration
source("C:/freeding/tbot202506/r_analysis/riskassetm/core/config.r")

# Load API engine
source("C:/freeding/tbot202506/r_analysis/riskassetm/core/api_engine.r")

# ==========================================================================================================
# 🎮 MAIN EXECUTION FUNCTIONS
# ==========================================================================================================

#' Main system execution function
execute_trading_system <- function(mode = "full", 
                                   symbols = NULL, 
                                   include_oi = TRUE, 
                                   include_risk = TRUE) {
  
  # Clear console for better readability
  cat("\014")  # Clear console
  display_system_header()
  
  cat("\n🚀 === TRADING SYSTEM EXECUTION === 🚀\n")
  cat("Mode:", toupper(mode), "| Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Initialize symbols
  if (is.null(symbols)) {
    symbols <- PORTFOLIO_ASSETS
  }
  
  cat("📊 Assets:", paste(symbols, collapse = ", "), "\n")
  
  # Execute based on mode
  execution_results <- switch(mode,
                              "full" = execute_full_analysis(symbols, include_oi, include_risk),
                              "quick" = execute_quick_check(symbols),
                              "summary" = execute_summary_only(symbols),
                              "positions" = execute_position_check(),
                              "risk" = execute_risk_analysis(symbols),
                              "quiet" = execute_quiet_mode(symbols),
                              execute_full_analysis(symbols, include_oi, include_risk)  # Default
  )
  
  # Display execution summary
  display_execution_summary(execution_results, mode)
  
  return(execution_results)
}

#' Full comprehensive analysis
execute_full_analysis <- function(symbols, include_oi = TRUE, include_risk = TRUE) {
  
  cat("\n📊 === FULL MARKET ANALYSIS === 📊\n")
  
  results <- list(
    timestamp = Sys.time(),
    symbols = symbols,
    market_data = list(),
    positions = NULL,
    risk_assessment = NULL,
    recommendations = list()
  )
  
  # Step 1: Collect market data
  cat("\n1️⃣ Collecting market data...\n")
  
  for (symbol in symbols) {
    cat("   📈", symbol, "...")
    
    # Get enhanced ticker data
    ticker <- get_enhanced_ticker_data(symbol)
    
    if (!is.null(ticker)) {
      results$market_data[[symbol]] <- ticker
      
      # Quick display
      display_ticker_summary(ticker)
      
    } else {
      cat(" ❌ Failed\n")
    }
    
    Sys.sleep(0.3)  # Rate limiting
  }
  
  # Step 2: Load additional modules if needed
  if (include_risk || !is.null(get_current_positions())) {
    # Load risk manager
    source("C:/freeding/tbot202506/r_analysis/riskassetm/trading/risk_manager.r")
    
    cat("\n2️⃣ Checking positions and risk...\n")
    results$positions <- get_current_positions()
    
    if (nrow(results$positions) > 0) {
      display_positions_summary(results$positions)
      
      if (include_risk) {
        results$risk_assessment <- assess_portfolio_risk(results$positions)
        display_risk_summary(results$risk_assessment)
      }
    } else {
      cat("   ℹ️ No open positions\n")
    }
  }
  
  # Step 3: Generate recommendations
  cat("\n3️⃣ Generating recommendations...\n")
  results$recommendations <- generate_simple_recommendations(results$market_data)
  display_recommendations(results$recommendations)
  
  return(results)
}

#' Quick market check
execute_quick_check <- function(symbols) {
  
  cat("\n⚡ === QUICK MARKET CHECK === ⚡\n")
  
  results <- list(
    timestamp = Sys.time(),
    quick_data = list()
  )
  
  for (symbol in symbols) {
    ticker <- get_enhanced_ticker_data(symbol)
    
    if (!is.null(ticker)) {
      results$quick_data[[symbol]] <- list(
        price = ticker$last_price,
        change_24h = ticker$change_24h_pct,
        volume = ticker$volume_24h_usdt,
        status = "OK"
      )
      
      # Display inline
      trend_icon <- get_trend_icon(ticker$change_24h_pct)
      cat(sprintf("%s %s: %.4f (%+.2f%%) %s | Vol: %.1fM\n",
                  DISPLAY_CONFIG$icons$success,
                  symbol,
                  ticker$last_price,
                  ticker$change_24h_pct,
                  trend_icon,
                  ticker$volume_24h_usdt / 1000000))
    } else {
      results$quick_data[[symbol]] <- list(
        status = "ERROR"
      )
      cat(DISPLAY_CONFIG$icons$error, symbol, ": No data\n")
    }
  }
  
  return(results)
}

#' Summary only mode - minimal output
execute_summary_only <- function(symbols) {
  
  # Execute silently
  results <- suppressMessages(execute_quick_check(symbols))
  
  # Show summary
  cat("\n📊 === MARKET SUMMARY === 📊\n")
  
  for (symbol in names(results$quick_data)) {
    data <- results$quick_data[[symbol]]
    if (data$status == "OK") {
      cat(sprintf("%s: %.4f (%+.2f%%) | %.1fM USDT\n",
                  symbol, data$price, data$change_24h, data$volume / 1000000))
    }
  }
  
  cat("\n⏱️ Updated:", format(Sys.time(), "%H:%M:%S"), "\n")
  
  return(invisible(results))
}

#' Position check
execute_position_check <- function() {
  
  cat("\n💼 === POSITION CHECK === 💼\n")
  
  positions <- get_current_positions()
  
  if (nrow(positions) == 0) {
    cat("ℹ️ No open positions\n")
    return(list(positions = positions))
  }
  
  # Display positions
  total_pnl <- 0
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    pnl_icon <- if (pos$unrealized_pnl >= 0) "✅" else "❌"
    
    cat(sprintf("%s %s: %s %.0f @ %.4f | PnL: %+.2f USDT (%.1f%%)\n",
                pnl_icon,
                pos$symbol,
                pos$side,
                pos$size,
                pos$avg_price,
                pos$unrealized_pnl,
                pos$pnl_ratio * 100))
    
    total_pnl <- total_pnl + pos$unrealized_pnl
  }
  
  cat("\n💰 Total PnL:", sprintf("%+.2f USDT", total_pnl), "\n")
  
  return(list(positions = positions, total_pnl = total_pnl))
}

#' Risk analysis mode
execute_risk_analysis <- function(symbols) {
  
  cat("\n🛡️ === RISK ANALYSIS === 🛡️\n")
  
  # Load risk manager
  source("C:/freeding/tbot202506/r_analysis/riskassetm/trading/risk_manager.r")
  
  results <- analyze_portfolio_risk(symbols)
  
  return(results)
}

#' Quiet mode - no output
execute_quiet_mode <- function(symbols) {
  invisible(execute_quick_check(symbols))
}

# ==========================================================================================================
# 🎮 INTERACTIVE INTERFACE
# ==========================================================================================================

#' Launch interactive trading console
launch_interactive_console <- function() {
  
  cat("\n🎮 === INTERACTIVE TRADING CONSOLE === 🎮\n")
  cat("Commands: help, market, positions, risk, execute [mode], quit\n")
  
  repeat {
    cat("\n> ")
    command <- trimws(tolower(readline()))
    
    if (command %in% c("quit", "q", "exit")) {
      cat("👋 Goodbye!\n")
      break
    }
    
    process_command(command)
  }
}

#' Process interactive commands
process_command <- function(command) {
  
  parts <- strsplit(command, " ")[[1]]
  cmd <- parts[1]
  args <- if (length(parts) > 1) parts[-1] else character(0)
  
  switch(cmd,
         "help" = show_help(),
         "market" = execute_quick_check(PORTFOLIO_ASSETS),
         "positions" = execute_position_check(),
         "risk" = execute_risk_analysis(PORTFOLIO_ASSETS),
         "execute" = {
           mode <- if (length(args) > 0) args[1] else "full"
           execute_trading_system(mode)
         },
         "balance" = show_balance(),
         "test" = test_api_connection(),
         cat("❓ Unknown command. Type 'help' for available commands.\n")
  )
}

#' Show help menu
show_help <- function() {
  cat("\n📖 === AVAILABLE COMMANDS === 📖\n")
  cat("market              - Quick market overview\n")
  cat("positions           - Show current positions\n")
  cat("risk               - Risk analysis\n")
  cat("balance            - Account balance\n")
  cat("execute [mode]     - Run analysis (full/quick/summary)\n")
  cat("test               - Test API connection\n")
  cat("help               - Show this menu\n")
  cat("quit               - Exit console\n")
}

#' Show account balance
show_balance <- function() {
  balance <- get_account_balance()
  
  if (!is.null(balance)) {
    cat("\n💰 === ACCOUNT BALANCE === 💰\n")
    cat("Available:", round(balance$available, 2), "USDT\n")
    cat("Equity:", round(balance$equity, 2), "USDT\n")
    cat("Unrealized PnL:", sprintf("%+.2f", balance$unrealized_pnl), "USDT\n")
    cat("Margin Used:", round(balance$locked, 2), "USDT\n")
  } else {
    cat("❌ Could not fetch balance\n")
  }
}

# ==========================================================================================================
# 🎨 DISPLAY FUNCTIONS
# ==========================================================================================================

#' Display system header
display_system_header <- function() {
  cat("╔════════════════════════════════════════════════════════════════════════╗\n")
  cat("║                    🚀 TRADING SYSTEM V3.0 🚀                           ║\n")
  cat("║                     Multi-Asset Trading Platform                       ║\n")
  cat("╚════════════════════════════════════════════════════════════════════════╝\n")
}

#' Display ticker summary
display_ticker_summary <- function(ticker) {
  trend_icon <- get_trend_icon(ticker$change_24h_pct)
  
  cat(sprintf(" ✅ %.4f (%+.2f%%) %s | Vol: %.1fM\n",
              ticker$last_price,
              ticker$change_24h_pct,
              trend_icon,
              ticker$volume_24h_usdt / 1000000))
}

#' Display positions summary
display_positions_summary <- function(positions) {
  cat("   📊 Active Positions:", nrow(positions), "\n")
  
  total_pnl <- sum(positions$unrealized_pnl)
  pnl_icon <- if (total_pnl >= 0) "✅" else "❌"
  
  cat("   ", pnl_icon, "Total PnL:", sprintf("%+.2f USDT", total_pnl), "\n")
}

#' Display risk summary
display_risk_summary <- function(risk_assessment) {
  if (is.null(risk_assessment)) return()
  
  risk_level <- risk_assessment$overall_risk_level %||% "UNKNOWN"
  risk_icon <- switch(risk_level,
                      "LOW" = "🟢",
                      "MEDIUM" = "🟡", 
                      "HIGH" = "🔴",
                      "⚪"
  )
  
  cat("   ", risk_icon, "Risk Level:", risk_level, "\n")
}

#' Display recommendations
display_recommendations <- function(recommendations) {
  for (symbol in names(recommendations)) {
    rec <- recommendations[[symbol]]
    cat("   ", symbol, ":", rec$action, "-", rec$reason, "\n")
  }
}

#' Display execution summary
display_execution_summary <- function(results, mode) {
  cat("\n✅ === EXECUTION COMPLETE === ✅\n")
  cat("Mode:", toupper(mode), "\n")
  cat("Duration:", round(difftime(Sys.time(), results$timestamp, units = "secs"), 2), "seconds\n")
  
  if (!is.null(results$market_data)) {
    cat("Assets analyzed:", length(results$market_data), "\n")
  }
}

#' Get trend icon
get_trend_icon <- function(change_pct) {
  if (change_pct > 2) return(DISPLAY_CONFIG$trend_icons$strong_up)
  if (change_pct > 0) return(DISPLAY_CONFIG$trend_icons$up)
  if (change_pct > -2) return(DISPLAY_CONFIG$trend_icons$neutral)
  if (change_pct > -5) return(DISPLAY_CONFIG$trend_icons$down)
  return(DISPLAY_CONFIG$trend_icons$strong_down)
}

# ==========================================================================================================
# 🛠️ HELPER FUNCTIONS
# ==========================================================================================================

#' Generate simple recommendations
generate_simple_recommendations <- function(market_data) {
  recommendations <- list()
  
  for (symbol in names(market_data)) {
    data <- market_data[[symbol]]
    
    # Simple logic based on 24h change
    if (data$change_24h_pct > 5) {
      recommendations[[symbol]] <- list(
        action = "WATCH",
        reason = "Strong upward momentum"
      )
    } else if (data$change_24h_pct < -5) {
      recommendations[[symbol]] <- list(
        action = "CAUTION",
        reason = "Strong downward pressure"
      )
    } else {
      recommendations[[symbol]] <- list(
        action = "HOLD",
        reason = "Normal market conditions"
      )
    }
  }
  
  return(recommendations)
}

# ==========================================================================================================
# 🚀 QUICK ACCESS FUNCTIONS
# ==========================================================================================================

#' Quick market check
quick_market_check <- function() {
  execute_trading_system("quick")
}

#' Quick position check
quick_position_check <- function() {
  execute_trading_system("positions")
}

#' Daily analysis
daily_analysis <- function() {
  cat("\n🌅 === DAILY MARKET ANALYSIS === 🌅\n")
  execute_trading_system("full", include_risk = TRUE)
}

#' Silent execution
execute_silent <- function(symbols = NULL, mode = "quick") {
  invisible(suppressMessages(
    execute_trading_system(mode = mode, symbols = symbols)
  ))
}

cat("✅ MAIN.R loaded successfully!\n")
cat("🎮 Quick commands:\n")
cat("   quick_market_check()     - Fast market overview\n")
cat("   quick_position_check()   - Check positions\n")
cat("   daily_analysis()         - Full daily analysis\n")
cat("   launch_interactive_console() - Interactive mode\n")