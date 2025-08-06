# ==========================================================================================================
# 🎮 MAIN TRADING SYSTEM CONTROLLER V3
# ==========================================================================================================
# Pfad: core/main.r
# Zentrales Steuerungsmodul für das gesamte Trading-System
# Koordiniert alle Komponenten und bietet einheitliche Schnittstelle
# ==========================================================================================================

cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("           🚀 BITGET AUTOMATED TRADING SYSTEM V3 🚀\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("\n")

# ==========================================================================================================
# 🔧 SYSTEM INITIALIZATION
# ==========================================================================================================

#' Initialize trading system
initialize_trading_system <- function(config_file = NULL) {
  
  cat("🔄 Initializing Trading System...\n\n")
  
  # Track initialization status
  SYSTEM_STATUS <<- list(
    initialized = FALSE,
    components = list(),
    start_time = Sys.time(),
    errors = list()
  )
  
  # Load configuration
  cat("📁 Loading configuration...\n")
  tryCatch({
    source("config.r")
    SYSTEM_STATUS$components$config <<- TRUE
    cat("   ✅ Configuration loaded\n")
  }, error = function(e) {
    cat("   ❌ Configuration failed:", e$message, "\n")
    SYSTEM_STATUS$errors$config <<- e$message
    return(FALSE)
  })
  
  # Load API engine
  cat("🔌 Loading API engine...\n")
  tryCatch({
    source("api_engine.r")
    SYSTEM_STATUS$components$api_engine <<- TRUE
    cat("   ✅ API engine loaded\n")
    
    # Test API connection
    if (test_api_connection()) {
      cat("   ✅ API connection verified\n")
    } else {
      cat("   ⚠️ API connection failed\n")
    }
  }, error = function(e) {
    cat("   ❌ API engine failed:", e$message, "\n")
    SYSTEM_STATUS$errors$api_engine <<- e$message
  })
  
  # Load trading components
  cat("💼 Loading trading components...\n")
  
  components <- list(
    "order_manager" = "../trading/order_manager.r",
    "risk_manager" = "../trading/risk_manager.r",
    "position_manager" = "../trading/position_manager.r"
  )
  
  for (name in names(components)) {
    tryCatch({
      source(components[[name]])
      SYSTEM_STATUS$components[[name]] <<- TRUE
      cat("   ✅", name, "loaded\n")
    }, error = function(e) {
      cat("   ❌", name, "failed:", e$message, "\n")
      SYSTEM_STATUS$errors[[name]] <<- e$message
    })
  }
  
  # Load strategies
  cat("🎯 Loading trading strategies...\n")
  
  strategies <- list(
    "oi_momentum" = "../strategies/oi_momentum_strategy.r",
    "scalping" = "../strategies/scalping_strategy.r"
  )
  
  for (name in names(strategies)) {
    tryCatch({
      source(strategies[[name]])
      SYSTEM_STATUS$components[[paste0("strategy_", name)]] <<- TRUE
      cat("   ✅", name, "strategy loaded\n")
    }, error = function(e) {
      cat("   ⚠️", name, "strategy failed:", e$message, "\n")
      SYSTEM_STATUS$errors[[paste0("strategy_", name)]] <<- e$message
    })
  }
  
  # Load analytics
  cat("📊 Loading analytics modules...\n")
  
  analytics <- list(
    "market_scanner" = "../analytics/market_scanner.r",
    "performance_tracker" = "../analytics/performance_tracker.r"
  )
  
  for (name in names(analytics)) {
    tryCatch({
      source(analytics[[name]])
      SYSTEM_STATUS$components[[name]] <<- TRUE
      cat("   ✅", name, "loaded\n")
    }, error = function(e) {
      cat("   ❌", name, "failed:", e$message, "\n")
      SYSTEM_STATUS$errors[[name]] <<- e$message
    })
  }
  
  # Load utilities
  cat("🔧 Loading utility modules...\n")
  
  utilities <- list(
    "notifications" = "../utils/notifications.r",
    "data_manager" = "../utils/data_manager.r"
  )
  
  for (name in names(utilities)) {
    tryCatch({
      source(utilities[[name]])
      SYSTEM_STATUS$components[[name]] <<- TRUE
      cat("   ✅", name, "loaded\n")
    }, error = function(e) {
      cat("   ⚠️", name, "failed:", e$message, "\n")
      SYSTEM_STATUS$errors[[name]] <<- e$message
    })
  }
  
  # Initialize data connections
  if (SYSTEM_STATUS$components$data_manager) {
    cat("\n💾 Initializing data storage...\n")
    # Data manager auto-initializes
  }
  
  # Set system as initialized
  SYSTEM_STATUS$initialized <<- TRUE
  
  # Display initialization summary
  display_initialization_summary()
  
  # Show main menu
  if (SYSTEM_STATUS$initialized) {
    show_main_menu()
  }
  
  return(SYSTEM_STATUS$initialized)
}

# ==========================================================================================================
# 🎮 MAIN CONTROL INTERFACE
# ==========================================================================================================

#' Show main menu
show_main_menu <- function() {
  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║              🎮 MAIN CONTROL PANEL 🎮                      ║\n")
  cat("╠════════════════════════════════════════════════════════════╣\n")
  cat("║                                                            ║\n")
  cat("║  TRADING OPERATIONS:                                       ║\n")
  cat("║  [1] 📊 Market Overview        [2] 🔍 Scan Markets        ║\n")
  cat("║  [3] 🎯 Strategy Signals       [4] 💼 Positions           ║\n")
  cat("║  [5] ⚡ Execute Trade          [6] 🛡️  Risk Status        ║\n")
  cat("║                                                            ║\n")
  cat("║  MONITORING:                                               ║\n")
  cat("║  [7] 📈 Performance            [8] 📜 Trade History       ║\n")
  cat("║  [9] 🔔 Notifications          [10] 📊 Live Dashboard     ║\n")
  cat("║                                                            ║\n")
  cat("║  SYSTEM:                                                   ║\n")
  cat("║  [11] ⚙️  Settings             [12] 💾 Backup Data        ║\n")
  cat("║  [13] 🔄 Restart               [14] 🛑 Shutdown           ║\n")
  cat("║                                                            ║\n")
  cat("║  [H] Help    [S] Status    [Q] Quick Actions              ║\n")
  cat("║                                                            ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
}

#' Process menu command
process_command <- function(command) {
  
  command <- toupper(command)
  
  switch(command,
    # Trading Operations
    "1" = show_market_overview(),
    "2" = run_market_scan_interface(),
    "3" = show_strategy_signals(),
    "4" = show_positions_interface(),
    "5" = execute_trade_interface(),
    "6" = show_risk_status(),
    
    # Monitoring
    "7" = show_performance_dashboard(),
    "8" = show_trade_history(),
    "9" = show_notifications_interface(),
    "10" = start_live_dashboard(),
    
    # System
    "11" = show_settings_menu(),
    "12" = backup_system_data(),
    "13" = restart_system(),
    "14" = shutdown_system(),
    
    # Quick commands
    "H" = show_help(),
    "S" = show_system_status(),
    "Q" = show_quick_actions(),
    
    # Default
    cat("❌ Unknown command. Type 'H' for help.\n")
  )
}

# ==========================================================================================================
# 📊 MARKET OPERATIONS
# ==========================================================================================================

#' Show market overview
show_market_overview <- function() {
  
  cat("\n📊 === MARKET OVERVIEW === 📊\n")
  cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Get account info
  account <- get_account_balance()
  if (!is.null(account)) {
    cat("💰 ACCOUNT:\n")
    cat("├─ Equity: $", format(account$equity, big.mark = ",", digits = 2), "\n")
    cat("├─ Available: $", format(account$available, big.mark = ",", digits = 2), "\n")
    cat("└─ Margin Used: $", format(account$locked, big.mark = ",", digits = 2), "\n\n")
  }
  
  # Get positions
  positions <- get_current_positions()
  cat("📈 POSITIONS:", nrow(positions), "open\n")
  
  if (nrow(positions) > 0) {
    total_pnl <- sum(positions$unrealized_pnl)
    cat("└─ Total Unrealized P&L: $", 
        format(total_pnl, big.mark = ",", digits = 2),
        " (", sprintf("%+.2f%%", total_pnl / account$equity * 100), ")\n\n")
  }
  
  # Market summary
  cat("🌍 MARKET CONDITIONS:\n")
  
  # Get top assets
  for (symbol in head(PORTFOLIO_ASSETS, 5)) {
    ticker <- get_ticker(symbol)
    if (!is.null(ticker)) {
      cat(sprintf("├─ %-8s $%-8.2f %+6.2f%%\n",
                  symbol,
                  ticker$last,
                  ticker$change_24h_pct))
    }
  }
  
  cat("\nPress Enter to continue...")
  readline()
}

#' Run market scan interface
run_market_scan_interface <- function() {
  
  cat("\n🔍 === MARKET SCANNER === 🔍\n")
  cat("Select scan type:\n")
  cat("[1] Quick Scan (Top 10)\n")
  cat("[2] Full Scan (Top 50)\n")
  cat("[3] Custom Symbols\n")
  cat("[0] Back\n")
  
  choice <- readline("Choice: ")
  
  if (choice == "0") return()
  
  scan_type <- switch(choice,
    "1" = "quick",
    "2" = "full",
    "quick"
  )
  
  symbols <- NULL
  if (choice == "3") {
    input <- readline("Enter symbols (comma-separated): ")
    symbols <- trimws(strsplit(input, ",")[[1]])
  }
  
  # Run scan
  cat("\n🔄 Scanning markets...\n")
  results <- MARKET_SCANNER$scan(scan_type, symbols)
  
  readline("\nPress Enter to continue...")
}

#' Show strategy signals
show_strategy_signals <- function() {
  
  cat("\n🎯 === STRATEGY SIGNALS === 🎯\n")
  cat("Select strategy:\n")
  cat("[1] OI Momentum Strategy\n")
  cat("[2] Scalping Strategy\n")
  cat("[3] All Strategies\n")
  cat("[0] Back\n")
  
  choice <- readline("Choice: ")
  
  if (choice == "0") return()
  
  # Generate signals based on choice
  if (choice == "1" || choice == "3") {
    cat("\n📈 OI Momentum Signals:\n")
    oi_signals <- OI_MOMENTUM_STRATEGY$generate_signals()
  }
  
  if (choice == "2" || choice == "3") {
    cat("\n⚡ Scalping Signals:\n")
    scalp_signals <- SCALPING_STRATEGY$scan()
  }
  
  readline("\nPress Enter to continue...")
}

# ==========================================================================================================
# 💼 POSITION MANAGEMENT
# ==========================================================================================================

#' Show positions interface
show_positions_interface <- function() {
  
  cat("\n💼 === POSITION MANAGEMENT === 💼\n")
  
  # Display current positions
  POSITION_MANAGER_FUNCTIONS$display()
  
  cat("\nOptions:\n")
  cat("[1] Close Position\n")
  cat("[2] Modify Stop/Target\n")
  cat("[3] Scale Out\n")
  cat("[4] Protect All Positions\n")
  cat("[0] Back\n")
  
  choice <- readline("Choice: ")
  
  if (choice == "0") return()
  
  switch(choice,
    "1" = close_position_interface(),
    "2" = modify_position_interface(),
    "3" = scale_out_interface(),
    "4" = protect_all_positions()
  )
}

#' Execute trade interface
execute_trade_interface <- function() {
  
  cat("\n⚡ === EXECUTE TRADE === ⚡\n")
  cat("Select trade type:\n")
  cat("[1] Manual Trade\n")
  cat("[2] Strategy Signal Trade\n")
  cat("[3] Quick Buy/Sell\n")
  cat("[0] Back\n")
  
  choice <- readline("Choice: ")
  
  if (choice == "0") return()
  
  switch(choice,
    "1" = manual_trade_interface(),
    "2" = strategy_trade_interface(),
    "3" = quick_trade_interface()
  )
}

# ==========================================================================================================
# 📈 MONITORING & PERFORMANCE
# ==========================================================================================================

#' Show performance dashboard
show_performance_dashboard <- function() {
  
  cat("\n📈 === PERFORMANCE DASHBOARD === 📈\n")
  
  # Use performance tracker
  PERFORMANCE_TRACKER$dashboard()
  
  cat("\nOptions:\n")
  cat("[1] Detailed Report\n")
  cat("[2] Trade Analysis\n")
  cat("[3] Risk Metrics\n")
  cat("[4] Export Report\n")
  cat("[0] Back\n")
  
  choice <- readline("Choice: ")
  
  if (choice == "0") return()
  
  switch(choice,
    "1" = PERFORMANCE_TRACKER$report("daily"),
    "2" = PERFORMANCE_TRACKER$analyze_trades(),
    "3" = show_risk_metrics(),
    "4" = export_performance_report()
  )
}

#' Show risk status
show_risk_status <- function() {
  
  cat("\n🛡️ === RISK STATUS === 🛡️\n")
  
  # Analyze portfolio risk
  risk_analysis <- analyze_portfolio_risk()
  
  # Check emergency conditions
  emergency_check <- check_emergency_conditions()
  
  if (emergency_check$triggered) {
    cat("\n🚨 EMERGENCY CONDITIONS DETECTED! 🚨\n")
    for (action in emergency_check$actions) {
      cat("⚠️", action$message, "\n")
    }
  }
  
  readline("\nPress Enter to continue...")
}

# ==========================================================================================================
# 🔄 AUTOMATED OPERATIONS
# ==========================================================================================================

#' Start automated trading
start_automated_trading <- function(strategies = NULL, dry_run = TRUE) {
  
  cat("\n🤖 === AUTOMATED TRADING === 🤖\n")
  
  if (dry_run) {
    cat("⚠️ DRY RUN MODE - No real trades will be executed\n")
  }
  
  # Confirm
  confirm <- readline("Start automated trading? (y/n): ")
  if (tolower(confirm) != "y") return()
  
  # Initialize automation state
  AUTOMATION_STATE <<- list(
    active = TRUE,
    dry_run = dry_run,
    strategies = strategies %||% c("oi_momentum", "scalping"),
    start_time = Sys.time(),
    iteration = 0,
    last_scan = NULL
  )
  
  cat("\n🚀 Automated trading started\n")
  cat("Press Ctrl+C to stop\n\n")
  
  # Main automation loop
  while (AUTOMATION_STATE$active) {
    tryCatch({
      AUTOMATION_STATE$iteration <- AUTOMATION_STATE$iteration + 1
      
      # Update performance metrics
      update_performance_metrics()
      
      # Monitor positions
      monitor_all_positions()
      
      # Check risk limits
      if (!check_trading_allowed()) {
        cat("⚠️ Trading halted due to risk limits\n")
        Sys.sleep(300)  # Wait 5 minutes
        next
      }
      
      # Run market scan
      if (is.null(AUTOMATION_STATE$last_scan) || 
          difftime(Sys.time(), AUTOMATION_STATE$last_scan, units = "mins") > 5) {
        
        cat("\n🔍 Scanning markets...\n")
        scan_results <- MARKET_SCANNER$quick_scan()
        AUTOMATION_STATE$last_scan <- Sys.time()
      }
      
      # Generate and evaluate signals
      for (strategy in AUTOMATION_STATE$strategies) {
        evaluate_strategy_signals(strategy, dry_run)
      }
      
      # Display status
      if (AUTOMATION_STATE$iteration %% 10 == 0) {
        display_automation_status()
      }
      
      # Wait before next iteration
      Sys.sleep(30)  # 30 seconds
      
    }, interrupt = function(e) {
      cat("\n⏹️ Automated trading stopped by user\n")
      AUTOMATION_STATE$active <- FALSE
    }, error = function(e) {
      cat("\n❌ Error in automation:", e$message, "\n")
      NOTIFICATIONS$error(e$message, "automated_trading")
    })
  }
  
  # Cleanup
  cat("\n🛑 Automated trading stopped\n")
  cat("Duration:", format(difftime(Sys.time(), AUTOMATION_STATE$start_time)), "\n")
}

#' Monitor all positions
monitor_all_positions <- function() {
  
  # Monitor OI positions
  if (exists("monitor_oi_positions")) {
    monitor_oi_positions()
  }
  
  # Monitor scalping positions
  if (exists("monitor_active_scalps")) {
    monitor_active_scalps()
  }
  
  # General position monitoring
  positions <- get_current_positions()
  
  for (i in seq_len(nrow(positions))) {
    pos <- positions[i, ]
    
    # Check for trailing stop updates
    if (pos$pnl_ratio > 0.02) {  # 2% profit
      update_position_trailing_stop(pos$symbol)
    }
  }
}

#' Evaluate strategy signals
evaluate_strategy_signals <- function(strategy, dry_run = TRUE) {
  
  if (strategy == "oi_momentum") {
    signals <- OI_MOMENTUM_STRATEGY$generate_signals()
    
    if (length(signals$ranked) > 0) {
      # Take best signal
      best_signal <- signals$ranked[[1]]
      symbol <- names(signals$ranked)[1]
      
      if (best_signal$confidence > 0.7) {
        cat("\n🎯 OI Signal:", symbol, "-", best_signal$direction, "\n")
        OI_MOMENTUM_STRATEGY$execute_trade(symbol, best_signal, dry_run)
      }
    }
    
  } else if (strategy == "scalping") {
    signals <- SCALPING_STRATEGY$scan()
    
    if (length(signals) > 0) {
      # Take first valid signal
      for (symbol in names(signals)) {
        signal <- signals[[symbol]]
        
        if (signal$strength > 0.75) {
          cat("\n⚡ Scalp Signal:", symbol, "-", signal$direction, "\n")
          SCALPING_STRATEGY$execute(symbol, signal, dry_run)
          break  # One scalp at a time
        }
      }
    }
  }
}

# ==========================================================================================================
# 🛠️ SYSTEM UTILITIES
# ==========================================================================================================

#' Show system status
show_system_status <- function() {
  
  cat("\n🔧 === SYSTEM STATUS === 🔧\n")
  cat("Uptime:", format(difftime(Sys.time(), SYSTEM_STATUS$start_time)), "\n\n")
  
  cat("📦 Components:\n")
  for (comp in names(SYSTEM_STATUS$components)) {
    status <- if (SYSTEM_STATUS$components[[comp]]) "✅" else "❌"
    cat("  ", status, comp, "\n")
  }
  
  if (length(SYSTEM_STATUS$errors) > 0) {
    cat("\n⚠️ Errors:\n")
    for (err in names(SYSTEM_STATUS$errors)) {
      cat("  •", err, ":", SYSTEM_STATUS$errors[[err]], "\n")
    }
  }
  
  # Database stats
  if (exists("DATA_MANAGER")) {
    cat("\n💾 Database:\n")
    db_stats <- DATA_MANAGER$stats()
    cat("  • Size:", round(db_stats$db_size_mb, 1), "MB\n")
    cat("  • Trades:", db_stats$trades_count, "\n")
    cat("  • Cache entries:", db_stats$cache_entries, "\n")
  }
  
  # API status
  cat("\n🔌 API Status:\n")
  api_test <- test_api_connection()
  
  readline("\nPress Enter to continue...")
}

#' Backup system data
backup_system_data <- function() {
  
  cat("\n💾 === BACKUP SYSTEM === 💾\n")
  
  confirm <- readline("Create system backup? (y/n): ")
  if (tolower(confirm) != "y") return()
  
  cat("\n🔄 Creating backup...\n")
  
  # Backup database
  if (exists("DATA_MANAGER")) {
    backup_file <- DATA_MANAGER$backup()
    if (!is.null(backup_file)) {
      cat("✅ Database backed up to:", backup_file, "\n")
    }
  }
  
  # Save current configurations
  save_all_configurations()
  
  # Export recent reports
  DATA_MANAGER$generate_report(7)  # Last 7 days
  
  cat("\n✅ Backup complete\n")
  readline("Press Enter to continue...")
}

#' Save all configurations
save_all_configurations <- function() {
  
  # Save strategy configurations
  if (exists("OI_MOMENTUM_CONFIG")) {
    DATA_MANAGER$save_config("oi_momentum", OI_MOMENTUM_CONFIG)
  }
  
  if (exists("SCALPING_CONFIG")) {
    DATA_MANAGER$save_config("scalping", SCALPING_CONFIG)
  }
  
  # Save system configuration
  system_config <- list(
    portfolio_assets = PORTFOLIO_ASSETS,
    risk_params = RISK_PARAMS,
    api_config = API_CONFIG
  )
  
  DATA_MANAGER$save_config("system", system_config)
}

# ==========================================================================================================
# 🎯 QUICK ACTIONS
# ==========================================================================================================

#' Show quick actions menu
show_quick_actions <- function() {
  
  cat("\n⚡ === QUICK ACTIONS === ⚡\n")
  cat("[1] 🚨 Emergency Close All\n")
  cat("[2] 🛡️ Protect All Positions\n")
  cat("[3] 📊 Quick Market Check\n")
  cat("[4] 💰 Quick Buy\n")
  cat("[5] 💸 Quick Sell\n")
  cat("[0] Back\n")
  
  choice <- readline("Choice: ")
  
  switch(choice,
    "1" = emergency_close_all(),
    "2" = protect_all_positions(),
    "3" = quick_market_check(),
    "4" = quick_buy_interface(),
    "5" = quick_sell_interface()
  )
}

#' Emergency close all positions
emergency_close_all <- function() {
  
  cat("\n🚨 === EMERGENCY CLOSE ALL === 🚨\n")
  cat("⚠️ This will close ALL open positions immediately!\n")
  
  positions <- get_current_positions()
  if (nrow(positions) == 0) {
    cat("No open positions.\n")
    return()
  }
  
  cat("\nPositions to close:\n")
  for (i in 1:nrow(positions)) {
    cat("  •", positions$symbol[i], "-", positions$side[i], 
        "- P&L:", round(positions$unrealized_pnl[i], 2), "\n")
  }
  
  confirm <- readline("\nConfirm CLOSE ALL? (type 'YES'): ")
  
  if (confirm == "YES") {
    close_all_positions(dry_run = FALSE)
    NOTIFICATIONS$risk("EMERGENCY: All positions closed", "User initiated emergency close")
  }
}

#' Protect all positions
protect_all_positions <- function() {
  
  cat("\n🛡️ Protecting all positions...\n")
  
  positions <- get_current_positions()
  
  for (i in 1:nrow(positions)) {
    pos <- positions[i, ]
    
    if (pos$pnl_ratio > 0) {
      # Set breakeven stop for profitable positions
      breakeven_price <- pos$avg_price
      
      cat("Setting breakeven stop for", pos$symbol, "\n")
      adjust_position_protection(pos$symbol, new_sl_price = breakeven_price)
    }
  }
  
  cat("✅ Position protection updated\n")
}

#' Quick market check
quick_market_check <- function() {
  
  cat("\n📊 Quick Market Check...\n")
  
  # Top movers
  cat("\n🔥 TOP MOVERS:\n")
  
  for (symbol in PORTFOLIO_ASSETS[1:10]) {
    ticker <- get_ticker(symbol)
    if (!is.null(ticker) && abs(ticker$change_24h_pct) > 5) {
      cat(sprintf("  %s %-8s %+.1f%%\n",
                  if (ticker$change_24h_pct > 0) "🟢" else "🔴",
                  symbol,
                  ticker$change_24h_pct))
    }
  }
  
  # Quick sentiment
  cat("\n🎭 MARKET SENTIMENT:\n")
  bullish <- 0
  bearish <- 0
  
  for (symbol in PORTFOLIO_ASSETS) {
    ticker <- get_ticker(symbol)
    if (!is.null(ticker)) {
      if (ticker$change_24h_pct > 0) bullish <- bullish + 1
      else bearish <- bearish + 1
    }
  }
  
  cat("  Bullish:", bullish, "| Bearish:", bearish, "\n")
  cat("  Sentiment:", if (bullish > bearish * 1.5) "BULLISH" 
                     else if (bearish > bullish * 1.5) "BEARISH"
                     else "NEUTRAL", "\n")
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

#' Display initialization summary
display_initialization_summary <- function() {
  
  cat("\n📋 === INITIALIZATION SUMMARY === 📋\n")
  
  loaded <- sum(unlist(SYSTEM_STATUS$components))
  total <- length(SYSTEM_STATUS$components)
  
  cat("Components loaded:", loaded, "/", total, "\n")
  
  if (length(SYSTEM_STATUS$errors) > 0) {
    cat("\n⚠️ Initialization warnings:\n")
    for (err in names(SYSTEM_STATUS$errors)) {
      cat("  •", err, "\n")
    }
  }
  
  if (loaded == total) {
    cat("\n✅ System fully operational!\n")
  } else if (loaded >= total * 0.7) {
    cat("\n⚠️ System operational with limited features\n")
  } else {
    cat("\n❌ System initialization failed\n")
  }
}

#' Check if trading is allowed
check_trading_allowed <- function() {
  
  # Check daily loss limit
  daily_stats <- get_daily_trading_stats()
  
  if (daily_stats$loss_pct > RISK_PARAMS$daily_loss_limit) {
    return(FALSE)
  }
  
  # Check drawdown
  if (exists("calculate_current_drawdown")) {
    current_dd <- calculate_current_drawdown()
    if (current_dd > RISK_PARAMS$max_drawdown) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Get daily trading statistics
get_daily_trading_stats <- function() {
  
  if (exists("DATA_MANAGER")) {
    trades_today <- DATA_MANAGER$get_trades(
      start_date = Sys.Date(),
      end_date = Sys.Date()
    )
    
    if (nrow(trades_today) > 0) {
      return(list(
        trades = nrow(trades_today),
        pnl = sum(trades_today$pnl),
        loss_pct = min(0, sum(trades_today$pnl)) / get_account_balance()$equity
      ))
    }
  }
  
  return(list(trades = 0, pnl = 0, loss_pct = 0))
}

#' Display automation status
display_automation_status <- function() {
  
  cat("\n🤖 Automation Status - Iteration", AUTOMATION_STATE$iteration, "\n")
  cat("Running for:", format(difftime(Sys.time(), AUTOMATION_STATE$start_time)), "\n")
  
  # Quick performance check
  if (exists("PERFORMANCE_TRACKER")) {
    current <- PERFORMANCE_TRACKER$get_current()
    if (!is.null(current)) {
      cat("Today's P&L: $", round(current$total_pnl_today, 2), "\n")
    }
  }
}

#' Restart system
restart_system <- function() {
  
  cat("\n🔄 Restarting system...\n")
  
  # Save current state
  save_all_configurations()
  
  # Clear all objects except essentials
  rm(list = setdiff(ls(globalenv()), c("initialize_trading_system")), 
     envir = globalenv())
  
  # Reinitialize
  initialize_trading_system()
}

#' Shutdown system
shutdown_system <- function() {
  
  cat("\n🛑 === SYSTEM SHUTDOWN === 🛑\n")
  
  confirm <- readline("Shutdown trading system? (y/n): ")
  if (tolower(confirm) != "y") return()
  
  # Check for open positions
  positions <- get_current_positions()
  if (nrow(positions) > 0) {
    cat("\n⚠️ Warning:", nrow(positions), "positions still open!\n")
    close_confirm <- readline("Close all positions? (y/n): ")
    
    if (tolower(close_confirm) == "y") {
      close_all_positions(dry_run = FALSE)
    }
  }
  
  # Final backup
  cat("\n💾 Creating final backup...\n")
  DATA_MANAGER$backup("shutdown_backup")
  
  # Disconnect
  if (exists("DB_CONNECTION")) {
    dbDisconnect(DB_CONNECTION)
  }
  
  # Log shutdown
  NOTIFICATIONS$system("shutdown", "Trading system shut down gracefully")
  
  cat("\n✅ System shut down successfully\n")
  cat("Thank you for using Bitget Trading System!\n")
  
  q(save = "no")
}

# ==========================================================================================================
# 🚀 MAIN EXECUTION
# ==========================================================================================================

#' Main system loop
run_trading_system <- function() {
  
  # Initialize if not already done
  if (!exists("SYSTEM_STATUS") || !SYSTEM_STATUS$initialized) {
    if (!initialize_trading_system()) {
      cat("❌ System initialization failed\n")
      return()
    }
  }
  
  # Main command loop
  while (TRUE) {
    show_main_menu()
    
    command <- readline("Enter command: ")
    
    if (toupper(command) == "EXIT") {
      shutdown_system()
      break
    }
    
    process_command(command)
  }
}

# ==========================================================================================================
# 🎯 QUICK START FUNCTIONS
# ==========================================================================================================

#' Quick start automated trading
quick_start_auto <- function(dry_run = TRUE) {
  initialize_trading_system()
  Sys.sleep(2)
  start_automated_trading(dry_run = dry_run)
}

#' Quick start manual mode
quick_start_manual <- function() {
  initialize_trading_system()
  run_trading_system()
}

# Create main interface
TRADING_SYSTEM <- list(
  initialize = initialize_trading_system,
  run = run_trading_system,
  auto_trade = start_automated_trading,
  
  # Quick access
  market_overview = show_market_overview,
  positions = show_positions_interface,
  performance = show_performance_dashboard,
  scan_markets = function() MARKET_SCANNER$quick_scan(),
  
  # Controls
  stop = function() AUTOMATION_STATE$active <<- FALSE,
  status = show_system_status,
  backup = backup_system_data
)

# ==========================================================================================================
# 🏁 STARTUP MESSAGE
# ==========================================================================================================

cat("\n✅ MAIN.R loaded successfully!\n")
cat("\n🚀 TO START THE TRADING SYSTEM:\n")
cat("   • Full System:    run_trading_system()\n")
cat("   • Auto Trading:   quick_start_auto(dry_run = TRUE)\n")
cat("   • Manual Mode:    quick_start_manual()\n")
cat("\n💡 For help, type: TRADING_SYSTEM$status()\n")
cat("\n")