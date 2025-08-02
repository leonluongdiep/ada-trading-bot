# ==========================================================================================================
# 📈 ADVANCED BITGET ORDER MANAGEMENT SYSTEM V2
# ==========================================================================================================
# Erweitert: bitget_order_creation.r + Advanced Features
# Komplettes Order Management mit Tracking, Validation und Auto-Execution
# ==========================================================================================================

cat("📈 Loading Advanced Bitget Order Management System V2...\n")

# ==========================================================================================================
# 🔧 SYSTEM INITIALIZATION & DEPENDENCIES
# ==========================================================================================================

# Load central configuration if not already loaded
if (!exists("SYSTEM_CONFIG_LOADED")) {
  source("system_config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
}

# Load core engine if not already loaded
if (!exists("CORE_ENGINE_LOADED")) {
  source("bitget_core_engine.r")
  CORE_ENGINE_LOADED <- TRUE
}

# Load risk manager if not already loaded
if (!exists("RISK_MANAGER_LOADED")) {
  source("portfolio_risk_manager.r")
  RISK_MANAGER_LOADED <- TRUE
}

# ==========================================================================================================
# 🛠️ ENHANCED ORDER VALIDATION SYSTEM
# ==========================================================================================================

#' Advanced order validation with real-time checks
validate_order_comprehensive <- function(symbol, side, size, price = NULL, order_type = "market") {
  cat("🔍 Validating order:", side, size, symbol, "\n")
  
  validation_results <- list(
    valid = TRUE,
    warnings = list(),
    errors = list(),
    checks_performed = list()
  )
  
  # 1. Symbol validation
  validation_results$checks_performed$symbol_check <- validate_symbol_active(symbol)
  if (!validation_results$checks_performed$symbol_check$valid) {
    validation_results$valid <- FALSE
    validation_results$errors <- append(validation_results$errors, 
                                       paste("Invalid symbol:", symbol))
  }
  
  # 2. Market hours check
  validation_results$checks_performed$market_hours <- check_market_hours(symbol)
  if (!validation_results$checks_performed$market_hours$active) {
    validation_results$warnings <- append(validation_results$warnings,
                                         "Market may be closed or low liquidity period")
  }
  
  # 3. Position size validation
  validation_results$checks_performed$size_check <- validate_position_size(symbol, size)
  if (!validation_results$checks_performed$size_check$valid) {
    validation_results$valid <- FALSE
    validation_results$errors <- append(validation_results$errors,
                                       validation_results$checks_performed$size_check$error)
  }
  
  # 4. Available balance check
  validation_results$checks_performed$balance_check <- check_available_balance(symbol, side, size, price)
  if (!validation_results$checks_performed$balance_check$sufficient) {
    validation_results$valid <- FALSE
    validation_results$errors <- append(validation_results$errors,
                                       "Insufficient balance for order")
  }
  
  # 5. Risk management check
  validation_results$checks_performed$risk_check <- validate_risk_limits(symbol, side, size)
  if (!validation_results$checks_performed$risk_check$within_limits) {
    validation_results$valid <- FALSE
    validation_results$errors <- append(validation_results$errors,
                                       "Order exceeds risk management limits")
  }
  
  # 6. Price validation (for limit orders)
  if (!is.null(price) && order_type == "limit") {
    validation_results$checks_performed$price_check <- validate_limit_price(symbol, price, side)
    if (!validation_results$checks_performed$price_check$reasonable) {
      validation_results$warnings <- append(validation_results$warnings,
                                           validation_results$checks_performed$price_check$warning)
    }
  }
  
  # 7. Liquidity check
  validation_results$checks_performed$liquidity_check <- check_order_liquidity(symbol, size)
  if (validation_results$checks_performed$liquidity_check$impact_high) {
    validation_results$warnings <- append(validation_results$warnings,
                                         "Order may have high market impact")
  }
  
  # Display validation results
  if (validation_results$valid) {
    cat("✅ Order validation passed\n")
    if (length(validation_results$warnings) > 0) {
      cat("⚠️ Warnings:\n")
      for (warning in validation_results$warnings) {
        cat("   -", warning, "\n")
      }
    }
  } else {
    cat("❌ Order validation failed\n")
    for (error in validation_results$errors) {
      cat("   ❌", error, "\n")
    }
  }
  
  return(validation_results)
}

#' Validate symbol is active and tradeable
validate_symbol_active <- function(symbol) {
  tryCatch({
    ticker_data <- get_enhanced_ticker_data(symbol)
    
    if (is.null(ticker_data)) {
      return(list(valid = FALSE, error = "Cannot fetch symbol data"))
    }
    
    # Check if price is valid and volume exists
    if (ticker_data$last_price > 0 && ticker_data$volume_24h_usdt > 0) {
      return(list(valid = TRUE, status = "ACTIVE"))
    } else {
      return(list(valid = FALSE, error = "Symbol appears inactive"))
    }
    
  }, error = function(e) {
    return(list(valid = FALSE, error = paste("Symbol validation error:", e$message)))
  })
}

#' Check if market is open/active
check_market_hours <- function(symbol) {
  # Crypto markets are 24/7, but we can check for liquidity periods
  ticker_data <- get_enhanced_ticker_data(symbol)
  
  if (is.null(ticker_data)) {
    return(list(active = FALSE, reason = "Cannot fetch market data"))
  }
  
  # Simple volume-based activity check
  volume_24h <- ticker_data$volume_24h_usdt %||% 0
  asset_config <- get_asset_config(symbol)
  min_volume <- asset_config$typical_volume_threshold %||% 1000000
  
  if (volume_24h > min_volume * 0.1) {  # At least 10% of typical volume
    return(list(active = TRUE, volume_status = "NORMAL"))
  } else {
    return(list(active = FALSE, reason = "Low liquidity period", volume_24h = volume_24h))
  }
}

#' Validate position size against limits
validate_position_size <- function(symbol, size) {
  asset_config <- get_asset_config(symbol)
  
  # Check minimum size
  min_size <- asset_config$min_size %||% 0.001
  if (size < min_size) {
    return(list(valid = FALSE, error = paste("Size below minimum:", min_size)))
  }
  
  # Check maximum reasonable size
  max_size <- asset_config$max_size %||% 1000000
  if (size > max_size) {
    return(list(valid = FALSE, error = paste("Size above maximum:", max_size)))
  }
  
  return(list(valid = TRUE, size = size))
}

#' Check available balance for order
check_available_balance <- function(symbol, side, size, price = NULL) {
  tryCatch({
    # Get account balance
    balance_response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/account/accounts",
      params = list(productType = "USDT-FUTURES")
    )
    
    if (is.null(balance_response) || is.null(balance_response$data)) {
      return(list(sufficient = FALSE, error = "Cannot fetch balance"))
    }
    
    # Find USDT balance
    usdt_balance <- NULL
    for (account in balance_response$data) {
      if (account$marginCoin == "USDT") {
        usdt_balance <- as.numeric(account$available)
        break
      }
    }
    
    if (is.null(usdt_balance)) {
      return(list(sufficient = FALSE, error = "USDT balance not found"))
    }
    
    # Calculate required margin
    if (is.null(price)) {
      # For market orders, use current price
      ticker_data <- get_enhanced_ticker_data(symbol)
      price <- ticker_data$last_price
    }
    
    required_margin <- size * price * 0.1  # Assuming 10x leverage
    
    return(list(
      sufficient = usdt_balance >= required_margin,
      available_balance = usdt_balance,
      required_margin = required_margin,
      remaining_after_order = usdt_balance - required_margin
    ))
    
  }, error = function(e) {
    return(list(sufficient = FALSE, error = paste("Balance check error:", e$message)))
  })
}

#' Validate against risk management limits
validate_risk_limits <- function(symbol, side, size) {
  # Get current positions
  positions <- get_current_positions()
  
  # Calculate total exposure
  current_exposure <- sum(abs(positions$unrealized_pnl), na.rm = TRUE)
  
  # Get position limits from config
  max_portfolio_exposure <- POSITION_LIMITS$max_portfolio_exposure * 100000  # Assuming 100k portfolio
  max_single_position <- POSITION_LIMITS$max_single_position * 100000
  
  # Calculate order value
  ticker_data <- get_enhanced_ticker_data(symbol)
  order_value <- size * ticker_data$last_price
  
  # Check limits
  if (current_exposure + order_value > max_portfolio_exposure) {
    return(list(within_limits = FALSE, error = "Exceeds portfolio exposure limit"))
  }
  
  if (order_value > max_single_position) {
    return(list(within_limits = FALSE, error = "Exceeds single position limit"))
  }
  
  return(list(within_limits = TRUE, current_exposure = current_exposure, order_value = order_value))
}

#' Validate limit price reasonableness
validate_limit_price <- function(symbol, price, side) {
  ticker_data <- get_enhanced_ticker_data(symbol)
  current_price <- ticker_data$last_price
  
  price_diff_percent <- abs(price - current_price) / current_price * 100
  
  if (price_diff_percent > 10) {  # More than 10% away from market
    warning_msg <- paste("Price", round(price_diff_percent, 2), "% away from market")
    
    if (side == "buy" && price > current_price * 1.05) {
      warning_msg <- paste(warning_msg, "- Buy price high, will execute immediately")
    } else if (side == "sell" && price < current_price * 0.95) {
      warning_msg <- paste(warning_msg, "- Sell price low, will execute immediately")
    }
    
    return(list(reasonable = FALSE, warning = warning_msg, price_diff = price_diff_percent))
  }
  
  return(list(reasonable = TRUE, price_diff = price_diff_percent))
}

#' Check order liquidity impact
check_order_liquidity <- function(symbol, size) {
  # Get orderbook data
  orderbook <- get_enhanced_orderbook(symbol)
  
  if (is.null(orderbook)) {
    return(list(impact_high = FALSE, error = "Cannot assess liquidity"))
  }
  
  # Simple liquidity check based on top level volumes
  total_bid_volume <- orderbook$bid_volume_total %||% size * 10
  total_ask_volume <- orderbook$ask_volume_total %||% size * 10
  
  # Consider high impact if order is more than 10% of visible liquidity
  impact_ratio <- size / min(total_bid_volume, total_ask_volume)
  
  return(list(
    impact_high = impact_ratio > 0.1,
    impact_ratio = impact_ratio,
    bid_volume = total_bid_volume,
    ask_volume = total_ask_volume
  ))
}

# ==========================================================================================================
# 🎯 SMART ORDER EXECUTION SYSTEM
# ==========================================================================================================

#' Smart order execution with automatic optimization
execute_smart_order <- function(symbol, side, size, target_price = NULL, execution_strategy = "optimal", dry_run = TRUE) {
  cat("🎯 Smart Order Execution:\n")
  cat("   Symbol:", symbol, "| Side:", side, "| Size:", size, "\n")
  cat("   Strategy:", execution_strategy, "| Dry Run:", dry_run, "\n")
  
  # 1. Comprehensive validation
  validation <- validate_order_comprehensive(symbol, side, size, target_price, "market")
  
  if (!validation$valid) {
    cat("❌ Order validation failed - aborting execution\n")
    return(list(success = FALSE, error = "Validation failed", validation = validation))
  }
  
  # 2. Choose execution strategy
  execution_plan <- create_execution_plan(symbol, side, size, target_price, execution_strategy)
  
  cat("📋 Execution Plan:\n")
  cat("   Method:", execution_plan$method, "\n")
  cat("   Estimated Slippage:", round(execution_plan$estimated_slippage, 4), "%\n")
  cat("   Number of Sub-Orders:", length(execution_plan$sub_orders), "\n")
  
  # 3. Execute order(s)
  if (dry_run) {
    cat("🔍 DRY RUN - Order would be executed with plan above\n")
    return(list(success = TRUE, mode = "dry_run", execution_plan = execution_plan))
  }
  
  # Real execution
  execution_results <- execute_order_plan(execution_plan)
  
  # 4. Post-execution analysis
  post_execution_analysis <- analyze_execution_performance(execution_results, execution_plan)
  
  return(list(
    success = execution_results$overall_success,
    execution_plan = execution_plan,
    execution_results = execution_results,
    performance_analysis = post_execution_analysis
  ))
}

#' Create optimized execution plan
create_execution_plan <- function(symbol, side, size, target_price, strategy) {
  ticker_data <- get_enhanced_ticker_data(symbol)
  current_price <- ticker_data$last_price
  
  plan <- list(
    symbol = symbol,
    side = side,
    total_size = size,
    strategy = strategy,
    sub_orders = list()
  )
  
  # Strategy-specific planning
  if (strategy == "market") {
    # Simple market order
    plan$method <- "SINGLE_MARKET"
    plan$estimated_slippage <- 0.05  # 0.05% estimated slippage
    plan$sub_orders <- list(list(
      type = "market",
      size = size,
      price = NULL
    ))
    
  } else if (strategy == "twap") {
    # Time Weighted Average Price - split into smaller orders
    plan$method <- "TWAP"
    plan$estimated_slippage <- 0.02
    
    num_splits <- min(10, max(2, floor(size / 100)))  # Split large orders
    split_size <- size / num_splits
    
    for (i in 1:num_splits) {
      plan$sub_orders[[i]] <- list(
        type = "market",
        size = split_size,
        delay_seconds = (i - 1) * 30  # 30 second intervals
      )
    }
    
  } else if (strategy == "iceberg") {
    # Iceberg order - hide size
    plan$method <- "ICEBERG"
    plan$estimated_slippage <- 0.03
    
    visible_size <- min(size * 0.2, 100)  # Show max 20% or 100 units
    remaining_size <- size
    
    while (remaining_size > 0) {
      order_size <- min(visible_size, remaining_size)
      plan$sub_orders[[length(plan$sub_orders) + 1]] <- list(
        type = "limit",
        size = order_size,
        price = if (side == "buy") current_price * 0.9995 else current_price * 1.0005
      )
      remaining_size <- remaining_size - order_size
    }
    
  } else {
    # Optimal strategy (default)
    plan$method <- "OPTIMAL"
    
    # Choose best method based on order size and market conditions
    if (size * current_price > 10000) {  # Large orders (>$10k)
      plan <- create_execution_plan(symbol, side, size, target_price, "twap")
    } else if (size > 1000) {  # Medium orders
      plan <- create_execution_plan(symbol, side, size, target_price, "iceberg")
    } else {  # Small orders
      plan <- create_execution_plan(symbol, side, size, target_price, "market")
    }
  }
  
  return(plan)
}

#' Execute the planned order strategy
execute_order_plan <- function(execution_plan) {
  results <- list(
    sub_order_results = list(),
    overall_success = TRUE,
    total_filled = 0,
    average_price = 0,
    total_fees = 0
  )
  
  cat("🚀 Executing order plan with", length(execution_plan$sub_orders), "sub-orders\n")
  
  for (i in 1:length(execution_plan$sub_orders)) {
    sub_order <- execution_plan$sub_orders[[i]]
    
    cat("   Executing sub-order", i, "- Size:", sub_order$size, "\n")
    
    # Add delay if specified
    if (!is.null(sub_order$delay_seconds) && sub_order$delay_seconds > 0) {
      cat("   Waiting", sub_order$delay_seconds, "seconds...\n")
      Sys.sleep(sub_order$delay_seconds)
    }
    
    # Execute sub-order
    sub_result <- execute_single_order(
      execution_plan$symbol,
      execution_plan$side,
      sub_order$size,
      sub_order$price,
      sub_order$type
    )
    
    results$sub_order_results[[i]] <- sub_result
    
    if (sub_result$success) {
      results$total_filled <- results$total_filled + sub_result$filled_size
      results$total_fees <- results$total_fees + (sub_result$fees %||% 0)
      cat("   ✅ Sub-order", i, "completed\n")
    } else {
      results$overall_success <- FALSE
      cat("   ❌ Sub-order", i, "failed:", sub_result$error, "\n")
      break  # Stop execution on failure
    }
    
    # Brief pause between orders
    Sys.sleep(0.5)
  }
  
  # Calculate average price
  if (results$total_filled > 0) {
    total_value <- sum(sapply(results$sub_order_results, function(x) {
      if (x$success) x$filled_size * x$average_price else 0
    }))
    results$average_price <- total_value / results$total_filled
  }
  
  return(results)
}

#' Execute single order (atomic operation)
execute_single_order <- function(symbol, side, size, price = NULL, order_type = "market") {
  tryCatch({
    # Prepare order parameters
    params <- list(
      symbol = symbol,
      productType = "USDT-FUTURES",
      marginMode = "crossed",
      marginCoin = "USDT",
      side = tolower(side),
      orderType = tolower(order_type),
      size = as.character(size)
    )
    
    # Add price for limit orders
    if (!is.null(price)) {
      params$price <- as.character(price)
    }
    
    # Execute API call
    response <- bitget_request(
      method = "POST",
      endpoint = "/api/v2/mix/order/place-order",
      params = params
    )
    
    if (!is.null(response) && !is.null(response$data) && !is.null(response$data$orderId)) {
      # Wait a moment for order to process
      Sys.sleep(1)
      
      # Check order status
      order_status <- get_order_status(response$data$orderId)
      
      return(list(
        success = TRUE,
        order_id = response$data$orderId,
        filled_size = order_status$filled_size %||% size,
        average_price = order_status$average_price %||% price,
        fees = order_status$fees %||% 0,
        status = order_status$status %||% "FILLED"
      ))
    } else {
      return(list(success = FALSE, error = "API call failed", response = response))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# ==========================================================================================================
# 📊 ORDER TRACKING & MONITORING SYSTEM
# ==========================================================================================================

#' Comprehensive order tracking system
track_all_orders <- function(show_details = TRUE) {
  cat("\n📊 === ORDER TRACKING SYSTEM === 📊\n")
  
  # Get all orders (open, filled, cancelled)
  all_orders <- get_comprehensive_orders()
  
  if (length(all_orders) == 0) {
    cat("ℹ️ No orders found\n")
    return(data.frame())
  }
  
  # Categorize orders
  open_orders <- all_orders[all_orders$status %in% c("LIVE", "PENDING", "PARTIALLY_FILLED"), ]
  filled_orders <- all_orders[all_orders$status == "FILLED", ]
  cancelled_orders <- all_orders[all_orders$status %in% c("CANCELLED", "REJECTED"), ]
  
  # Display summary
  cat("📈 Order Summary:\n")
  cat("   Open Orders:", nrow(open_orders), "\n")
  cat("   Filled Orders (24h):", nrow(filled_orders), "\n")
  cat("   Cancelled Orders (24h):", nrow(cancelled_orders), "\n")
  
  # Show details if requested
  if (show_details && nrow(open_orders) > 0) {
    cat("\n🔄 Open Orders:\n")
    display_orders_table(open_orders)
  }
  
  if (show_details && nrow(filled_orders) > 0) {
    cat("\n✅ Recent Filled Orders:\n")
    display_orders_table(head(filled_orders, 5))  # Show last 5
  }
  
  # Calculate performance metrics
  if (nrow(filled_orders) > 0) {
    performance <- calculate_order_performance(filled_orders)
    display_order_performance(performance)
  }
  
  return(all_orders)
}

#' Get comprehensive order data
get_comprehensive_orders <- function() {
  all_orders_list <- list()
  
  # Get pending orders
  tryCatch({
    pending_response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/order/orders-pending",
      params = list(productType = "USDT-FUTURES")
    )
    
    if (!is.null(pending_response$data)) {
      for (order in pending_response$data) {
        all_orders_list[[length(all_orders_list) + 1]] <- parse_order_data(order, "PENDING")
      }
    }
  }, error = function(e) {
    cat("⚠️ Could not fetch pending orders\n")
  })
  
  # Get order history
  tryCatch({
    history_response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/order/orders-history",
      params = list(
        productType = "USDT-FUTURES",
        limit = "50"
      )
    )
    
    if (!is.null(history_response$data)) {
      for (order in history_response$data) {
        all_orders_list[[length(all_orders_list) + 1]] <- parse_order_data(order, "HISTORY")
      }
    }
  }, error = function(e) {
    cat("⚠️ Could not fetch order history\n")
  })
  
  # Convert to data frame
  if (length(all_orders_list) > 0) {
    return(do.call(rbind, all_orders_list))
  } else {
    return(data.frame())
  }
}

#' Parse order data into standardized format
parse_order_data <- function(order, source) {
  data.frame(
    order_id = order$orderId %||% "Unknown",
    symbol = order$symbol %||% "Unknown",
    side = order$side %||% "Unknown",
    size = as.numeric(order$size %||% 0),
    filled_size = as.numeric(order$filledSize %||% 0),
    price = as.numeric(order$price %||% 0),
    average_price = as.numeric(order$averagePrice %||% 0),
    order_type = order$orderType %||% "Unknown",
    status = order$status %||% "Unknown",
    created_time = as.POSIXct(as.numeric(order$cTime %||% 0) / 1000, origin = "1970-01-01"),
    source = source,
    stringsAsFactors = FALSE
  )
}

#' Display orders in formatted table
display_orders_table <- function(orders) {
  if (nrow(orders) == 0) return()
  
  for (i in 1:nrow(orders)) {
    order <- orders[i, ]
    cat(sprintf("   %d. %s %s %s: %.4f @ %.6f [%s]\n",
                i,
                order$symbol,
                order$side,
                order$order_type,
                order$size,
                order$price,
                order$status))
  }
}

#' Calculate order execution performance
calculate_order_performance <- function(filled_orders) {
  if (nrow(filled_orders) == 0) return(list())
  
  # Calculate fill rate
  total_orders <- nrow(filled_orders)
  filled_completely <- sum(filled_orders$filled_size == filled_orders$size)
  fill_rate <- filled_completely / total_orders
  
  # Calculate average slippage (simplified)
  slippage_data <- filled_orders[filled_orders$price > 0 & filled_orders$average_price > 0, ]
  if (nrow(slippage_data) > 0) {
    slippages <- abs(slippage_data$average_price - slippage_data$price) / slippage_data$price
    avg_slippage <- mean(slippages, na.rm = TRUE)
  } else {
    avg_slippage <- 0
  }
  
  # Trading volume
  total_volume <- sum(filled_orders$filled_size * filled_orders$average_price, na.rm = TRUE)
  
  return(list(
    total_orders = total_orders,
    fill_rate = fill_rate,
    avg_slippage = avg_slippage,
    total_volume = total_volume,
    avg_order_size = mean(filled_orders$filled_size, na.rm = TRUE)
  ))
}

#' Display order performance metrics
display_order_performance <- function(performance) {
  cat("\n📊 Order Performance (24h):\n")
  cat("   Fill Rate:", round(performance$fill_rate * 100, 1), "%\n")
  cat("   Average Slippage:", round(performance$avg_slippage * 100, 4), "%\n")
  cat("   Total Volume:", round(performance$total_volume, 2), "USDT\n")
  cat("   Average Order Size:", round(performance$avg_order_size, 2), "\n")
}

#' Get single order status
get_order_status <- function(order_id) {
  tryCatch({
    response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/order/detail",
      params = list(
        orderId = order_id,
        productType = "USDT-FUTURES"
      )
    )
    
    if (!is.null(response$data)) {
      order <- response$data
      return(list(
        status = order$status,
        filled_size = as.numeric(order$filledSize %||% 0),
        average_price = as.numeric(order$averagePrice %||% 0),
        fees = as.numeric(order$fee %||% 0)
      ))
    }
    
    return(list(status = "UNKNOWN"))
    
  }, error = function(e) {
    return(list(status = "ERROR", error = e$message))
  })
}

# ==========================================================================================================
# 🤖 AUTOMATED TRADING STRATEGIES
# ==========================================================================================================

#' Auto-scalping strategy with risk management
auto_scalping_strategy <- function(symbol, max_positions = 3, profit_target = 0.5, stop_loss = 0.3, dry_run = TRUE) {
  cat("🤖 === AUTO SCALPING STRATEGY === 🤖\n")
  cat("Symbol:", symbol, "| Max Positions:", max_positions, "\n")
  cat("Profit Target:", profit_target, "% | Stop Loss:", stop_loss, "%\n")
  
  if (dry_run) {
    cat("🔍 DRY RUN MODE - No real trades\n")
  }
  
  # Check current positions
  positions <- get_current_positions()
  symbol_positions <- positions[positions$symbol == symbol, ]
  
  if (nrow(symbol_positions) >= max_positions) {
    cat("⚠️ Maximum positions reached for", symbol, "\n")
    return(list(action = "WAIT", reason = "Max positions reached"))
  }
  
  # Analyze market conditions
  market_analysis <- analyze_scalping_opportunity(symbol)
  
  if (market_analysis$signal == "NONE") {
    cat("📊 No scalping opportunity detected\n")
    return(list(action = "WAIT", reason = "No opportunity"))
  }
  
  # Calculate position size
  position_size <- calculate_scalping_size(symbol, market_analysis$volatility)
  
  cat("🎯 Scalping Signal:", market_analysis$signal, "\n")
  cat("📏 Position Size:", position_size, "\n")
  
  if (dry_run) {
    cat("🔍 Would execute:", market_analysis$signal, "order for", position_size, symbol, "\n")
    return(list(action = "TRADE", signal = market_analysis$signal, size = position_size, mode = "dry_run"))
  }
  
  # Execute scalping trade
  trade_result <- execute_scalping_trade(symbol, market_analysis$signal, position_size, profit_target, stop_loss)
  
  return(trade_result)
}

#' Analyze market for scalping opportunities
analyze_scalping_opportunity <- function(symbol) {
  # Get enhanced market data
  ticker_data <- get_enhanced_ticker_data(symbol)
  orderbook_data <- get_enhanced_orderbook(symbol)
  
  if (is.null(ticker_data) || is.null(orderbook_data)) {
    return(list(signal = "NONE", reason = "No data"))
  }
  
  # Calculate volatility
  volatility <- abs(ticker_data$change_24h_pct) / 100
  
  # Check spread
  spread_percent <- orderbook_data$spread_pct
  
  # Check volume
  volume_ratio <- ticker_data$volume_24h_usdt / (get_asset_config(symbol)$typical_volume_threshold %||% 1000000)
  
  # Scalping conditions
  conditions <- list(
    low_spread = spread_percent < 0.1,          # Tight spread
    good_volume = volume_ratio > 0.5,           # Decent volume
    medium_volatility = volatility > 0.002 && volatility < 0.02  # 0.2% to 2% volatility
  )
  
  # Determine signal
  if (all(unlist(conditions))) {
    # Simple momentum signal
    if (ticker_data$change_24h_pct > 0.1) {
      signal <- "BUY"
    } else if (ticker_data$change_24h_pct < -0.1) {
      signal <- "SELL"
    } else {
      signal <- "NONE"
    }
  } else {
    signal <- "NONE"
  }
  
  return(list(
    signal = signal,
    volatility = volatility,
    spread = spread_percent,
    volume_ratio = volume_ratio,
    conditions = conditions
  ))
}

#' Calculate appropriate scalping position size
calculate_scalping_size <- function(symbol, volatility) {
  # Get account balance
  balance_info <- check_available_balance(symbol, "buy", 1)
  available_balance <- balance_info$available_balance %||% 1000
  
  # Risk 1% of balance per scalp
  risk_amount <- available_balance * 0.01
  
  # Calculate size based on volatility
  ticker_data <- get_enhanced_ticker_data(symbol)
  current_price <- ticker_data$last_price
  
  # Higher volatility = smaller size
  volatility_factor <- 1 / (1 + volatility * 100)
  position_value <- risk_amount * volatility_factor
  
  size <- position_value / current_price
  
  # Round to appropriate precision
  asset_config <- get_asset_config(symbol)
  min_size <- asset_config$min_size %||% 1
  
  return(max(min_size, round(size, 2)))
}

#' Execute scalping trade with automatic TP/SL
execute_scalping_trade <- function(symbol, side, size, profit_target, stop_loss) {
  cat("🚀 Executing scalping trade:", side, size, symbol, "\n")
  
  # 1. Enter position
  entry_result <- execute_smart_order(symbol, side, size, execution_strategy = "market", dry_run = FALSE)
  
  if (!entry_result$success) {
    cat("❌ Failed to enter position\n")
    return(list(success = FALSE, error = "Entry failed"))
  }
  
  cat("✅ Position entered successfully\n")
  
  # 2. Set up TP/SL orders
  entry_price <- entry_result$execution_results$average_price
  
  if (side == "buy") {
    tp_price <- entry_price * (1 + profit_target / 100)
    sl_price <- entry_price * (1 - stop_loss / 100)
    exit_side <- "sell"
  } else {
    tp_price <- entry_price * (1 - profit_target / 100)
    sl_price <- entry_price * (1 + stop_loss / 100)
    exit_side <- "buy"
  }
  
  # Place TP order
  tp_result <- place_take_profit_order_api(symbol, exit_side, size, tp_price)
  sl_result <- place_stop_loss_order_api(symbol, exit_side, size, sl_price)
  
  cat("🎯 TP Order:", if (tp_result$success) "✅ Placed" else "❌ Failed", "\n")
  cat("🛡️ SL Order:", if (sl_result$success) "✅ Placed" else "❌ Failed", "\n")
  
  return(list(
    success = TRUE,
    entry_price = entry_price,
    tp_price = tp_price,
    sl_price = sl_price,
    tp_order_id = tp_result$order_id,
    sl_order_id = sl_result$order_id
  ))
}

# ==========================================================================================================
# 🎮 INTERACTIVE ORDER MANAGEMENT INTERFACE
# ==========================================================================================================

#' Launch interactive order management console
launch_order_management_console <- function() {
  cat("\n🎮 === INTERACTIVE ORDER MANAGEMENT CONSOLE === 🎮\n")
  cat("Available commands: help, create, track, cancel, auto, balance, quit\n")
  
  repeat {
    cat("\n[ORDER] > ")
    command <- trimws(tolower(readline()))
    
    if (command == "quit" || command == "q" || command == "exit") {
      cat("👋 Exiting order management console\n")
      break
    }
    
    process_order_command(command)
  }
}

#' Process order management commands
process_order_command <- function(command) {
  parts <- strsplit(command, " ")[[1]]
  cmd <- parts[1]
  args <- if (length(parts) > 1) parts[2:length(parts)] else character(0)
  
  switch(cmd,
    "help" = display_order_help(),
    "create" = interactive_order_creation(),
    "track" = track_all_orders(show_details = TRUE),
    "cancel" = interactive_order_cancellation(),
    "auto" = launch_auto_strategy(),
    "balance" = display_account_balance(),
    "validate" = interactive_order_validation(),
    "smart" = interactive_smart_order(),
    cat("❓ Unknown command. Type 'help' for available commands.\n")
  )
}

#' Display order management help
display_order_help <- function() {
  cat("\n📖 === ORDER MANAGEMENT COMMANDS === 📖\n")
  cat("🛠️ Order Creation:\n")
  cat("   create      - Interactive order creation wizard\n")
  cat("   smart       - Smart order execution with optimization\n")
  cat("   validate    - Validate order parameters\n")
  cat("\n📊 Order Monitoring:\n")
  cat("   track       - Track all orders and performance\n")
  cat("   cancel      - Cancel specific orders\n")
  cat("   balance     - Show account balance and margins\n")
  cat("\n🤖 Automation:\n")
  cat("   auto        - Launch automated trading strategies\n")
  cat("\n🎮 Navigation:\n")
  cat("   help        - Show this help menu\n")
  cat("   quit        - Exit order management console\n")
}

#' Interactive order creation wizard
interactive_order_creation <- function() {
  cat("\n🎯 === ORDER CREATION WIZARD === 🎯\n")
  
  # Get symbol
  cat("Available symbols:", paste(PORTFOLIO_ASSETS, collapse = ", "), "\n")
  symbol <- toupper(trimws(readline("Enter symbol: ")))
  
  if (!symbol %in% PORTFOLIO_ASSETS) {
    cat("❌ Invalid symbol\n")
    return()
  }
  
  # Get side
  side <- tolower(trimws(readline("Enter side (buy/sell): ")))
  if (!side %in% c("buy", "sell")) {
    cat("❌ Invalid side\n")
    return()
  }
  
  # Get size
  size <- as.numeric(trimws(readline("Enter size: ")))
  if (is.na(size) || size <= 0) {
    cat("❌ Invalid size\n")
    return()
  }
  
  # Get order type
  order_type <- tolower(trimws(readline("Order type (market/limit): ")))
  if (!order_type %in% c("market", "limit")) {
    order_type <- "market"
  }
  
  # Get price for limit orders
  price <- NULL
  if (order_type == "limit") {
    price <- as.numeric(trimws(readline("Enter limit price: ")))
    if (is.na(price) || price <= 0) {
      cat("❌ Invalid price\n")
      return()
    }
  }
  
  # Get execution mode
  cat("Execution modes: market, twap, iceberg, optimal\n")
  execution_strategy <- tolower(trimws(readline("Execution strategy (optimal): ")))
  if (execution_strategy == "") execution_strategy <- "optimal"
  
  # Confirm order
  cat("\n📋 Order Summary:\n")
  cat("   Symbol:", symbol, "\n")
  cat("   Side:", side, "\n")
  cat("   Size:", size, "\n")
  cat("   Type:", order_type, "\n")
  if (!is.null(price)) cat("   Price:", price, "\n")
  cat("   Strategy:", execution_strategy, "\n")
  
  confirm <- tolower(trimws(readline("Confirm order? (y/n): ")))
  
  if (confirm == "y" || confirm == "yes") {
    # Execute order
    result <- execute_smart_order(symbol, side, size, price, execution_strategy, dry_run = FALSE)
    
    if (result$success) {
      cat("✅ Order executed successfully!\n")
    } else {
      cat("❌ Order execution failed:", result$error, "\n")
    }
  } else {
    cat("❌ Order cancelled\n")
  }
}

#' Interactive smart order execution
interactive_smart_order <- function() {
  cat("\n🧠 === SMART ORDER EXECUTION === 🧠\n")
  cat("This will use AI-optimized execution strategies\n")
  
  # Symbol selection
  cat("Portfolio symbols:", paste(PORTFOLIO_ASSETS, collapse = ", "), "\n")
  symbol <- toupper(trimws(readline("Enter symbol: ")))
  
  # Quick order parameters
  side <- tolower(trimws(readline("Side (buy/sell): ")))
  amount_usd <- as.numeric(trimws(readline("Amount in USD: ")))
  
  if (is.na(amount_usd) || amount_usd <= 0) {
    cat("❌ Invalid amount\n")
    return()
  }
  
  # Calculate size
  ticker_data <- get_enhanced_ticker_data(symbol)
  size <- amount_usd / ticker_data$last_price
  
  cat("📊 Calculated size:", round(size, 4), "\n")
  cat("💰 Current price:", ticker_data$last_price, "\n")
  
  # Execute with optimal strategy
  result <- execute_smart_order(symbol, side, size, execution_strategy = "optimal", dry_run = FALSE)
  
  if (result$success) {
    cat("✅ Smart order completed!\n")
    cat("📊 Performance analysis:\n")
    if (!is.null(result$performance_analysis)) {
      cat("   Execution Quality:", result$performance_analysis$quality_score, "\n")
      cat("   Slippage:", round(result$performance_analysis$actual_slippage * 100, 4), "%\n")
    }
  } else {
    cat("❌ Smart order failed:", result$error, "\n")
  }
}

#' Display account balance and margin info
display_account_balance <- function() {
  cat("\n💰 === ACCOUNT BALANCE === 💰\n")
  
  tryCatch({
    balance_response <- bitget_request(
      method = "GET",
      endpoint = "/api/v2/mix/account/accounts",
      params = list(productType = "USDT-FUTURES")
    )
    
    if (!is.null(balance_response$data)) {
      for (account in balance_response$data) {
        if (account$marginCoin == "USDT") {
          cat("💵 USDT Account:\n")
          cat("   Available:", round(as.numeric(account$available), 2), "USDT\n")
          cat("   Total Equity:", round(as.numeric(account$equity), 2), "USDT\n")
          cat("   Used Margin:", round(as.numeric(account$locked), 2), "USDT\n")
          cat("   Unrealized PnL:", round(as.numeric(account$unrealizedPL), 2), "USDT\n")
          
          # Calculate utilization
          utilization <- (as.numeric(account$locked) / as.numeric(account$equity)) * 100
          cat("   Margin Utilization:", round(utilization, 1), "%\n")
          
          break
        }
      }
    } else {
      cat("❌ Could not fetch balance information\n")
    }
    
  }, error = function(e) {
    cat("❌ Error fetching balance:", e$message, "\n")
  })
}

# ==========================================================================================================
# 🎯 QUICK ACCESS FUNCTIONS
# ==========================================================================================================

#' Quick market buy for specified USD amount
quick_buy_usd <- function(symbol, usd_amount, dry_run = TRUE) {
  ticker_data <- get_enhanced_ticker_data(symbol)
  size <- usd_amount / ticker_data$last_price
  
  cat("💵 Quick Buy:", usd_amount, "USD of", symbol, "\n")
  cat("📊 Size:", round(size, 4), "at", ticker_data$last_price, "\n")
  
  return(execute_smart_order(symbol, "buy", size, execution_strategy = "optimal", dry_run = dry_run))
}

#' Quick market sell for specified coin amount
quick_sell_coins <- function(symbol, coin_amount, dry_run = TRUE) {
  ticker_data <- get_enhanced_ticker_data(symbol)
  usd_value <- coin_amount * ticker_data$last_price
  
  cat("🪙 Quick Sell:", coin_amount, symbol, "\n")
  cat("💵 Value:", round(usd_value, 2), "USD at", ticker_data$last_price, "\n")
  
  return(execute_smart_order(symbol, "sell", coin_amount, execution_strategy = "optimal", dry_run = dry_run))
}

#' One-click position protection
protect_position <- function(symbol, tp_percent = NULL, sl_percent = NULL) {
  positions <- get_current_positions()
  position <- positions[positions$symbol == symbol, ]
  
  if (nrow(position) == 0) {
    cat("❌ No position found for", symbol, "\n")
    return(FALSE)
  }
  
  # Use default percentages if not provided
  asset_config <- get_asset_config(symbol)
  tp_percent <- tp_percent %||% asset_config$default_tp_percent %||% 2.0
  sl_percent <- sl_percent %||% asset_config$default_sl_percent %||% 1.5
  
  cat("🛡️ Protecting position for", symbol, "\n")
  cat("🎯 TP:", tp_percent, "% | 🛡️ SL:", sl_percent, "%\n")
  
  # Place TP and SL orders
  tp_result <- place_tp_simple(symbol, tp_percent, dry_run = FALSE)
  sl_result <- place_sl_simple(symbol, sl_percent, dry_run = FALSE)
  
  cat("Results: TP", if (tp_result) "✅" else "❌", "| SL", if (sl_result) "✅" else "❌", "\n")
  
  return(tp_result && sl_result)
}

cat("✅ ADVANCED BITGET ORDER MANAGEMENT SYSTEM V2 LOADED!\n")
cat("🎯 Smart order execution with optimization\n") 
cat("📊 Comprehensive order tracking and monitoring\n")
cat("🤖 Automated trading strategies\n")
cat("🎮 Interactive order management console\n")
cat("🛡️ Advanced validation and risk management\n")
cat("\n💡 Quick Start Commands:\n")
cat("   launch_order_management_console()     # Interactive console\n")
cat("   execute_smart_order(symbol, side, size) # Smart execution\n")
cat("   track_all_orders()                    # Monitor all orders\n") 
cat("   quick_buy_usd(symbol, amount)         # Quick USD purchase\n")
cat("   protect_position(symbol)              # Add TP/SL protection\n")
cat("   auto_scalping_strategy(symbol)        # Auto scalping\n")