# ==========================================================================================================
# 🔔 ADVANCED NOTIFICATIONS SYSTEM V1
# ==========================================================================================================
# Pfad: utils/notifications.r
# Multi-Channel-Benachrichtigungssystem für Trading-Alerts
# Email, Telegram, Discord, Sound-Alerts, Log-System
# ==========================================================================================================

cat("🔔 Loading Advanced Notifications System V1...\n")

# ==========================================================================================================
# 🔧 DEPENDENCIES & CONFIGURATION
# ==========================================================================================================

# Load required packages
required_packages <- c("httr", "jsonlite")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load system config
if (!exists("SYSTEM_CONFIG_LOADED")) {
  source("../core/config.r")
  SYSTEM_CONFIG_LOADED <- TRUE
}

# ==========================================================================================================
# ⚙️ NOTIFICATION CONFIGURATION
# ==========================================================================================================

NOTIFICATION_CONFIG <- list(
  # Channels configuration
  channels = list(
    console = list(enabled = TRUE, priority = "all"),
    log_file = list(enabled = TRUE, priority = "all", 
                   path = file.path(SYSTEM_PATHS$logs, "notifications.log")),
    sound = list(enabled = TRUE, priority = c("high", "critical")),
    telegram = list(enabled = FALSE, priority = c("medium", "high", "critical")),
    discord = list(enabled = FALSE, priority = c("high", "critical")),
    email = list(enabled = FALSE, priority = c("critical"))
  ),
  
  # Alert priorities
  priorities = list(
    critical = list(color = "red", icon = "🚨", sound = "critical.wav"),
    high = list(color = "orange", icon = "⚠️", sound = "alert.wav"),
    medium = list(color = "yellow", icon = "📢", sound = "notice.wav"),
    low = list(color = "blue", icon = "ℹ️", sound = NULL),
    info = list(color = "green", icon = "✅", sound = NULL)
  ),
  
  # Alert categories
  categories = list(
    trade = list(icon = "💰", enabled = TRUE),
    risk = list(icon = "🛡️", enabled = TRUE),
    opportunity = list(icon = "🎯", enabled = TRUE),
    system = list(icon = "🔧", enabled = TRUE),
    performance = list(icon = "📊", enabled = TRUE),
    error = list(icon = "❌", enabled = TRUE)
  ),
  
  # Rate limiting
  rate_limits = list(
    max_per_minute = 10,
    max_per_hour = 100,
    cooldown_seconds = 60
  ),
  
  # Message formatting
  formatting = list(
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    max_message_length = 1000,
    include_system_info = TRUE
  )
)

# ==========================================================================================================
# 📨 CORE NOTIFICATION ENGINE
# ==========================================================================================================

#' Send notification through all configured channels
send_notification <- function(message, 
                            priority = "medium", 
                            category = "system",
                            title = NULL,
                            data = NULL,
                            force = FALSE) {
  
  # Check rate limits unless forced
  if (!force && !check_rate_limits()) {
    return(FALSE)
  }
  
  # Create notification object
  notification <- create_notification_object(
    message, priority, category, title, data
  )
  
  # Store in history
  store_notification(notification)
  
  # Send through appropriate channels
  channels_used <- list()
  
  # Console output
  if (should_send_to_channel("console", priority)) {
    send_console_notification(notification)
    channels_used$console <- TRUE
  }
  
  # Log file
  if (should_send_to_channel("log_file", priority)) {
    send_log_notification(notification)
    channels_used$log_file <- TRUE
  }
  
  # Sound alert
  if (should_send_to_channel("sound", priority)) {
    send_sound_notification(notification)
    channels_used$sound <- TRUE
  }
  
  # Telegram
  if (should_send_to_channel("telegram", priority)) {
    send_telegram_notification(notification)
    channels_used$telegram <- TRUE
  }
  
  # Discord
  if (should_send_to_channel("discord", priority)) {
    send_discord_notification(notification)
    channels_used$discord <- TRUE
  }
  
  # Email
  if (should_send_to_channel("email", priority)) {
    send_email_notification(notification)
    channels_used$email <- TRUE
  }
  
  # Update rate limit counters
  update_rate_limit_counters()
  
  return(channels_used)
}

#' Create notification object
create_notification_object <- function(message, priority, category, title, data) {
  
  notification <- list(
    id = generate_notification_id(),
    timestamp = Sys.time(),
    priority = priority,
    category = category,
    title = title %||% paste(toupper(category), "ALERT"),
    message = message,
    data = data,
    system_info = if (NOTIFICATION_CONFIG$formatting$include_system_info) {
      get_system_info()
    } else NULL
  )
  
  return(notification)
}

#' Check if should send to specific channel
should_send_to_channel <- function(channel, priority) {
  
  channel_config <- NOTIFICATION_CONFIG$channels[[channel]]
  
  # Check if channel is enabled
  if (!channel_config$enabled) return(FALSE)
  
  # Check priority
  if ("all" %in% channel_config$priority) return(TRUE)
  
  return(priority %in% channel_config$priority)
}

# ==========================================================================================================
# 📢 CHANNEL IMPLEMENTATIONS
# ==========================================================================================================

#' Send console notification
send_console_notification <- function(notification) {
  
  # Get formatting
  priority_info <- NOTIFICATION_CONFIG$priorities[[notification$priority]]
  category_info <- NOTIFICATION_CONFIG$categories[[notification$category]]
  
  # Format timestamp
  timestamp <- format(notification$timestamp, NOTIFICATION_CONFIG$formatting$timestamp_format)
  
  # Build console message
  cat("\n")
  cat(priority_info$icon, category_info$icon, " [", timestamp, "] ", sep = "")
  cat(notification$title, "\n", sep = "")
  
  # Message with proper formatting
  cat("  ", notification$message, "\n", sep = "")
  
  # Additional data if present
  if (!is.null(notification$data)) {
    for (key in names(notification$data)) {
      cat("  ", key, ": ", notification$data[[key]], "\n", sep = "")
    }
  }
  
  cat("\n")
}

#' Send log file notification
send_log_notification <- function(notification) {
  
  tryCatch({
    # Ensure log directory exists
    log_dir <- dirname(NOTIFICATION_CONFIG$channels$log_file$path)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }
    
    # Format log entry
    log_entry <- sprintf(
      "[%s] %s | %s | %s | %s",
      format(notification$timestamp, "%Y-%m-%d %H:%M:%S"),
      toupper(notification$priority),
      toupper(notification$category),
      notification$title,
      notification$message
    )
    
    # Add data if present
    if (!is.null(notification$data)) {
      data_str <- paste(names(notification$data), notification$data, 
                       sep = "=", collapse = ", ")
      log_entry <- paste(log_entry, "| DATA:", data_str)
    }
    
    # Write to log
    cat(log_entry, "\n", 
        file = NOTIFICATION_CONFIG$channels$log_file$path, 
        append = TRUE)
    
  }, error = function(e) {
    cat("Warning: Could not write to log file\n")
  })
}

#' Send sound notification
send_sound_notification <- function(notification) {
  
  priority_info <- NOTIFICATION_CONFIG$priorities[[notification$priority]]
  
  if (!is.null(priority_info$sound)) {
    play_alert_sound(priority_info$sound)
  }
}

#' Play alert sound
play_alert_sound <- function(sound_file) {
  
  # Platform-specific sound playing
  os_type <- Sys.info()["sysname"]
  
  tryCatch({
    if (os_type == "Windows") {
      # Windows beep
      shell("rundll32 user32.dll,MessageBeep -1", wait = FALSE)
    } else if (os_type == "Darwin") {
      # macOS
      system("afplay /System/Library/Sounds/Glass.aiff", wait = FALSE)
    } else {
      # Linux
      system("beep", wait = FALSE)
    }
  }, error = function(e) {
    # Silent fail - sound is not critical
  })
}

#' Send Telegram notification
send_telegram_notification <- function(notification) {
  
  # Check if Telegram is configured
  if (!exists("TELEGRAM_BOT_TOKEN") || !exists("TELEGRAM_CHAT_ID")) {
    return(FALSE)
  }
  
  tryCatch({
    # Format message for Telegram
    telegram_message <- format_telegram_message(notification)
    
    # Telegram API endpoint
    url <- paste0("https://api.telegram.org/bot", TELEGRAM_BOT_TOKEN, "/sendMessage")
    
    # Send request
    response <- POST(
      url,
      body = list(
        chat_id = TELEGRAM_CHAT_ID,
        text = telegram_message,
        parse_mode = "HTML"
      ),
      encode = "json"
    )
    
    return(status_code(response) == 200)
    
  }, error = function(e) {
    return(FALSE)
  })
}

#' Format Telegram message
format_telegram_message <- function(notification) {
  
  priority_info <- NOTIFICATION_CONFIG$priorities[[notification$priority]]
  category_info <- NOTIFICATION_CONFIG$categories[[notification$category]]
  
  # Build HTML message
  message <- paste0(
    priority_info$icon, " <b>", notification$title, "</b>\n",
    category_info$icon, " ", toupper(notification$category), "\n\n",
    notification$message
  )
  
  # Add data if present
  if (!is.null(notification$data)) {
    message <- paste0(message, "\n\n")
    for (key in names(notification$data)) {
      message <- paste0(message, "<b>", key, ":</b> ", 
                       notification$data[[key]], "\n")
    }
  }
  
  # Add timestamp
  message <- paste0(
    message, "\n\n<i>",
    format(notification$timestamp, "%H:%M:%S"),
    "</i>"
  )
  
  return(message)
}

#' Send Discord notification
send_discord_notification <- function(notification) {
  
  # Check if Discord webhook is configured
  if (!exists("DISCORD_WEBHOOK_URL")) {
    return(FALSE)
  }
  
  tryCatch({
    # Create Discord embed
    embed <- create_discord_embed(notification)
    
    # Send webhook
    response <- POST(
      DISCORD_WEBHOOK_URL,
      body = list(embeds = list(embed)),
      encode = "json",
      content_type_json()
    )
    
    return(status_code(response) == 204)
    
  }, error = function(e) {
    return(FALSE)
  })
}

#' Create Discord embed
create_discord_embed <- function(notification) {
  
  priority_info <- NOTIFICATION_CONFIG$priorities[[notification$priority]]
  
  # Color mapping
  color_map <- list(
    red = 15158332,
    orange = 15105570,
    yellow = 15844367,
    blue = 3447003,
    green = 3066993
  )
  
  embed <- list(
    title = paste(priority_info$icon, notification$title),
    description = notification$message,
    color = color_map[[priority_info$color]] %||% 0,
    timestamp = format(notification$timestamp, "%Y-%m-%dT%H:%M:%SZ"),
    footer = list(
      text = paste("Trading Bot -", toupper(notification$category))
    )
  )
  
  # Add fields for data
  if (!is.null(notification$data)) {
    embed$fields <- lapply(names(notification$data), function(key) {
      list(
        name = key,
        value = as.character(notification$data[[key]]),
        inline = TRUE
      )
    })
  }
  
  return(embed)
}

#' Send email notification
send_email_notification <- function(notification) {
  
  # This is a placeholder - actual implementation would use
  # an email service like SendGrid, AWS SES, or SMTP
  
  cat("Email notification would be sent for:", 
      notification$title, "\n")
  
  return(FALSE)
}

# ==========================================================================================================
# 🎯 SPECIALIZED ALERT FUNCTIONS
# ==========================================================================================================

#' Send trade alert
send_trade_alert <- function(action, symbol, side, size, price, 
                           pnl = NULL, priority = "medium") {
  
  message <- sprintf(
    "%s %s: %s %s at %s",
    toupper(action), symbol, side, 
    format(size, big.mark = ","),
    format(price, big.mark = ",", digits = 4)
  )
  
  data <- list(
    symbol = symbol,
    side = side,
    size = size,
    price = price
  )
  
  if (!is.null(pnl)) {
    data$pnl <- sprintf("%+.2f", pnl)
    message <- paste(message, sprintf("(P&L: %+.2f)", pnl))
  }
  
  send_notification(
    message = message,
    priority = priority,
    category = "trade",
    title = paste("Trade", action),
    data = data
  )
}

#' Send risk alert
send_risk_alert <- function(risk_type, message, data = NULL, priority = "high") {
  
  title <- switch(risk_type,
    "drawdown" = "DRAWDOWN ALERT",
    "margin" = "MARGIN WARNING",
    "exposure" = "EXPOSURE LIMIT",
    "loss" = "LOSS ALERT",
    "RISK ALERT"
  )
  
  send_notification(
    message = message,
    priority = priority,
    category = "risk",
    title = title,
    data = data
  )
}

#' Send opportunity alert
send_opportunity_alert <- function(symbol, opportunity_type, details, priority = "medium") {
  
  message <- sprintf(
    "%s opportunity detected: %s",
    symbol, opportunity_type
  )
  
  send_notification(
    message = message,
    priority = priority,
    category = "opportunity",
    title = "TRADING OPPORTUNITY",
    data = c(list(symbol = symbol, type = opportunity_type), details)
  )
}

#' Send system alert
send_system_alert <- function(system_event, message, priority = "low") {
  
  send_notification(
    message = message,
    priority = priority,
    category = "system",
    title = paste("SYSTEM:", toupper(system_event))
  )
}

#' Send performance alert
send_performance_alert <- function(metric, value, threshold, direction = "above") {
  
  message <- sprintf(
    "%s is %s threshold: %.2f (threshold: %.2f)",
    metric, direction, value, threshold
  )
  
  priority <- if (abs(value - threshold) / threshold > 0.2) "high" else "medium"
  
  send_notification(
    message = message,
    priority = priority,
    category = "performance",
    title = "PERFORMANCE ALERT",
    data = list(
      metric = metric,
      value = value,
      threshold = threshold,
      deviation = sprintf("%+.1f%%", (value - threshold) / threshold * 100)
    )
  )
}

#' Send error alert
send_error_alert <- function(error_message, function_name = NULL, additional_info = NULL) {
  
  data <- list(error = error_message)
  
  if (!is.null(function_name)) {
    data$function <- function_name
  }
  
  if (!is.null(additional_info)) {
    data <- c(data, additional_info)
  }
  
  send_notification(
    message = error_message,
    priority = "high",
    category = "error",
    title = "ERROR ALERT",
    data = data,
    force = TRUE  # Bypass rate limits for errors
  )
}

# ==========================================================================================================
# 📊 NOTIFICATION MANAGEMENT
# ==========================================================================================================

#' Initialize notification system
initialize_notifications <- function() {
  
  cat("\n🔔 Initializing notification system...\n")
  
  # Create notification history
  if (!exists("NOTIFICATION_HISTORY")) {
    NOTIFICATION_HISTORY <<- list()
  }
  
  # Initialize rate limit counters
  if (!exists("NOTIFICATION_COUNTERS")) {
    NOTIFICATION_COUNTERS <<- list(
      minute_count = 0,
      hour_count = 0,
      last_reset_minute = Sys.time(),
      last_reset_hour = Sys.time()
    )
  }
  
  # Test channels
  test_notification_channels()
  
  cat("✅ Notification system ready\n")
}

#' Test notification channels
test_notification_channels <- function() {
  
  cat("\nTesting notification channels:\n")
  
  for (channel in names(NOTIFICATION_CONFIG$channels)) {
    if (NOTIFICATION_CONFIG$channels[[channel]]$enabled) {
      cat("  •", channel, "... ")
      
      # Test based on channel type
      result <- switch(channel,
        "console" = TRUE,
        "log_file" = test_log_file_access(),
        "sound" = TRUE,
        "telegram" = exists("TELEGRAM_BOT_TOKEN"),
        "discord" = exists("DISCORD_WEBHOOK_URL"),
        "email" = FALSE
      )
      
      cat(if (result) "✅\n" else "❌\n")
    }
  }
}

#' Test log file access
test_log_file_access <- function() {
  tryCatch({
    log_path <- NOTIFICATION_CONFIG$channels$log_file$path
    log_dir <- dirname(log_path)
    
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE)
    }
    
    # Test write
    cat("# Notification log initialized\n", file = log_path, append = TRUE)
    
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Check rate limits
check_rate_limits <- function() {
  
  current_time <- Sys.time()
  
  # Reset minute counter if needed
  if (difftime(current_time, NOTIFICATION_COUNTERS$last_reset_minute, units = "mins") >= 1) {
    NOTIFICATION_COUNTERS$minute_count <<- 0
    NOTIFICATION_COUNTERS$last_reset_minute <<- current_time
  }
  
  # Reset hour counter if needed
  if (difftime(current_time, NOTIFICATION_COUNTERS$last_reset_hour, units = "hours") >= 1) {
    NOTIFICATION_COUNTERS$hour_count <<- 0
    NOTIFICATION_COUNTERS$last_reset_hour <<- current_time
  }
  
  # Check limits
  if (NOTIFICATION_COUNTERS$minute_count >= NOTIFICATION_CONFIG$rate_limits$max_per_minute) {
    return(FALSE)
  }
  
  if (NOTIFICATION_COUNTERS$hour_count >= NOTIFICATION_CONFIG$rate_limits$max_per_hour) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Update rate limit counters
update_rate_limit_counters <- function() {
  NOTIFICATION_COUNTERS$minute_count <<- NOTIFICATION_COUNTERS$minute_count + 1
  NOTIFICATION_COUNTERS$hour_count <<- NOTIFICATION_COUNTERS$hour_count + 1
}

#' Store notification in history
store_notification <- function(notification) {
  
  NOTIFICATION_HISTORY[[length(NOTIFICATION_HISTORY) + 1]] <<- notification
  
  # Maintain history limit
  if (length(NOTIFICATION_HISTORY) > 1000) {
    NOTIFICATION_HISTORY <<- tail(NOTIFICATION_HISTORY, 1000)
  }
}

#' Get notification history
get_notification_history <- function(n = 50, category = NULL, priority = NULL) {
  
  history <- NOTIFICATION_HISTORY
  
  # Filter by category
  if (!is.null(category)) {
    history <- Filter(function(x) x$category == category, history)
  }
  
  # Filter by priority
  if (!is.null(priority)) {
    history <- Filter(function(x) x$priority == priority, history)
  }
  
  # Return last n
  return(tail(history, n))
}

#' Display notification summary
display_notification_summary <- function() {
  
  cat("\n📊 === NOTIFICATION SUMMARY === 📊\n")
  
  if (length(NOTIFICATION_HISTORY) == 0) {
    cat("No notifications sent yet\n")
    return()
  }
  
  # Count by category
  categories <- table(sapply(NOTIFICATION_HISTORY, function(x) x$category))
  cat("\nBy Category:\n")
  for (cat in names(categories)) {
    cat("  •", cat, ":", categories[cat], "\n")
  }
  
  # Count by priority
  priorities <- table(sapply(NOTIFICATION_HISTORY, function(x) x$priority))
  cat("\nBy Priority:\n")
  for (pri in names(priorities)) {
    cat("  •", pri, ":", priorities[pri], "\n")
  }
  
  # Recent alerts
  recent <- tail(NOTIFICATION_HISTORY, 5)
  cat("\nRecent Notifications:\n")
  for (notif in recent) {
    cat(sprintf("  [%s] %s - %s\n",
                format(notif$timestamp, "%H:%M:%S"),
                notif$title,
                substr(notif$message, 1, 50)))
  }
  
  # Rate limit status
  cat("\nRate Limits:\n")
  cat("  • Per minute:", NOTIFICATION_COUNTERS$minute_count, "/",
      NOTIFICATION_CONFIG$rate_limits$max_per_minute, "\n")
  cat("  • Per hour:", NOTIFICATION_COUNTERS$hour_count, "/",
      NOTIFICATION_CONFIG$rate_limits$max_per_hour, "\n")
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

#' Generate notification ID
generate_notification_id <- function() {
  paste0("NOTIF_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
         sample(1000:9999, 1))
}

#' Get system info for notifications
get_system_info <- function() {
  
  # Get basic system info
  info <- list()
  
  # Account info if available
  if (exists("get_account_balance")) {
    balance <- get_account_balance()
    if (!is.null(balance)) {
      info$equity <- round(balance$equity, 2)
      info$available <- round(balance$available, 2)
    }
  }
  
  # Position count if available
  if (exists("get_current_positions")) {
    positions <- get_current_positions()
    info$open_positions <- nrow(positions)
  }
  
  return(info)
}

#' Configure notification channel
configure_notification_channel <- function(channel, enabled = NULL, priority = NULL) {
  
  if (!channel %in% names(NOTIFICATION_CONFIG$channels)) {
    cat("Unknown channel:", channel, "\n")
    return(FALSE)
  }
  
  if (!is.null(enabled)) {
    NOTIFICATION_CONFIG$channels[[channel]]$enabled <<- enabled
  }
  
  if (!is.null(priority)) {
    NOTIFICATION_CONFIG$channels[[channel]]$priority <<- priority
  }
  
  cat("Channel", channel, "configured:\n")
  cat("  • Enabled:", NOTIFICATION_CONFIG$channels[[channel]]$enabled, "\n")
  cat("  • Priority:", paste(NOTIFICATION_CONFIG$channels[[channel]]$priority, collapse = ", "), "\n")
  
  return(TRUE)
}

#' Set notification credentials
set_notification_credentials <- function(service, ...) {
  
  credentials <- list(...)
  
  if (service == "telegram") {
    if ("bot_token" %in% names(credentials)) {
      TELEGRAM_BOT_TOKEN <<- credentials$bot_token
    }
    if ("chat_id" %in% names(credentials)) {
      TELEGRAM_CHAT_ID <<- credentials$chat_id
    }
    cat("Telegram credentials set\n")
    
  } else if (service == "discord") {
    if ("webhook_url" %in% names(credentials)) {
      DISCORD_WEBHOOK_URL <<- credentials$webhook_url
    }
    cat("Discord webhook set\n")
    
  } else {
    cat("Unknown service:", service, "\n")
    return(FALSE)
  }
  
  return(TRUE)
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Initialize on load
initialize_notifications()

# Create notification interface
NOTIFICATIONS <- list(
  send = send_notification,
  
  # Specialized alerts
  trade = send_trade_alert,
  risk = send_risk_alert,
  opportunity = send_opportunity_alert,
  system = send_system_alert,
  performance = send_performance_alert,
  error = send_error_alert,
  
  # Management
  history = get_notification_history,
  summary = display_notification_summary,
  configure = configure_notification_channel,
  set_credentials = set_notification_credentials,
  
  # Direct channel access
  console = function(...) send_notification(..., force = TRUE),
  log = function(msg) send_log_notification(create_notification_object(msg, "info", "system", NULL, NULL))
)

cat("✅ NOTIFICATIONS.R loaded successfully!\n")
cat("🔔 Multi-channel notification system ready\n")
cat("📢 Alert categories configured\n")
cat("⚡ Rate limiting active\n")
cat("📊 Notification tracking enabled\n")