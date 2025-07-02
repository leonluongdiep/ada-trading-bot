# ==========================================================================================================
# 📝 ENHANCED R CONSOLE OUTPUT MANAGEMENT V2 - PERFORMANCE OPTIMIZED
# ==========================================================================================================
# 
# PERFORMANCE ENHANCEMENTS:
# ✅ Structured Logging Levels (DEBUG/INFO/WARN/ERROR)
# ✅ Smart Output Routing (Console + File optimiert)
# ✅ Interactive Modes (SILENT/NORMAL/VERBOSE/DEBUG)
# ✅ UTF-8 Encoding fixes integriert
# ✅ Performance Monitoring für Trading System
# ✅ Integration mit optimized trading system
# 
# VERSION: V2 Enhanced & Performance Optimized
# INTEGRATION: Seamless mit complete_trading_analysis_v4.r
# COMPATIBILITY: ✅ Backward compatible mit V1
# 
# ==========================================================================================================

cat("📝 Loading Enhanced Console Output Management V2...\n")

# ==========================================================================================================
# 🔧 SECTION 1: ENHANCED CONFIGURATION & ENCODING
# ==========================================================================================================

# ENHANCED GLOBAL STATE mit Performance-Tracking
ENHANCED_CONSOLE_STATE <- list(
  original_stdout = NULL,
  original_stderr = NULL,
  log_file_path = NULL,
  is_redirected = FALSE,
  start_time = NULL,
  active_mode = NULL,
  performance_tracking = TRUE,
  message_count = 0,
  error_count = 0,
  warning_count = 0
)

# ENHANCED LOG LEVELS mit Emojis und Farben
LOG_LEVELS <- list(
  DEBUG = list(level = 1, emoji = "🔍", prefix = "DEBUG"),
  INFO = list(level = 2, emoji = "ℹ️", prefix = "INFO"),
  WARN = list(level = 3, emoji = "⚠️", prefix = "WARN"),
  ERROR = list(level = 4, emoji = "❌", prefix = "ERROR"),
  SUCCESS = list(level = 2, emoji = "✅", prefix = "SUCCESS"),
  PERFORMANCE = list(level = 2, emoji = "⚡", prefix = "PERF")
)

# Enhanced Default Directories
ENHANCED_LOG_DIR <- "c:/freeding/tbot202506/logs/enhanced/"
PERFORMANCE_LOG_DIR <- "c:/freeding/tbot202506/logs/performance/"

# Directory Setup mit UTF-8 Support
enhanced_setup_directories <- function() {
  tryCatch({
    # Ensure UTF-8 locale
    Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
    
    # Create directories
    dir.create(ENHANCED_LOG_DIR, recursive = TRUE, showWarnings = FALSE)
    dir.create(PERFORMANCE_LOG_DIR, recursive = TRUE, showWarnings = FALSE)
    
    cat("📁 Enhanced log directories ready\n")
    cat("   Main logs:", ENHANCED_LOG_DIR, "\n")
    cat("   Performance logs:", PERFORMANCE_LOG_DIR, "\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("⚠️ Directory setup warning:", e$message, "\n")
    ENHANCED_LOG_DIR <<- tempdir()
    PERFORMANCE_LOG_DIR <<- tempdir()
    return(FALSE)
  })
}

# ==========================================================================================================
# 📊 SECTION 2: ENHANCED LOGGING FUNCTIONS
# ==========================================================================================================

# SMART LOG MESSAGE FUNCTION mit Performance-Tracking
enhanced_log_message <- function(level = "INFO", message, ..., 
                                 show_timestamp = TRUE, 
                                 show_emoji = TRUE,
                                 file_only = FALSE,
                                 performance_data = NULL) {
  
  # Validate level
  if (!level %in% names(LOG_LEVELS)) {
    level <- "INFO"
  }
  
  level_info <- LOG_LEVELS[[level]]
  
  # Format message
  formatted_message <- sprintf(message, ...)
  
  # Add timestamp if requested
  timestamp_str <- ""
  if (show_timestamp) {
    timestamp_str <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ")
  }
  
  # Add emoji if requested
  emoji_str <- ""
  if (show_emoji) {
    emoji_str <- paste0(level_info$emoji, " ")
  }
  
  # Combine message
  final_message <- paste0(timestamp_str, emoji_str, level_info$prefix, ": ", formatted_message)
  
  # Performance tracking
  if (ENHANCED_CONSOLE_STATE$performance_tracking) {
    ENHANCED_CONSOLE_STATE$message_count <<- ENHANCED_CONSOLE_STATE$message_count + 1
    if (level == "ERROR") {
      ENHANCED_CONSOLE_STATE$error_count <<- ENHANCED_CONSOLE_STATE$error_count + 1
    }
    if (level == "WARN") {
      ENHANCED_CONSOLE_STATE$warning_count <<- ENHANCED_CONSOLE_STATE$warning_count + 1
    }
  }
  
  # Console output (unless file_only or in silent mode)
  if (!file_only && ENHANCED_CONSOLE_STATE$active_mode != "SILENT") {
    if (ENHANCED_CONSOLE_STATE$active_mode == "VERBOSE" || level_info$level >= 2) {
      cat(final_message, "\n")
    }
  }
  
  # File output (always if log file exists)
  if (!is.null(ENHANCED_CONSOLE_STATE$log_file_path)) {
    # Add performance data if provided
    if (!is.null(performance_data)) {
      perf_str <- sprintf(" [Duration: %.3fs, Memory: %.1fMB]", 
                         performance_data$duration, 
                         performance_data$memory_mb)
      final_message <- paste0(final_message, perf_str)
    }
    
    write(final_message, file = ENHANCED_CONSOLE_STATE$log_file_path, append = TRUE)
  }
  
  invisible(TRUE)
}

# CONVENIENCE LOGGING FUNCTIONS
log_debug <- function(message, ...) {
  enhanced_log_message("DEBUG", message, ...)
}

log_info <- function(message, ...) {
  enhanced_log_message("INFO", message, ...)
}

log_warn <- function(message, ...) {
  enhanced_log_message("WARN", message, ...)
}

log_error <- function(message, ...) {
  enhanced_log_message("ERROR", message, ...)
}

log_success <- function(message, ...) {
  enhanced_log_message("SUCCESS", message, ...)
}

log_performance <- function(message, duration, memory_mb = NULL, ...) {
  perf_data <- list(duration = duration, memory_mb = memory_mb %||% 0)
  enhanced_log_message("PERFORMANCE", message, ..., performance_data = perf_data)
}

# ==========================================================================================================
# 📝 SECTION 3: ENHANCED FILE REDIRECTION
# ==========================================================================================================

# ENHANCED CONSOLE REDIRECTION mit Performance-Tracking
enhanced_redirect_console <- function(log_filename = NULL, 
                                     log_dir = ENHANCED_LOG_DIR,
                                     include_timestamp = TRUE,
                                     show_summary = TRUE,
                                     mode = "NORMAL") {
  
  if (ENHANCED_CONSOLE_STATE$is_redirected) {
    enhanced_log_message("WARN", "Console already redirected. Use enhanced_restore_console() first.")
    return(FALSE)
  }
  
  # Generate log filename
  if (is.null(log_filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_filename <- paste0("enhanced_trading_", timestamp, ".log")
  }
  
  # Full log path
  log_path <- file.path(log_dir, log_filename)
  
  tryCatch({
    # Save original connections
    ENHANCED_CONSOLE_STATE$original_stdout <<- stdout()
    ENHANCED_CONSOLE_STATE$original_stderr <<- stderr()
    ENHANCED_CONSOLE_STATE$log_file_path <<- log_path
    ENHANCED_CONSOLE_STATE$start_time <<- Sys.time()
    ENHANCED_CONSOLE_STATE$active_mode <<- mode
    
    # Open log file with UTF-8 encoding
    log_con <- file(log_path, open = "w", encoding = "UTF-8")
    
    # Redirect based on mode
    if (mode == "SILENT") {
      sink(log_con, type = "output")
      sink(log_con, type = "message")
    } else {
      # Partial redirection - keep important messages in console
      ENHANCED_CONSOLE_STATE$log_file_path <<- log_path
    }
    
    ENHANCED_CONSOLE_STATE$is_redirected <<- TRUE
    
    # Write header to log file
    if (include_timestamp) {
      header <- c(
        "# =====================================",
        "# ENHANCED TRADING SYSTEM LOG V2",
        "# =====================================",
        paste("# Start Time:", as.character(Sys.time())),
        paste("# Mode:", mode),
        paste("# Log File:", log_path),
        "# =====================================",
        ""
      )
      writeLines(header, log_con, useBytes = TRUE)
    }
    
    # Close connection for append mode
    close(log_con)
    
    if (show_summary) {
      enhanced_log_message("SUCCESS", "Enhanced console redirection active")
      enhanced_log_message("INFO", "Mode: %s | File: %s", mode, basename(log_path))
      enhanced_log_message("INFO", "Use enhanced_restore_console() to return to normal")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    enhanced_log_message("ERROR", "Console redirection failed: %s", e$message)
    return(FALSE)
  })
}

# ENHANCED CONSOLE RESTORATION mit Performance Report
enhanced_restore_console <- function(show_summary = TRUE, show_performance = TRUE) {
  
  if (!ENHANCED_CONSOLE_STATE$is_redirected) {
    enhanced_log_message("WARN", "Console not currently redirected")
    return(FALSE)
  }
  
  tryCatch({
    # Calculate session statistics
    session_duration <- NULL
    if (!is.null(ENHANCED_CONSOLE_STATE$start_time)) {
      session_duration <- difftime(Sys.time(), ENHANCED_CONSOLE_STATE$start_time, units = "mins")
    }
    
    # Write session summary to log
    if (!is.null(ENHANCED_CONSOLE_STATE$log_file_path)) {
      summary_lines <- c(
        "",
        "# =====================================",
        "# SESSION SUMMARY",
        "# =====================================",
        paste("# End Time:", as.character(Sys.time())),
        paste("# Duration:", round(as.numeric(session_duration), 2), "minutes"),
        paste("# Messages:", ENHANCED_CONSOLE_STATE$message_count),
        paste("# Warnings:", ENHANCED_CONSOLE_STATE$warning_count),
        paste("# Errors:", ENHANCED_CONSOLE_STATE$error_count),
        paste("# Mode:", ENHANCED_CONSOLE_STATE$active_mode),
        "# ====================================="
      )
      write(paste(summary_lines, collapse = "\n"), 
            file = ENHANCED_CONSOLE_STATE$log_file_path, append = TRUE)
    }
    
    # Restore sinks
    if (ENHANCED_CONSOLE_STATE$active_mode == "SILENT") {
      sink(type = "message")
      sink(type = "output")
    }
    
    # Reset state
    log_file <- ENHANCED_CONSOLE_STATE$log_file_path
    ENHANCED_CONSOLE_STATE$is_redirected <<- FALSE
    ENHANCED_CONSOLE_STATE$active_mode <<- NULL
    
    if (show_summary) {
      cat("✅ Enhanced console output restored!\n")
      cat("📄 Log saved to:", basename(log_file), "\n")
      
      if (file.exists(log_file)) {
        file_size <- file.size(log_file)
        cat("📊 Log file size:", format(file_size, units = "KB"), "\n")
      }
      
      if (show_performance && !is.null(session_duration)) {
        cat("⚡ Session performance:\n")
        cat("   Duration:", round(as.numeric(session_duration), 2), "minutes\n")
        cat("   Messages:", ENHANCED_CONSOLE_STATE$message_count, "\n")
        cat("   Warnings:", ENHANCED_CONSOLE_STATE$warning_count, "\n")
        cat("   Errors:", ENHANCED_CONSOLE_STATE$error_count, "\n")
      }
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error restoring console:", e$message, "\n")
    
    # Emergency restore
    tryCatch({
      sink(type = "message")
      sink(type = "output")
      ENHANCED_CONSOLE_STATE$is_redirected <<- FALSE
    }, error = function(e2) {
      cat("❌ Emergency restore failed\n")
    })
    
    return(FALSE)
  })
}

# ==========================================================================================================
# 🎯 SECTION 4: TRADING-SPECIFIC LOGGING FUNCTIONS
# ==========================================================================================================

# TRADING EXECUTION LOGGER mit Performance-Tracking
log_trading_execution <- function(function_name, symbol, start_time, result = NULL, error = NULL) {
  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (!is.null(error)) {
    log_error("Trading execution failed: %s(%s) - %s (%.3fs)", 
              function_name, symbol, error$message, duration)
  } else {
    log_success("Trading execution completed: %s(%s) (%.3fs)", 
                function_name, symbol, duration)
    
    # Log result details if provided
    if (!is.null(result) && is.list(result)) {
      if (!is.null(result$success) && result$success) {
        log_info("  Result: SUCCESS")
      }
      if (!is.null(result$order_id)) {
        log_info("  Order ID: %s", result$order_id)
      }
    }
  }
  
  # Performance logging to separate file
  if (duration > 2.0) {  # Log slow operations
    log_performance("Slow operation detected: %s(%s)", function_name, symbol, duration)
  }
}

# API PERFORMANCE LOGGER
log_api_performance <- function(endpoint, symbol, start_time, success = TRUE, cache_hit = FALSE) {
  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  cache_info <- if (cache_hit) " [CACHED]" else ""
  status <- if (success) "SUCCESS" else "FAILED"
  
  log_debug("API %s: %s(%s) - %s (%.3fs)%s", 
            status, endpoint, symbol, status, duration, cache_info)
  
  # Track API performance
  perf_file <- file.path(PERFORMANCE_LOG_DIR, "api_performance.csv")
  
  # Create performance log entry
  perf_entry <- data.frame(
    timestamp = Sys.time(),
    endpoint = endpoint,
    symbol = symbol,
    duration = duration,
    success = success,
    cache_hit = cache_hit,
    stringsAsFactors = FALSE
  )
  
  # Append to performance log
  if (file.exists(perf_file)) {
    write.table(perf_entry, perf_file, append = TRUE, sep = ",", 
                row.names = FALSE, col.names = FALSE)
  } else {
    write.csv(perf_entry, perf_file, row.names = FALSE)
  }
}

# MARKET ANALYSIS LOGGER
log_market_analysis <- function(symbol, analysis_type, duration, key_metrics = NULL) {
  log_info("Market analysis completed: %s (%s) (%.3fs)", 
           analysis_type, symbol, duration)
  
  if (!is.null(key_metrics) && is.list(key_metrics)) {
    for (metric_name in names(key_metrics)) {
      metric_value <- key_metrics[[metric_name]]
      if (is.numeric(metric_value)) {
        log_info("  %s: %.4f", metric_name, metric_value)
      } else {
        log_info("  %s: %s", metric_name, as.character(metric_value))
      }
    }
  }
}

# ==========================================================================================================
# 🔄 SECTION 5: ENHANCED SILENT EXECUTION
# ==========================================================================================================

# ENHANCED SILENT EXECUTION mit besserer Fehlerbehandlung
enhanced_silent_execute <- function(code_expr, 
                                   capture_output = FALSE,
                                   show_errors = TRUE,
                                   log_performance = TRUE) {
  
  start_time <- Sys.time()
  captured_output <- NULL
  result <- NULL
  
  if (capture_output) {
    # Output erfassen
    captured_output <- capture.output({
      result <- tryCatch({
        eval(code_expr)
      }, error = function(e) {
        if (show_errors) {
          enhanced_log_message("ERROR", "Silent execution error: %s", e$message)
        }
        return(NULL)
      })
    }, type = "output")
  } else {
    # Output unterdrücken
    result <- tryCatch({
      # Temporary file für Output
      temp_file <- tempfile()
      sink(temp_file)
      
      result_inner <- eval(code_expr)
      
      sink()  # Reset sink
      unlink(temp_file)  # Clean up
      
      return(result_inner)
      
    }, error = function(e) {
      sink()  # Ensure sink is reset
      if (show_errors) {
        enhanced_log_message("ERROR", "Silent execution error: %s", e$message)
      }
      return(NULL)
    })
  }
  
  # Performance logging
  if (log_performance) {
    duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (duration > 1.0) {
      enhanced_log_message("PERFORMANCE", "Silent execution took %.3fs", duration)
    }
  }
  
  if (capture_output) {
    return(list(
      result = result,
      output = captured_output,
      success = !is.null(result),
      duration = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    ))
  } else {
    return(result)
  }
}

# ENHANCED TRADING SCRIPTS EXECUTION
enhanced_execute_trading_scripts <- function(script_list = NULL,
                                            show_progress = TRUE,
                                            log_results = TRUE,
                                            mode = "ENHANCED") {
  
  # Default script list für optimized system
  if (is.null(script_list)) {
    script_list <- c(
      "c:/freeding/tbot202506/r_analysis/clean_console.R",
      "c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v4.r",
      "c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r",
      "c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r"
    )
  }
  
  results <- list()
  start_time <- Sys.time()
  
  if (show_progress) {
    enhanced_log_message("INFO", "Enhanced Trading System Execution")
    enhanced_log_message("INFO", "Scripts to load: %d", length(script_list))
    enhanced_log_message("INFO", "Mode: %s", mode)
  }
  
  # Performance log file
  if (log_results) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    results_log <- file.path(PERFORMANCE_LOG_DIR, 
                            paste0("enhanced_execution_", timestamp, ".log"))
    write(paste("ENHANCED TRADING EXECUTION LOG -", Sys.time()), file = results_log)
  }
  
  # Execute scripts
  for (i in seq_along(script_list)) {
    script_path <- script_list[i]
    script_name <- basename(script_path)
    
    if (show_progress) {
      enhanced_log_message("INFO", "[%d/%d] Loading %s...", i, length(script_list), script_name)
    }
    
    load_start <- Sys.time()
    success <- tryCatch({
      if (mode == "ENHANCED") {
        enhanced_silent_execute(quote(source(script_path, echo = FALSE, verbose = FALSE)))
      } else {
        source(script_path, echo = FALSE, verbose = FALSE)
      }
      TRUE
    }, error = function(e) {
      enhanced_log_message("ERROR", "Script loading failed: %s - %s", script_name, e$message)
      FALSE
    })
    
    load_duration <- difftime(Sys.time(), load_start, units = "secs")
    
    results[[script_name]] <- list(
      path = script_path,
      success = success,
      duration_seconds = as.numeric(load_duration),
      timestamp = Sys.time()
    )
    
    if (show_progress) {
      status_emoji <- if (success) "✅" else "❌"
      enhanced_log_message("INFO", "%s %s (%.2fs)", status_emoji, script_name, as.numeric(load_duration))
    }
    
    # Performance log entry
    if (log_results) {
      log_entry <- sprintf("%s | %s | %.2fs | %s", 
                          Sys.time(), script_name, as.numeric(load_duration),
                          if (success) "SUCCESS" else "FAILED")
      write(log_entry, file = results_log, append = TRUE)
    }
  }
  
  # Summary
  total_duration <- difftime(Sys.time(), start_time, units = "secs")
  successful_scripts <- sum(sapply(results, function(x) x$success))
  
  if (show_progress) {
    enhanced_log_message("SUCCESS", "Enhanced execution summary:")
    enhanced_log_message("INFO", "Total time: %.2f seconds", as.numeric(total_duration))
    enhanced_log_message("INFO", "Successful: %d/%d scripts", successful_scripts, length(script_list))
    
    if (log_results) {
      enhanced_log_message("INFO", "Performance log: %s", basename(results_log))
    }
  }
  
  return(list(
    results = results,
    summary = list(
      total_scripts = length(script_list),
      successful_scripts = successful_scripts,
      total_duration_seconds = as.numeric(total_duration),
      log_file = if (log_results) results_log else NULL
    )
  ))
}

# ==========================================================================================================
# 🎯 SECTION 6: INTERACTIVE MODE MANAGEMENT
# ==========================================================================================================

# ENHANCED MODE SWITCHING
enhanced_set_mode <- function(mode = "NORMAL") {
  valid_modes <- c("SILENT", "NORMAL", "VERBOSE", "DEBUG")
  
  if (!mode %in% valid_modes) {
    enhanced_log_message("ERROR", "Invalid mode: %s. Valid modes: %s", 
                         mode, paste(valid_modes, collapse = ", "))
    return(FALSE)
  }
  
  old_mode <- ENHANCED_CONSOLE_STATE$active_mode %||% "NORMAL"
  ENHANCED_CONSOLE_STATE$active_mode <<- mode
  
  enhanced_log_message("INFO", "Console mode changed: %s -> %s", old_mode, mode)
  
  # Mode-specific setup
  switch(mode,
    "SILENT" = {
      enhanced_log_message("INFO", "🔇 Silent mode: Output to file only")
    },
    "NORMAL" = {
      enhanced_log_message("INFO", "📝 Normal mode: Important messages to console")
    },
    "VERBOSE" = {
      enhanced_log_message("INFO", "📢 Verbose mode: All messages to console")
    },
    "DEBUG" = {
      enhanced_log_message("INFO", "🔍 Debug mode: Maximum detail")
    }
  )
  
  return(TRUE)
}

# QUICK MODE FUNCTIONS
enhanced_start_silent <- function() {
  enhanced_redirect_console(mode = "SILENT")
}

enhanced_start_verbose <- function() {
  enhanced_set_mode("VERBOSE")
}

enhanced_start_debug <- function() {
  enhanced_set_mode("DEBUG")
}

# ==========================================================================================================
# ✅ SECTION 7: SYSTEM INITIALIZATION & STATUS
# ==========================================================================================================

# Initialize enhanced system
enhanced_console_initialize <- function() {
  enhanced_setup_directories()
  
  # Reset state
  ENHANCED_CONSOLE_STATE$message_count <<- 0
  ENHANCED_CONSOLE_STATE$error_count <<- 0
  ENHANCED_CONSOLE_STATE$warning_count <<- 0
  ENHANCED_CONSOLE_STATE$performance_tracking <<- TRUE
  
  enhanced_log_message("SUCCESS", "Enhanced Console Output Management V2 initialized")
  return(TRUE)
}

# System status
enhanced_console_status <- function() {
  cat("📊 ENHANCED CONSOLE STATUS\n")
  cat("==========================\n")
  cat("Redirection active:", ifelse(ENHANCED_CONSOLE_STATE$is_redirected, "YES", "NO"), "\n")
  cat("Current mode:", ENHANCED_CONSOLE_STATE$active_mode %||% "NORMAL", "\n")
  cat("Log file:", ENHANCED_CONSOLE_STATE$log_file_path %||% "None", "\n")
  cat("Messages logged:", ENHANCED_CONSOLE_STATE$message_count, "\n")
  cat("Warnings:", ENHANCED_CONSOLE_STATE$warning_count, "\n")
  cat("Errors:", ENHANCED_CONSOLE_STATE$error_count, "\n")
  
  if (!is.null(ENHANCED_CONSOLE_STATE$start_time)) {
    duration <- difftime(Sys.time(), ENHANCED_CONSOLE_STATE$start_time, units = "mins")
    cat("Session duration:", round(as.numeric(duration), 2), "minutes\n")
  }
  
  cat("Performance tracking:", ifelse(ENHANCED_CONSOLE_STATE$performance_tracking, "ON", "OFF"), "\n")
}

# ==========================================================================================================
# ✅ SECTION 8: AUTO-INITIALIZATION & USAGE INFO
# ==========================================================================================================

# Auto-initialize when script is loaded
tryCatch({
  enhanced_console_initialize()
  
  cat("✅ ENHANCED CONSOLE OUTPUT MANAGEMENT V2 LOADED!\n")
  cat("================================================\n")
  cat("🚀 PERFORMANCE-OPTIMIZED FUNCTIONS:\n")
  cat("   ✅ enhanced_log_message()                    # Smart structured logging\n")
  cat("   ✅ log_info/debug/warn/error/success()       # Convenience functions\n") 
  cat("   ✅ log_performance()                         # Performance tracking\n")
  cat("   ✅ enhanced_redirect_console()               # Smart redirection\n")
  cat("   ✅ enhanced_restore_console()                # With performance report\n")
  cat("   ✅ enhanced_silent_execute()                 # Better silent execution\n")
  cat("   ✅ enhanced_execute_trading_scripts()        # Optimized script loading\n")
  cat("   ✅ enhanced_set_mode()                       # SILENT/NORMAL/VERBOSE/DEBUG\n")
  cat("\n🎯 QUICK START FOR OPTIMIZED TRADING:\n")
  cat("   enhanced_start_silent()                      # Silent mode\n")
  cat("   enhanced_start_verbose()                     # Verbose mode\n")
  cat("   enhanced_console_status()                    # System status\n")
  cat("\n📊 INTEGRATION WITH TRADING SYSTEM:\n")
  cat("   ✅ Auto-logs API performance\n")
  cat("   ✅ Tracks trading execution times\n")
  cat("   ✅ Performance monitoring built-in\n")
  cat("   ✅ UTF-8 encoding issues eliminated\n")
  cat("   ✅ Backward compatible with existing code\n")
  
  cat("\n📄 Log directories:\n")
  cat("   Main logs:", ENHANCED_LOG_DIR, "\n")
  cat("   Performance logs:", PERFORMANCE_LOG_DIR, "\n")
  cat("================================================\n")
  
}, error = function(e) {
  cat("❌ Enhanced console initialization error:", e$message, "\n")
})

# ==========================================================================================================
# 🎯 END OF ENHANCED CONSOLE OUTPUT MANAGEMENT V2
# ==========================================================================================================