# ==========================================================================================================
# 📝 MINIMAL CONSOLE OUTPUT MANAGER - FÜR KOMPATIBILITÄT
# ==========================================================================================================
# 
# ZWECK: Ersetzt die fehlende r_console_output_manager.r in Ihrem System
# KOMPATIBILITÄT: Macht Ihr rexecution_v1.r funktionsfähig
# 
# Speichern unter: c:/freeding/tbot202506/r_analysis/r_console_output_manager.r
# 
# ==========================================================================================================

cat("📝 Loading minimal console output manager (compatibility mode)...\n")

# ==========================================================================================================
# 🔧 MINIMAL FUNCTIONS FÜR KOMPATIBILITÄT
# ==========================================================================================================

# Console State Management
CONSOLE_STATE <- list(
  is_redirected = FALSE,
  log_file = NULL,
  start_time = NULL
)

# Simple silent mode function
start_silent_mode <- function(mode = "file") {
  if (mode == "file") {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- paste0("c:/freeding/tbot202506/logs/console_", timestamp, ".log")
    
    # Create logs directory if needed
    dir.create("c:/freeding/tbot202506/logs/", showWarnings = FALSE, recursive = TRUE)
    
    # Redirect output to file
    sink(log_file, type = "output")
    sink(log_file, type = "message", append = TRUE)
    
    CONSOLE_STATE$is_redirected <<- TRUE
    CONSOLE_STATE$log_file <<- log_file
    CONSOLE_STATE$start_time <<- Sys.time()
    
    cat("📝 Console redirected to:", basename(log_file), "\n")
    cat("🔕 Console output now silent - check log file for details\n")
    cat("📄 Use end_silent_mode() to return to normal output\n")
    
    return(TRUE)
  }
  
  return(FALSE)
}

# Simple end silent mode function
end_silent_mode <- function() {
  if (CONSOLE_STATE$is_redirected) {
    # Restore output
    sink(type = "message")
    sink(type = "output")
    
    CONSOLE_STATE$is_redirected <<- FALSE
    
    # Show summary
    if (!is.null(CONSOLE_STATE$log_file)) {
      cat("✅ Console output restored!\n")
      cat("📄 Log saved to:", basename(CONSOLE_STATE$log_file), "\n")
      
      if (!is.null(CONSOLE_STATE$start_time)) {
        duration <- difftime(Sys.time(), CONSOLE_STATE$start_time, units = "mins")
        cat("⏱️ Session duration:", round(as.numeric(duration), 2), "minutes\n")
      }
    }
    
    return(TRUE)
  }
  
  cat("⚠️ Console was not redirected\n")
  return(FALSE)
}

# Simple restore function (alias)
restore_console <- function() {
  return(end_silent_mode())
}

# Check if console is redirected
is_console_redirected <- function() {
  return(CONSOLE_STATE$is_redirected)
}

# Get current log file
get_current_log_file <- function() {
  return(CONSOLE_STATE$log_file)
}

# ==========================================================================================================
# ✅ COMPATIBILITY CONFIRMATION
# ==========================================================================================================

cat("✅ MINIMAL CONSOLE MANAGER LOADED!\n")
cat("==================================\n")
cat("📋 Available functions:\n")
cat("   ✅ start_silent_mode()      # Redirect to file\n")
cat("   ✅ end_silent_mode()        # Restore console\n")
cat("   ✅ restore_console()        # Alias for end_silent_mode\n")
cat("   ✅ is_console_redirected()  # Check status\n")
cat("\n🎯 Your rexecution_v1.r should now work!\n")
cat("==================================\n")

# ==========================================================================================================
# 🎯 END MINIMAL CONSOLE MANAGER
# ==========================================================================================================