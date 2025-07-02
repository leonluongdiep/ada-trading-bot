# ==========================================================================================================
# 📝 ROBUST CONSOLE OUTPUT MANAGER - VERBESSERTE VERSION
# ==========================================================================================================
# 
# FIXES: Silent Mode "Gefängnis" Problem
# HINZUFÜGT: Sichere Ein-/Ausgangs-Mechanismen
# ERSETZT: Ihr problematisches r_console_output_manager.r
# 
# SPEICHERN ALS: c:/freeding/tbot202506/r_analysis/r_console_output_manager.r
# 
# ==========================================================================================================

cat("📝 Loading ROBUST Console Output Manager...\n")

# ==========================================================================================================
# 🔧 SECTION 1: CONSOLE STATE MANAGEMENT
# ==========================================================================================================

# Globaler Console State (sicher verwaltet)
if (!exists("CONSOLE_STATE")) {
  CONSOLE_STATE <- list(
    is_redirected = FALSE,
    log_file = NULL,
    start_time = NULL,
    original_sinks = list(
      output_connections = c(),
      message_connections = c()
    ),
    backup_created = FALSE
  )
}

# State backup and restore functions
backup_console_state <- function() {
  CONSOLE_STATE$original_sinks$output_connections <<- capture.output(sink.number())
  CONSOLE_STATE$original_sinks$message_connections <<- capture.output(sink.number(type = "message"))
  CONSOLE_STATE$backup_created <<- TRUE
  cat("💾 Console state backed up\n")
}

restore_console_state <- function() {
  # Sichere Wiederherstellung aller Sink-Verbindungen
  tryCatch({
    # Alle aktiven Sinks schließen
    while(sink.number() > 0) {
      sink()
    }
    while(sink.number(type = "message") > 0) {
      sink(type = "message")
    }
    
    CONSOLE_STATE$is_redirected <<- FALSE
    cat("🔓 Console state fully restored\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("⚠️ Error in restore:", e$message, "\n")
    return(FALSE)
  })
}

# ==========================================================================================================
# 🔧 SECTION 2: SAFE SILENT MODE FUNCTIONS
# ==========================================================================================================

start_silent_mode <- function(mode = "file") {
  cat("🔕 Starting safe silent mode...\n")
  
  if (CONSOLE_STATE$is_redirected) {
    cat("⚠️ Already in silent mode - ending first\n")
    end_silent_mode()
  }
  
  # Backup current state
  backup_console_state()
  
  if (mode == "file") {
    # Erstelle Log-File mit Timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_file <- paste0("c:/freeding/tbot202506/logs/trading_console_", timestamp, ".log")
    
    # Erstelle Log-Verzeichnis falls nötig
    log_dir <- dirname(log_file)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    tryCatch({
      # Sichere Log-File Erstellung
      cat("# =====================================\n", file = log_file)
      cat("# TRADING SYSTEM CONSOLE LOG\n", file = log_file, append = TRUE)
      cat("# =====================================\n", file = log_file, append = TRUE)
      cat("# Start Time:", as.character(Sys.time()), "\n", file = log_file, append = TRUE)
      cat("# Log File:", log_file, "\n", file = log_file, append = TRUE)
      cat("# =====================================\n\n", file = log_file, append = TRUE)
      
      # Sichere Umleitung
      sink(log_file, type = "output", append = TRUE)
      sink(log_file, type = "message", append = TRUE)
      
      # State speichern
      CONSOLE_STATE$is_redirected <<- TRUE
      CONSOLE_STATE$log_file <<- log_file
      CONSOLE_STATE$start_time <<- Sys.time()
      
      # Bestätigung (geht ins Log)
      cat("📝 Console redirected to:", basename(log_file), "\n")
      cat("🔕 Console output now silent - check log file for details\n")
      cat("📄 Use end_silent_mode() to return to normal output\n")
      
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Failed to start silent mode:", e$message, "\n")
      restore_console_state()
      return(FALSE)
    })
  } else {
    cat("⚠️ Only 'file' mode supported\n")
    return(FALSE)
  }
}

end_silent_mode <- function() {
  cat("🔓 Ending silent mode...\n")
  
  if (!CONSOLE_STATE$is_redirected) {
    cat("⚠️ Console was not redirected\n")
    return(FALSE)
  }
  
  # Log-Zusammenfassung vor Beendigung
  if (!is.null(CONSOLE_STATE$start_time)) {
    duration <- difftime(Sys.time(), CONSOLE_STATE$start_time, units = "mins")
    cat("⏱️ Session duration:", round(as.numeric(duration), 2), "minutes\n")
  }
  
  cat("🔚 Ending console redirection...\n")
  
  # Sichere Wiederherstellung
  success <- restore_console_state()
  
  if (success && !is.null(CONSOLE_STATE$log_file)) {
    # Nach der Wiederherstellung zur normalen Console
    cat("✅ Console output restored!\n")
    cat("📄 Log saved to:", basename(CONSOLE_STATE$log_file), "\n")
    
    if (!is.null(CONSOLE_STATE$start_time)) {
      duration <- difftime(Sys.time(), CONSOLE_STATE$start_time, units = "mins")
      cat("⏱️ Session duration:", round(as.numeric(duration), 2), "minutes\n")
    }
    
    # State zurücksetzen
    CONSOLE_STATE$log_file <<- NULL
    CONSOLE_STATE$start_time <<- NULL
  }
  
  return(success)
}

# ==========================================================================================================
# 🔧 SECTION 3: EMERGENCY FUNCTIONS
# ==========================================================================================================

# Notfall-Funktion: Alle Verbindungen schließen
emergency_console_reset <- function() {
  cat("🚨 EMERGENCY CONSOLE RESET\n")
  
  tryCatch({
    # Alle Sink-Verbindungen brutal schließen
    while(sink.number() > 0) {
      sink()
    }
    while(sink.number(type = "message") > 0) {
      sink(type = "message")
    }
    
    # State zurücksetzen
    CONSOLE_STATE$is_redirected <<- FALSE
    CONSOLE_STATE$log_file <<- NULL
    CONSOLE_STATE$start_time <<- NULL
    
    cat("✅ Emergency reset completed\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Emergency reset failed:", e$message, "\n")
    cat("💀 Use clean_console.R as last resort\n")
    return(FALSE)
  })
}

# Status-Check Funktion
check_console_status <- function() {
  cat("📊 CONSOLE STATUS CHECK\n")
  cat("=======================\n")
  cat("🔍 Redirected:", CONSOLE_STATE$is_redirected, "\n")
  cat("📄 Log file:", if(!is.null(CONSOLE_STATE$log_file)) basename(CONSOLE_STATE$log_file) else "None", "\n")
  cat("⏱️ Start time:", if(!is.null(CONSOLE_STATE$start_time)) as.character(CONSOLE_STATE$start_time) else "None", "\n")
  cat("📊 Output sinks:", sink.number(), "\n")
  cat("📧 Message sinks:", sink.number(type = "message"), "\n")
  
  if (CONSOLE_STATE$is_redirected && sink.number() == 0) {
    cat("⚠️ WARNING: State says redirected but no active sinks!\n")
    cat("💡 Try: emergency_console_reset()\n")
  }
  
  # Test output
  cat("🧪 Test output: This should be visible\n")
  
  return(CONSOLE_STATE)
}

# ==========================================================================================================
# 🔧 SECTION 4: ALIASES AND COMPATIBILITY
# ==========================================================================================================

# Kompatibilitäts-Aliases
restore_console <- function() {
  return(end_silent_mode())
}

is_console_redirected <- function() {
  return(CONSOLE_STATE$is_redirected)
}

get_current_log_file <- function() {
  return(CONSOLE_STATE$log_file)
}

# Safe wrapper für bestehende Scripts
safe_start_silent <- function(mode = "file") {
  if (CONSOLE_STATE$is_redirected) {
    cat("📝 Console already redirected, continuing...\n")
    return(TRUE)
  } else {
    return(start_silent_mode(mode))
  }
}

# ==========================================================================================================
# 🔧 SECTION 5: AUTO-RECOVERY MECHANISM
# ==========================================================================================================

# Automatische Wiederherstellung beim Laden
auto_recovery_check <- function() {
  if (sink.number() > 0 || sink.number(type = "message") > 0) {
    cat("⚠️ Detected active sinks from previous session\n")
    cat("🔧 Auto-recovering console...\n")
    
    emergency_console_reset()
    
    cat("✅ Auto-recovery completed\n")
  }
}

# ==========================================================================================================
# ✅ INITIALIZATION
# ==========================================================================================================

# Auto-recovery beim Laden ausführen
auto_recovery_check()

cat("✅ ROBUST CONSOLE OUTPUT MANAGER LOADED!\n")
cat("==========================================\n")
cat("🔧 Available functions:\n")
cat("   ✅ start_silent_mode(mode)         # Safe silent mode\n")
cat("   ✅ end_silent_mode()               # Reliable exit\n")
cat("   ✅ restore_console()               # Alias for end_silent_mode\n")
cat("   ✅ check_console_status()          # Status check\n")
cat("   ✅ emergency_console_reset()       # Emergency exit\n")
cat("   ✅ safe_start_silent(mode)         # Safe wrapper\n")
cat("\n🎯 FEATURES:\n")
cat("   ✅ No more silent mode 'prison'\n")
cat("   ✅ Auto-recovery on load\n")
cat("   ✅ Emergency reset functions\n")
cat("   ✅ State backup/restore\n")
cat("   ✅ Reliable log file management\n")
cat("==========================================\n")

# Auto-test nach dem Laden
cat("🧪 Testing basic functionality...\n")
test_time <- Sys.time()
cat("✅ Basic output working - Time:", as.character(test_time), "\n")
cat("🎉 Console manager ready for use!\n")