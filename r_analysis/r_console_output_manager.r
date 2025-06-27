# ==========================================================================================================
# 📝 R CONSOLE OUTPUT MANAGEMENT - TRADING SYSTEM OPTIMIERT
# ==========================================================================================================
# 
# ZWECK: Console-Ausgabe umleiten/unterdrücken für umfangreiche Trading-Script-Ausführung
# KOMPATIBEL: Mit Ihrem bestehenden Bitget Trading System
# VERSION: 1.0 - Production Ready
# 
# ==========================================================================================================

cat("📝 Loading Console Output Management System...\n")

# ==========================================================================================================
# 🔧 CONFIGURATION
# ==========================================================================================================

# Globale Variablen für Output Management
CONSOLE_OUTPUT_STATE <- list(
  original_stdout = NULL,
  original_stderr = NULL,
  log_file_path = NULL,
  is_redirected = FALSE,
  start_time = NULL
)

# Default Log Directory
DEFAULT_LOG_DIR <- "c:/freeding/tbot202506/logs/"

# Erstelle Log-Verzeichnis falls nicht vorhanden
tryCatch({
  dir.create(DEFAULT_LOG_DIR, recursive = TRUE, showWarnings = FALSE)
  cat("📁 Log directory ready:", DEFAULT_LOG_DIR, "\n")
}, error = function(e) {
  DEFAULT_LOG_DIR <<- tempdir()
  cat("⚠️ Using temporary log directory:", DEFAULT_LOG_DIR, "\n")
})

# ==========================================================================================================
# 📝 MAIN OUTPUT REDIRECTION FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ REDIRECT CONSOLE TO FILE - Komplette Console-Umleitung in Log-Datei                                │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
redirect_console_to_file <- function(log_filename = NULL, 
                                    log_dir = DEFAULT_LOG_DIR,
                                    include_timestamp = TRUE,
                                    show_summary = TRUE) {
  
  if (CONSOLE_OUTPUT_STATE$is_redirected) {
    cat("⚠️ Console already redirected. Use restore_console() first.\n")
    return(FALSE)
  }
  
  # Generiere Log-Filename
  if (is.null(log_filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    log_filename <- paste0("trading_console_", timestamp, ".log")
  }
  
  # Vollständiger Log-Pfad
  log_path <- file.path(log_dir, log_filename)
  
  tryCatch({
    # Speichere ursprüngliche Connections
    CONSOLE_OUTPUT_STATE$original_stdout <<- stdout()
    CONSOLE_OUTPUT_STATE$original_stderr <<- stderr()
    CONSOLE_OUTPUT_STATE$log_file_path <<- log_path
    CONSOLE_OUTPUT_STATE$start_time <<- Sys.time()
    
    # Öffne Log-Datei
    log_con <- file(log_path, open = "w")
    
    # Umleitung aktivieren
    sink(log_con, type = "output")     # stdout
    sink(log_con, type = "message")    # stderr/warnings
    
    CONSOLE_OUTPUT_STATE$is_redirected <<- TRUE
    
    # Header in Log-Datei schreiben
    if (include_timestamp) {
      cat("# =====================================\n")
      cat("# TRADING SYSTEM CONSOLE LOG\n") 
      cat("# =====================================\n")
      cat("# Start Time:", as.character(Sys.time()), "\n")
      cat("# Log File:", log_path, "\n")
      cat("# =====================================\n\n")
    }
    
    if (show_summary) {
      # Diese Nachricht wird in die Console geschrieben (vor Umleitung)
      message("📝 Console redirected to: ", basename(log_path))
      message("🔕 Console output now silent - check log file for details")
      message("📄 Use restore_console() to return to normal output")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error redirecting console:", e$message, "\n")
    return(FALSE)
  })
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ RESTORE CONSOLE - Normale Console-Ausgabe wiederherstellen                                          │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
restore_console <- function(show_summary = TRUE) {
  
  if (!CONSOLE_OUTPUT_STATE$is_redirected) {
    cat("⚠️ Console not currently redirected.\n")
    return(FALSE)
  }
  
  tryCatch({
    # Schreibe Abschluss-Info in Log
    cat("\n# =====================================\n")
    cat("# SESSION END\n")
    cat("# End Time:", as.character(Sys.time()), "\n")
    
    if (!is.null(CONSOLE_OUTPUT_STATE$start_time)) {
      duration <- difftime(Sys.time(), CONSOLE_OUTPUT_STATE$start_time, units = "mins")
      cat("# Duration:", round(as.numeric(duration), 2), "minutes\n")
    }
    
    cat("# =====================================\n")
    
    # Sink zurücksetzen
    sink(type = "message")
    sink(type = "output")
    
    # Status zurücksetzen
    CONSOLE_OUTPUT_STATE$is_redirected <<- FALSE
    
    if (show_summary) {
      cat("✅ Console output restored!\n")
      cat("📄 Log saved to:", CONSOLE_OUTPUT_STATE$log_file_path, "\n")
      
      # Log-Datei Größe anzeigen
      if (file.exists(CONSOLE_OUTPUT_STATE$log_file_path)) {
        file_size <- file.size(CONSOLE_OUTPUT_STATE$log_file_path)
        cat("📊 Log file size:", format(file_size, units = "KB"), "\n")
      }
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error restoring console:", e$message, "\n")
    
    # Versuche Notfall-Wiederherstellung
    tryCatch({
      sink(type = "message")
      sink(type = "output")
      CONSOLE_OUTPUT_STATE$is_redirected <<- FALSE
    }, error = function(e2) {
      cat("❌ Emergency restore failed\n")
    })
    
    return(FALSE)
  })
}

# ==========================================================================================================
# 🔇 SILENT EXECUTION FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ SILENT EXECUTE - Führt Code ohne Console-Ausgabe aus                                                │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
silent_execute <- function(code_expr, 
                          capture_output = FALSE,
                          show_errors = TRUE) {
  
  captured_output <- NULL
  result <- NULL
  
  if (capture_output) {
    # Output in Variable erfassen
    captured_output <- capture.output({
      result <- tryCatch({
        eval(code_expr)
      }, error = function(e) {
        if (show_errors) {
          cat("❌ Error in silent execution:", e$message, "\n")
        }
        return(NULL)
      })
    }, type = "output")
  } else {
    # Output komplett unterdrücken
    result <- tryCatch({
      sink(tempfile())  # Output in temporäre Datei
      eval(code_expr)
    }, error = function(e) {
      if (show_errors) {
        sink()  # Sink zurücksetzen vor Error-Anzeige
        cat("❌ Error in silent execution:", e$message, "\n")
      }
      return(NULL)
    }, finally = {
      sink()  # Sink immer zurücksetzen
    })
  }
  
  if (capture_output) {
    return(list(
      result = result,
      output = captured_output,
      success = !is.null(result)
    ))
  } else {
    return(result)
  }
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ SILENT SOURCE - Script-Dateien ohne Console-Ausgabe laden                                           │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
silent_source <- function(script_path, 
                         show_progress = TRUE,
                         capture_errors = TRUE) {
  
  if (show_progress) {
    cat("🔇 Loading", basename(script_path), "silently...\n")
  }
  
  result <- tryCatch({
    # Output unterdrücken während source()
    sink(tempfile())
    source(script_path, echo = FALSE, verbose = FALSE)
    sink()
    
    if (show_progress) {
      cat("✅", basename(script_path), "loaded\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    sink()  # Sink zurücksetzen im Fehlerfall
    
    if (capture_errors) {
      cat("❌ Error loading", basename(script_path), ":", e$message, "\n")
    }
    
    return(FALSE)
  })
  
  return(result)
}

# ==========================================================================================================
# 📊 SELECTIVE OUTPUT FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ FILTERED OUTPUT - Nur bestimmte Nachrichten anzeigen                                                │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
create_filtered_output <- function(keywords = c("✅", "❌", "🚀", "📊", "⚠️"),
                                  log_all = TRUE,
                                  log_dir = DEFAULT_LOG_DIR) {
  
  if (log_all) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    full_log_path <- file.path(log_dir, paste0("full_log_", timestamp, ".log"))
    filtered_log_path <- file.path(log_dir, paste0("filtered_log_", timestamp, ".log"))
  }
  
  # Custom cat-Funktion erstellen
  original_cat <<- cat
  
  filtered_cat <<- function(..., sep = " ", fill = FALSE, labels = NULL) {
    
    # Nachricht zusammensetzen
    message_text <- paste(..., sep = sep)
    
    # In vollständiges Log schreiben (falls aktiviert)
    if (log_all && exists("full_log_path")) {
      write(paste(Sys.time(), "-", message_text), 
            file = full_log_path, append = TRUE)
    }
    
    # Prüfe ob Nachricht Keywords enthält
    show_message <- any(sapply(keywords, function(kw) grepl(kw, message_text, fixed = TRUE)))
    
    if (show_message) {
      # Zeige in Console und schreibe in gefilterten Log
      original_cat(message_text, sep = sep, fill = fill, labels = labels)
      
      if (log_all && exists("filtered_log_path")) {
        write(paste(Sys.time(), "-", message_text), 
              file = filtered_log_path, append = TRUE)
      }
    }
  }
  
  # cat-Funktion überschreiben
  assignInNamespace("cat", filtered_cat, ns = ".GlobalEnv")
  
  cat("🔍 Filtered output activated\n")
  cat("📝 Keywords:", paste(keywords, collapse = " "), "\n")
  if (log_all) {
    cat("📄 Full log:", basename(full_log_path), "\n")
    cat("📄 Filtered log:", basename(filtered_log_path), "\n")
  }
  
  return(TRUE)
}

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ RESTORE FILTERED OUTPUT - Normale cat-Funktion wiederherstellen                                     │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
restore_filtered_output <- function() {
  if (exists("original_cat")) {
    assignInNamespace("cat", original_cat, ns = ".GlobalEnv")
    cat("✅ Normal output restored\n")
    return(TRUE)
  } else {
    cat("⚠️ No filtered output to restore\n")
    return(FALSE)
  }
}

# ==========================================================================================================
# 🎯 TRADING SYSTEM OPTIMIZED FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ EXECUTE TRADING SCRIPTS SILENTLY - Optimiert für Ihr Trading System                                │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
execute_trading_scripts_silent <- function(script_list = NULL,
                                          show_progress = TRUE,
                                          log_results = TRUE) {
  
  # Default Script-Liste basierend auf Ihren Dokumenten
  if (is.null(script_list)) {
    script_list <- c(
      "c:/freeding/tbot202506/r_analysis/clean_console.R",
      "c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r",
      "c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r",
      "c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r"
    )
  }
  
  results <- list()
  start_time <- Sys.time()
  
  if (show_progress) {
    cat("🚀 SILENT TRADING SYSTEM EXECUTION\n")
    cat(strrep("=", 50), "\n")
    cat("Scripts to load:", length(script_list), "\n")
    cat("Start time:", as.character(start_time), "\n\n")
  }
  
  # Log-Datei für Ergebnisse
  if (log_results) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    results_log <- file.path(DEFAULT_LOG_DIR, paste0("trading_execution_", timestamp, ".log"))
    write(paste("TRADING SYSTEM EXECUTION LOG -", Sys.time()), file = results_log)
  }
  
  # Scripts nacheinander laden
  for (i in seq_along(script_list)) {
    script_path <- script_list[i]
    script_name <- basename(script_path)
    
    if (show_progress) {
      cat(sprintf("📝 [%d/%d] Loading %s...", i, length(script_list), script_name))
    }
    
    load_start <- Sys.time()
    success <- silent_source(script_path, show_progress = FALSE, capture_errors = TRUE)
    load_duration <- difftime(Sys.time(), load_start, units = "secs")
    
    results[[script_name]] <- list(
      path = script_path,
      success = success,
      duration_seconds = as.numeric(load_duration),
      timestamp = Sys.time()
    )
    
    if (show_progress) {
      status <- if (success) "✅" else "❌"
      cat(sprintf(" %s (%.2fs)\n", status, as.numeric(load_duration)))
    }
    
    # Log-Eintrag
    if (log_results) {
      log_entry <- sprintf("%s | %s | %.2fs | %s", 
                          Sys.time(), script_name, as.numeric(load_duration),
                          if (success) "SUCCESS" else "FAILED")
      write(log_entry, file = results_log, append = TRUE)
    }
    
    # Kleine Pause zwischen Scripts
    Sys.sleep(0.1)
  }
  
  # Zusammenfassung
  total_duration <- difftime(Sys.time(), start_time, units = "secs")
  successful_scripts <- sum(sapply(results, function(x) x$success))
  
  if (show_progress) {
    cat("\n📊 EXECUTION SUMMARY:\n")
    cat(sprintf("Total time: %.2f seconds\n", as.numeric(total_duration)))
    cat(sprintf("Successful: %d/%d scripts\n", successful_scripts, length(script_list)))
    
    if (log_results) {
      cat(sprintf("📄 Results log: %s\n", basename(results_log)))
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

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ QUIET ANALYSIS - Führt Trading-Analysen ohne Output aus                                             │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
quiet_analysis <- function(symbols = c("ADAUSDT_UMCBL", "BTCUSDT_UMCBL", "ETHUSDT_UMCBL"),
                          functions_to_run = c("complete_trading_analysis", "get_enhanced_market_data"),
                          show_summary = TRUE) {
  
  analysis_results <- list()
  
  for (symbol in symbols) {
    if (show_summary) {
      cat(sprintf("🔇 Analyzing %s...", symbol))
    }
    
    symbol_results <- list()
    
    for (func_name in functions_to_run) {
      if (exists(func_name)) {
        tryCatch({
          # Funktion still ausführen
          result <- silent_execute(call(func_name, symbol))
          symbol_results[[func_name]] <- result
        }, error = function(e) {
          symbol_results[[func_name]] <- NULL
        })
      }
    }
    
    analysis_results[[symbol]] <- symbol_results
    
    if (show_summary) {
      successful_funcs <- sum(sapply(symbol_results, function(x) !is.null(x)))
      cat(sprintf(" ✅ (%d/%d functions)\n", successful_funcs, length(functions_to_run)))
    }
  }
  
  return(analysis_results)
}

# ==========================================================================================================
# 📱 CONVENIENCE WRAPPER FUNCTIONS
# ==========================================================================================================

# ┌─────────────────────────────────────────────────────────────────────────────────────────────────────┐
# │ QUICK SILENT MODE - Ein-Klick Lösung für stille Ausführung                                          │
# └─────────────────────────────────────────────────────────────────────────────────────────────────────┘
start_silent_mode <- function(mode = "file") {  # "file", "filter", "suppress"
  
  cat("🔇 Starting silent mode:", mode, "\n")
  
  switch(mode,
    "file" = {
      redirect_console_to_file()
    },
    "filter" = {
      create_filtered_output()
    },
    "suppress" = {
      cat("🔕 Output suppression active - use end_silent_mode() to restore\n")
      CONSOLE_OUTPUT_STATE$mode <<- "suppress"
    }
  )
  
  CONSOLE_OUTPUT_STATE$active_mode <<- mode
  return(TRUE)
}

end_silent_mode <- function() {
  
  mode <- CONSOLE_OUTPUT_STATE$active_mode
  
  switch(mode,
    "file" = {
      restore_console()
    },
    "filter" = {
      restore_filtered_output()
    },
    "suppress" = {
      cat("🔊 Output suppression ended\n")
    }
  )
  
  CONSOLE_OUTPUT_STATE$active_mode <<- NULL
  return(TRUE)
}

# ==========================================================================================================
# ✅ STATUS & USAGE INFORMATION
# ==========================================================================================================

cat("✅ CONSOLE OUTPUT MANAGEMENT LOADED!\n")
cat(strrep("=", 50), "\n")
cat("🎯 QUICK START FOR YOUR TRADING SYSTEM:\n\n")

cat("📝 COMPLETE REDIRECTION:\n")
cat("   start_silent_mode('file')           # All output to log file\n")
cat("   # ... run your trading scripts ...\n") 
cat("   end_silent_mode()                   # Restore normal output\n\n")

cat("🔍 FILTERED OUTPUT (show only important):\n")
cat("   start_silent_mode('filter')         # Only show ✅❌🚀📊⚠️\n")
cat("   # ... run your trading scripts ...\n")
cat("   end_silent_mode()                   # Restore normal output\n\n")

cat("🚀 SILENT SCRIPT EXECUTION:\n")
cat("   execute_trading_scripts_silent()    # Load all your scripts silently\n\n")

cat("🔇 INDIVIDUAL FUNCTIONS:\n")
cat("   silent_execute(expression)          # Run code without output\n")
cat("   silent_source('script.r')           # Load script without output\n")
cat("   quiet_analysis(symbols)             # Analyze without console spam\n\n")

cat("📊 OPTIMIZED FOR YOUR SYSTEM:\n")
cat("   ✅ Compatible with Bitget trading scripts\n")
cat("   ✅ Handles multiple asset analysis\n")
cat("   ✅ Automatic log file generation\n")
cat("   ✅ Error handling and recovery\n")
cat("   ✅ Progress indicators for long operations\n\n")

cat("📄 Log files saved to:", DEFAULT_LOG_DIR, "\n")
cat(strrep("=", 50), "\n")

# ==========================================================================================================
# 🎯 END OF CONSOLE OUTPUT MANAGEMENT SYSTEM
# ==========================================================================================================