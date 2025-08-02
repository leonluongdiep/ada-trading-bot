# ==========================================================================================================
# 💾 ADVANCED DATA MANAGEMENT SYSTEM V1
# ==========================================================================================================
# Pfad: utils/data_manager.r
# Umfassendes Datenmanagement für Trading-System
# Speicherung, Backup, Recovery, Analyse-Datenbanken
# ==========================================================================================================

cat("💾 Loading Advanced Data Management System V1...\n")

# ==========================================================================================================
# 🔧 DEPENDENCIES & CONFIGURATION
# ==========================================================================================================

# Load required packages
required_packages <- c("DBI", "RSQLite", "jsonlite", "data.table")
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
# ⚙️ DATA MANAGER CONFIGURATION
# ==========================================================================================================

DATA_MANAGER_CONFIG <- list(
  # Database configuration
  database = list(
    type = "sqlite",
    path = file.path(SYSTEM_PATHS$data, "trading_data.db"),
    backup_path = file.path(SYSTEM_PATHS$backup, "trading_data_backup.db"),
    memory_cache = TRUE,
    auto_backup_hours = 24
  ),
  
  # Data retention policies
  retention = list(
    tick_data_days = 7,
    candle_data_days = 90,
    trade_history_days = 365,
    performance_metrics_days = 365,
    system_logs_days = 30
  ),
  
  # Storage formats
  formats = list(
    database = c("trades", "positions", "performance"),
    csv = c("daily_summaries", "reports"),
    json = c("strategies", "configurations"),
    rds = c("models", "analytics")
  ),
  
  # Compression settings
  compression = list(
    enabled = TRUE,
    threshold_mb = 10,
    algorithm = "gzip"
  ),
  
  # Cache settings
  cache = list(
    enabled = TRUE,
    max_size_mb = 100,
    ttl_seconds = 300  # 5 minutes
  )
)

# ==========================================================================================================
# 🗄️ DATABASE MANAGEMENT
# ==========================================================================================================

#' Initialize database connection
initialize_database <- function() {
  
  cat("\n🗄️ Initializing database...\n")
  
  # Ensure directories exist
  ensure_data_directories()
  
  # Connect to database
  DB_CONNECTION <<- dbConnect(
    RSQLite::SQLite(), 
    DATA_MANAGER_CONFIG$database$path
  )
  
  # Create tables if not exist
  create_database_tables()
  
  # Initialize cache
  if (DATA_MANAGER_CONFIG$cache$enabled) {
    DATA_CACHE <<- list()
    CACHE_TIMESTAMPS <<- list()
  }
  
  cat("✅ Database initialized\n")
  
  return(TRUE)
}

#' Create database tables
create_database_tables <- function() {
  
  # Trades table
  dbExecute(DB_CONNECTION, "
    CREATE TABLE IF NOT EXISTS trades (
      trade_id TEXT PRIMARY KEY,
      symbol TEXT NOT NULL,
      strategy TEXT,
      side TEXT,
      entry_time TIMESTAMP,
      exit_time TIMESTAMP,
      entry_price REAL,
      exit_price REAL,
      size REAL,
      pnl REAL,
      pnl_percentage REAL,
      fees REAL,
      metadata TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Positions table
  dbExecute(DB_CONNECTION, "
    CREATE TABLE IF NOT EXISTS positions (
      position_id TEXT PRIMARY KEY,
      symbol TEXT NOT NULL,
      side TEXT,
      size REAL,
      entry_price REAL,
      current_price REAL,
      unrealized_pnl REAL,
      realized_pnl REAL,
      margin_used REAL,
      leverage INTEGER,
      opened_at TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Performance metrics table
  dbExecute(DB_CONNECTION, "
    CREATE TABLE IF NOT EXISTS performance_metrics (
      metric_id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TIMESTAMP,
      metric_type TEXT,
      metric_name TEXT,
      metric_value REAL,
      additional_data TEXT
    )
  ")
  
  # Market data table
  dbExecute(DB_CONNECTION, "
    CREATE TABLE IF NOT EXISTS market_data (
      data_id INTEGER PRIMARY KEY AUTOINCREMENT,
      symbol TEXT NOT NULL,
      timestamp TIMESTAMP,
      timeframe TEXT,
      open REAL,
      high REAL,
      low REAL,
      close REAL,
      volume REAL,
      oi_data TEXT
    )
  ")
  
  # System events table
  dbExecute(DB_CONNECTION, "
    CREATE TABLE IF NOT EXISTS system_events (
      event_id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      event_type TEXT,
      event_category TEXT,
      description TEXT,
      data TEXT
    )
  ")
  
  # Strategy configurations table
  dbExecute(DB_CONNECTION, "
    CREATE TABLE IF NOT EXISTS strategy_configs (
      config_id INTEGER PRIMARY KEY AUTOINCREMENT,
      strategy_name TEXT UNIQUE,
      configuration TEXT,
      version INTEGER,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Create indices for performance
  dbExecute(DB_CONNECTION, "CREATE INDEX IF NOT EXISTS idx_trades_symbol ON trades(symbol)")
  dbExecute(DB_CONNECTION, "CREATE INDEX IF NOT EXISTS idx_trades_time ON trades(exit_time)")
  dbExecute(DB_CONNECTION, "CREATE INDEX IF NOT EXISTS idx_positions_symbol ON positions(symbol)")
  dbExecute(DB_CONNECTION, "CREATE INDEX IF NOT EXISTS idx_market_data_symbol_time ON market_data(symbol, timestamp)")
  dbExecute(DB_CONNECTION, "CREATE INDEX IF NOT EXISTS idx_performance_time ON performance_metrics(timestamp)")
}

# ==========================================================================================================
# 💾 DATA STORAGE FUNCTIONS
# ==========================================================================================================

#' Save trade to database
save_trade <- function(trade_data) {
  
  tryCatch({
    # Prepare trade record
    trade_record <- data.frame(
      trade_id = trade_data$trade_id %||% generate_trade_id(),
      symbol = trade_data$symbol,
      strategy = trade_data$strategy %||% "UNKNOWN",
      side = trade_data$side,
      entry_time = format(trade_data$entry_time, "%Y-%m-%d %H:%M:%S"),
      exit_time = format(trade_data$exit_time, "%Y-%m-%d %H:%M:%S"),
      entry_price = trade_data$entry_price,
      exit_price = trade_data$exit_price,
      size = trade_data$size,
      pnl = trade_data$pnl,
      pnl_percentage = trade_data$pnl_percentage,
      fees = trade_data$fees %||% 0,
      metadata = toJSON(trade_data$metadata %||% list()),
      stringsAsFactors = FALSE
    )
    
    # Insert into database
    dbWriteTable(DB_CONNECTION, "trades", trade_record, append = TRUE)
    
    # Invalidate cache
    invalidate_cache("trades")
    
    # Log event
    log_system_event("trade_saved", "data", 
                    paste("Trade saved:", trade_data$symbol))
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error saving trade:", e$message, "\n")
    return(FALSE)
  })
}

#' Save position snapshot
save_position_snapshot <- function(positions) {
  
  if (is.null(positions) || nrow(positions) == 0) return(TRUE)
  
  tryCatch({
    # Prepare position records
    position_records <- data.frame(
      position_id = sapply(1:nrow(positions), function(i) 
        paste0(positions$symbol[i], "_", format(Sys.time(), "%Y%m%d%H%M%S"))),
      symbol = positions$symbol,
      side = positions$side,
      size = positions$size,
      entry_price = positions$avg_price,
      current_price = positions$mark_price,
      unrealized_pnl = positions$unrealized_pnl,
      realized_pnl = 0,  # Would need to track separately
      margin_used = positions$margin,
      leverage = positions$leverage,
      opened_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    
    # Delete old snapshots for these symbols
    dbExecute(DB_CONNECTION, 
              paste0("DELETE FROM positions WHERE symbol IN (",
                    paste0("'", unique(positions$symbol), "'", collapse = ","), ")"))
    
    # Insert new snapshots
    dbWriteTable(DB_CONNECTION, "positions", position_records, append = TRUE)
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error saving positions:", e$message, "\n")
    return(FALSE)
  })
}

#' Save performance metrics
save_performance_metrics <- function(metrics, metric_type = "portfolio") {
  
  tryCatch({
    # Prepare metric records
    metric_records <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      metric_type = metric_type,
      metric_name = names(metrics),
      metric_value = unlist(metrics),
      additional_data = "",
      stringsAsFactors = FALSE
    )
    
    # Insert into database
    dbWriteTable(DB_CONNECTION, "performance_metrics", metric_records, append = TRUE)
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error saving metrics:", e$message, "\n")
    return(FALSE)
  })
}

#' Save market data
save_market_data <- function(symbol, timeframe, candles, oi_data = NULL) {
  
  if (is.null(candles) || nrow(candles) == 0) return(FALSE)
  
  tryCatch({
    # Prepare market data records
    market_records <- data.frame(
      symbol = symbol,
      timestamp = candles$timestamp,
      timeframe = timeframe,
      open = candles$open,
      high = candles$high,
      low = candles$low,
      close = candles$close,
      volume = candles$volume,
      oi_data = if (!is.null(oi_data)) toJSON(oi_data) else "",
      stringsAsFactors = FALSE
    )
    
    # Insert into database (avoiding duplicates)
    for (i in 1:nrow(market_records)) {
      dbExecute(DB_CONNECTION, "
        INSERT OR REPLACE INTO market_data 
        (symbol, timestamp, timeframe, open, high, low, close, volume, oi_data)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      ", params = as.list(market_records[i, ]))
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error saving market data:", e$message, "\n")
    return(FALSE)
  })
}

#' Save strategy configuration
save_strategy_config <- function(strategy_name, configuration) {
  
  tryCatch({
    # Check if exists
    existing <- dbGetQuery(DB_CONNECTION, 
                          "SELECT version FROM strategy_configs WHERE strategy_name = ?",
                          params = list(strategy_name))
    
    version <- if (nrow(existing) > 0) existing$version[1] + 1 else 1
    
    # Prepare record
    config_record <- data.frame(
      strategy_name = strategy_name,
      configuration = toJSON(configuration, auto_unbox = TRUE),
      version = version,
      stringsAsFactors = FALSE
    )
    
    # Insert or update
    if (nrow(existing) > 0) {
      dbExecute(DB_CONNECTION, "
        UPDATE strategy_configs 
        SET configuration = ?, version = ?, updated_at = CURRENT_TIMESTAMP
        WHERE strategy_name = ?
      ", params = list(config_record$configuration, version, strategy_name))
    } else {
      dbWriteTable(DB_CONNECTION, "strategy_configs", config_record, append = TRUE)
    }
    
    cat("✅ Strategy configuration saved: version", version, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error saving strategy config:", e$message, "\n")
    return(FALSE)
  })
}

# ==========================================================================================================
# 📖 DATA RETRIEVAL FUNCTIONS
# ==========================================================================================================

#' Get trades from database
get_trades <- function(start_date = NULL, end_date = NULL, symbol = NULL, 
                      strategy = NULL, limit = NULL) {
  
  # Check cache first
  cache_key <- paste("trades", start_date, end_date, symbol, strategy, limit, sep = "_")
  cached_data <- get_from_cache(cache_key)
  if (!is.null(cached_data)) return(cached_data)
  
  # Build query
  query <- "SELECT * FROM trades WHERE 1=1"
  params <- list()
  
  if (!is.null(start_date)) {
    query <- paste(query, "AND exit_time >= ?")
    params <- append(params, list(format(start_date, "%Y-%m-%d")))
  }
  
  if (!is.null(end_date)) {
    query <- paste(query, "AND exit_time <= ?")
    params <- append(params, list(format(end_date, "%Y-%m-%d")))
  }
  
  if (!is.null(symbol)) {
    query <- paste(query, "AND symbol = ?")
    params <- append(params, list(symbol))
  }
  
  if (!is.null(strategy)) {
    query <- paste(query, "AND strategy = ?")
    params <- append(params, list(strategy))
  }
  
  query <- paste(query, "ORDER BY exit_time DESC")
  
  if (!is.null(limit)) {
    query <- paste(query, "LIMIT", limit)
  }
  
  # Execute query
  trades <- dbGetQuery(DB_CONNECTION, query, params = params)
  
  # Parse metadata
  if (nrow(trades) > 0) {
    trades$metadata <- lapply(trades$metadata, fromJSON)
  }
  
  # Cache result
  store_in_cache(cache_key, trades)
  
  return(trades)
}

#' Get performance metrics
get_performance_metrics <- function(metric_type = NULL, start_date = NULL, 
                                  end_date = NULL, metric_names = NULL) {
  
  # Build query
  query <- "SELECT * FROM performance_metrics WHERE 1=1"
  params <- list()
  
  if (!is.null(metric_type)) {
    query <- paste(query, "AND metric_type = ?")
    params <- append(params, list(metric_type))
  }
  
  if (!is.null(start_date)) {
    query <- paste(query, "AND timestamp >= ?")
    params <- append(params, list(format(start_date, "%Y-%m-%d")))
  }
  
  if (!is.null(end_date)) {
    query <- paste(query, "AND timestamp <= ?")
    params <- append(params, list(format(end_date, "%Y-%m-%d")))
  }
  
  if (!is.null(metric_names)) {
    placeholders <- paste(rep("?", length(metric_names)), collapse = ",")
    query <- paste(query, "AND metric_name IN (", placeholders, ")")
    params <- append(params, as.list(metric_names))
  }
  
  query <- paste(query, "ORDER BY timestamp")
  
  # Execute query
  metrics <- dbGetQuery(DB_CONNECTION, query, params = params)
  
  return(metrics)
}

#' Get market data
get_market_data <- function(symbol, timeframe, start_time, end_time = NULL) {
  
  query <- "SELECT * FROM market_data WHERE symbol = ? AND timeframe = ? AND timestamp >= ?"
  params <- list(symbol, timeframe, format(start_time, "%Y-%m-%d %H:%M:%S"))
  
  if (!is.null(end_time)) {
    query <- paste(query, "AND timestamp <= ?")
    params <- append(params, list(format(end_time, "%Y-%m-%d %H:%M:%S")))
  }
  
  query <- paste(query, "ORDER BY timestamp")
  
  # Execute query
  market_data <- dbGetQuery(DB_CONNECTION, query, params = params)
  
  # Parse OI data if present
  if (nrow(market_data) > 0 && "oi_data" %in% names(market_data)) {
    market_data$oi_data <- lapply(market_data$oi_data, function(x) {
      if (x != "") fromJSON(x) else NULL
    })
  }
  
  return(market_data)
}

#' Get strategy configuration
get_strategy_config <- function(strategy_name, version = NULL) {
  
  if (is.null(version)) {
    # Get latest version
    config <- dbGetQuery(DB_CONNECTION, "
      SELECT * FROM strategy_configs 
      WHERE strategy_name = ? 
      ORDER BY version DESC 
      LIMIT 1
    ", params = list(strategy_name))
  } else {
    # Get specific version
    config <- dbGetQuery(DB_CONNECTION, "
      SELECT * FROM strategy_configs 
      WHERE strategy_name = ? AND version = ?
    ", params = list(strategy_name, version))
  }
  
  if (nrow(config) > 0) {
    config$configuration <- fromJSON(config$configuration[1])
    return(config[1, ])
  }
  
  return(NULL)
}

# ==========================================================================================================
# 🔄 BACKUP & RECOVERY
# ==========================================================================================================

#' Backup database
backup_database <- function(backup_name = NULL) {
  
  cat("\n💾 Creating database backup...\n")
  
  tryCatch({
    # Generate backup filename
    if (is.null(backup_name)) {
      backup_name <- paste0("backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    
    backup_file <- file.path(
      dirname(DATA_MANAGER_CONFIG$database$backup_path),
      paste0(backup_name, ".db")
    )
    
    # Ensure backup directory exists
    dir.create(dirname(backup_file), recursive = TRUE, showWarnings = FALSE)
    
    # Create backup
    backup_conn <- dbConnect(RSQLite::SQLite(), backup_file)
    
    # Copy all tables
    tables <- dbListTables(DB_CONNECTION)
    
    for (table in tables) {
      data <- dbReadTable(DB_CONNECTION, table)
      dbWriteTable(backup_conn, table, data, overwrite = TRUE)
    }
    
    dbDisconnect(backup_conn)
    
    # Compress if enabled
    if (DATA_MANAGER_CONFIG$compression$enabled) {
      compress_backup(backup_file)
    }
    
    cat("✅ Backup created:", backup_file, "\n")
    
    # Log event
    log_system_event("backup_created", "data", 
                    paste("Database backup created:", backup_name))
    
    return(backup_file)
    
  }, error = function(e) {
    cat("❌ Backup failed:", e$message, "\n")
    return(NULL)
  })
}

#' Restore database from backup
restore_database <- function(backup_file) {
  
  cat("\n🔄 Restoring database from backup...\n")
  
  tryCatch({
    # Check if backup exists
    if (!file.exists(backup_file)) {
      cat("❌ Backup file not found:", backup_file, "\n")
      return(FALSE)
    }
    
    # Decompress if needed
    if (grepl("\\.gz$", backup_file)) {
      backup_file <- decompress_backup(backup_file)
    }
    
    # Close current connection
    if (exists("DB_CONNECTION") && !is.null(DB_CONNECTION)) {
      dbDisconnect(DB_CONNECTION)
    }
    
    # Backup current database
    current_backup <- paste0(DATA_MANAGER_CONFIG$database$path, ".pre_restore")
    file.copy(DATA_MANAGER_CONFIG$database$path, current_backup)
    
    # Copy backup to main database
    file.copy(backup_file, DATA_MANAGER_CONFIG$database$path, overwrite = TRUE)
    
    # Reconnect
    initialize_database()
    
    cat("✅ Database restored from:", backup_file, "\n")
    
    # Log event
    log_system_event("database_restored", "data", 
                    paste("Database restored from:", backup_file))
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Restore failed:", e$message, "\n")
    
    # Try to restore original
    if (exists("current_backup") && file.exists(current_backup)) {
      file.copy(current_backup, DATA_MANAGER_CONFIG$database$path, overwrite = TRUE)
      initialize_database()
    }
    
    return(FALSE)
  })
}

#' Compress backup file
compress_backup <- function(file_path) {
  
  # Check file size
  file_size_mb <- file.info(file_path)$size / 1024^2
  
  if (file_size_mb > DATA_MANAGER_CONFIG$compression$threshold_mb) {
    compressed_file <- paste0(file_path, ".gz")
    
    # Compress using gzip
    file_conn <- file(file_path, "rb")
    compressed_conn <- gzfile(compressed_file, "wb")
    
    writeBin(readBin(file_conn, "raw", file.info(file_path)$size), 
             compressed_conn)
    
    close(file_conn)
    close(compressed_conn)
    
    # Remove original
    file.remove(file_path)
    
    cat("  Compressed backup:", 
        round(file_size_mb, 1), "MB ->", 
        round(file.info(compressed_file)$size / 1024^2, 1), "MB\n")
  }
}

# ==========================================================================================================
# 📊 DATA EXPORT FUNCTIONS
# ==========================================================================================================

#' Export data to CSV
export_to_csv <- function(data_type, file_path = NULL, start_date = NULL, end_date = NULL) {
  
  # Generate default file path
  if (is.null(file_path)) {
    file_path <- file.path(
      SYSTEM_PATHS$reports,
      paste0(data_type, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    )
  }
  
  # Ensure directory exists
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  # Get data based on type
  data <- switch(data_type,
    "trades" = get_trades(start_date, end_date),
    "performance" = get_performance_metrics(start_date = start_date, end_date = end_date),
    "positions" = dbReadTable(DB_CONNECTION, "positions"),
    NULL
  )
  
  if (is.null(data)) {
    cat("❌ Unknown data type:", data_type, "\n")
    return(FALSE)
  }
  
  # Write to CSV
  write.csv(data, file_path, row.names = FALSE)
  
  cat("✅ Data exported to:", file_path, "\n")
  return(file_path)
}

#' Generate performance report
generate_performance_report <- function(period_days = 30, output_dir = NULL) {
  
  cat("\n📊 Generating performance report...\n")
  
  # Set output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(SYSTEM_PATHS$reports, 
                           format(Sys.time(), "%Y%m%d_performance"))
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Get data
  end_date <- Sys.Date()
  start_date <- end_date - period_days
  
  # 1. Trade summary
  trades <- get_trades(start_date, end_date)
  if (nrow(trades) > 0) {
    trade_summary <- summarize_trades(trades)
    write.csv(trade_summary, file.path(output_dir, "trade_summary.csv"), row.names = FALSE)
  }
  
  # 2. Daily performance
  daily_metrics <- aggregate_daily_performance(start_date, end_date)
  write.csv(daily_metrics, file.path(output_dir, "daily_performance.csv"), row.names = FALSE)
  
  # 3. Strategy breakdown
  if (nrow(trades) > 0) {
    strategy_performance <- analyze_strategy_performance(trades)
    write.csv(strategy_performance, file.path(output_dir, "strategy_performance.csv"), row.names = FALSE)
  }
  
  # 4. Risk metrics
  risk_metrics <- calculate_risk_metrics_report(trades)
  write.csv(risk_metrics, file.path(output_dir, "risk_metrics.csv"), row.names = FALSE)
  
  # 5. Create summary file
  create_report_summary(output_dir, period_days)
  
  cat("✅ Report generated in:", output_dir, "\n")
  
  return(output_dir)
}

# ==========================================================================================================
# 🗑️ DATA CLEANUP
# ==========================================================================================================

#' Clean old data based on retention policies
clean_old_data <- function() {
  
  cat("\n🗑️ Cleaning old data...\n")
  
  retention <- DATA_MANAGER_CONFIG$retention
  current_date <- Sys.Date()
  
  # Clean market data
  market_cutoff <- current_date - retention$tick_data_days
  deleted <- dbExecute(DB_CONNECTION, 
                      "DELETE FROM market_data WHERE timestamp < ? AND timeframe IN ('1m', '5m')",
                      params = list(format(market_cutoff, "%Y-%m-%d")))
  cat("  • Deleted", deleted, "old tick records\n")
  
  # Clean old candles
  candle_cutoff <- current_date - retention$candle_data_days
  deleted <- dbExecute(DB_CONNECTION,
                      "DELETE FROM market_data WHERE timestamp < ? AND timeframe NOT IN ('1m', '5m')",
                      params = list(format(candle_cutoff, "%Y-%m-%d")))
  cat("  • Deleted", deleted, "old candle records\n")
  
  # Clean system events
  event_cutoff <- current_date - retention$system_logs_days
  deleted <- dbExecute(DB_CONNECTION,
                      "DELETE FROM system_events WHERE timestamp < ?",
                      params = list(format(event_cutoff, "%Y-%m-%d")))
  cat("  • Deleted", deleted, "old system events\n")
  
  # Vacuum database
  dbExecute(DB_CONNECTION, "VACUUM")
  
  cat("✅ Data cleanup complete\n")
  
  # Log event
  log_system_event("data_cleanup", "maintenance", "Old data cleaned based on retention policies")
}

#' Archive old trades
archive_old_trades <- function(days_old = 365) {
  
  cutoff_date <- Sys.Date() - days_old
  
  # Get old trades
  old_trades <- get_trades(end_date = cutoff_date)
  
  if (nrow(old_trades) > 0) {
    # Save to archive file
    archive_file <- file.path(
      SYSTEM_PATHS$backup,
      paste0("trades_archive_", format(cutoff_date, "%Y%m%d"), ".rds")
    )
    
    saveRDS(old_trades, archive_file)
    
    # Delete from main database
    dbExecute(DB_CONNECTION,
             "DELETE FROM trades WHERE exit_time < ?",
             params = list(format(cutoff_date, "%Y-%m-%d")))
    
    cat("✅ Archived", nrow(old_trades), "trades to:", archive_file, "\n")
  }
}

# ==========================================================================================================
# 💾 CACHE MANAGEMENT
# ==========================================================================================================

#' Store data in cache
store_in_cache <- function(key, data) {
  
  if (!DATA_MANAGER_CONFIG$cache$enabled) return()
  
  # Check cache size
  if (length(DATA_CACHE) > 100) {
    # Remove oldest entries
    oldest_keys <- names(sort(unlist(CACHE_TIMESTAMPS))[1:20])
    for (k in oldest_keys) {
      DATA_CACHE[[k]] <<- NULL
      CACHE_TIMESTAMPS[[k]] <<- NULL
    }
  }
  
  # Store in cache
  DATA_CACHE[[key]] <<- data
  CACHE_TIMESTAMPS[[key]] <<- Sys.time()
}

#' Get data from cache
get_from_cache <- function(key) {
  
  if (!DATA_MANAGER_CONFIG$cache$enabled) return(NULL)
  
  if (key %in% names(DATA_CACHE)) {
    # Check if expired
    age <- as.numeric(difftime(Sys.time(), CACHE_TIMESTAMPS[[key]], units = "secs"))
    
    if (age < DATA_MANAGER_CONFIG$cache$ttl_seconds) {
      return(DATA_CACHE[[key]])
    } else {
      # Remove expired entry
      DATA_CACHE[[key]] <<- NULL
      CACHE_TIMESTAMPS[[key]] <<- NULL
    }
  }
  
  return(NULL)
}

#' Invalidate cache entries
invalidate_cache <- function(pattern = NULL) {
  
  if (is.null(pattern)) {
    # Clear all cache
    DATA_CACHE <<- list()
    CACHE_TIMESTAMPS <<- list()
  } else {
    # Clear matching keys
    matching_keys <- grep(pattern, names(DATA_CACHE), value = TRUE)
    for (key in matching_keys) {
      DATA_CACHE[[key]] <<- NULL
      CACHE_TIMESTAMPS[[key]] <<- NULL
    }
  }
}

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

#' Ensure data directories exist
ensure_data_directories <- function() {
  
  dirs <- c(
    SYSTEM_PATHS$data,
    SYSTEM_PATHS$logs,
    SYSTEM_PATHS$reports,
    SYSTEM_PATHS$backup
  )
  
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("Created directory:", dir, "\n")
    }
  }
}

#' Generate trade ID
generate_trade_id <- function() {
  paste0("TRD_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
         sample(10000:99999, 1))
}

#' Log system event
log_system_event <- function(event_type, category, description, data = NULL) {
  
  tryCatch({
    dbExecute(DB_CONNECTION, "
      INSERT INTO system_events (event_type, event_category, description, data)
      VALUES (?, ?, ?, ?)
    ", params = list(
      event_type,
      category,
      description,
      if (!is.null(data)) toJSON(data) else ""
    ))
  }, error = function(e) {
    # Silent fail - logging should not break system
  })
}

#' Summarize trades
summarize_trades <- function(trades) {
  
  if (nrow(trades) == 0) {
    return(data.frame())
  }
  
  summary <- data.frame(
    total_trades = nrow(trades),
    winning_trades = sum(trades$pnl > 0),
    losing_trades = sum(trades$pnl < 0),
    total_pnl = sum(trades$pnl),
    avg_pnl = mean(trades$pnl),
    win_rate = sum(trades$pnl > 0) / nrow(trades),
    avg_win = mean(trades$pnl[trades$pnl > 0]),
    avg_loss = mean(trades$pnl[trades$pnl < 0]),
    profit_factor = abs(sum(trades$pnl[trades$pnl > 0])) / 
                    abs(sum(trades$pnl[trades$pnl < 0])),
    max_win = max(trades$pnl),
    max_loss = min(trades$pnl)
  )
  
  return(summary)
}

#' Aggregate daily performance
aggregate_daily_performance <- function(start_date, end_date) {
  
  # Get trades
  trades <- get_trades(start_date, end_date)
  
  if (nrow(trades) == 0) {
    return(data.frame())
  }
  
  # Convert to daily
  trades$date <- as.Date(trades$exit_time)
  
  daily_summary <- aggregate(
    trades[c("pnl", "size")],
    by = list(date = trades$date),
    FUN = function(x) c(
      sum = sum(x),
      count = length(x),
      mean = mean(x)
    )
  )
  
  return(daily_summary)
}

#' Analyze strategy performance
analyze_strategy_performance <- function(trades) {
  
  strategies <- unique(trades$strategy)
  
  strategy_summary <- do.call(rbind, lapply(strategies, function(s) {
    s_trades <- trades[trades$strategy == s, ]
    
    data.frame(
      strategy = s,
      trades = nrow(s_trades),
      total_pnl = sum(s_trades$pnl),
      avg_pnl = mean(s_trades$pnl),
      win_rate = sum(s_trades$pnl > 0) / nrow(s_trades),
      profit_factor = abs(sum(s_trades$pnl[s_trades$pnl > 0])) / 
                      abs(sum(s_trades$pnl[s_trades$pnl < 0]))
    )
  }))
  
  return(strategy_summary)
}

#' Calculate risk metrics for report
calculate_risk_metrics_report <- function(trades) {
  
  if (nrow(trades) == 0) {
    return(data.frame())
  }
  
  # Sort by time
  trades <- trades[order(trades$exit_time), ]
  
  # Calculate cumulative P&L
  cum_pnl <- cumsum(trades$pnl)
  
  # Calculate drawdowns
  running_max <- cummax(cum_pnl)
  drawdowns <- cum_pnl - running_max
  
  risk_metrics <- data.frame(
    max_drawdown = min(drawdowns),
    max_drawdown_pct = min(drawdowns) / max(running_max) * 100,
    recovery_factor = sum(trades$pnl) / abs(min(drawdowns)),
    avg_risk_per_trade = mean(abs(trades$pnl)),
    risk_reward_ratio = mean(trades$pnl[trades$pnl > 0]) / 
                       abs(mean(trades$pnl[trades$pnl < 0])),
    consecutive_wins = max(rle(trades$pnl > 0)$lengths[rle(trades$pnl > 0)$values]),
    consecutive_losses = max(rle(trades$pnl < 0)$lengths[rle(trades$pnl < 0)$values])
  )
  
  return(risk_metrics)
}

#' Create report summary
create_report_summary <- function(output_dir, period_days) {
  
  summary_file <- file.path(output_dir, "REPORT_SUMMARY.txt")
  
  summary_text <- paste(
    "==============================================",
    "TRADING PERFORMANCE REPORT",
    "==============================================",
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("Period:", period_days, "days"),
    "",
    "Files in this report:",
    "- trade_summary.csv: Overall trade statistics",
    "- daily_performance.csv: Daily P&L breakdown",
    "- strategy_performance.csv: Performance by strategy",
    "- risk_metrics.csv: Risk analysis metrics",
    "",
    "==============================================",
    sep = "\n"
  )
  
  writeLines(summary_text, summary_file)
}

#' Get database statistics
get_database_stats <- function() {
  
  stats <- list()
  
  # Get table sizes
  tables <- dbListTables(DB_CONNECTION)
  
  for (table in tables) {
    count <- dbGetQuery(DB_CONNECTION, 
                       paste("SELECT COUNT(*) as count FROM", table))$count
    stats[[paste0(table, "_count")]] <- count
  }
  
  # Get database file size
  stats$db_size_mb <- file.info(DATA_MANAGER_CONFIG$database$path)$size / 1024^2
  
  # Get cache stats
  stats$cache_entries <- length(DATA_CACHE)
  
  return(stats)
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Initialize database on load
if (!exists("DB_CONNECTION") || is.null(DB_CONNECTION)) {
  initialize_database()
}

# Schedule automatic backup
schedule_auto_backup <- function() {
  # This would be implemented with a scheduler in production
  # For now, just a reminder
  cat("\n💡 Remember to backup regularly using DATA_MANAGER$backup()\n")
}

# Data manager interface
DATA_MANAGER <- list(
  # Storage functions
  save_trade = save_trade,
  save_positions = save_position_snapshot,
  save_metrics = save_performance_metrics,
  save_market_data = save_market_data,
  save_config = save_strategy_config,
  
  # Retrieval functions
  get_trades = get_trades,
  get_metrics = get_performance_metrics,
  get_market_data = get_market_data,
  get_config = get_strategy_config,
  
  # Maintenance
  backup = backup_database,
  restore = restore_database,
  clean = clean_old_data,
  archive = archive_old_trades,
  
  # Export
  export_csv = export_to_csv,
  generate_report = generate_performance_report,
  
  # Utilities
  stats = get_database_stats,
  clear_cache = function() invalidate_cache(),
  
  # Direct DB access
  query = function(sql, params = NULL) dbGetQuery(DB_CONNECTION, sql, params),
  execute = function(sql, params = NULL) dbExecute(DB_CONNECTION, sql, params)
)

cat("✅ DATA_MANAGER.R loaded successfully!\n")
cat("💾 Database connected and initialized\n")
cat("📊 Data storage and retrieval ready\n")
cat("🔄 Backup and recovery enabled\n")
cat("📈 Reporting system available\n")