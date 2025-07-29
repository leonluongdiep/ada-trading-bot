# ==========================================================================================================
# ⚙️ CENTRAL SYSTEM CONFIGURATION - TRADING SYSTEM V2
# ==========================================================================================================
# 
# ZWECK: Zentrale Konfigurationsdatei für das gesamte Trading System
# KONSOLIDIERT: Alle Parameter aus 11 separaten Dateien
# VERSION: 2.0 - Cleaned & Unified
# 
# ==========================================================================================================

cat("⚙️ Loading Central System Configuration V2...\n")

# ==========================================================================================================
# 🔐 API CONFIGURATION
# ==========================================================================================================

# API Connection Settings
API_CONFIG <- list(
  base_url = "https://api.bitget.com",
  timeout_seconds = 10,
  order_delay_seconds = 1,
  max_retries = 3,
  rate_limit_delay = 0.5
)

# API Endpoints
API_ENDPOINTS <- list(
  ticker = "/api/mix/v1/market/ticker",
  orderbook = "/api/mix/v1/market/depth", 
  trades = "/api/mix/v1/market/fills",
  open_interest = "/api/mix/v1/market/open-interest",
  positions = "/api/mix/v1/position/allPosition",
  place_order = "/api/mix/v1/order/placeOrder",
  place_plan = "/api/mix/v1/plan/placePlan",
  current_plan = "/api/mix/v1/plan/currentPlan",
  contracts = "/api/v2/mix/market/contracts"
)

# ==========================================================================================================
# 🎯 MULTI-ASSET CONFIGURATION
# ==========================================================================================================

# Comprehensive Asset Configuration
MULTI_ASSET_CONFIG <- list(
  "ADAUSDT_UMCBL" = list(
    name = "Cardano",
    symbol = "ADAUSDT_UMCBL",
    base_asset = "ADA",
    quote_asset = "USDT",
    price_decimals = 4,
    tick_size = 0.0001,
    min_size = 1,
    max_leverage = 20,
    typical_volume_threshold = 50000000,  # 50M USDT
    volatility_factor = 1.0,              # Base volatility
    psychological_levels = c(0.50, 0.60, 0.70, 0.80, 0.90, 1.00),
    support_resistance_strength = 1.2,
    icon = "🔷",
    color_theme = "blue"
  ),
  
  "ALGOUSDT_UMCBL" = list(
    name = "Algorand", 
    symbol = "ALGOUSDT_UMCBL",
    base_asset = "ALGO",
    quote_asset = "USDT",
    price_decimals = 4,
    tick_size = 0.0001,
    min_size = 10,
    max_leverage = 20,
    typical_volume_threshold = 20000000,  # 20M USDT
    volatility_factor = 1.3,              # Higher volatility than ADA
    psychological_levels = c(0.15, 0.20, 0.25, 0.30, 0.35, 0.40),
    support_resistance_strength = 1.1,
    icon = "⚫",
    color_theme = "green"
  ),
  
  "BTCUSDT_UMCBL" = list(
    name = "Bitcoin",
    symbol = "BTCUSDT_UMCBL",
    base_asset = "BTC", 
    quote_asset = "USDT",
    price_decimals = 2,
    tick_size = 0.01,
    min_size = 0.001,
    max_leverage = 125,
    typical_volume_threshold = 1000000000,  # 1B USDT
    volatility_factor = 0.8,                # Lower volatility
    psychological_levels = c(50000, 55000, 60000, 65000, 70000, 75000),
    support_resistance_strength = 1.5,
    icon = "🟠",
    color_theme = "orange"
  ),
  
  "ETHUSDT_UMCBL" = list(
    name = "Ethereum",
    symbol = "ETHUSDT_UMCBL",
    base_asset = "ETH",
    quote_asset = "USDT", 
    price_decimals = 3,
    tick_size = 0.001,
    min_size = 0.01,
    max_leverage = 75,
    typical_volume_threshold = 500000000,  # 500M USDT
    volatility_factor = 0.9,               # Moderate volatility
    psychological_levels = c(3000, 3200, 3400, 3600, 3800, 4000),
    support_resistance_strength = 1.3,
    icon = "🔵",
    color_theme = "purple"
  )
)

# Active Portfolio Assets
PORTFOLIO_ASSETS <- c("ADAUSDT_UMCBL", "ALGOUSDT_UMCBL")

# Supported Assets (for expansion)
SUPPORTED_SYMBOLS <- names(MULTI_ASSET_CONFIG)

# ==========================================================================================================
# 📊 TRADING PARAMETERS
# ==========================================================================================================

# Core Trading Settings
TRADING_CONFIG <- list(
  # Default Targets
  default_tp_percent = 2.0,               # Take Profit %
  default_sl_percent = 1.5,               # Stop Loss %
  default_symbol = "ADAUSDT_UMCBL",       # Primary trading pair
  
  # Technical Analysis
  rsi_period = 14,                        # RSI calculation period
  sma_short_period = 10,                  # Short SMA
  sma_long_period = 20,                   # Long SMA
  macd_fast = 12,                         # MACD Fast EMA
  macd_slow = 26,                         # MACD Slow EMA
  macd_signal = 9,                        # MACD Signal line
  
  # Bollinger Bands
  bb_period = 20,                         # BB period
  bb_std_dev = 2,                         # BB standard deviations
  
  # Timeframes & Candles
  default_timeframes = c("5m", "15m", "1h"),
  default_candle_periods = 100,
  historical_lookback_hours = 24,
  
  # Volume Analysis
  volume_sma_period = 20,
  high_volume_threshold = 1.5,            # 1.5x average volume
  
  # Signal Thresholds
  rsi_oversold = 30,
  rsi_overbought = 70,
  trend_confirmation_periods = 3
)

# ==========================================================================================================
# 🛡️ RISK MANAGEMENT CONFIGURATION
# ==========================================================================================================

# Position Size Limits
POSITION_LIMITS <- list(
  max_portfolio_exposure = 0.8,           # 80% max portfolio exposure
  max_single_position = 0.3,              # 30% max single position
  max_leverage_per_asset = list(
    "ADAUSDT_UMCBL" = 10,                # Conservative for ADA
    "ALGOUSDT_UMCBL" = 8,                # More conservative for ALGO
    "BTCUSDT_UMCBL" = 15,                # Moderate for BTC
    "ETHUSDT_UMCBL" = 12                 # Moderate for ETH
  ),
  min_position_usdt = 50,                 # Minimum position size
  max_position_usdt = 10000               # Maximum position size
)

# Trailing Stop Loss Configuration
TRAILING_SL_CONFIG <- list(
  # Asset-specific trailing percentages
  default_trailing_percent = list(
    "ADAUSDT_UMCBL" = 2.5,               # Conservative for ADA
    "ALGOUSDT_UMCBL" = 3.5,              # More room for ALGO volatility
    "BTCUSDT_UMCBL" = 2.0,               # Tight for BTC
    "ETHUSDT_UMCBL" = 2.8                # Moderate for ETH
  ),
  
  # Dynamic adjustment factors
  volatility_adjustment = TRUE,
  volume_adjustment = TRUE,
  oi_level_adjustment = TRUE,
  
  # Volatility-based adjustments
  high_volatility_multiplier = 1.5,      # Widen SL during high volatility
  low_volatility_multiplier = 0.7,       # Tighten SL during low volatility
  volatility_threshold_high = 5.0,       # 5% daily change = high volatility
  volatility_threshold_low = 1.0,        # 1% daily change = low volatility
  
  # Emergency protection
  emergency_sl_percent = 5.0,             # Emergency SL if no protection
  auto_protect_threshold = 100            # Auto-protect positions > 100 USDT
)

# Risk Assessment Parameters
RISK_ASSESSMENT_CONFIG <- list(
  correlation_threshold = 0.7,            # High correlation warning
  var_confidence_level = 0.95,            # VaR confidence level
  max_daily_loss_percent = 3.0,           # Max daily loss tolerance
  position_concentration_warning = 0.4,   # Warn if single position > 40%
  leverage_warning_threshold = 10         # Warn if leverage > 10x
)

# ==========================================================================================================
# 📈 OI ANALYTICS CONFIGURATION
# ==========================================================================================================

# Open Interest Analysis Settings
OI_ANALYTICS_CONFIG <- list(
  # Heatmap Settings
  default_price_bins = 120,               # Price levels for heatmap
  price_range_multiplier = 1.2,           # Price range expansion factor
  
  # OI Concentration Factors
  base_concentration_weight = 0.2,        # Base OI concentration
  current_price_weight = 0.25,            # Weight around current price
  psychological_level_weight = 0.15,      # Weight for round numbers
  high_low_weight = 0.12,                 # Weight for 24h high/low
  fibonacci_weight = 0.1,                 # Weight for Fibonacci levels
  
  # Magnet Strength Calculation
  magnet_strength_periods = 24,           # Hours for magnet analysis
  top_magnets_count = 10,                 # Top OI levels to analyze
  
  # Flow Analysis
  oi_flow_periods = 2,                    # Hours for OI flow analysis
  flow_interval_minutes = 15,             # Flow data intervals
  
  # Signal Generation
  bullish_flow_threshold = 0.3,           # OI change for bullish signal
  bearish_flow_threshold = -0.2,          # OI change for bearish signal
  momentum_strength_high = 70,            # High momentum threshold
  momentum_strength_medium = 40           # Medium momentum threshold
)

# Institutional Table Settings
INSTITUTIONAL_CONFIG <- list(
  table_refresh_seconds = 300,            # 5 minutes auto-refresh
  export_csv_path = "c:/freeding/tbot202506/oi_analysis/",
  max_historical_records = 1000,
  confidence_score_factors = list(
    oi_strength = 0.5,
    volume_factor = 0.3, 
    distance_factor = 0.2
  )
)

# ==========================================================================================================
# 🎨 DISPLAY CONFIGURATION
# ==========================================================================================================

# Console Output Settings
DISPLAY_CONFIG <- list(
  # Output Levels
  output_level = "normal",                # verbose, normal, minimal, silent
  show_debug_info = FALSE,
  show_api_calls = FALSE,
  show_calculations = TRUE,
  
  # Table Formatting
  table_width = 80,
  header_separator = "=",
  subheader_separator = "-",
  
  # Color Themes
  use_colors = TRUE,
  color_scheme = "default",               # default, dark, light
  
  # Chart Settings
  interactive_charts = TRUE,
  chart_theme = "minimal",
  chart_height = 600,
  chart_width = 900,
  
  # Progress Indicators
  show_progress_bars = TRUE,
  show_timestamps = TRUE,
  show_execution_time = TRUE
)

# Icon Mapping
ASSET_ICONS <- list(
  "ADA" = "🔷", "ALGO" = "⚫", "BTC" = "🟠", "ETH" = "🔵",
  "success" = "✅", "warning" = "⚠️", "error" = "❌", 
  "info" = "💡", "money" = "💰", "chart" = "📊"
)

# Status Icons
STATUS_ICONS <- list(
  "BULLISH" = "🟢", "BEARISH" = "🔴", "NEUTRAL" = "🟡",
  "HIGH" = "🔴", "MEDIUM" = "🟡", "LOW" = "🟢",
  "STRONG" = "💪", "WEAK" = "📉", "LOADING" = "⏳"
)

# ==========================================================================================================
# 🚀 EXECUTION CONFIGURATION
# ==========================================================================================================

# System Performance Settings
PERFORMANCE_CONFIG <- list(
  max_concurrent_requests = 5,
  batch_size = 50,
  memory_cleanup_interval = 3600,        # 1 hour
  cache_expiry_seconds = 300,             # 5 minutes
  parallel_processing = TRUE,
  max_workers = 4
)

# File Paths
FILE_PATHS <- list(
  base_path = "c:/freeding/tbot202506/",
  logs_path = "c:/freeding/tbot202506/logs/",
  data_path = "c:/freeding/tbot202506/bitget_data/",
  reports_path = "c:/freeding/tbot202506/reports/",
  config_path = "c:/freeding/tbot202506/config/",
  backup_path = "c:/freeding/tbot202506/backup/"
)

# Auto-creation of directories
AUTO_CREATE_DIRS <- c(
  FILE_PATHS$logs_path,
  FILE_PATHS$data_path,
  paste0(FILE_PATHS$data_path, "historical/"),
  paste0(FILE_PATHS$data_path, "metadata/"),
  FILE_PATHS$reports_path,
  FILE_PATHS$backup_path
)

# ==========================================================================================================
# 🔧 HELPER FUNCTIONS
# ==========================================================================================================

# Get asset configuration
get_asset_config <- function(symbol) {
  if (!symbol %in% names(MULTI_ASSET_CONFIG)) {
    warning(paste("Symbol", symbol, "not in configuration. Using ADA defaults."))
    return(MULTI_ASSET_CONFIG[["ADAUSDT_UMCBL"]])
  }
  return(MULTI_ASSET_CONFIG[[symbol]])
}

# Get trading parameters for asset
get_trading_params <- function(symbol) {
  config <- get_asset_config(symbol)
  return(list(
    tp_percent = TRADING_CONFIG$default_tp_percent,
    sl_percent = TRADING_CONFIG$default_sl_percent,
    trailing_percent = TRAILING_SL_CONFIG$default_trailing_percent[[symbol]],
    max_leverage = POSITION_LIMITS$max_leverage_per_asset[[symbol]],
    volatility_factor = config$volatility_factor
  ))
}

# Validate symbol
is_supported_symbol <- function(symbol) {
  return(symbol %in% SUPPORTED_SYMBOLS)
}

# Get all portfolio symbols
get_portfolio_symbols <- function() {
  return(PORTFOLIO_ASSETS)
}

# Get display settings
get_display_settings <- function() {
  return(DISPLAY_CONFIG)
}

# Create directories if they don't exist
create_system_directories <- function() {
  for (dir_path in AUTO_CREATE_DIRS) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
  }
}

# ==========================================================================================================
# 🎯 INITIALIZATION
# ==========================================================================================================

# System initialization
initialize_system_config <- function() {
  cat("🔧 Initializing Trading System V2 Configuration...\n")
  
  # Create necessary directories
  create_system_directories()
  
  # Validate configuration
  validate_config()
  
  # Set global flags
  SYSTEM_CONFIG_LOADED <<- TRUE
  SYSTEM_VERSION <<- "2.0"
  SYSTEM_INIT_TIME <<- Sys.time()
  
  cat("✅ System Configuration V2 loaded successfully!\n")
  cat(sprintf("   📊 Assets configured: %d\n", length(MULTI_ASSET_CONFIG)))
  cat(sprintf("   💼 Portfolio assets: %d\n", length(PORTFOLIO_ASSETS)))
  cat(sprintf("   ⏰ Initialized at: %s\n", format(SYSTEM_INIT_TIME, "%H:%M:%S")))
  
  return(TRUE)
}

# Configuration validation
validate_config <- function() {
  # Validate required fields
  required_fields <- c("MULTI_ASSET_CONFIG", "TRADING_CONFIG", "PORTFOLIO_ASSETS")
  
  for (field in required_fields) {
    if (!exists(field)) {
      stop(paste("Missing required configuration:", field))
    }
  }
  
  # Validate portfolio assets are in config
  for (asset in PORTFOLIO_ASSETS) {
    if (!asset %in% names(MULTI_ASSET_CONFIG)) {
      stop(paste("Portfolio asset", asset, "not found in MULTI_ASSET_CONFIG"))
    }
  }
  
  cat("✅ Configuration validation passed\n")
  return(TRUE)
}

# ==========================================================================================================
# 📋 CONFIGURATION SUMMARY
# ==========================================================================================================

display_config_summary <- function() {
  cat("\n⚙️ TRADING SYSTEM V2 CONFIGURATION SUMMARY\n")
  cat(strrep("=", 60), "\n")
  
  cat("📊 ASSETS:\n")
  for (symbol in names(MULTI_ASSET_CONFIG)) {
    config <- MULTI_ASSET_CONFIG[[symbol]]
    active <- if (symbol %in% PORTFOLIO_ASSETS) "ACTIVE" else "AVAILABLE"
    cat(sprintf("   %s %s (%s) - %s\n", 
                config$icon, config$name, config$base_asset, active))
  }
  
  cat("\n🎯 TRADING SETTINGS:\n")
  cat(sprintf("   Default TP: %.1f%% | Default SL: %.1f%%\n", 
              TRADING_CONFIG$default_tp_percent, TRADING_CONFIG$default_sl_percent))
  cat(sprintf("   RSI Period: %d | SMA Periods: %d/%d\n",
              TRADING_CONFIG$rsi_period, TRADING_CONFIG$sma_short_period, TRADING_CONFIG$sma_long_period))
  
  cat("\n🛡️ RISK MANAGEMENT:\n")
  cat(sprintf("   Max Portfolio Exposure: %.0f%%\n", POSITION_LIMITS$max_portfolio_exposure * 100))
  cat(sprintf("   Max Single Position: %.0f%%\n", POSITION_LIMITS$max_single_position * 100))
  
  cat(strrep("=", 60), "\n")
}

# Auto-initialize when loaded
if (!exists("SYSTEM_CONFIG_LOADED")) {
  initialize_system_config()
}

# ==========================================================================================================
# ✅ CONFIGURATION COMPLETE
# ==========================================================================================================

cat("✅ CENTRAL SYSTEM CONFIGURATION V2 READY!\n")
cat("🎯 All 11 original files consolidated into unified config\n")
cat("📋 Use display_config_summary() to view complete settings\n")
cat(strrep("=", 60), "\n")