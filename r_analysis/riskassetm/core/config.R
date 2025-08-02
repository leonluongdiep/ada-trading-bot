# ==========================================================================================================
# ⚙️ ZENTRALE SYSTEM-KONFIGURATION - TRADING SYSTEM V3
# ==========================================================================================================
# Pfad: C:\freeding\tbot202506\r_analysis\riskassetm\core\config.r
# Konsolidiert und optimiert aus system_config.r
# ==========================================================================================================

cat("⚙️ Loading Central System Configuration V3...\n")

# ==========================================================================================================
# 🔧 SYSTEM PATHS
# ==========================================================================================================

SYSTEM_PATHS <- list(
  # Base paths
  base = "C:/freeding/tbot202506/r_analysis/riskassetm/",
  core = "C:/freeding/tbot202506/r_analysis/riskassetm/core/",
  trading = "C:/freeding/tbot202506/r_analysis/riskassetm/trading/",
  analytics = "C:/freeding/tbot202506/r_analysis/riskassetm/analytics/",
  utils = "C:/freeding/tbot202506/r_analysis/riskassetm/utils/",
  
  # Data & Log paths
  data = "C:/freeding/tbot202506/bitget_data/",
  logs = "C:/freeding/tbot202506/logs/",
  reports = "C:/freeding/tbot202506/reports/",
  backup = "C:/freeding/tbot202506/backup/",
  
  # Environment file
  env_file = "C:/freeding/tbot202506/.env"
)

# ==========================================================================================================
# 🔐 API CONFIGURATION
# ==========================================================================================================

API_CONFIG <- list(
  base_url = "https://api.bitget.com",
  timeout_seconds = 10,
  max_retries = 3,
  rate_limit_delay = 0.5,
  
  # API Endpoints
  endpoints = list(
    # Market endpoints
    ticker = "/api/v2/mix/market/ticker",
    tickers = "/api/v2/mix/market/tickers", 
    orderbook = "/api/v2/mix/market/depth",
    trades = "/api/v2/mix/market/fills",
    candles = "/api/v2/mix/market/candles",
    
    # Position & Order endpoints
    positions = "/api/v2/mix/position/all-position",
    place_order = "/api/v2/mix/order/place-order",
    place_plan = "/api/v2/mix/order/place-plan-order",
    orders_pending = "/api/v2/mix/order/orders-pending",
    orders_history = "/api/v2/mix/order/orders-history",
    orders_plan_pending = "/api/v2/mix/order/orders-plan-pending",
    
    # Account endpoints
    accounts = "/api/v2/mix/account/accounts"
  )
)

# ==========================================================================================================
# 🎯 PORTFOLIO CONFIGURATION (5 COINS)
# ==========================================================================================================

PORTFOLIO_CONFIG <- list(
  "ADAUSDT_UMCBL" = list(
    name = "Cardano",
    symbol = "ADAUSDT_UMCBL",
    base_asset = "ADA",
    price_decimals = 4,
    min_size = 1,
    max_leverage = 20,
    typical_volume = 50000000,  # 50M USDT
    volatility_factor = 1.0,
    
    # Risk parameters
    default_tp_percent = 2.0,
    default_sl_percent = 1.5,
    trailing_sl_percent = 2.5,
    
    # Display
    icon = "🔷",
    color = "blue"
  ),
  
  "ALGOUSDT_UMCBL" = list(
    name = "Algorand", 
    symbol = "ALGOUSDT_UMCBL",
    base_asset = "ALGO",
    price_decimals = 4,
    min_size = 10,
    max_leverage = 20,
    typical_volume = 20000000,  # 20M USDT
    volatility_factor = 1.3,
    
    default_tp_percent = 2.0,
    default_sl_percent = 1.5,
    trailing_sl_percent = 3.5,
    
    icon = "⚫",
    color = "green"
  ),
  
  "ICPUSDT_UMCBL" = list(
    name = "Internet Computer",
    symbol = "ICPUSDT_UMCBL", 
    base_asset = "ICP",
    price_decimals = 3,
    min_size = 1,
    max_leverage = 20,
    typical_volume = 30000000,  # 30M USDT
    volatility_factor = 1.5,
    
    default_tp_percent = 2.5,
    default_sl_percent = 2.0,
    trailing_sl_percent = 3.0,
    
    icon = "🔵",
    color = "purple"
  ),
  
  "ETCUSDT_UMCBL" = list(
    name = "Ethereum Classic",
    symbol = "ETCUSDT_UMCBL",
    base_asset = "ETC", 
    price_decimals = 3,
    min_size = 1,
    max_leverage = 20,
    typical_volume = 40000000,  # 40M USDT
    volatility_factor = 1.4,
    
    default_tp_percent = 2.0,
    default_sl_percent = 1.5,
    trailing_sl_percent = 2.8,
    
    icon = "🟢",
    color = "darkgreen"
  ),
  
  "VETUSDT_UMCBL" = list(
    name = "VeChain",
    symbol = "VETUSDT_UMCBL",
    base_asset = "VET",
    price_decimals = 5,
    min_size = 100,
    max_leverage = 20,
    typical_volume = 15000000,  # 15M USDT
    volatility_factor = 1.2,
    
    default_tp_percent = 2.0,
    default_sl_percent = 1.5,
    trailing_sl_percent = 2.5,
    
    icon = "🔶",
    color = "orange"
  )
)

# Active portfolio assets
PORTFOLIO_ASSETS <- names(PORTFOLIO_CONFIG)

# ==========================================================================================================
# 🎯 TRADING PARAMETERS
# ==========================================================================================================

TRADING_PARAMS <- list(
  # Technical indicators
  rsi_period = 14,
  rsi_oversold = 30,
  rsi_overbought = 70,
  
  sma_short = 10,
  sma_long = 20,
  
  macd_fast = 12,
  macd_slow = 26,
  macd_signal = 9,
  
  bb_period = 20,
  bb_std_dev = 2,
  
  # Analysis settings
  candle_periods = 100,
  timeframes = c("5m", "15m", "1h"),
  
  # Execution delays
  order_delay_seconds = 1,
  batch_delay_seconds = 0.5
)

# ==========================================================================================================
# 🛡️ RISK MANAGEMENT
# ==========================================================================================================

RISK_LIMITS <- list(
  # Portfolio limits
  max_portfolio_exposure = 0.8,     # 80% max
  max_single_position = 0.3,        # 30% max per asset
  
  # Position sizing
  min_position_usdt = 50,
  max_position_usdt = 10000,
  
  # Emergency thresholds
  emergency_portfolio_loss = 0.1,   # 10% portfolio loss
  emergency_position_loss = 0.15,   # 15% position loss
  
  # Per-asset leverage limits
  max_leverage = list(
    "ADAUSDT_UMCBL" = 10,
    "ALGOUSDT_UMCBL" = 8,
    "ICPUSDT_UMCBL" = 10,
    "ETCUSDT_UMCBL" = 10,
    "VETUSDT_UMCBL" = 8
  )
)

# ==========================================================================================================
# 🎨 DISPLAY SETTINGS
# ==========================================================================================================

DISPLAY_CONFIG <- list(
  # Console output
  output_level = "normal",  # minimal, normal, verbose
  show_debug = FALSE,
  show_api_calls = FALSE,
  use_colors = TRUE,
  
  # Icons
  icons = list(
    success = "✅",
    error = "❌",
    warning = "⚠️",
    info = "💡",
    money = "💰",
    chart = "📊",
    shield = "🛡️",
    rocket = "🚀"
  ),
  
  # Market trend icons
  trend_icons = list(
    strong_up = "📈",
    up = "🔼",
    neutral = "➡️",
    down = "🔽",
    strong_down = "📉"
  )
)

# ==========================================================================================================
# 🛠️ HELPER FUNCTIONS
# ==========================================================================================================

# NULL coalescing operator
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# Get asset configuration
get_asset_config <- function(symbol) {
  config <- PORTFOLIO_CONFIG[[symbol]]
  if (is.null(config)) {
    warning(paste("Unknown symbol:", symbol))
    # Return first asset as default
    config <- PORTFOLIO_CONFIG[[1]]
  }
  return(config)
}

# Validate portfolio setup
validate_portfolio <- function() {
  cat("\n🔍 Validating portfolio configuration...\n")
  
  valid <- TRUE
  for (symbol in PORTFOLIO_ASSETS) {
    config <- PORTFOLIO_CONFIG[[symbol]]
    if (is.null(config)) {
      cat("❌", symbol, "- Missing configuration\n")
      valid <- FALSE
    } else {
      cat("✅", symbol, "-", config$name, "\n")
    }
  }
  
  if (valid) {
    cat("✅ Portfolio validation passed!\n")
  } else {
    cat("❌ Portfolio validation failed!\n")
  }
  
  return(valid)
}

# Create required directories
create_directories <- function() {
  dirs <- c(
    SYSTEM_PATHS$data,
    SYSTEM_PATHS$logs,
    SYSTEM_PATHS$reports,
    SYSTEM_PATHS$backup,
    paste0(SYSTEM_PATHS$data, "historical/"),
    paste0(SYSTEM_PATHS$data, "metadata/")
  )
  
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      cat("📁 Created directory:", dir, "\n")
    }
  }
}

# Load API credentials from .env
load_api_credentials <- function() {
  tryCatch({
    if (file.exists(SYSTEM_PATHS$env_file)) {
      readRenviron(SYSTEM_PATHS$env_file)
      
      credentials <- list(
        api_key = Sys.getenv("BITGET_API_KEY"),
        api_secret = Sys.getenv("BITGET_API_SECRET"),
        passphrase = Sys.getenv("BITGET_PASSPHRASE")
      )
      
      if (nchar(credentials$api_key) > 0) {
        cat("🔐 API credentials loaded successfully\n")
        return(credentials)
      }
    }
    
    cat("❌ API credentials not found in .env file\n")
    return(NULL)
    
  }, error = function(e) {
    cat("❌ Error loading credentials:", e$message, "\n")
    return(NULL)
  })
}

# ==========================================================================================================
# 🚀 INITIALIZATION
# ==========================================================================================================

# Initialize configuration
initialize_config <- function() {
  cat("\n🚀 Initializing Trading System Configuration V3...\n")
  
  # Create directories
  create_directories()
  
  # Load API credentials
  API_CREDENTIALS <<- load_api_credentials()
  
  # Validate portfolio
  validate_portfolio()
  
  # Set global flags
  CONFIG_LOADED <<- TRUE
  SYSTEM_VERSION <<- "3.0"
  CONFIG_LOAD_TIME <<- Sys.time()
  
  cat("✅ Configuration loaded successfully!\n")
  cat("📊 Portfolio:", length(PORTFOLIO_ASSETS), "assets\n")
  cat("⏰ Loaded at:", format(CONFIG_LOAD_TIME, "%H:%M:%S"), "\n")
  
  return(TRUE)
}

# Auto-initialize
if (!exists("CONFIG_LOADED")) {
  initialize_config()
}

# Display config summary
show_config <- function() {
  cat("\n⚙️ === SYSTEM CONFIGURATION === ⚙️\n")
  cat("Version:", SYSTEM_VERSION, "\n")
  cat("Base Path:", SYSTEM_PATHS$base, "\n")
  cat("Portfolio Assets:", paste(PORTFOLIO_ASSETS, collapse = ", "), "\n")
  cat("API Status:", if(!is.null(API_CREDENTIALS)) "🟢 Connected" else "🔴 Not Connected", "\n")
  cat("Risk Limits: Max Exposure", RISK_LIMITS$max_portfolio_exposure * 100, "%\n")
}

cat("✅ CONFIG.R loaded successfully!\n")