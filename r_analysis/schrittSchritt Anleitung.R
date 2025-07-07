# Console Output Management (für saubere Ausgaben)
source("c:/freeding/tbot202506/r_analysis/r_console_output_manager.r")

# Optional: Silent Mode für weniger Output
# start_silent_mode("filter")


start_silent_mode("file")
# KRITISCH: Complete Trading Analysis V3 (mit TP/SL Funktionen)
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")


# Enhanced System mit Encoding-Fixes und Multi-Asset Support
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r")

source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")


# Prüfen, welche Funktionen verfügbar sind
exists("complete_trading_analysis")
exists("complete_trading_analysis_enhanced")
exists("get_enhanced_market_data")

cat("🚀 Starting ADA analysis...\n")
ada_analysis <- complete_trading_analysis_enhanced('ADAUSDT_UMCBL')
