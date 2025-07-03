

# Ihre bestehenden Trading-System Dateien laden:
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r") 
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")


# Umfassenden Test starten:
source("c:/freeding/tbot202506/r_analysis/comprehensive_trading_test.r")

# Speichern Sie das Script als:
# C:/freeding/tbot202506/r_analysis/comprehensive_trading_test.r


setwd("C:/freeding/tbot202506")
source("r_analysis/comprehensive_trading_test.r")


place_strategic_limit_order('ADAUSDT_UMCBL', 'long', '5000', 0.5000)
get_current_open_orders('ADAUSDT_UMCBL')

