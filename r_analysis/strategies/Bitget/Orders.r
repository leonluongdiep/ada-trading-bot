
#---------------------------------------------------------------------------------------------------------------------#
# ========================= init load =============================='
#-------------------------------------------------------------------------------------------------------#


source("c:/freeding/tbot202506/r_analysis/clean_console.R")

# Lade das final gefixtе System
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")


# =================== init analysis ======================

# Teste nur die Analyse (KEINE ORDERS)
analysis <- complete_trading_analysis('ADAUSDT_UMCBL')

# Schaue deine aktuelle Position
get_current_positions('ADAUSDT_UMCBL')



#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# =================== place orders =================================
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Statt der kompletten Position (7000), nur Teilmenge absichern:

# 1. Nur 3000 Kontrakte absichern (konservativ)
tp_sl_results <- place_intelligent_tp_sl('ADAUSDT_UMCBL', analysis, tp_percent = 3, sl_percent = 3.0)
# Aber mit manueller size-Anpassung im Script


# 50% bei Entry-Preis schließen, 50% laufen lassen
place_breakeven_orders('ADAUSDT_UMCBL', 'long', '3000', 0.5825)


#--------------------------------------------------------------------------------------------------------------------------------#
# ----------------------------- indicator collection --------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#


# Am Ende Ihres complete_trading_analysis_v3.r Scripts:
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")

# 1. Complete Enhanced Analysis:
enhanced_analysis <- complete_trading_analysis_enhanced('ADAUSDT_UMCBL')

# 2. Individual Market Data:
market_data <- get_enhanced_market_data('ADAUSDT_UMCBL')

# 3. Sentiment Details:
print(market_data$sentiment)

# 4. Trades Analysis:
print(market_data$trades)
