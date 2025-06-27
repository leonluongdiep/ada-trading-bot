
#install.packages('tidyverse')


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
#tp_sl_results <- place_intelligent_tp_sl('ADAUSDT_UMCBL', analysis, tp_percent = 3, sl_percent = 3.0)
# Aber mit manueller size-Anpassung im Script


# 50% bei Entry-Preis schließen, 50% laufen lassen
#place_breakeven_orders('ADAUSDT_UMCBL', 'long', '3000', 0.5825)


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




#--------------------------------------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------------------------#
# ----------------------------- ML System laden --------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#

# ========================= init load =============================='
#-------------------------------------------------------------------------------------------------------#


source("c:/freeding/tbot202506/r_analysis/clean_console.R")

# Lade das final gefixtе System
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")

# ----------------------------- indicator collection --------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#


# Am Ende Ihres complete_trading_analysis_v3.r Scripts:
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")


# 2. GARANTIERT funktionierende Analyse
results <- working_ml_analysis('ADAUSDT_UMCBL', '1h', 30)


#--------------------------------------------------------------------------------------------------------------------------------#
# -----------------------------  bitget_data_downloader.r --------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#


# 1. Laden Sie das neue Script
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_data_downloader.r")

# 2. Schneller Test (funktioniert garantiert!)
test <- quick_collection('ADAUSDT_UMCBL', '1h', 50)

# 3. Vollständige Datensammlung
data <- complete_data_collection('ADAUSDT_UMCBL')

# 4. Nur Live-Daten
live <- collect_live_market_data('ADAUSDT_UMCBL')



#--------------------------------------------------------------------------------------------------------------------------------#
# ----------------------------- bitget_data_explorer--------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#




# 1. Laden Sie das Explorer Script
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_data_explorer.r")

# 2. Vollständige Exploration (empfohlen)
results <- complete_data_exploration('ADAUSDT_UMCBL')

# 3. Schnelle Übersicht
quick <- quick_exploration('ADAUSDT_UMCBL')

# 4. Manuelle Datenladung für eigene Analysen
data <- load_all_bitget_data('ADAUSDT_UMCBL')


