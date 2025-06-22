
source("c:/freeding/tbot202506/r_analysis/clean_console.R")

# Lade das final gefixtе System
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")



# Teste nur die Analyse (KEINE ORDERS)
analysis <- complete_trading_analysis('ADAUSDT_UMCBL')

# Schaue deine aktuelle Position
get_current_positions('ADAUSDT_UMCBL')




# Statt der kompletten Position (7000), nur Teilmenge absichern:

# 1. Nur 3000 Kontrakte absichern (konservativ)
tp_sl_results <- place_intelligent_tp_sl('ADAUSDT_UMCBL', analysis, tp_percent = 2, sl_percent = 3.0)
# Aber mit manueller size-Anpassung im Script


# 50% bei Entry-Preis schließen, 50% laufen lassen
place_breakeven_orders('ADAUSDT_UMCBL', 'long', '1000', 0.5725)
