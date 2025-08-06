
# hub System neu laden
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/trading_execution_hub_v2_with_console_mgmt.r")

#summary function
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/improved_summary_functions.r")

# 5coins
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/fix_coin_names_api.r")

#order management
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/advanced_order_management.r")


#bitget_api
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_api_fixes.r")

#complete_system
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_system_fix.r")



#summary_system
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/fixed_summary_system.r")

#summary_system
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/multi_asset_fix_complete.r")



force_portfolio_update()



# Alle Positionen schützen
protect_position("ETCUSDT_UMCBL")    # 41 USDT Gewinn sichern
protect_position("ALGOUSDT_UMCBL")   # 5.96 USDT Gewinn sichern  
protect_position("ADAUSDT_UMCBL")    # 6.16 USDT Gewinn sichern
protect_position("VETUSDT_UMCBL")    # 5.48 USDT Gewinn sichern
protect_position("ICPUSDT_UMCBL")    # 12.40 USDT Gewinn sichern
protect_position("BTCUSDT_UMCBL")    # 12.40 USDT Gewinn sichern
protect_position("ETHUSDT_UMCBL")    # 12.40 USDT Gewinn sichern



execute_summary_only_fixed()             # Fixed summary function
daily_market_check_fixed()               # Fixed daily check
quick_market_check()                     # Quick market overview
full_daily_check()                       # Complete daily routine

prices_only() 


get_current_positions(debug = TRUE)     


#headmaps system
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/fixed_algo_heatmap.r")

source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/heatmap_explanation.r")
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/ggplot2_heatmap_viz.r")



explain_all()                 # Komplette Score-Erklärung
algo_oi_dashboard()          # ALGO Dashboard


# Heatmap-Daten erstellen (braucht fixed_algo_heatmap.r)
heatmap_result <- algo_heatmap()
# Visualisierung erstellen (braucht ggplot2_heatmap_viz.r)
plots <- visualize_my_heatmap(heatmap_result)
print(plots$interactive)



#---- ETC -------head map
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/etc_heatmap.r")
ETC_oi_dashboard()

#---- ADA -----head map
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/ADA_heatmap.r")
ADA_oi_dashboard()

#----- BTC ------head map
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/BTC_heatmap.r")
BTC_oi_dashboard()


#----- ETH ------head map
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/ETH_heatmap.r")
ETH_oi_dashboard()



getwd()



# ❌ VORHER: Einzelne Asset-Verarbeitung
for (coin in PORTFOLIO_ASSETS) {
  execute_silent(coin, 'full')  # Nur letztes Ergebnis bleibt
}

# ✅ NACHHER: Batch-Verarbeitung
execute_silent(PORTFOLIO_ASSETS, 'full')  # Alle Assets zusammen


# 1. Fixes anwenden
apply_complete_multi_asset_fixes()

# 2. Vollständige Analyse (alle 5 Assets)
complete_market_check()

# 3. Schnelle Überprüfungt
quick_market_check()

# 4. Tägliche erweiterte Analyse
daily_market_check_enhanced()

