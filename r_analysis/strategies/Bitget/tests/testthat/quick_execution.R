

# System neu laden
source("C:/freeding/tbot202506/r_analysis/strategies/Bitget/trading_execution_hub.r")

# Full Analysis testen
execute_trading_system(mode = "full")

execute_trading_system(mode = "quick")




# Direkte Ausführung
execute_trading_system(mode = "quick")    # Schneller Market Check
execute_trading_system(mode = "full")     # Vollständige Analyse
execute_trading_system(mode = "risk")     # Risk Assessment
execute_trading_system(mode = "oi")       # OI Analysis
execute_trading_system(mode = "sentiment") # Sentiment Analysis

# Interactive Mode
launch_interactive_interface()
execute quick
diagnose
status
risk
help
