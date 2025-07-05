

# 1. Trading-System laden:
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/complete_trading_analysis_v3.r")
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/bitget_system_fixes.r") 
source("c:/freeding/tbot202506/r_analysis/strategies/Bitget/corrected_bitget_collector_v6.r")

# 2. Comprehensive Test ausführen:
source("c:/freeding/tbot202506/r_analysis/comprehensive_trading_test.r")


# Speichern Sie das Script als:
# C:/freeding/tbot202506/r_analysis/comprehensive_trading_test.r



#place_strategic_limit_order('ADAUSDT_UMCBL', 'long', '5000', 0.5000)
#get_current_open_orders('ADAUSDT_UMCBL')




# SOFORTIGE CONSOLE WIEDERHERSTELLUNG
sink(type = "message")
sink(type = "output")
cat("✅ Console wiederhergestellt!\n")

version
# NOTFALL CONSOLE RESET
tryCatch({
  sink(type = "message")
  sink(type = "output") 
  sink()  # Reset all sinks
}, error = function(e) {
  cat("Versuche Notfall-Reset...\n")
})
sink()  # Zusätzlicher Reset
cat("✅ Console sollte jetzt funktionieren!\n")



# KOMPLETTER SINK RESET
for(i in 1:10) {
  tryCatch(sink(), error = function(e) NULL)
  tryCatch(sink(type = "message"), error = function(e) NULL)
}
cat("🔧 Kompletter Reset durchgeführt!\n")