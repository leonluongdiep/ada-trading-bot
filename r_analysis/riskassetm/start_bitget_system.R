# 1. Navigieren Sie zum core Verzeichnis
setwd("C:/freeding/tbot202506/r_analysis/riskassetm/core")

# 2. Laden Sie das Hauptmodul
source("main.r")

# 3. Starten Sie das System:

# Option A: Vollständiges interaktives System
run_trading_system()

# Option B: Automatisiertes Trading (Dry-Run)
quick_start_auto(dry_run = TRUE)

# Option C: Nur manueller Modus
quick_start_manual()
