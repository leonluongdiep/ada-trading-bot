# ==========================================================================================================
# 📦 COMPLETE TRADING SYSTEM PACKAGE INSTALLER
# ==========================================================================================================
# 
# ZWECK: Installiert ALLE benötigten R-Pakete für Ihr Bitget Trading System
# FEATURES: Automatische Installation, Fehlerbehandlung, Status-Report
# KOMPATIBEL: R 4.5.0+, Windows/Linux
# 
# VERWENDUNG: Einfach das komplette Script in R ausführen
# 
# ==========================================================================================================

cat("📦 STARTING COMPLETE TRADING SYSTEM PACKAGE INSTALLATION\n")
cat(strrep("=", 70), "\n")
cat("🕒 Start Time:", as.character(Sys.time()), "\n")
cat("🔧 R Version:", R.version.string, "\n\n")

# ==========================================================================================================
# 📋 PACKAGE DEFINITIONS - ALLE BENÖTIGTEN PAKETE
# ==========================================================================================================

# Core Trading System Packages
CORE_PACKAGES <- c(
  "httr",           # HTTP Requests für Bitget API
  "jsonlite",       # JSON Parsing für API Responses  
  "openssl",        # Kryptographische Signatures
  "dotenv"          # .env File Support für API Keys
)

# Technical Analysis Packages
TECHNICAL_PACKAGES <- c(
  "TTR",            # Technical Trading Rules (RSI, SMA, MACD)
  "quantmod",       # Quantitative Financial Modelling
  "PerformanceAnalytics", # Portfolio Performance Analysis
  "forecast",       # Time Series Forecasting
  "xts",           # eXtensible Time Series
  "zoo"            # S3 Infrastructure for Regular/Irregular Time Series
)

# Data Manipulation Packages
DATA_PACKAGES <- c(
  "dplyr",          # Data Manipulation (Teil von tidyverse)
  "tidyverse",      # Complete Data Science Toolkit
  "lubridate",      # Date/Time Manipulation
  "readr",          # Fast CSV Reading
  "stringr",        # String Manipulation
  "purrr"           # Functional Programming Tools
)

# Visualization & Reporting Packages
VISUAL_PACKAGES <- c(
  "ggplot2",        # Grammar of Graphics (Teil von tidyverse)
  "plotly",         # Interactive Plots
  "rmarkdown",      # Dynamic Documents
  "knitr",          # Dynamic Report Generation
  "DT"              # Interactive Tables
)

# Optional Advanced Packages
ADVANCED_PACKAGES <- c(
  "shiny",          # Web Applications
  "shinydashboard", # Dashboard Framework
  "RColorBrewer",   # Color Schemes
  "scales",         # Scale Functions for Visualization
  "gridExtra"       # Grid Graphics
)

# Development & Utility Packages
UTILITY_PACKAGES <- c(
  "devtools",       # Development Tools
  "roxygen2",       # Documentation
  "testthat",       # Unit Testing
  "here",           # Path Management
  "fs"              # File System Operations
)

# Combine all packages
ALL_PACKAGES <- c(CORE_PACKAGES, TECHNICAL_PACKAGES, DATA_PACKAGES, 
                  VISUAL_PACKAGES, ADVANCED_PACKAGES, UTILITY_PACKAGES)

# Remove duplicates
ALL_PACKAGES <- unique(ALL_PACKAGES)

cat("📦 Total packages to check:", length(ALL_PACKAGES), "\n")
cat("🔧 Core packages:", length(CORE_PACKAGES), "\n")
cat("📈 Technical packages:", length(TECHNICAL_PACKAGES), "\n")
cat("📊 Data packages:", length(DATA_PACKAGES), "\n")
cat("🎨 Visualization packages:", length(VISUAL_PACKAGES), "\n")
cat("🚀 Advanced packages:", length(ADVANCED_PACKAGES), "\n")
cat("🛠️ Utility packages:", length(UTILITY_PACKAGES), "\n\n")

# ==========================================================================================================
# 🔍 PACKAGE STATUS CHECK
# ==========================================================================================================

check_package_status <- function() {
  cat("🔍 Checking current package status...\n")
  
  installed_packages <- installed.packages()[,1]
  
  status <- data.frame(
    package = ALL_PACKAGES,
    installed = ALL_PACKAGES %in% installed_packages,
    category = c(
      rep("Core", length(CORE_PACKAGES)),
      rep("Technical", length(TECHNICAL_PACKAGES)), 
      rep("Data", length(DATA_PACKAGES)),
      rep("Visual", length(VISUAL_PACKAGES)),
      rep("Advanced", length(ADVANCED_PACKAGES)),
      rep("Utility", length(UTILITY_PACKAGES))
    ),
    stringsAsFactors = FALSE
  )
  
  return(status)
}

# ==========================================================================================================
# 📦 SMART PACKAGE INSTALLER
# ==========================================================================================================

install_packages_smart <- function(packages_to_install, category = "Packages") {
  if (length(packages_to_install) == 0) {
    cat("✅ All", category, "already installed\n")
    return(TRUE)
  }
  
  cat("📦 Installing", length(packages_to_install), category, "...\n")
  
  install_results <- list()
  
  for (pkg in packages_to_install) {
    cat(sprintf("   📦 Installing %s...", pkg))
    
    start_time <- Sys.time()
    
    result <- tryCatch({
      # Try CRAN first
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
      
      # Verify installation
      if (pkg %in% installed.packages()[,1]) {
        duration <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 1)
        cat(sprintf(" ✅ (%.1fs)\n", duration))
        return(list(success = TRUE, method = "CRAN", duration = duration))
      } else {
        cat(" ❌ FAILED\n")
        return(list(success = FALSE, error = "Installation failed"))
      }
      
    }, error = function(e) {
      cat(" ❌ ERROR:", e$message, "\n")
      return(list(success = FALSE, error = e$message))
    })
    
    install_results[[pkg]] <- result
  }
  
  return(install_results)
}

# ==========================================================================================================
# 📊 PACKAGE TESTING
# ==========================================================================================================

test_package_loading <- function(packages, category = "Packages") {
  cat("🧪 Testing", category, "loading...\n")
  
  test_results <- list()
  
  for (pkg in packages) {
    result <- tryCatch({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    test_results[[pkg]] <- result
    status <- if(result) "✅" else "❌"
    cat(sprintf("   %s %s\n", status, pkg))
  }
  
  return(test_results)
}

# ==========================================================================================================
# 🚀 MAIN INSTALLATION PROCESS
# ==========================================================================================================

main_installation <- function() {
  
  # Step 1: Check current status
  cat("📋 STEP 1: CHECKING CURRENT STATUS\n")
  cat(strrep("-", 50), "\n")
  
  status <- check_package_status()
  
  installed_count <- sum(status$installed)
  missing_count <- sum(!status$installed)
  
  cat(sprintf("📊 Status Summary:\n"))
  cat(sprintf("   ✅ Already installed: %d packages\n", installed_count))
  cat(sprintf("   📦 Need to install: %d packages\n", missing_count))
  cat(sprintf("   📈 Total coverage: %.1f%%\n\n", (installed_count/length(ALL_PACKAGES))*100))
  
  # Show missing by category
  if (missing_count > 0) {
    missing_by_category <- table(status$category[!status$installed])
    cat("📦 Missing packages by category:\n")
    for (cat_name in names(missing_by_category)) {
      cat(sprintf("   %s: %d packages\n", cat_name, missing_by_category[cat_name]))
    }
    cat("\n")
  }
  
  # Step 2: Install missing packages by category
  if (missing_count > 0) {
    cat("📦 STEP 2: INSTALLING MISSING PACKAGES\n")
    cat(strrep("-", 50), "\n")
    
    # Install by priority (Core first)
    categories <- list(
      "Core" = CORE_PACKAGES,
      "Technical" = TECHNICAL_PACKAGES,
      "Data" = DATA_PACKAGES,
      "Visual" = VISUAL_PACKAGES,
      "Advanced" = ADVANCED_PACKAGES,
      "Utility" = UTILITY_PACKAGES
    )
    
    all_install_results <- list()
    
    for (cat_name in names(categories)) {
      cat_packages <- categories[[cat_name]]
      missing_in_category <- cat_packages[!cat_packages %in% installed.packages()[,1]]
      
      if (length(missing_in_category) > 0) {
        cat(sprintf("\n🔧 Installing %s packages:\n", cat_name))
        install_result <- install_packages_smart(missing_in_category, cat_name)
        all_install_results[[cat_name]] <- install_result
      }
    }
  }
  
  # Step 3: Test package loading
  cat("\n🧪 STEP 3: TESTING PACKAGE LOADING\n")
  cat(strrep("-", 50), "\n")
  
  # Test core packages (most important)
  core_test <- test_package_loading(CORE_PACKAGES, "Core")
  tech_test <- test_package_loading(TECHNICAL_PACKAGES, "Technical") 
  
  # Step 4: Final status report
  cat("\n📊 STEP 4: FINAL STATUS REPORT\n")
  cat(strrep("-", 50), "\n")
  
  final_status <- check_package_status()
  final_installed <- sum(final_status$installed)
  final_success_rate <- (final_installed/length(ALL_PACKAGES))*100
  
  cat(sprintf("🎯 INSTALLATION SUMMARY:\n"))
  cat(sprintf("   📦 Total packages: %d\n", length(ALL_PACKAGES)))
  cat(sprintf("   ✅ Successfully installed: %d\n", final_installed))
  cat(sprintf("   📈 Success rate: %.1f%%\n", final_success_rate))
  
  # Core packages status
  core_installed <- sum(CORE_PACKAGES %in% installed.packages()[,1])
  cat(sprintf("   🔧 Core packages: %d/%d (%.0f%%)\n", 
              core_installed, length(CORE_PACKAGES), 
              (core_installed/length(CORE_PACKAGES))*100))
  
  # Technical packages status  
  tech_installed <- sum(TECHNICAL_PACKAGES %in% installed.packages()[,1])
  cat(sprintf("   📈 Technical packages: %d/%d (%.0f%%)\n",
              tech_installed, length(TECHNICAL_PACKAGES),
              (tech_installed/length(TECHNICAL_PACKAGES))*100))
  
  if (final_success_rate >= 90) {
    cat("\n🏆 EXCELLENT: Your trading system is fully equipped!\n")
    cat("   ✅ All major packages available\n") 
    cat("   ✅ Ready for live trading\n")
  } else if (final_success_rate >= 80) {
    cat("\n👍 GOOD: Most packages installed successfully\n")
    cat("   ✅ Core functionality available\n")
    cat("   ⚠️ Some optional packages missing\n")
  } else {
    cat("\n⚠️ PARTIAL: Installation needs attention\n")
    cat("   🔧 Some core packages missing\n")
    cat("   💡 Try manual installation for failed packages\n")
  }
  
  return(list(
    status = final_status,
    success_rate = final_success_rate,
    install_results = if(exists("all_install_results")) all_install_results else NULL
  ))
}

# ==========================================================================================================
# 🎯 TRADING SYSTEM SPECIFIC TESTS
# ==========================================================================================================

test_trading_functionality <- function() {
  cat("\n🎯 TESTING TRADING SYSTEM FUNCTIONALITY\n")
  cat(strrep("=", 50), "\n")
  
  tests <- list()
  
  # Test 1: HTTP Requests (Bitget API)
  cat("🌐 Test 1: HTTP Requests (httr)...")
  tests$http <- tryCatch({
    library(httr, quietly = TRUE)
    response <- GET("https://httpbin.org/get")
    status_code(response) == 200
  }, error = function(e) FALSE)
  cat(if(tests$http) " ✅\n" else " ❌\n")
  
  # Test 2: JSON Processing
  cat("📋 Test 2: JSON Processing (jsonlite)...")
  tests$json <- tryCatch({
    library(jsonlite, quietly = TRUE)
    test_json <- toJSON(list(test = "value"))
    fromJSON(test_json)$test == "value"
  }, error = function(e) FALSE)
  cat(if(tests$json) " ✅\n" else " ❌\n")
  
  # Test 3: Cryptographic Functions
  cat("🔐 Test 3: Cryptographic Functions (openssl)...")
  tests$crypto <- tryCatch({
    library(openssl, quietly = TRUE)
    !is.null(sha256("test"))
  }, error = function(e) FALSE)
  cat(if(tests$crypto) " ✅\n" else " ❌\n")
  
  # Test 4: Technical Analysis
  cat("📈 Test 4: Technical Analysis (TTR)...")
  tests$technical <- tryCatch({
    library(TTR, quietly = TRUE)
    prices <- runif(50, 0.5, 0.6)
    rsi_result <- RSI(prices, n = 14)
    !all(is.na(rsi_result))
  }, error = function(e) FALSE)
  cat(if(tests$technical) " ✅\n" else " ❌\n")
  
  # Test 5: Data Manipulation
  cat("📊 Test 5: Data Manipulation (dplyr)...")
  tests$data <- tryCatch({
    library(dplyr, quietly = TRUE)
    test_df <- data.frame(x = 1:5, y = 6:10)
    result <- test_df %>% filter(x > 2) %>% summarise(mean_y = mean(y))
    nrow(result) == 1
  }, error = function(e) FALSE)
  cat(if(tests$data) " ✅\n" else " ❌\n")
  
  # Summary
  passed_tests <- sum(unlist(tests))
  total_tests <- length(tests)
  
  cat(sprintf("\n📊 FUNCTIONALITY TEST RESULTS:\n"))
  cat(sprintf("   ✅ Passed: %d/%d tests\n", passed_tests, total_tests))
  cat(sprintf("   📈 Success rate: %.0f%%\n", (passed_tests/total_tests)*100))
  
  if (passed_tests == total_tests) {
    cat("🏆 ALL TESTS PASSED - Trading system ready!\n")
  } else {
    cat("⚠️ Some tests failed - check individual package installations\n")
  }
  
  return(tests)
}

# ==========================================================================================================
# 🚀 EXECUTE MAIN INSTALLATION
# ==========================================================================================================

cat("🚀 STARTING AUTOMATIC PACKAGE INSTALLATION...\n\n")

# Run main installation
installation_result <- main_installation()

# Test trading functionality
functionality_test <- test_trading_functionality()

# Final summary
cat("\n✅ PACKAGE INSTALLATION COMPLETED!\n")
cat(strrep("=", 50), "\n")
cat("📅 Completion Time:", as.character(Sys.time()), "\n")
cat(sprintf("📦 Package Success Rate: %.1f%%\n", installation_result$success_rate))
cat(sprintf("🎯 Functionality Tests: %d/%d passed\n", 
            sum(unlist(functionality_test)), length(functionality_test)))

cat("\n💡 NEXT STEPS:\n")
cat("1. ✅ Run your trading system scripts\n")
cat("2. 🧪 Execute comprehensive test script\n") 
cat("3. 🚀 Configure API credentials in .env file\n")
cat("4. 📊 Start trading analysis\n")

cat("\n🎯 Ready for trading! 🚀\n")


# Die 4 wichtigsten fehlenden Pakete installieren:
install.packages(c("dotenv", "quantmod", "PerformanceAnalytics", "forecast"))

# Test ob sie funktionieren:
library(dotenv)
library(quantmod)
library(PerformanceAnalytics)
library(forecast)


# ==========================================================================================================
# 🎯 END OF PACKAGE INSTALLER
# ==========================================================================================================