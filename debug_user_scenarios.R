# DEBUG USER SCENARIOS - Systematisk fejlidentifikation
# ================================================================
#
# Dette script definerer problematiske brugersekvenser til debugging
# af Shiny-kontekst specifikke fejl efter Phase 4-5 refactoring.
#
# USAGE:
# 1. Kør appen med: export SHINY_DEBUG_MODE=TRUE; export SPC_LOG_LEVEL=DEBUG
# 2. Følg scenarierne step-by-step i browser
# 3. Observer debug logs i console for anomalier

# SCENARIO TEST CHECKLIST
# ================================================================

cat("=== DEBUG USER SCENARIOS ===\n")
cat("Følgende scenarios skal testes systematisk:\n\n")

# Scenario 1: Hurtig fil upload → auto-detect → manual column change
cat("🔍 SCENARIO 1: Hurtig fil upload workflow\n")
cat("Steps:\n")
cat("1. Upload CSV fil (fx spc_exampledata1.csv)\n")
cat("2. Vent på auto-detect completion\n")
cat("3. Manually change X-column dropdown\n")
cat("4. Check at selectize inputs er synkroniseret\n")
cat("5. Observer reactive chain i debug logs\n")
cat("EXPECTED: Auto-detect resultater skal ikke overskrives\n\n")

# Scenario 2: Session restore konflikter
cat("🔍 SCENARIO 2: Session restore conflicts\n")
cat("Steps:\n")
cat("1. Upload data og configure columns\n")
cat("2. Reload browser (trigger session restore)\n")
cat("3. Upload ny fil immediately efter restore\n")
cat("4. Check for state inconsistencies\n")
cat("5. Observer dual-state sync i logs\n")
cat("EXPECTED: Ingen state desync mellem values$ og app_state\n\n")

# Scenario 3: Test mode transitions
cat("🔍 SCENARIO 3: Test mode switching\n")
cat("Steps:\n")
cat("1. Start med TEST_MODE_AUTO_LOAD <- TRUE\n")
cat("2. Vent på auto-load completion\n")
cat("3. Switch til manual mode (reload uden TEST_MODE)\n")
cat("4. Upload manual fil\n")
cat("5. Switch tilbage til test mode\n")
cat("EXPECTED: Smooth transitions uden observer conflicts\n\n")

# Scenario 4: Race condition testing
cat("🔍 SCENARIO 4: Race conditions ved hurtige clicks\n")
cat("Steps:\n")
cat("1. Upload fil og trigger auto-detect\n")
cat("2. Click multiple times på column dropdowns UNDER auto-detect\n")
cat("3. Check at UI inputs ikke bliver locked\n")
cat("4. Observer observer priorities i debug logs\n")
cat("EXPECTED: Observer priorities skal forhindre race conditions\n\n")

# Scenario 5: Browser refresh under processing
cat("🔍 SCENARIO 5: Browser refresh timing\n")
cat("Steps:\n")
cat("1. Upload stor fil (> 1MB)\n")
cat("2. Refresh browser MID-PROCESSING\n")
cat("3. Check at app state recovers gracefully\n")
cat("4. Observer session cleanup i logs\n")
cat("EXPECTED: Graceful error handling og state recovery\n\n")

# DEBUG COMMAND REFERENCE
cat("=== DEBUG COMMANDS ===\n")
cat("Kør appen med maximum debugging:\n")
cat("export SHINY_DEBUG_MODE=TRUE\n")
cat("export SPC_LOG_LEVEL=DEBUG\n")
cat("R -e \"shiny::runApp(port = 4040)\"\n\n")

cat("Filter logs efter komponenter:\n")
cat("grep 'AUTO_DETECT' debug.log\n")
cat("grep 'UI_SYNC' debug.log\n")
cat("grep 'STATE_VALIDATOR' debug.log\n\n")

# LOGGING PATTERNS TO WATCH FOR
cat("=== FEJL-PATTERNS AT HOLDE ØJE MED ===\n")
cat("❌ 'State inconsistencies found' - Dual-state desync\n")
cat("❌ 'UI sync pending, would override' - Race condition\n")
cat("❌ 'outside reactive' under forventet reactive context\n")
cat("❌ Observer priority violations\n")
cat("❌ Memory leaks ved session cleanup\n")
cat("❌ Auto-detect results overwritten af column management\n\n")

cat("✅ Start debugging session nu!\n")