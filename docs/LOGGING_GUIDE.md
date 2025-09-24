# SPC App Logging Guide

Logging-systemet understøtter fleksible log-niveauer for fejlsøgning og produktionsovervågning.

## Log Niveauer

| Niveau | Beskrivelse | Hvornår at bruge |
|--------|-------------|------------------|
| `DEBUG` | Alle beskeder (mest detaljeret) | Fejlsøgning af reactive flows, troubleshooting |
| `INFO`  | Informative beskeder og højere | Overvågning af app-adfærd, development |
| `WARN`  | Kun advarsler og fejl | Standard produktionsindstilling |
| `ERROR` | Kun fejlbeskeder | Minimal logging, kritiske fejl |

## Brug i run_app()

### Grundlæggende eksempler:

```r
# Standard brug (auto-detekterer miljø)
run_app()

# Debug mode til fejlsøgning
run_app(log_level = "DEBUG")

# Produktionsmode med minimal logging
run_app(log_level = "WARN")

# Kombination med andre parametre
run_app(port = 4040, log_level = "DEBUG", enable_test_mode = TRUE)
```

### Fejlsøgning scenarios:

```r
# Problem med valueboxes ikke opdaterer
run_app(log_level = "DEBUG")
# Se efter: "VISUALIZATION" logs i console

# Problem med data load
run_app(log_level = "INFO", enable_test_mode = TRUE)
# Se efter: "TEST_MODE" og "DATA_PROC" logs

# Performance issues
run_app(log_level = "DEBUG")
# Se efter: reactive trigger patterns og timing
```

## Runtime Log Niveau Ændringer

Du kan også ændre log niveau mens appen kører:

```r
# Set via convenience funktioner
set_log_level_development()  # DEBUG niveau
set_log_level_info()         # INFO niveau
set_log_level_production()   # WARN niveau
set_log_level_quiet()        # ERROR niveau

# Eller direkte
set_log_level("DEBUG")

# Tjek nuværende niveau
get_log_level_name()  # Returns: "DEBUG", "INFO", etc.
```

## Praktiske Use Cases

### 1. Debugging af "Beregner..." Problem

```r
# Start med debug logging
run_app(log_level = "DEBUG", port = 4040)

# Se efter disse patterns i console:
# - "spc_plot reactive triggered" - hvor mange gange?
# - "Setting anhoej_results" vs "SKIPPING anhoej_results"
# - "Status determination" - hvad er anhoej_exists værdien?
```

### 2. Performance Monitoring

```r
# INFO niveau viser nøgle operationer uden for meget støj
run_app(log_level = "INFO")

# Se efter:
# - Data load times
# - Cache hit/miss ratios
# - Event processing flow
```

### 3. Produktionsdrift

```r
# Minimal logging, kun advarsler og fejl
run_app(log_level = "WARN", launch_browser = TRUE)

# Eller environment variable
Sys.setenv(SPC_LOG_LEVEL = "WARN")
run_app()
```

## Miljøvariabel Konfiguration

Du kan også sætte log niveau via miljøvariabel:

```bash
# I terminal før R start
export SPC_LOG_LEVEL=DEBUG
R -e "run_app()"

# Eller i R
Sys.setenv(SPC_LOG_LEVEL = "DEBUG")
run_app()
```

## Auto-Detection Logik

Hvis intet log niveau specificeres:

- **Interactive/Development**: Bruger `INFO`
- **Production**: Bruger `WARN`
- **Eksisterende miljøvariabel**: Respekterer `SPC_LOG_LEVEL`

Dette sikrer fornuftige defaults mens du stadig har fuld kontrol.