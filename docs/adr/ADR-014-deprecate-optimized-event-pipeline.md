# ADR-014: Deprecation af Optimized Event Pipeline

## Status
Accepted

## Kontekst
Under Technical Debt Remediation (Fase 4, M7) identificerede vi `R/utils_server_performance_opt.R` som ubrugt legacy-kode med følgende problemer:

1. **Duplicate execution warnings**: Filen eksporterer `setup_optimized_event_listeners()` med eksplicit advarsel:
   _"WARNING: setup_optimized_event_listeners creates duplicate pipeline execution!"_

2. **Ingen anvendelse**: Grep-søgning viste 0 eksterne referencer i codebase - kun selvreferencer i filen selv.

3. **Deprecated funktioner**: Filen indeholder `create_data_signature()` markeret som DEPRECATED (allerede migreret til `utils_data_signatures.R`).

4. **Maintenance overhead**: Eksporteret funktion i NAMESPACE øger API surface uden værdi.

5. **Forvirringsrisiko**: Risiko for utilsigtet aktivering → dobbelt event processing og race conditions.

### Historisk kontekst
Filen repræsenterer et tidligere eksperiment med performance-optimeret event pipeline. Den standardiserede event-bus (via `setup_event_listeners()`) viste sig mere vedligeholdelig og blev den anbefalede løsning.

## Beslutning
Vi **deprecater og fjerner** `utils_server_performance_opt.R` fra runtime-kodebasen:

1. Filen flyttes til `candidates_for_deletion/utils_server_performance_opt.R`
2. Man-side fjernet (`man/setup_optimized_event_listeners.Rd`)
3. NAMESPACE regenereret for at fjerne eksporter
4. Ingen erstatning implementeres - standardiseret event-bus er tilstrækkelig

**Rationale:**
- Ubrugt kode øger cognitive load for udviklere
- Duplicate execution warnings indikerer arkitektonisk konflikt
- Standard event-bus dækker alle use cases
- Performance-data understøtter ikke behov for alternativ pipeline

## Konsekvenser

### Positive
- **Reduceret kompleksitet**: Fjerner konkurrerende event-systemer
- **Klarere arkitektur**: Én standardiseret event-bus
- **Mindre NAMESPACE**: Færre eksporterede funktioner
- **Lavere maintenance-byrde**: Mindre kode at vedligeholde

### Negative
- **Historisk tab**: Hvis performance-optimering skulle blive relevant igen, skal løsningen genopfindes
- **Mitigering**: Filen bevares i `candidates_for_deletion/` og git-historik

### Påvirkede komponenter
- **Ingen runtime impact**: Filen var ubrugt
- **Build pipeline**: NAMESPACE og dokumentation opdateret
- **Tests**: Ingen tests eksisterede for funktionaliteten

## Alternativer overvejet

### Option B: Feature flag implementation
```r
if (getOption("spc.use_optimized_listeners", FALSE)) {
  setup_optimized_event_listeners(...)
} else {
  setup_event_listeners(...)
}
```

**Afvist fordi:**
- Kræver tests for begge modes
- Øger kompleksitet uden dokumenteret performance-gevinst
- Duplicate execution warnings indikerer fundamentale konflikter

## Implementering
- **Dato**: 2025-10-11
- **Commit**: M7 - Deprecate utils_server_performance_opt.R (duplicate event pipeline)
- **Verificering**: `devtools::check()` bestået uden nye advarsler

## Opfølgning
- Efter 3-6 måneder: Hvis ingen behov opstår, permanent slet fra `candidates_for_deletion/`
- Hvis performance-behov genopstår: Konsulter git-historik og denne ADR før genimplementering

## Referencer
- `todo/technical-debt-remediation.md` - M7: Beslut skæbne for utils_server_performance_opt.R
- `R/utils_server_event_listeners.R` - Standardiseret event-bus (nuværende løsning)
- `CLAUDE.md` - Event-Driven Architecture principles (sektion 10.1)
