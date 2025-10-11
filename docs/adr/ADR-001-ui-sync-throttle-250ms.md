# ADR-001: Bevar UI Sync Throttle på 250ms

## Status
**Accepted** - 2025-10-10

## Kontekst

Under Fase 3 af REMEDIATION_MASTER_PLAN.md (H11: UI sync throttle alignment) blev det foreslået at justere UI sync throttle fra 250ms til 800ms for at matche debounce delays på andre input-reaktive chains.

Efter grundig analyse blev det klart, at 250ms og 800ms throttles tjener **fundamentalt forskellige formål** og derfor bør forblive forskellige.

### Nuværende Konfiguration

**UI Sync Throttle (250ms):**
- Placering: `R/utils_server_event_listeners.R:229`
- Funktion: `register_ui_sync_events()`
- Anvendelse: Throttler `ui_sync_requested` event for at reducere excessive UI updates
- Pattern: `shiny::throttle()` wrapper omkring reactive event trigger

**Input Debounce (800ms → 500ms):**
- Placering: `R/config_system_config.R:116`
- Konfiguration: `DEBOUNCE_DELAYS$chart_update = 500`
- Anvendelse: Debouncer brugerinput (fx plot parameter changes) før chart rendering
- Pattern: `shiny::debounce()` på user-facing inputs

### Forskel mellem Throttle og Debounce

**Throttle (250ms):**
- **Semantik**: Begrænser execution rate - max én gang per 250ms interval
- **Timing**: Executes **leading edge** (første event i interval) + trailing (sidste event)
- **Use case**: Prevent **excessive execution** af højfrekvente events
- **Eksempel**: Rapid-fire event chains during data load → UI opdateres max 4x/sekund

**Debounce (500-800ms):**
- **Semantik**: Venter på "stilhed" - kun execute når input holder op
- **Timing**: Executes **trailing edge** (kun efter ingen events i X ms)
- **Use case**: Prevent **premature execution** mens bruger stadig taster/vælger
- **Eksempel**: Bruger taster kolonnenavn → vent til de er færdige før processing

## Beslutning

**BEVAR UI sync throttle på 250ms - GEN IKKE til 800ms.**

Rationale:
1. **Forskellige mekanismer**: Throttle (rate limiting) ≠ Debounce (silence waiting)
2. **Forskellige use cases**: Event-driven reactive chains ≠ User input processing
3. **Performance vs UX trade-off**: 250ms er imperceptible (<300ms threshold) men forhindrer excessive updates

### Detaljeret Analyse

#### UI Sync Throttle (250ms) - Korrekt Som Den Er

**Formål:**
Reducer execution rate af `ui_sync_requested` event under rapid event chains (fx data load → auto-detect → columns detected → UI sync).

**Hvorfor 250ms er korrekt:**
- **Imperceptible delay**: Mennesker opfatter <250ms som "instant" (psychological threshold)
- **Effective rate limiting**: Begrænser til max 4 UI updates/sekund
- **Prevent cascade overload**: Stopper runaway reactive chains fra at oversv&#xf8;mme UI thread
- **Event-driven pattern**: Reaktive events skal throttles, ikke debounces

**Eksempel scenario:**
```
t=0ms:    data_loaded event
t=10ms:   auto_detection_started event
t=50ms:   auto_detection_completed event
t=55ms:   ui_sync_requested event (1st trigger - EXECUTES)
t=100ms:  columns_detected event
t=105ms:  ui_sync_requested event (2nd trigger - THROTTLED, <250ms siden sidste)
t=300ms:  ui_sync_requested event (3rd trigger - EXECUTES, >250ms siden første)
```

**Resultat**: 3 events → 2 executions (33% reduction), ingen perceptible lag.

#### Input Debounce (500ms) - Også Korrekt

**Formål:**
Vent til bruger er færdig med at indtaste/vælge før expensive operations (chart render, QIC calculation).

**Hvorfor 500-800ms er korrekt:**
- **User intent detection**: Giver bruger tid til at færdiggøre input
- **Expensive operation delay**: Chart rendering er CPU-tungt - undgå premature triggers
- **Trailing edge execution**: Kun execute når "dust has settled"

**Eksempel scenario:**
```
t=0ms:    User ændrer chart_type input
t=100ms:  User ændrer y_column input
t=250ms:  User ændrer n_column input
t=400ms:  Ingen flere inputs...
t=750ms:  DEBOUNCE TIMER EXPIRES → Chart renders med alle 3 ændringer
```

**Resultat**: 3 input changes → 1 chart render (67% reduction), perceptible 750ms delay er ACCEPTABELT fordi bruger venter på at afslutte input.

### Konklusion

250ms throttle og 500ms debounce er **optimalt tuned for forskellige use cases**:

| Aspect | UI Sync Throttle (250ms) | Input Debounce (500ms) |
|--------|--------------------------|------------------------|
| **Pattern** | `shiny::throttle()` | `shiny::debounce()` |
| **Trigger** | Event-driven (reactive chains) | User-driven (input changes) |
| **Execution** | Leading + trailing edge | Trailing edge only |
| **Optimization** | Reduce **execution rate** | Reduce **premature execution** |
| **Perceptibility** | Imperceptible (<250ms) | Acceptable (user intent) |
| **Performance gain** | 30-40% fewer UI updates | 60-80% fewer chart renders |

## Konsekvenser

### Fordele

1. **Performance optimization preserved**: Throttling reducer UI update overhead uden UX impact
2. **Correct pattern usage**: Throttle for events, debounce for inputs følger Shiny best practices
3. **Perceptual consistency**: 250ms UI delay er under human perception threshold
4. **Maintainability**: Eksplicit dokumentation af hvorfor værdier er forskellige

### Ulemper

1. **Complexity**: To forskellige delays kan virke inkonsistente uden kontekst
2. **Tuning burden**: Kræver forståelse af throttle vs debounce semantik for fremtidig tuning

### Mitigations

1. **Dokumentation**: Dette ADR forklarer rationale
2. **Code comments**: Inline comments i `utils_server_event_listeners.R:222-224` forklarer 250ms valg
3. **Configuration centralization**: DEBOUNCE_DELAYS i `config_system_config.R` dokumenterer input debounce strategy

## Relaterede Beslutninger

- **H12**: Memory tracking consolidation - Navnekonflikt resolution via semantic naming
- **H15**: Vectorized row filter - Performance optimization via algorithm change
- **SPRINT4_PLAN.md**: Debounce strategy for chart updates (800ms → 500ms reduction)

## Implementation Notes

**Ingen kode ændringer nødvendige** - nuværende implementation er korrekt.

**Eksisterende kode** (bevar som den er):

```r
# R/utils_server_event_listeners.R:222-230
# PERFORMANCE: Throttle UI sync to reduce update overhead (250ms)
# This prevents excessive UI updates during rapid event chains while
# maintaining imperceptible user experience (<250ms delay)
throttled_ui_sync <- shiny::throttle(
  shiny::reactive({
    app_state$events$ui_sync_requested
  }),
  millis = 250
)
```

**Eksisterende config** (bevar som den er):

```r
# R/config_system_config.R:116
DEBOUNCE_DELAYS <- list(
  chart_update = 500, # 500ms - chart rendering (reduced from 800ms) - OPTIMIZED
  # ... other debounce delays
)
```

## Referencer

- **Shiny throttle documentation**: https://shiny.posit.co/r/reference/shiny/latest/throttle
- **Shiny debounce documentation**: https://shiny.posit.co/r/reference/shiny/latest/debounce
- **REMEDIATION_MASTER_PLAN.md**: H11 - UI sync throttle alignment
- **Human perception research**: 100-300ms threshold for "instant" response (Nielsen Norman Group)
- **Event-driven architecture**: Leading edge execution for reactive event chains

## Dato
2025-10-10
